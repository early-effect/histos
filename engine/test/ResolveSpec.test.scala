package histos.resolve

import histos.jdk.{JdkMajor, JdkVersion, Sha256}
import histos.zinc.{CompileError, CompileRequest, CompileToolchain, JvmCompile, ResolvedJdk, ScalacFlags}
import histos.{Dep, DepKind, Module, Platform, Platforms, ProjectGraph, ProjectName, ScalaVersion, ScalaVersions, SourcePos}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.jar.{JarEntry, JarOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO, durationInt}
import zio.test.*

object ResolveSpec extends ZIOSpecDefault:
  private val v390 = ScalaVersion.trusted("3.9.0")
  private val v213 = ScalaVersion.trusted("2.13.16")
  private val core = project("core")
  private val lib  = Dep.trusted("dev.histos", "lib", "1.0.0", DepKind.Java)
  private val base = Dep.trusted("dev.histos", "base", "1.1.0", DepKind.Java)
  private val binary = Dep.trusted("dev.zio", "zio", "2.1.26", DepKind.Binary)
  private val full   = Dep.trusted("dev.zio", "zio", "2.1.26", DepKind.Full)
  private val javaDep = Dep.trusted("org.scala-lang", "scala-library", "2.13.16", DepKind.Java)
  private val digest = Sha256.of(zio.Chunk.fromArray("lock".getBytes(StandardCharsets.UTF_8)))

  private val names = suite("artifact names")(
    test("jvm, js, and native suffixes are one function"):
      assertTrue(
        ArtifactName.of(binary, v390, Platform.Jvm) == "zio_3",
        ArtifactName.of(binary, v213, Platform.Jvm) == "zio_2.13",
        ArtifactName.of(full, v390, Platform.Jvm) == "zio_3.9.0",
        ArtifactName.of(binary, v390, Platform.Js) == "zio_sjs1_3",
        ArtifactName.of(full, v390, Platform.Js) == "zio_sjs1_3.9.0",
        ArtifactName.of(binary, v390, Platform.Native) == "zio_native0.5_3",
        ArtifactName.of(full, v390, Platform.Native) == "zio_native0.5_3.9.0",
        ArtifactName.of(javaDep, v390, Platform.Jvm) == "scala-library",
        ArtifactName.of(javaDep, v390, Platform.Js) == "scala-library",
        ArtifactName.of(javaDep, v390, Platform.Native) == "scala-library",
      )
    ,
    test("declared repositories stay ahead of central"):
      val maven = Repository.Maven(RepoUrl.file(Path.of("/tmp/repo")))
      assertTrue(Repositories.ordered(Repositories(Repository.Central, List(maven))) == List(maven, Repository.Central))
    ,
    test("a file url matches the repository that served it"):
      RepoUrl.from("file:///tmp/repo") match
        case Right(url) =>
          assertTrue(
            Repository.serves(Repository.Maven(url), "file:/tmp/repo/dev/histos/lib-1.0.0.jar"),
            Repository.serves(Repository.Central, "https://repo1.maven.org/maven2/org/foo/bar.jar"),
            !Repository.serves(Repository.Central, "https://example.test/org/foo/bar.jar"),
          )
        case Left(_) => assertTrue(false)
  )

  private val lockFile = suite("dependency lock")(
    test("comments and a full cell round-trip"):
      val text =
        s"""# pinned
           |
           |cell core 3.9.0 jvm
           |direct dev.zio zio 2.1.26 binary
           |artifact dev.zio zio_3 2.1.26 ${digest.value} central
           |evict dev.zio zio_3 2.1.20 2.1.26
           |""".stripMargin
      DependencyLock.parse(text) match
        case Right(lock) =>
          val again = DependencyLock.parse(DependencyLock.render(lock))
          assertTrue(again == Right(lock), lock.cells.length == 1)
        case Left(_) => assertTrue(false)
    ,
    test("a bad line, a short field, a repeated cell, a bad digest, and a bad repository are lock errors"):
      val short = "cell core 3.9.0 jvm\ndirect dev.zio"
      val twice =
        """cell core 3.9.0 jvm
          |cell core 3.9.0 jvm
          |""".stripMargin
      val sha = s"artifact dev.zio zio_3 2.1.26 zzzz central"
      val repo = s"artifact dev.zio zio_3 2.1.26 ${digest.value} nowhere"
      assertTrue(
        DependencyLock.parse("nope") == Left(LockError.BadLine("nope")),
        DependencyLock.parse("direct dev.zio zio 2.1.26 binary") == Left(LockError.Missing(LockField.Cell)),
        DependencyLock.parse(short) == Left(LockError.Missing(LockField.Direct)),
        DependencyLock.parse(twice) == Left(LockError.DuplicateCell(core, v390, Platform.Jvm, ClasspathScope.Compile)),
        DependencyLock.parse(s"cell core 3.9.0 jvm\n$sha") == Left(LockError.BadSha256("zzzz")),
        DependencyLock.parse(s"cell core 3.9.0 jvm\n$repo") == Left(LockError.BadRepository("nowhere")),
        DependencyLock.parse("") == Left(LockError.Empty),
      )
    ,
    test("a test cell follows the compile cell, and a repeated test cell is refused"):
      val text =
        """cell core 3.9.0 jvm
          |direct dev.histos lib 1.0.0 java
          |test core 3.9.0 jvm
          |direct dev.histos lib 1.0.0 java
          |direct dev.histos probe 1.0.0 java
          |""".stripMargin
      val twice =
        """test core 3.9.0 jvm
          |test core 3.9.0 jvm
          |""".stripMargin
      DependencyLock.parse(text) match
        case Right(lock) =>
          val again = DependencyLock.parse(DependencyLock.render(lock))
          assertTrue(
            again == Right(lock),
            lock.cells.map(_.scope) == List(ClasspathScope.Compile, ClasspathScope.Test),
            lock.cells.flatMap(_.directs.map(_.name)) == List("lib", "lib", "probe"),
            DependencyLock.parse(twice) == Left(LockError.DuplicateCell(core, v390, Platform.Jvm, ClasspathScope.Test)),
          )
        case Left(_) => assertTrue(false)
  )

  private val fetch = suite("file repository")(
    test("a range is rejected before fetch, and a hyphen is an exact revision"):
      ZIO.scoped:
        for
          open  <- opened
          plus  <- resolve(open, open.cell.copy(dependencies = List(Dep.trusted("dev.histos", "lib", "1.+", DepKind.Java))))
          latest <- resolve(open, open.cell.copy(dependencies = List(Dep.trusted("dev.histos", "lib", "latest.release", DepKind.Java))))
          bracket <- resolve(open, open.cell.copy(dependencies = List(Dep.trusted("dev.histos", "lib", "[1.0,2.0]", DepKind.Java))))
          milestone <- resolve(open, open.cell.copy(dependencies = List(Dep.trusted("dev.histos", "missing", "1.0.0-M1", DepKind.Java))))
          snapshot <- resolve(open, open.cell.copy(dependencies = List(Dep.trusted("dev.histos", "missing", "0.5.13-SNAPSHOT", DepKind.Java))))
        yield assertTrue(
          plus == Left(ResolveError.Range(Dep.trusted("dev.histos", "lib", "1.+", DepKind.Java))),
          latest == Left(ResolveError.Range(Dep.trusted("dev.histos", "lib", "latest.release", DepKind.Java))),
          bracket == Left(ResolveError.Range(Dep.trusted("dev.histos", "lib", "[1.0,2.0]", DepKind.Java))),
          milestone == Left(ResolveError.NotFound("dev.histos", "missing", "1.0.0-M1")),
          snapshot == Left(ResolveError.NotFound("dev.histos", "missing", "0.5.13-SNAPSHOT")),
        )
    ,
    test("a js cell fetches the sjs1 jar and does not ask the network"):
      ZIO.scoped:
        for
          open <- opened
          _ <- ZIO.attempt(writeMaven(
                 open.maven,
                 "dev.zio",
                 "zio_sjs1_3",
                 "2.1.26",
                 emptyJar,
                 pom("dev.zio", "zio_sjs1_3", "2.1.26", None),
               ))
          cell = ResolveCell(
                   core,
                   List(binary),
                   v390,
                   Platform.Js,
                   open.repositories,
                 )
          classpath <- ZIO.serviceWithZIO[Resolve](_.resolve(cell)).provide(Resolve.live(open.cache)).either
          names = classpath.map(_.artifacts.map(_.name))
        yield assertTrue(names == Right(List("zio_sjs1_3")))
    ,
    test("a native cell fetches the native0.5 jar and does not ask the network"):
      ZIO.scoped:
        for
          open <- opened
          _ <- ZIO.attempt(writeMaven(
                 open.maven,
                 "dev.zio",
                 "zio_native0.5_3",
                 "2.1.26",
                 emptyJar,
                 pom("dev.zio", "zio_native0.5_3", "2.1.26", None),
               ))
          cell = ResolveCell(
                   core,
                   List(binary),
                   v390,
                   Platform.Native,
                   open.repositories,
                 )
          classpath <- ZIO.serviceWithZIO[Resolve](_.resolve(cell)).provide(Resolve.live(open.cache)).either
          names = classpath.map(_.artifacts.map(_.name))
        yield assertTrue(names == Right(List("zio_native0.5_3")))
    ,
    test("one transitive is evicted onto the direct revision"):
      ZIO.scoped:
        for
          open      <- opened
          classpath <- ZIO.serviceWithZIO[Resolve](_.resolve(open.cell)).provide(Resolve.live(open.cache)).either
          names = classpath.map(_.artifacts.map(artifact => s"${artifact.name}:${artifact.revision}"))
          evicted = classpath.map(_.evictions)
        yield assertTrue(
          open.javac == 0 && open.javacText.isEmpty,
          names == Right(List("base:1.1.0", "lib:1.0.0")) || names == Right(List("lib:1.0.0", "base:1.1.0")),
          evicted == Right(List(Eviction("dev.histos", "base", "1.0.0", "1.1.0"))),
        )
    ,
    test("a changed direct is lock drift and does not fetch"):
      ZIO.scoped:
        for
          open <- opened
          lock = DependencyLock(List(LockedCell(
                   open.cell.module,
                   open.cell.scalaVersion,
                   open.cell.platform,
                   List(LockedDirect("dev.histos", "lib", "9.9.9", DepKind.Java)),
                   Nil,
                   Nil,
                 )))
          out     <- Resolve.materialize(open.cell, lock).provide(Resolve.live(open.cache)).either
          touched  = Files.exists(open.cache)
        yield assertTrue(
          out == Left(ResolveError.LockDrift(open.cell.module, open.cell.scalaVersion, open.cell.platform, ClasspathScope.Compile)),
          !touched,
        )
    ,
    test("a locked checksum that disagrees with the bytes fails, including --check"):
      ZIO.scoped:
        for
          open    <- opened
          fresh   <- ZIO.serviceWithZIO[Resolve](_.resolve(open.cell)).provide(Resolve.live(open.cache))
          held     = hold(open.cell, fresh)
          flipped  = tamper(held)
          fetched <- Resolve.materialize(open.cell, flipped).provide(Resolve.live(open.cache)).either
          _       <- Resolve.update(open.root, graph(open.cell.dependencies), open.repositories).provide(Resolve.live(open.cache))
          file     = Resolve.lockPath(open.root)
          text    <- ZIO.attempt(Files.readString(file))
          parsed   = DependencyLock.parse(text).map(lock => tamper(lock))
          _       <- ZIO.foreachDiscard(parsed.toOption)(lock => ZIO.attempt(Files.writeString(file, DependencyLock.render(lock))))
          checked <- Resolve.check(open.root, graph(open.cell.dependencies), open.repositories).provide(Resolve.live(open.cache)).either
          wrote    = parsed.isRight
        yield assertTrue(
          wrote,
          fetched match
            case Left(ResolveError.Checksum(_, expected, actual)) => expected != actual
            case _                                                 => false
          ,
          checked match
            case Left(ResolveError.Checksum(_, expected, actual)) => expected != actual
            case _                                                 => false
          ,
        )
    ,
    test("a test dependency is a second cell and is not on the compile classpath"):
      ZIO.scoped:
        for
          open <- opened
          _ <- ZIO.attempt(writeMaven(
                 open.maven,
                 "dev.histos",
                 "probe",
                 "1.0.0",
                 emptyJar,
                 pom("dev.histos", "probe", "1.0.0", None),
               ))
          probe = Dep.trusted("dev.histos", "probe", "1.0.0", DepKind.Java)
          built = ProjectGraph.trusted(List(Module(
                     core,
                     Platforms(Platform.Jvm, Nil),
                     ScalaVersions(v390, Nil),
                     List(lib),
                     Nil,
                     SourcePos("histos.scala", 1, 1),
                     testDependencies = List(probe),
                   )))
          _ <- Resolve.update(open.root, built, open.repositories).provide(Resolve.live(open.cache))
          checked <- Resolve.check(open.root, built, open.repositories).provide(Resolve.live(open.cache))
          text <- ZIO.attempt(Files.readString(Resolve.lockPath(open.root)))
          parsed = DependencyLock.parse(text)
        yield assertTrue:
          checked == "core 3.9.0 jvm\ncore 3.9.0 jvm test" &&
            (parsed match
              case Right(lock) =>
                val compile = lock.cells.find(_.scope == ClasspathScope.Compile)
                val tested = lock.cells.find(_.scope == ClasspathScope.Test)
                compile.exists(cell => cell.directs.map(_.name) == List("lib") && !cell.artifacts.exists(_.name == "probe")) &&
                  tested.exists(cell => cell.directs.map(_.name) == List("lib", "probe") && cell.artifacts.exists(_.name == "probe"))
              case Left(_) => false
            )
    ,
    test("compile reads the lock and calls a class from the resolved jar"):
      ZIO.scoped:
        for
          open   <- opened
          path   <- Resolve.update(open.root, graph(open.cell.dependencies), open.repositories).provide(Resolve.live(open.cache))
          file    = Resolve.lockPath(open.root)
          exists  = Files.isRegularFile(file)
          text   <- ZIO.attempt(if exists then Files.readString(file) else "")
          parsed  = DependencyLock.parse(text)
          again   = parsed.flatMap(lock => DependencyLock.parse(DependencyLock.render(lock)))
          checked <- Resolve.check(open.root, graph(open.cell.dependencies), open.repositories).provide(Resolve.live(open.cache))
          materialized <- parsed match
            case Right(lock) => Resolve.materialize(open.cell, lock).provide(Resolve.live(open.cache)).either
            case Left(error) => ZIO.succeed(Left(ResolveError.Lock(error)))
          source <- ZIO.attempt(write(open.root, "App.scala", app))
          jdk     = running
          request = materialized match
            case Right(classpath) =>
              CompileRequest(
                CompileToolchain(JvmCompile.Scala390, Platform.Jvm, jdk, ScalacFlags.empty),
                List(source),
                classpath.artifacts.map(_.path),
                open.root.resolve("classes"),
                open.root.resolve("analysis"),
                classpath.artifacts.map(_.sha256),
              )
            case Left(_) =>
              CompileRequest(
                CompileToolchain(JvmCompile.Scala390, Platform.Jvm, jdk, ScalacFlags.empty),
                List(source),
                Nil,
                open.root.resolve("classes"),
                open.root.resolve("analysis"),
                Nil,
              )
          compiled <- JvmCompile.compile(request).either
          classFile = Files.isRegularFile(open.root.resolve("classes").resolve("App.class"))
          detail = compiled match
            case Left(CompileError.Failed(diagnostics)) => diagnostics.map(_.text).mkString("\n")
            case Left(other)                            => other.message
            case Right(_)                               => ""
          missing <- Resolve.verify(open.root.resolve("absent"), graph(Nil), open.repositories).provide(Resolve.live(open.cache)).either
        yield assertTrue(
          open.javac == 0 && open.javacText.isEmpty,
          path == file.toString && exists,
          parsed == again,
          checked == "core 3.9.0 jvm",
          materialized.isRight && compiled.isRight && classFile && detail.isEmpty,
          missing == Left(ResolveError.NoLock(Resolve.lockPath(open.root.resolve("absent")))),
        )
  )

  private val app =
    """object App:
      |  def greet: String = demo.Greeting.hi()
      |""".stripMargin

  private val greetingSource =
    """package demo;
      |public final class Greeting {
      |  public static String hi() { return "hi"; }
      |}
      |""".stripMargin

  private final case class Opened(
      root: Path,
      cache: Path,
      maven: Path,
      cell: ResolveCell,
      repositories: Repositories,
      javac: Int,
      javacText: String,
  )

  private def resolve(open: Opened, cell: ResolveCell): ZIO[Any, Nothing, Either[ResolveError, Classpath]] =
    ZIO.serviceWithZIO[Resolve](_.resolve(cell)).provide(Resolve.live(open.cache)).either

  private def opened: ZIO[Scope, Throwable, Opened] =
    for
      root <- temp
      repo <- ZIO.attempt(Files.createDirectories(root.resolve("repo")).toRealPath())
      compiled <- ZIO.attempt(compileGreeting(root))
      _ <- ZIO.when(compiled.code == 0)(ZIO.attempt(publish(repo, compiled.bytes)))
    yield
      val repositories = Repositories(Repository.Maven(RepoUrl.file(repo)), Nil)
      Opened(root, root.resolve("cache"), repo, ResolveCell(core, List(lib, base), v390, Platform.Jvm, repositories), repositories, compiled.code, compiled.text)

  private final case class Compiled(code: Int, text: String, bytes: Array[Byte])

  private def compileGreeting(root: Path): Compiled =
    val sources = Files.createDirectories(root.resolve("java"))
    val classes = Files.createDirectories(root.resolve("jclasses"))
    val file    = sources.resolve("Greeting.java")
    Files.writeString(file, greetingSource)
    val javac   = Path.of(java.lang.System.getProperty("java.home")).resolve("bin").resolve("javac")
    val process = new ProcessBuilder(javac.toString, "-d", classes.toString, file.toString).redirectErrorStream(true).start()
    val text    = new String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
    val code    = process.waitFor()
    val bytes   = if code == 0 then jarBytes(classes) else Array.emptyByteArray
    Compiled(code, text, bytes)

  private def publish(root: Path, greeting: Array[Byte]): Unit =
    writeMaven(root, "dev.histos", "base", "1.0.0", emptyJar, pom("dev.histos", "base", "1.0.0", None))
    writeMaven(root, "dev.histos", "base", "1.1.0", emptyJar, pom("dev.histos", "base", "1.1.0", None))
    writeMaven(root, "dev.histos", "lib", "1.0.0", greeting, pom("dev.histos", "lib", "1.0.0", Some("base", "1.0.0")))

  private def writeMaven(root: Path, group: String, name: String, revision: String, jar: Array[Byte], pomText: String): Unit =
    val dir = (group.split("\\.").toList :+ name :+ revision).foldLeft(root)(_.resolve(_))
    Files.createDirectories(dir)
    val jarName  = s"$name-$revision.jar"
    val pomName  = s"$name-$revision.pom"
    val pomBytes = pomText.getBytes(StandardCharsets.UTF_8)
    Files.write(dir.resolve(jarName), jar)
    Files.write(dir.resolve(pomName), pomBytes)
    Files.writeString(dir.resolve(s"$jarName.sha1"), sha1(jar) + "\n")
    Files.writeString(dir.resolve(s"$pomName.sha1"), sha1(pomBytes) + "\n")

  private def pom(group: String, name: String, revision: String, dependency: Option[(String, String)]): String =
    val deps = dependency match
      case None => ""
      case Some((artifact, version)) =>
        s"""  <dependencies>
           |    <dependency>
           |      <groupId>dev.histos</groupId>
           |      <artifactId>$artifact</artifactId>
           |      <version>$version</version>
           |    </dependency>
           |  </dependencies>
           |""".stripMargin
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<project>
       |  <modelVersion>4.0.0</modelVersion>
       |  <groupId>$group</groupId>
       |  <artifactId>$name</artifactId>
       |  <version>$revision</version>
       |$deps</project>
       |""".stripMargin

  private def emptyJar: Array[Byte] =
    val buffer = new ByteArrayOutputStream
    val output = new JarOutputStream(buffer)
    output.close()
    buffer.toByteArray

  private def jarBytes(classes: Path): Array[Byte] =
    val buffer = new ByteArrayOutputStream
    val output = new JarOutputStream(buffer)
    val walk   = Files.walk(classes)
    try
      walk.iterator.asScala.foreach: path =>
        if Files.isRegularFile(path) then
          val name = classes.relativize(path).iterator.asScala.map(_.toString).mkString("/")
          output.putNextEntry(new JarEntry(name))
          output.write(Files.readAllBytes(path))
          output.closeEntry()
    finally
      walk.close()
      output.close()
    buffer.toByteArray

  private def sha1(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-1").digest(bytes).map(byte => f"${byte & 0xff}%02x").mkString

  private def hold(cell: ResolveCell, classpath: Classpath): DependencyLock =
    DependencyLock(List(LockedCell(
      cell.module,
      cell.scalaVersion,
      cell.platform,
      cell.dependencies.map(dependency => LockedDirect(dependency.organization, dependency.name, dependency.revision, dependency.kind)),
      classpath.artifacts.map(artifact => LockedArtifact(artifact.organization, artifact.name, artifact.revision, artifact.sha256, artifact.repository)),
      classpath.evictions,
    )))

  private def tamper(lock: DependencyLock): DependencyLock =
    DependencyLock(lock.cells.map(cell => cell.copy(artifacts = flip(cell.artifacts))))

  private def flip(artifacts: List[LockedArtifact]): List[LockedArtifact] =
    artifacts match
      case head :: tail => head.copy(sha256 = flipped(head.sha256)) :: tail
      case Nil          => Nil

  private def flipped(sha: Sha256): Sha256 =
    val raw  = sha.value
    val last = if raw.endsWith("a") then "b" else "a"
    Sha256.from(raw.dropRight(1) + last) match
      case Right(value) => value
      case Left(_)      => sha

  private def graph(dependencies: List[Dep]): ProjectGraph =
    ProjectGraph.trusted(List(Module(
      core,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(v390, Nil),
      dependencies,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )))

  private def project(raw: String): ProjectName =
    ProjectName.from(raw) match
      case Right(name) => name
      case Left(_)     => project("core")

  private def running: ResolvedJdk =
    val home = Path.of(java.lang.System.getProperty("java.home"))
    val text = if Files.isRegularFile(home.resolve("release")) then Files.readString(home.resolve("release")) else ""
    val fallback = JdkVersion.trusted("21.0.2", JdkMajor.trusted(21))
    JdkVersion.fromRelease(text) match
      case Right(version) => ResolvedJdk(home, version)
      case Left(_)        => ResolvedJdk(home, fallback)

  private def write(root: Path, name: String, text: String): Path =
    val path = root.resolve(name)
    Files.writeString(path, text)
    path

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-resolve"))): dir =>
      delete(dir)

  private def delete(dir: Path): zio.UIO[Unit] =
    ZIO.attempt:
      if Files.exists(dir) then
        val walk = Files.walk(dir)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    .ignore

  def spec = suite("resolve")(names, lockFile, fetch) @@ TestAspect.sequential @@ TestAspect.timeout(3.minutes)
