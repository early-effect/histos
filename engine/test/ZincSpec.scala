package histos.zinc

import histos.jdk.{JdkMajor, JdkVersion, Sha256}
import histos.schedule.{ActionCache, ActionId, CacheError, Digest, RemoteCache, Scheduler}
import histos.{Platform, ScalaVersion}

import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Ref, Scope, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

object ZincSpec extends ZIOSpecDefault:
  private val v21 = JdkVersion.trusted("21.0.2", JdkMajor.trusted(21))
  private val v17 = JdkVersion.trusted("17.0.9", JdkMajor.trusted(17))
  private val sha = Sha256.of(Chunk.fromArray("hello".getBytes))

  private val keys = suite("compile key")(
    test("flags, the Scala version, the platform, the JDK, and artifact checksums are part of the key"):
      val base  = sample(ScalacFlags.empty, v21, Platform.Jvm, JvmCompile.Scala390)
      val flags = sample(ScalacFlags.of(ScalacFlag.EncodingUtf8), v21, Platform.Jvm, JvmCompile.Scala390)
      val older = sample(ScalacFlags.empty, v21, Platform.Jvm, ScalaVersion.trusted("3.8.1"))
      val js    = sample(ScalacFlags.empty, v21, Platform.Js, JvmCompile.Scala390)
      val jdk17 = sample(ScalacFlags.empty, v17, Platform.Jvm, JvmCompile.Scala390)
      val jar    = Sha256.of(Chunk.fromArray("jar".getBytes))
      val hashed = base.copy(artifactHashes = List(jar))
      val key    = CompileKey.of(base, sha)
      assertTrue(
        key != CompileKey.of(flags, sha),
        key != CompileKey.of(older, sha),
        key != CompileKey.of(js, sha),
        key != CompileKey.of(jdk17, sha),
        key != CompileKey.of(hashed, sha),
        key == CompileKey.of(base, sha),
      )
    ,
    test("js and Scala 2.12 are rejected, and Scala 3 and 2.13 reach the compiler"):
      val js = sample(ScalacFlags.empty, v21, Platform.Js, JvmCompile.Scala390)
      val scala212 = sample(ScalacFlags.empty, v21, Platform.Jvm, ScalaVersion.trusted("2.12.20"))
      val scala3 = sample(ScalacFlags.empty, v21, Platform.Jvm, ScalaVersion.trusted("3.8.1"))
      val scala213 = sample(ScalacFlags.empty, v21, Platform.Jvm, ScalaVersion.trusted("2.13.15"))
      for
        platform <- JvmCompile.compile(js).either
        rejected <- JvmCompile.compile(scala212).either
        any3     <- JvmCompile.compile(scala3).either
        any213   <- JvmCompile.compile(scala213).either
      yield assertTrue(
        platform == Left(CompileError.UnsupportedPlatform(Platform.Js)),
        rejected == Left(CompileError.UnsupportedScala(ScalaVersion.trusted("2.12.20"))),
        any3 == Left(CompileError.NoSources),
        any213 == Left(CompileError.NoSources),
      )
  )

  private val banned = suite("user classpath")(
    test("a declared compiler is a library, and a histos api jar is not"):
      ZIO.scoped:
        for
          root     <- temp
          compiler <- located(classOf[dotty.tools.dotc.Driver])
          api      <- located(classOf[histos.HistosBuild])
          onCompiler <- JvmCompile.compile(sample(ScalacFlags.empty, v21, Platform.Jvm, JvmCompile.Scala390).copy(
                          classes = root.resolve("c1"),
                          analysis = root.resolve("a1"),
                          classpath = List(compiler),
                        )).either
          onApi <- JvmCompile.compile(sample(ScalacFlags.empty, v21, Platform.Jvm, JvmCompile.Scala390).copy(
                     classes = root.resolve("c2"),
                     analysis = root.resolve("a2"),
                     classpath = List(api),
                   )).either
        yield assertTrue(
          onCompiler == Left(CompileError.NoSources),
          Files.isDirectory(api) && onApi == Left(CompileError.NoSources) ||
            Files.isRegularFile(api) && onApi == Left(CompileError.Banned(api, ClasspathBan.Histos)),
        )
    ,
    test("a plugin marker is banned in a jar and in a classes directory"):
      ZIO.scoped:
        for
          root <- temp
          jar = root.resolve("plugin.jar")
          classes = root.resolve("marked")
          _ <- ZIO.attempt {
                 val output = new JarOutputStream(Files.newOutputStream(jar))
                 try
                   output.putNextEntry(new JarEntry("META-INF/histos/plugin"))
                   output.write("dev.example.Echo$\n".getBytes(java.nio.charset.StandardCharsets.UTF_8))
                   output.closeEntry()
                 finally output.close()
                 val marker = classes.resolve("META-INF").resolve("histos")
                 Files.createDirectories(marker)
                 Files.writeString(marker.resolve("plugin"), "dev.example.Echo$\n")
               }
          onJar <- JvmCompile.compile(sample(ScalacFlags.empty, v21, Platform.Jvm, JvmCompile.Scala390).copy(
                     classes = root.resolve("c1"),
                     analysis = root.resolve("a1"),
                     classpath = List(jar),
                   )).either
          onDir <- JvmCompile.compile(sample(ScalacFlags.empty, v21, Platform.Jvm, JvmCompile.Scala390).copy(
                     classes = root.resolve("c2"),
                     analysis = root.resolve("a2"),
                     classpath = List(classes),
                   )).either
        yield assertTrue(
          onJar == Left(CompileError.Banned(jar, ClasspathBan.Histos)),
          onDir == Left(CompileError.Banned(classes, ClasspathBan.Histos)),
        )
  )

  private val builds = suite("zinc")(
    test("the second identical compile is a cache hit, and a changed source is not"):
      ZIO.scoped:
        for
          root   <- temp
          source <- ZIO.attempt(write(root, "Hello.scala", hello))
          live   <- running
          req     = liveRequest(root, source, live)
          key    <- JvmCompile.sourceDigest(req.sources).map(CompileKey.of(req, _))
          id     <- ZIO.fromEither(ActionId.from("compile")).orElseFail(CompileError.NoSources)
          calls  <- Ref.make(0)
          cache  <- memory
          local   = (_: histos.schedule.Action) => calls.update(_ + 1) *> JvmCompile.compile(req).map(_.classFiles.toString)
          run     = (actionKey: Sha256) =>
            ZIO.serviceWithZIO[Scheduler](_.execute(id, List(CompileKey.action(id, actionKey)), local))
          first <- run(key).provide(Scheduler.live, ZLayer.succeed(cache), RemoteCache.off)
          once  <- calls.get
          exists = Files.isRegularFile(root.resolve("classes").resolve("Hello.class"))
          second <- run(key).provide(Scheduler.live, ZLayer.succeed(cache), RemoteCache.off)
          still  <- calls.get
          _      <- ZIO.attempt(Files.writeString(source, hello + "\n"))
          changed <- JvmCompile.sourceDigest(req.sources).map(CompileKey.of(req, _))
          _       <- run(changed).provide(Scheduler.live, ZLayer.succeed(cache), RemoteCache.off)
          thrice  <- calls.get
        yield assertTrue(once == 1 && exists && second == first && still == 1 && thrice == 2 && changed != key)
    ,
    test("user code cannot see the compiler"):
      ZIO.scoped:
        for
          root   <- temp
          source <- ZIO.attempt(write(root, "Bad.scala", "object Bad:\n  def x = classOf[dotty.tools.dotc.Driver]\n"))
          live   <- running
          out    <- JvmCompile.compile(liveRequest(root, source, live)).either
        yield assertTrue:
          out match
            case Left(CompileError.Failed(diagnostics)) =>
              diagnostics.exists(diagnostic => diagnostic.text.contains("Driver") || diagnostic.text.contains("dotty"))
            case _ => false
    ,
    test("a project compiler does not load the engine Scala library or compiler"):
      for
        toolchain <- Scala3.jars(ScalaVersion.trusted("3.3.7"))
        isolated <- ZIO.attempt {
                      val loader = toolchain.openLoader(getClass.getClassLoader)
                      try
                        val engineList = classOf[scala.collection.immutable.List[?]]
                        val projectList = loader.loadClass("scala.collection.immutable.List")
                        val engineDriver = classOf[dotty.tools.dotc.Driver]
                        val projectDriver = loader.loadClass("dotty.tools.dotc.Driver")
                        val callback = classOf[xsbti.AnalysisCallback]
                        val projectCallback = loader.loadClass("xsbti.AnalysisCallback")
                        (projectList ne engineList) && (projectDriver ne engineDriver) && (projectCallback eq callback)
                      finally loader.close()
                    }
      yield assertTrue(isolated)
    ,
    test("Scala 3.10 compiles a named tuple that Scala 3.3 rejects"):
      ZIO.scoped:
        for
          root <- temp
          source <- ZIO.attempt(write(root, "Hello.scala", namedTuple))
          jdk <- running
          old <- JvmCompile.compile(request(root.resolve("old"), source, jdk, ScalaVersion.trusted("3.3.7"))).either
          fresh <- JvmCompile.compile(request(root.resolve("fresh"), source, jdk, ScalaVersion.trusted("3.10.0")))
          names <- ZIO.attempt {
                     val walk = Files.walk(fresh.classes)
                     try
                       walk.iterator.asScala.filter(Files.isRegularFile(_)).map(path => fresh.classes.relativize(path).toString).toList.sorted
                     finally walk.close()
                   }
        yield assertTrue(
          old match
            case Left(CompileError.Failed(_)) => true
            case _                             => false
          ,
          names == List("Hello$.class", "Hello.class", "Hello.tasty"),
        )
  )

  private val hello =
    """object Hello:
      |  def greet: String = "hi"
      |""".stripMargin

  private def sample(flags: ScalacFlags, version: JdkVersion, platform: Platform, scalaVersion: ScalaVersion): CompileRequest =
    CompileRequest(
      CompileToolchain(scalaVersion, platform, ResolvedJdk(Path.of("/jdks").resolve(version.value), version), flags),
      Nil,
      Nil,
      Path.of("/out"),
      Path.of("/analysis"),
      Nil,
    )

  private def running: zio.IO[GraphCompileError, ResolvedJdk] =
    GraphCompile.jdkHere

  private val namedTuple =
    """object Hello {
      |  def pair: (name: String) = (name = "hi")
      |}
      |""".stripMargin

  private def request(root: Path, source: Path, jdk: ResolvedJdk, version: ScalaVersion): CompileRequest =
    CompileRequest(
      CompileToolchain(version, Platform.Jvm, jdk, ScalacFlags.empty),
      List(source),
      Nil,
      root.resolve("classes"),
      root.resolve("analysis"),
      Nil,
    )

  private def liveRequest(root: Path, source: Path, jdk: ResolvedJdk): CompileRequest =
    CompileRequest(
      CompileToolchain(JvmCompile.Scala390, Platform.Jvm, jdk, ScalacFlags.empty),
      List(source),
      Nil,
      root.resolve("classes"),
      root.resolve("analysis"),
      Nil,
    )

  private def write(root: Path, name: String, text: String): Path =
    val path = root.resolve(name)
    Files.writeString(path, text)
    path

  private def located(cls: Class[?]): ZIO[Any, CompileError, Path] =
    val path = Option(cls.getProtectionDomain).flatMap(domain => Option(domain.getCodeSource)).flatMap(source => Option(source.getLocation)).map(url => Path.of(url.toURI))
    ZIO.fromOption(path).orElseFail(CompileError.Io(CompileIo.Read))

  private def memory: ZIO[Any, Nothing, ActionCache] =
    Ref.make(Map.empty[Digest, String]).map: ref =>
      new ActionCache:
        def get(digest: Digest): ZIO[Any, CacheError, Option[String]] = ref.get.map(_.get(digest))
        override def put(digest: Digest, value: String): ZIO[Any, CacheError, Unit] = ref.update(_ + (digest -> value))

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-zinc"))): dir =>
      delete(dir)

  private def delete(dir: Path): UIO[Unit] =
    ZIO.attempt:
      if Files.exists(dir) then
        val walk = Files.walk(dir)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    .ignore

  def spec = suite("jvm compile")(keys, banned, builds) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes)
