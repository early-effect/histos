package histos.zinc

import histos.jdk.{JdkMajor, JdkVersion, Sha256}
import histos.schedule.{ActionCache, ActionId, CacheError, Digest, Scheduler}
import histos.{Platform, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Ref, Scope, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

object ZincSpec extends ZIOSpecDefault:
  private val v21 = JdkVersion.trusted("21.0.2", JdkMajor.trusted(21))
  private val v17 = JdkVersion.trusted("17.0.9", JdkMajor.trusted(17))
  private val sha = Sha256.of(Chunk.fromArray("hello".getBytes))

  private val keys = suite("compile key")(
    test("flags, the Scala version, the platform, and the JDK are part of the key"):
      val base  = sample(ScalacFlags.empty, v21, Platform.Jvm, JvmCompile.Scala390)
      val flags = sample(ScalacFlags.of(ScalacFlag.EncodingUtf8), v21, Platform.Jvm, JvmCompile.Scala390)
      val older = sample(ScalacFlags.empty, v21, Platform.Jvm, ScalaVersion.trusted("3.8.1"))
      val js    = sample(ScalacFlags.empty, v21, Platform.Js, JvmCompile.Scala390)
      val jdk17 = sample(ScalacFlags.empty, v17, Platform.Jvm, JvmCompile.Scala390)
      val key   = CompileKey.of(base, sha)
      assertTrue(
        key != CompileKey.of(flags, sha),
        key != CompileKey.of(older, sha),
        key != CompileKey.of(js, sha),
        key != CompileKey.of(jdk17, sha),
        key == CompileKey.of(base, sha),
      )
    ,
    test("js and another Scala version are rejected before Zinc"):
      val js  = sample(ScalacFlags.empty, v21, Platform.Js, JvmCompile.Scala390)
      val old = sample(ScalacFlags.empty, v21, Platform.Jvm, ScalaVersion.trusted("3.8.1"))
      for
        platform <- JvmCompile.compile(js).either
        version  <- JvmCompile.compile(old).either
      yield assertTrue(
        platform == Left(CompileError.UnsupportedPlatform(Platform.Js)),
        version == Left(CompileError.UnsupportedScala(ScalaVersion.trusted("3.8.1"))),
      )
  )

  private val banned = suite("user classpath")(
    test("the Scala 3 compiler and histos are not user dependencies"):
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
          onCompiler == Left(CompileError.Banned(compiler, ClasspathBan.Scala3Compiler)),
          onApi == Left(CompileError.Banned(api, ClasspathBan.Histos)),
        )
  )

  private val builds = suite("zinc")(
    test("the second identical compile is a cache hit, and a changed source is not"):
      ZIO.scoped:
        for
          root   <- temp
          source <- ZIO.attempt(write(root, "Hello.scala", hello))
          live    = running
          req     = liveRequest(root, source, live)
          key    <- JvmCompile.sourceDigest(req.sources).map(CompileKey.of(req, _))
          id     <- ZIO.fromEither(ActionId.from("compile")).orElseFail(CompileError.NoSources)
          calls  <- Ref.make(0)
          cache  <- memory
          local   = (_: histos.schedule.Action) => calls.update(_ + 1) *> JvmCompile.compile(req).map(_.classFiles.toString)
          run     = (actionKey: Sha256) =>
            ZIO.serviceWithZIO[Scheduler](_.execute(id, List(CompileKey.action(id, actionKey)), local))
          first <- run(key).provide(Scheduler.live, ZLayer.succeed(cache))
          once  <- calls.get
          exists = Files.isRegularFile(root.resolve("classes").resolve("Hello.class"))
          second <- run(key).provide(Scheduler.live, ZLayer.succeed(cache))
          still  <- calls.get
          _      <- ZIO.attempt(Files.writeString(source, hello + "\n"))
          changed <- JvmCompile.sourceDigest(req.sources).map(CompileKey.of(req, _))
          _       <- run(changed).provide(Scheduler.live, ZLayer.succeed(cache))
          thrice  <- calls.get
        yield assertTrue(once == 1 && exists && second == first && still == 1 && thrice == 2 && changed != key)
    ,
    test("user code cannot see the compiler"):
      ZIO.scoped:
        for
          root   <- temp
          source <- ZIO.attempt(write(root, "Bad.scala", "object Bad:\n  def x = classOf[dotty.tools.dotc.Driver]\n"))
          out    <- JvmCompile.compile(liveRequest(root, source, running)).either
        yield assertTrue:
          out match
            case Left(CompileError.Failed(diagnostics)) =>
              diagnostics.exists(diagnostic => diagnostic.text.contains("Driver") || diagnostic.text.contains("dotty"))
            case _ => false
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
    )

  private def running: ResolvedJdk =
    val home = Path.of(java.lang.System.getProperty("java.home"))
    val text = if Files.isRegularFile(home.resolve("release")) then Files.readString(home.resolve("release")) else ""
    JdkVersion.fromRelease(text) match
      case Right(version) => ResolvedJdk(home, version)
      case Left(_)        => ResolvedJdk(home, v21)

  private def liveRequest(root: Path, source: Path, jdk: ResolvedJdk): CompileRequest =
    CompileRequest(
      CompileToolchain(JvmCompile.Scala390, Platform.Jvm, jdk, ScalacFlags.empty),
      List(source),
      Nil,
      root.resolve("classes"),
      root.resolve("analysis"),
    )

  private def write(root: Path, name: String, text: String): Path =
    val path = root.resolve(name)
    Files.writeString(path, text)
    path

  private def located(cls: Class[?]): ZIO[Any, CompileError, Path] =
    val path = Option(cls.getProtectionDomain).flatMap(domain => Option(domain.getCodeSource)).flatMap(source => Option(source.getLocation)).map(url => Path.of(url.toURI))
    ZIO.fromOption(path).orElseFail(CompileError.NoBridge)

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

  def spec = suite("jvm compile")(keys, banned, builds) @@ TestAspect.sequential @@ TestAspect.timeout(3.minutes)
