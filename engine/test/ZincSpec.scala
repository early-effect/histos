package histos.zinc

import histos.jdk.JdkRuntime
import histos.jdk.{JdkMajor, JdkVersion, Sha256}
import histos.workspace.{IoFailure, IoFault}
import histos.schedule.{
  Action,
  ActionCache,
  ActionId,
  CacheError,
  Digest,
  Input,
  InputName,
  InputValue,
  RemoteCache,
  Scheduler,
}
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
    test(
      "flags, the Scala version, the platform, the JDK, and artifact checksums are part of the key"
    ):
      val base =
        sample(
          ScalacFlags.empty,
          v21,
          Platform.Jvm,
          histos.ScalaVersion("3.9.0"),
        )
      val flags = sample(
        ScalacFlags.of(ScalacFlag.EncodingUtf8),
        v21,
        Platform.Jvm,
        histos.ScalaVersion("3.9.0"),
      )
      val older = sample(
        ScalacFlags.empty,
        v21,
        Platform.Jvm,
        ScalaVersion.trusted("3.8.1"),
      )
      val js = sample(
        ScalacFlags.empty,
        v21,
        Platform.Js,
        histos.ScalaVersion("3.9.0"),
      )
      val jdk17 =
        sample(
          ScalacFlags.empty,
          v17,
          Platform.Jvm,
          histos.ScalaVersion("3.9.0"),
        )
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
    test("locked tool bytes and local runtime identity affect compile keys"):
      val request = sample(
        ScalacFlags.empty,
        v21,
        Platform.Jvm,
        histos.ScalaVersion("3.9.0"),
      )
      val key            = CompileKey.of(request, sha)
      val one            = Sha256.of(Chunk.fromArray(Array[Byte](1)))
      val two            = Sha256.of(Chunk.fromArray(Array[Byte](2)))
      val changedRuntime = request.copy(toolchain =
        request.toolchain.copy(jdk =
          request.toolchain.jdk.copy(identity =
            histos.toolchain.RuntimeIdentity
              .Local(request.toolchain.jdk.home, two)
          )
        )
      )
      assertTrue(
        CompileKey.withTools(key, List(one)) != CompileKey
          .withTools(key, List(two)),
        CompileKey.withTools(key, List(one)) == CompileKey
          .withTools(key, List(one)),
        key != CompileKey.of(changedRuntime, sha),
      )
    ,
    test(
      "js and Scala 2.12 are rejected, and Scala 3 and 2.13 reach the compiler"
    ):
      val js = sample(
        ScalacFlags.empty,
        v21,
        Platform.Js,
        histos.ScalaVersion("3.9.0"),
      )
      val scala212 = sample(
        ScalacFlags.empty,
        v21,
        Platform.Jvm,
        ScalaVersion.trusted("2.12.20"),
      )
      val scala3 = sample(
        ScalacFlags.empty,
        v21,
        Platform.Jvm,
        ScalaVersion.trusted("3.8.1"),
      )
      val scala213 = sample(
        ScalacFlags.empty,
        v21,
        Platform.Jvm,
        ScalaVersion.trusted("2.13.15"),
      )
      for
        platform <- JvmCompile.compile(js).either
        rejected <- JvmCompile.compile(scala212).either
        any3     <- JvmCompile.compile(scala3).either
        any213   <- JvmCompile.compile(scala213).either
      yield assertTrue(
        platform == Left(CompileError.UnsupportedPlatform(Platform.Js)),
        rejected == Left(
          CompileError.UnsupportedScala(ScalaVersion.trusted("2.12.20"))
        ),
        any3 == Left(CompileError.NoSources),
        any213 == Left(CompileError.NoSources),
      ),
  )

  private val banned = suite("user classpath")(
    test("a declared compiler is a library, and a histos api jar is not"):
      ZIO.scoped:
        for
          root       <- temp
          compiler   <- located(classOf[dotty.tools.dotc.Driver])
          api        <- located(classOf[histos.HistosBuild])
          onCompiler <- JvmCompile
            .compile(
              sample(
                ScalacFlags.empty,
                v21,
                Platform.Jvm,
                histos.ScalaVersion("3.9.0"),
              )
                .copy(
                  classes = root.resolve("c1"),
                  analysis = root.resolve("a1"),
                  classpath = List(compiler),
                )
            )
            .either
          onApi <- JvmCompile
            .compile(
              sample(
                ScalacFlags.empty,
                v21,
                Platform.Jvm,
                histos.ScalaVersion("3.9.0"),
              )
                .copy(
                  classes = root.resolve("c2"),
                  analysis = root.resolve("a2"),
                  classpath = List(api),
                )
            )
            .either
        yield assertTrue(
          onCompiler == Left(CompileError.NoSources),
          Files.isDirectory(api) && onApi == Left(CompileError.NoSources) ||
            Files.isRegularFile(api) && onApi == Left(
              CompileError.Banned(api, ClasspathBan.Histos)
            ),
        )
    ,
    test("a plugin marker is banned in a jar and in a classes directory"):
      ZIO.scoped:
        for
          root <- temp
          jar     = root.resolve("plugin.jar")
          classes = root.resolve("marked")
          _ <- ZIO.attempt {
            val output = new JarOutputStream(Files.newOutputStream(jar))
            try
              output.putNextEntry(new JarEntry("META-INF/histos/plugin"))
              output.write(
                "dev.example.Echo$\n".getBytes(
                  java.nio.charset.StandardCharsets.UTF_8
                )
              )
              output.closeEntry()
            finally output.close()
            val marker = classes.resolve("META-INF").resolve("histos")
            Files.createDirectories(marker)
            Files.writeString(marker.resolve("plugin"), "dev.example.Echo$\n")
          }
          onJar <- JvmCompile
            .compile(
              sample(
                ScalacFlags.empty,
                v21,
                Platform.Jvm,
                histos.ScalaVersion("3.9.0"),
              )
                .copy(
                  classes = root.resolve("c1"),
                  analysis = root.resolve("a1"),
                  classpath = List(jar),
                )
            )
            .either
          onDir <- JvmCompile
            .compile(
              sample(
                ScalacFlags.empty,
                v21,
                Platform.Jvm,
                histos.ScalaVersion("3.9.0"),
              )
                .copy(
                  classes = root.resolve("c2"),
                  analysis = root.resolve("a2"),
                  classpath = List(classes),
                )
            )
            .either
        yield assertTrue(
          onJar == Left(CompileError.Banned(jar, ClasspathBan.Histos)),
          onDir == Left(CompileError.Banned(classes, ClasspathBan.Histos)),
        ),
  )

  private val builds = suite("zinc")(
    test(
      "production compiler keys ignore unused tools and reject corruption before a cache hit"
    ):
      ZIO.scoped:
        for
          root   <- temp
          source <- ZIO.attempt(write(root, "Hello.scala", hello))
          jdk    <- running
          request = liveRequest(root, source, jdk)
          tools <- ZIO.service[histos.resolve.LockedTools]
          first <- JvmCompile.compile(request).provide(ZLayer.succeed(tools))
          unused = histos.resolve.LockedTool(
            histos.resolve.ToolId(
              histos.resolve.ToolRole.JsEnvironment,
              histos.ScalaVersion("3.9.0"),
              histos.resolve.ToolVariant.Firefox,
            ),
            Nil,
            Nil,
            Nil,
            Nil,
          )
          second <- JvmCompile
            .compile(request)
            .provide(
              ZLayer.succeed(
                tools.copy(lock = tools.lock.copy(tools = tools.lock.tools :+ unused))
              )
            )
          broken = tools.copy(lock =
            tools.lock.copy(tools =
              tools.lock.tools.map(cell =>
                if cell.id.role == histos.resolve.ToolRole.Compiler && cell.id.scalaVersion == request.toolchain.version
                then cell.copy(artifacts = cell.artifacts.map(_.copy(sha256 = Sha256.of(Chunk.empty))))
                else cell
              )
            )
          )
          rejected <- JvmCompile
            .compile(request)
            .provide(ZLayer.succeed(broken))
            .either
        yield assertTrue(
          first.key == second.key,
          rejected.left.toOption.exists {
            case CompileError.Resolved(
                  histos.resolve.ResolveError.Checksum(_, _, _)
                ) =>
              true;
            case _ => false
          },
        )
    ,
    test("compiler loader leases close when later construction fails"):
      val compilerClosed = new java.util.concurrent.atomic.AtomicBoolean(false)
      val libraryClosed  = new java.util.concurrent.atomic.AtomicBoolean(false)
      def loader(closed: java.util.concurrent.atomic.AtomicBoolean) =
        new java.net.URLClassLoader(Array.empty[java.net.URL]):
          override def close(): Unit =
            super.close()
            closed.set(true)
      ZIO
        .scoped(
          for
            _      <- CompilerPool.acquireLoader(loader(compilerClosed))
            _      <- CompilerPool.acquireLoader(loader(libraryClosed))
            result <- ZIO.fail(CompileError.NoSources)
          yield result
        )
        .either
        .map(result =>
          assertTrue(
            result == Left(CompileError.NoSources),
            compilerClosed.get(),
            libraryClosed.get(),
          )
        )
    ,
    test(
      "the second identical compile is a cache hit, and a changed source is not"
    ):
      ZIO.scoped:
        for
          root   <- temp
          source <- ZIO.attempt(write(root, "Hello.scala", hello))
          live   <- running
          req = liveRequest(root, source, live)
          key <- JvmCompile.sourceDigest(req.sources).map(CompileKey.of(req, _))
          id  <- ZIO
            .fromEither(ActionId.from("compile"))
            .orElseFail(CompileError.NoSources)
          calls <- Ref.make(0)
          cache <- memory
          tools <- ZIO.service[histos.resolve.LockedTools]
          local = (_: Action, _: String) =>
            calls.update(_ + 1) *> JvmCompile
              .compile(req)
              .provide(ZLayer.succeed(tools))
              .map(_.classFiles.toString)
          run = (actionKey: Sha256) =>
            val action = Action(
              id,
              List(
                Input(InputName.trusted("compile"), InputValue.Hash(actionKey))
              ),
              Nil,
            )
            ZIO.serviceWithZIO[Scheduler](_.execute(id, List(action), local))
          first <- run(key)
            .provide(Scheduler.live, ZLayer.succeed(cache), RemoteCache.off)
          once <- calls.get
          exists = Files.isRegularFile(
            root.resolve("classes").resolve("Hello.class")
          )
          second <- run(key)
            .provide(Scheduler.live, ZLayer.succeed(cache), RemoteCache.off)
          still   <- calls.get
          _       <- ZIO.attempt(Files.writeString(source, hello + "\n"))
          changed <- JvmCompile
            .sourceDigest(req.sources)
            .map(CompileKey.of(req, _))
          _ <- run(changed)
            .provide(Scheduler.live, ZLayer.succeed(cache), RemoteCache.off)
          thrice <- calls.get
        yield assertTrue(
          once == 1 && exists && second == first && still == 1 && thrice == 2 && changed != key
        )
    ,
    test(
      "an unchanged class is left alone, a neighbor is rebuilt, and a removed source deletes its class"
    ):
      ZIO.scoped:
        for
          root <- temp
          jdk  <- running
          foo  <- ZIO.attempt(
            write(root, "Foo.scala", "object Foo:\n  def n: Int = 1\n")
          )
          bar <- ZIO.attempt(
            write(root, "Bar.scala", "object Bar:\n  def n: Int = 2\n")
          )
          both = liveRequest(root, foo, jdk).copy(sources = List(foo, bar))
          first <- JvmCompile.compile(both)
          stored = Files.isRegularFile(
            root.resolve("analysis").resolve("inc.zip")
          )
          fooClass = root.resolve("classes").resolve("Foo.class")
          barClass = root.resolve("classes").resolve("Bar.class")
          stamped <- ZIO.attempt:
            val old = java.nio.file.attribute.FileTime
              .from(java.time.Instant.now.minusSeconds(30))
            Files.setLastModifiedTime(fooClass, old)
            Files.getLastModifiedTime(fooClass)
          second <- JvmCompile.compile(both)
          _      <- ZIO.attempt(
            Files.writeString(bar, "object Bar:\n  def n: Int = 3\n")
          )
          edited  <- JvmCompile.compile(both)
          fooSame <- ZIO.attempt(Files.getLastModifiedTime(fooClass) == stamped)
          _       <- ZIO.attempt(Files.delete(bar))
          _       <- JvmCompile.compile(both.copy(sources = List(foo)))
          barGone <- ZIO.attempt(!Files.exists(barClass))
          fooKept <- ZIO.attempt(Files.isRegularFile(fooClass))
        yield assertTrue(
          stored && fooSame && barGone && fooKept,
          first.sources == Some(SourceCount(2, 0)),
          first.classesWritten == Some(first.classFiles),
          first.classFiles > 0,
          second.sources == Some(SourceCount(0, 2)),
          second.classesWritten == Some(0),
          !second.changed,
          second.classFiles == first.classFiles,
          edited.sources == Some(SourceCount(1, 1)),
          edited.classesWritten.exists(written => written > 0 && written < edited.classFiles),
        )
    ,
    test("two compiles on one pool open the compiler once"):
      ZIO.scoped:
        for
          root   <- temp
          jdk    <- running
          source <- ZIO.attempt(write(root, "Hello.scala", hello))
          req = liveRequest(root, source, jdk)
          pool   <- CompilerPool.make
          _      <- JvmCompile.compilePooled(pool, req, Nil, _ => ())
          opened <- pool.openings
          _      <- JvmCompile.compilePooled(pool, req, Nil, _ => ())
          again  <- pool.openings
        yield assertTrue(opened == 1 && again == 1)
    ,
    test("user code cannot see the compiler"):
      ZIO.scoped:
        for
          root   <- temp
          source <- ZIO.attempt(
            write(
              root,
              "Bad.scala",
              "object Bad:\n  def x = classOf[dotty.tools.dotc.Driver]\n",
            )
          )
          live <- running
          out  <- JvmCompile.compile(liveRequest(root, source, live)).either
        yield assertTrue:
          out match
            case Left(CompileError.Failed(diagnostics)) =>
              diagnostics.exists(diagnostic =>
                diagnostic.text.contains("Driver") || diagnostic.text.contains(
                  "dotty"
                )
              )
            case _ => false
    ,
    test(
      "a project compiler does not load the engine Scala library or compiler"
    ):
      for
        toolchain <- Scala3.jars(ScalaVersion.trusted("3.3.7"))
        isolated  <- ZIO.attempt {
          val loader = toolchain.openLoader(getClass.getClassLoader)
          try
            val engineList  = classOf[scala.collection.immutable.List[?]]
            val projectList =
              loader.loadClass("scala.collection.immutable.List")
            val engineDriver    = classOf[dotty.tools.dotc.Driver]
            val projectDriver   = loader.loadClass("dotty.tools.dotc.Driver")
            val callback        = classOf[xsbti.AnalysisCallback]
            val projectCallback = loader.loadClass("xsbti.AnalysisCallback")
            (projectList ne engineList) && (projectDriver ne engineDriver) && (projectCallback eq callback)
          finally loader.close()
        }
      yield assertTrue(isolated)
    ,
    test("Scala 3.10 compiles a named tuple that Scala 3.3 rejects"):
      ZIO.scoped:
        for
          root   <- temp
          source <- ZIO.attempt(write(root, "Hello.scala", namedTuple))
          jdk    <- running
          old    <- JvmCompile
            .compile(
              request(
                root.resolve("old"),
                source,
                jdk,
                ScalaVersion.trusted("3.3.7"),
              )
            )
            .either
          fresh <- JvmCompile.compile(
            request(
              root.resolve("fresh"),
              source,
              jdk,
              ScalaVersion.trusted("3.10.0"),
            )
          )
          names <- ZIO.attempt {
            val walk = Files.walk(fresh.classes)
            try
              walk.iterator.asScala
                .filter(Files.isRegularFile(_))
                .map(path => fresh.classes.relativize(path).toString)
                .toList
                .sorted
            finally walk.close()
          }
        yield assertTrue(
          old match
            case Left(CompileError.Failed(_)) => true
            case _                            => false
          ,
          names == List("Hello$.class", "Hello.class", "Hello.tasty"),
        ),
  )

  private val hello =
    """object Hello:
      |  def greet: String = "hi"
      |""".stripMargin

  private def sample(
      flags: ScalacFlags,
      version: JdkVersion,
      platform: Platform,
      scalaVersion: ScalaVersion,
  ): CompileRequest =
    CompileRequest(
      CompileToolchain(
        scalaVersion,
        platform,
        JdkRuntime(
          Path.of("/jdks").resolve(version.value),
          version,
          histos.toolchain.RuntimeIdentity
            .Local(Path.of("/jdks").resolve(version.value), sha),
        ),
        flags,
      ),
      Nil,
      Nil,
      Path.of("/out"),
      Path.of("/analysis"),
      Nil,
    )

  private def running: zio.IO[GraphCompileError, JdkRuntime] =
    GraphCompile.jdkHere

  private val namedTuple =
    """object Hello {
      |  def pair: (name: String) = (name = "hi")
      |}
      |""".stripMargin

  private def request(
      root: Path,
      source: Path,
      jdk: JdkRuntime,
      version: ScalaVersion,
  ): CompileRequest =
    CompileRequest(
      CompileToolchain(version, Platform.Jvm, jdk, ScalacFlags.empty),
      List(source),
      Nil,
      root.resolve("classes"),
      root.resolve("analysis"),
      Nil,
    )

  private def liveRequest(
      root: Path,
      source: Path,
      jdk: JdkRuntime,
  ): CompileRequest =
    CompileRequest(
      CompileToolchain(
        histos.ScalaVersion("3.9.0"),
        Platform.Jvm,
        jdk,
        ScalacFlags.empty,
      ),
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
    val path = Option(cls.getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
      .map(url => Path.of(url.toURI))
    ZIO
      .fromOption(path)
      .orElseFail(
        CompileError.Io(IoFailure(Path.of(cls.getName), IoFault.Stat))
      )

  private def memory: ZIO[Any, Nothing, ActionCache] =
    Ref
      .make(Map.empty[Digest, String])
      .map: ref =>
        new ActionCache:
          def get(digest: Digest): ZIO[Any, CacheError, Option[String]] =
            ref.get.map(_.get(digest))
          override def put(
              digest: Digest,
              value: String,
          ): ZIO[Any, CacheError, Unit] = ref.update(_ + (digest -> value))

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-zinc"))): dir =>
      delete(dir)

  private def delete(dir: Path): UIO[Unit] =
    ZIO
      .attempt:
        if Files.exists(dir) then
          val walk  = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths
            .sortBy(_.getNameCount)
            .reverse
            .foreach(path => Files.deleteIfExists(path))
      .ignore

  def spec = suite("jvm compile")(
    keys,
    banned,
    builds,
  ).provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
    histos.ToolFixture.jvm(
      histos.ScalaVersion("3.9.0"),
      histos.ScalaVersion("3.8.1"),
      histos.ScalaVersion("2.13.15"),
      histos.ScalaVersion("3.3.7"),
      histos.ScalaVersion("3.10.0"),
    )
  ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes)
end ZincSpec
