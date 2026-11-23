package histos.plugin

import histos.cache.ContentStore
import histos.jdk.{Jdk, JdkError, Sha256Digest}
import histos.load.{BootstrapLoad, BuildCache, BuildClasspath, Loader}
import histos.pack.{AssemblyFile, ClassJar}
import histos.report.WireJson.given
import histos.resolve.{Repositories, Resolve, ToolRequest}
import histos.schedule.RemoteCache
import histos.toolchain.{
  HostCompilerError,
  HostCompilerInputs,
  HostCompilerManifest,
  HostCompilerPackage,
  HostCompilerSession,
  HostToolRole,
}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompilerRequests, CompilerStarts, Compilers}
import histos.{Dep, HistosBuild, ScalaVersion}

import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.zip.ZipFile

import zio.json.*
import zio.test.*
import zio.{Chunk, IO, Ref, Scope, ZIO, ZLayer, durationInt}

/** The packaged host compiler, installed as a payload, rebuilds a plugin without a project compiler cell. */
object PluginArchitectureIntegrationSpec extends ZIOSpecDefault:
  private val Scala3101 = ScalaVersion("3.10.1")

  def spec = suite("assembled host")(
    test("the packaged closure materializes and the bridge digest matches the jar") {
      ZIO.scoped {
        for
          root        <- work("histos-p10-digest")
          files       <- packaged
          classes     <- classesDirectory(root)
          payload     <- payloadJar(root, files, None, corrupt = false)
          distributed <- BuildClasspath.distribution(payload)
          directory   <- BuildClasspath.distribution(classes)
          store       <- ZIO.attemptBlocking(ContentStore.in(Files.createDirectories(root.resolve("cas")))).orDie
          home        <- jdkHome
          session     <- HostCompilerInputs.materialize(payload, home, store).mapError(LoadError.HostCompiler(_))
          bridge      <- bridgeEntry(session)
          actual      <- jarEntryDigest(payload, s"bootstrap/compiler/${bridge.entry.relative.value}")
        yield assertTrue(
          distributed,
          !directory,
          session.manifest.scalaVersion == Scala3101,
          session.fingerprint == session.manifest.fingerprint,
          bridge.entry.digest == actual,
          session.tools.exists(_.entry.role == HostToolRole.Compiler),
          session.tools.exists(_.entry.role == HostToolRole.Interfaces),
          session.tools.exists(_.entry.role == HostToolRole.Runtime),
        )
      }
    },
    test("a missing bridge is EntryAbsent and does not solve") {
      hosted { (root, starts) =>
        for
          files    <- packaged
          payload  <- payloadJar(root, files, Some(HostToolRole.Bridge), corrupt = false)
          before   <- fixture(root)
          seen     <- Ref.make(0)
          api      <- apiJar(root.resolve("cache"))
          _        <- writeSources(root)
          exit     <- prepare(root, api, counting(seen), Some(payload)).either
          calls    <- seen.get
          after    <- readBytes(root.resolve("dependencies.lock"))
          count    <- starts.get
          checkout <- checkoutLock
        yield (exit, calls, before.sameElements(after), count, checkout)
      }.map { ran =>
        val (exit, calls, same, count, checkout) = ran.value
        assertTrue(
          calls == 0,
          same,
          count == 0,
          !fixtureText.contains("tool compiler"),
          checkout.isEmpty || checkout.linesIterator.exists(_.trim == "tool compiler 3.10.1 jvm"),
          rejects(exit) {
            case histos.load.LoadError.HostCompiler(HostCompilerError.EntryAbsent(HostToolRole.Bridge)) => true
            case _                                                                                      => false
          },
        )
      }
    },
    test("a corrupt bridge is BadDigest and does not solve") {
      hosted { (root, starts) =>
        for
          files   <- packaged
          payload <- payloadJar(root, files, None, corrupt = true)
          before  <- fixture(root)
          seen    <- Ref.make(0)
          api     <- apiJar(root.resolve("cache"))
          _       <- writeSources(root)
          exit    <- prepare(root, api, counting(seen), Some(payload)).either
          calls   <- seen.get
          after   <- readBytes(root.resolve("dependencies.lock"))
          count   <- starts.get
        yield (exit, calls, before.sameElements(after), count)
      }.map { ran =>
        val (exit, calls, same, count) = ran.value
        assertTrue(
          calls == 0,
          same,
          count == 0,
          rejects(exit) {
            case histos.load.LoadError.HostCompiler(HostCompilerError.BadDigest(HostToolRole.Bridge, _, _)) => true
            case _                                                                                          => false
          },
        )
      }
    },
    test("the packaged host rebuilds a mixed module, then a second load adds no compiler starts") {
      hosted { (root, starts) =>
        for
          files    <- packaged
          payload  <- payloadJar(root, files, None, corrupt = false)
          before   <- fixture(root)
          seen     <- Ref.make(0)
          api      <- apiJar(root.resolve("cache"))
          _        <- writeSources(root)
          store    <- ZIO.service[ContentStore]
          home     <- jdkHome
          loaded   <- prepare(root, api, counting(seen), Some(payload))
          session  <- HostCompilerInputs.materialize(payload, home, store).mapError(LoadError.HostCompiler(_))
          text     <- bridgeText(loaded.build, session)
          mid      <- starts.get
          _        <- prepare(root, api, counting(seen), Some(payload))
          end      <- starts.get
          calls    <- seen.get
          after    <- readBytes(root.resolve("dependencies.lock"))
          checkout <- checkoutLock
        yield (text, calls, before.sameElements(after), mid, end, checkout, !fixtureText.contains("tool compiler"))
      }.map { ran =>
        val (text, calls, same, mid, end, checkout, bare) = ran.value
        assertTrue(
          text == "from-java",
          calls == 0,
          same,
          bare,
          mid > 0,
          end == mid,
          checkout.isEmpty || checkout.linesIterator.exists(_.trim == "tool compiler 3.10.1 jvm"),
        )
      }
    },
    test("a lone histos.wiring.scala is not compiled") {
      ZIO.scoped {
        for
          root   <- work("histos-p10-wiring")
          api    <- apiJar(root.resolve("cache"))
          _      <- write(root.resolve("histos.scala"), plainMain)
          _      <- write(root.resolve("histos.wiring.scala"), "this is not scala {{{")
          loaded <- ZIO
            .serviceWithZIO[Loader](_.load(root, api))
            .provideSome[Scope](ZLayer.succeed(silent) >>> Loader.live)
        yield assertTrue(loaded.catalog.isEmpty, loaded.graph.projects.nonEmpty)
      }
    },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.minutes)

  private val fixtureText: String = "fixture\n"

  private val plainMain: String =
    """|import histos.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"3.10.1")
       |    Projects(app)
       |""".stripMargin

  private val bridgeCatalog: String =
    """|import histos.*
       |import histos.plugin.*
       |
       |object plugins extends HistosPlugins:
       |  val bridge = module("bridge", Jvm)(scala"3.10.1").in(ProjectPath("bridge"))
       |  def sources = PluginCatalog(PluginSources.module(bridge))
       |""".stripMargin

  private val marker: String =
    """|package demo;
       |public final class Marker {
       |  private Marker() {}
       |  public static String token() { return "from-java"; }
       |}
       |""".stripMargin

  private val bridge: String =
    """|package demo
       |object Bridge:
       |  def text: String = Marker.token()
       |""".stripMargin

  private final case class Ran[A](value: A)

  private def hosted[A](
      body: (Path, Ref[Int]) => ZIO[BuildCache.Env & histos.load.PluginProducts & Scope, LoadError, A]
  ): ZIO[Any, LoadError, Ran[A]] =
    ZIO.scoped {
      for
        root   <- work("histos-p10-host")
        cas    <- ZIO.attemptBlocking(Files.createDirectories(root.resolve("cas"))).orDie
        starts <- Ref.make(0)
        value  <- body(root, starts).provideSomeLayer[Scope](
          ZLayer.succeed(ContentStore.in(cas)) ++
            ZLayer.succeed(CompilerStarts(starts)) ++
            Compilers.live ++
            RemoteCache.off ++
            histos.load.PluginFlights.live ++ histos.load.PluginProducts.live
        )
      yield Ran(value)
    }

  private def packaged: ZIO[Scope, LoadError, List[AssemblyFile]] =
    for
      jdk   <- runningJdk
      files <- ZIO
        .serviceWithZIO[Resolve](
          _.resolveTool(CompilerRequests.compiler(Scala3101, Repositories.central))
            .mapError(LoadError.Resolve(_))
            .flatMap(classpath =>
              HostCompilerPackage
                .files(Scala3101, jdk, classpath.artifacts)
                .mapError(LoadError.HostCompiler(_))
            )
        )
        .provideSome[Scope](Resolve.coursier)
    yield files

  /** A zip `BuildClasspath.distribution` accepts: program class, nested api jar, and the packaged compiler entries. */
  private def payloadJar(
      root: Path,
      files: List[AssemblyFile],
      drop: Option[HostToolRole],
      corrupt: Boolean,
  ): IO[LoadError, Path] =
    val stage = root.resolve("stage")
    val jar   = root.resolve("host.jar")
    for
      manifest <- readManifest(files)
      dropped = drop match
        case None       => Set.empty[String]
        case Some(role) =>
          manifest.tools
            .filter(_.role == role)
            .map(entry => s"bootstrap/compiler/${entry.relative.value}")
            .toSet
      _ <- ZIO.foreachDiscard(files.filterNot(file => dropped.contains(file.entry))) { file =>
        copyFile(file.file, stage.resolve(file.entry))
      }
      _ <-
        if corrupt then
          manifest.tools.find(_.role == HostToolRole.Bridge) match
            case Some(entry) => flip(stage.resolve("bootstrap/compiler").resolve(entry.relative.value))
            case None        => ZIO.fail(LoadError.HostCompiler(HostCompilerError.EntryAbsent(HostToolRole.Bridge)))
        else ZIO.unit
      _ <- writeBytes(stage.resolve("histos/cli/Main.class"), Array[Byte](0xca.toByte, 0xfe.toByte))
      _ <- writeBytes(stage.resolve(BuildClasspath.Entry), Array[Byte](0x50, 0x4b))
      _ <- ClassJar.write(stage, jar).mapError(LoadError.Jar(_))
    yield jar

  private def classesDirectory(root: Path): IO[LoadError, Path] =
    val dir = root.resolve("classes")
    writeBytes(dir.resolve("demo/Marker.class"), Array[Byte](0xca.toByte, 0xfe.toByte)).as(dir)

  private def copyFile(from: Path, dest: Path): IO[LoadError, Unit] =
    ZIO
      .attemptBlocking {
        Option(dest.getParent).foreach(parent => Files.createDirectories(parent))
        Files.copy(from, dest, StandardCopyOption.REPLACE_EXISTING)
        ()
      }
      .mapError(_ => LoadError.Io(IoFailure(dest, IoFault.Write)))

  private def readManifest(files: List[AssemblyFile]): IO[LoadError, HostCompilerManifest] =
    files.find(_.entry == HostCompilerInputs.ManifestPath) match
      case None       => ZIO.fail(LoadError.HostCompiler(HostCompilerError.ManifestAbsent))
      case Some(file) =>
        ZIO
          .attemptBlocking(Files.readString(file.file, StandardCharsets.UTF_8))
          .mapError(_ => LoadError.Io(IoFailure(file.file, IoFault.Read)))
          .flatMap: text =>
            ZIO
              .fromEither(text.fromJson[HostCompilerManifest])
              .mapError(_ => LoadError.HostCompiler(HostCompilerError.ManifestUnreadable))

  private def bridgeEntry(session: HostCompilerSession): IO[LoadError, histos.toolchain.MaterializedTool] =
    session.tools.find(_.entry.role == HostToolRole.Bridge) match
      case Some(tool) => ZIO.succeed(tool)
      case None       => ZIO.fail(LoadError.HostCompiler(HostCompilerError.EntryAbsent(HostToolRole.Bridge)))

  private def jarEntryDigest(jar: Path, name: String): IO[LoadError, ContentHash] =
    ZIO
      .attemptBlocking {
        val zip = new ZipFile(jar.toFile)
        try
          Option(zip.getEntry(name)).map { entry =>
            val input = zip.getInputStream(entry)
            try input.readAllBytes()
            finally input.close()
          }
        finally zip.close()
      }
      .mapError(_ => LoadError.Io(IoFailure(jar, IoFault.Read)))
      .flatMap:
        case Some(bytes) => ZIO.succeed(ContentHash.trusted(Sha256Digest.of(Chunk.fromArray(bytes)).value))
        case None        => ZIO.fail(LoadError.HostCompiler(HostCompilerError.EntryAbsent(HostToolRole.Bridge)))

  private def flip(path: Path): IO[LoadError, Unit] =
    ZIO
      .attemptBlocking(Files.readAllBytes(path))
      .mapError(_ => LoadError.Io(IoFailure(path, IoFault.Read)))
      .flatMap: bytes =>
        if bytes.length == 0 then ZIO.fail(LoadError.Io(IoFailure(path, IoFault.Read)))
        else
          bytes(bytes.length - 1) = (bytes(bytes.length - 1) ^ 0xff.toByte).toByte
          ZIO
            .attemptBlocking(Files.write(path, bytes))
            .mapError(_ => LoadError.Io(IoFailure(path, IoFault.Write)))
            .unit

  private def writeSources(root: Path): IO[LoadError, Unit] =
    write(root.resolve("histos.plugins.scala"), bridgeCatalog) *>
      write(root.resolve("histos.scala"), plainMain) *>
      write(root.resolve("bridge/src/demo/Marker.java"), marker) *>
      write(root.resolve("bridge/src/demo/Bridge.scala"), bridge)

  private def fixture(root: Path): IO[LoadError, Array[Byte]] =
    write(root.resolve("dependencies.lock"), fixtureText) *> readBytes(root.resolve("dependencies.lock"))

  private def prepare(
      root: Path,
      api: Path,
      resolve: Resolve,
      payload: Option[Path],
  ): ZIO[BuildCache.Env & histos.load.PluginProducts & Scope, LoadError, histos.load.LoadedBuild] =
    BootstrapLoad.prepare(
      resolve,
      BootstrapLoad.Prepare(root, api, payload = payload),
    )

  private def bridgeText(
      build: HistosBuild,
      session: HostCompilerSession,
  ): ZIO[Scope, LoadError, String] =
    classesOf(build) match
      case None          => ZIO.fail(LoadError.Io(IoFailure(Path.of("demo/Bridge.class"), IoFault.Stat)))
      case Some(classes) =>
        val runtime = session.tools.filter(_.entry.role == HostToolRole.Runtime).map(_.path)
        ZIO
          .acquireRelease(
            ZIO
              .attemptBlocking(
                new URLClassLoader((classes :: runtime).map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader)
              )
              .mapError(_ => LoadError.Io(IoFailure(classes, IoFault.Read)))
          )(loader => ZIO.attemptBlocking(loader.close()).ignore)
          .flatMap: loader =>
            ZIO
              .attemptBlocking {
                val cls    = loader.loadClass("demo.Bridge$")
                val module = cls.getField("MODULE$").get(null)
                cls.getMethod("text").invoke(module)
              }
              .mapError(_ => LoadError.Io(IoFailure(classes, IoFault.Read)))
              .flatMap:
                case text: String => ZIO.succeed(text)
                case _            => ZIO.fail(LoadError.Io(IoFailure(classes, IoFault.Read)))

  private def classesOf(build: HistosBuild): Option[Path] =
    build.getClass.getClassLoader match
      case loader: URLClassLoader =>
        loader.getURLs.toList
          .flatMap(url => scala.util.Try(Path.of(url.toURI)).toOption)
          .find(path => Files.isRegularFile(path.resolve("demo/Bridge.class")))
      case _ => None

  private def runningJdk: IO[LoadError, histos.jdk.JdkVersion] =
    Option(System.getProperty("java.home")).filter(_.nonEmpty) match
      case None      => ZIO.fail(LoadError.Io(IoFailure(Path.of("java.home"), IoFault.Stat)))
      case Some(dir) =>
        val home = Path.of(dir)
        Jdk
          .home(home)
          .mapError:
            case JdkError.Io(failure) => LoadError.Io(failure)
            case JdkError.BadMajor(_) => LoadError.Io(IoFailure(home, IoFault.Read))
          .flatMap: found =>
            found.release match
              case Left(_)        => ZIO.fail(LoadError.Io(IoFailure(home.resolve("release"), IoFault.Read)))
              case Right(release) => ZIO.succeed(release.version)

  private def jdkHome: IO[LoadError, Path] =
    Option(System.getProperty("java.home")).filter(_.nonEmpty) match
      case Some(dir) => ZIO.succeed(Path.of(dir))
      case None      => ZIO.fail(LoadError.Io(IoFailure(Path.of("java.home"), IoFault.Stat)))

  private def checkoutLock: IO[LoadError, String] =
    val start = Option(System.getProperty("user.dir")).filter(_.nonEmpty).fold(Path.of("."))(Path.of(_))
    walkLock(start.toAbsolutePath.normalize, 8)

  private def walkLock(dir: Path, left: Int): IO[LoadError, String] =
    val lock = dir.resolve("dependencies.lock")
    ZIO
      .attemptBlocking(Files.isRegularFile(lock))
      .mapError(_ => LoadError.Io(IoFailure(lock, IoFault.Stat)))
      .flatMap:
        case true =>
          ZIO
            .attemptBlocking(Files.readString(lock, StandardCharsets.UTF_8))
            .mapError(_ => LoadError.Io(IoFailure(lock, IoFault.Read)))
        case false =>
          Option(dir.getParent) match
            case Some(parent) if left > 0 => walkLock(parent, left - 1)
            case _                        => ZIO.succeed("")

  private def apiJar(cache: Path): IO[LoadError, Path] =
    val location = Option(classOf[HistosBuild].getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
    location match
      case None      => ZIO.fail(histos.load.LoadError.MissingFile(Path.of("histos-api.jar")))
      case Some(url) =>
        ZIO
          .attempt(Path.of(url.toURI))
          .mapError(_ => LoadError.Io(IoFailure(Path.of(url.toString), IoFault.Read)))
          .flatMap(path => BuildClasspath(path, cache))

  private def writeBytes(path: Path, bytes: Array[Byte]): IO[LoadError, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.write(path, bytes)
        ()
      }
      .mapError(_ => LoadError.Io(IoFailure(path, IoFault.Write)))

  private def write(path: Path, text: String): IO[LoadError, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.writeString(path, text, StandardCharsets.UTF_8)
        ()
      }
      .mapError(_ => LoadError.Io(IoFailure(path, IoFault.Write)))

  private def readBytes(path: Path): IO[LoadError, Array[Byte]] =
    ZIO.attemptBlocking(Files.readAllBytes(path)).mapError(_ => LoadError.Io(IoFailure(path, IoFault.Read)))

  private def work(name: String): ZIO[Scope, LoadError, Path] =
    Workspace.work(name).mapError(LoadError.Io(_))

  private val silent: Resolve =
    new Resolve:
      def resolveTool(request: ToolRequest)                          = ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell)                  = ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact)             = ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: Repositories) = ZIO.dieMessage("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: histos.resolve.Repository) =
        ZIO.dieMessage("sources")

  private def counting(seen: Ref[Int]): Resolve =
    new Resolve:
      def resolveTool(request: ToolRequest) =
        seen.update(_ + 1) *> ZIO.dieMessage("resolveTool")
      def resolve(cell: histos.resolve.ResolveCell) =
        seen.update(_ + 1) *> ZIO.dieMessage("resolve")
      def fetch(artifact: histos.resolve.LockedArtifact) =
        seen.update(_ + 1) *> ZIO.dieMessage("fetch")
      def resolvePlugin(dependency: Dep, repositories: Repositories) =
        seen.update(_ + 1) *> ZIO.dieMessage("resolvePlugin")
      def sources(organization: String, name: String, revision: String, repository: histos.resolve.Repository) =
        seen.update(_ + 1) *> ZIO.dieMessage("sources")

  private def rejects(exit: Either[LoadError, ?])(accept: LoadError => Boolean): Boolean =
    exit match
      case Left(error) => accept(error)
      case Right(_)    => false

  private type LoadError = histos.load.LoadError
  private val LoadError = histos.load.LoadError
end PluginArchitectureIntegrationSpec
