package histos.publish

import histos.assembly.{Assemble, AssembleError, AssemblyPath}
import histos.jdk.{JdkRuntime, Machine}
import histos.native.NativeOrigin
import histos.program.Program
import histos.resolve.ResolutionService
import histos.schedule.{CacheConfig, RemoteCache}
import histos.toolchain.Runtimes
import histos.version.Ship
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.CompileFeed
import histos.{Module, PairAttempt, PairError, PairProof, ProjectGraph, ToolchainIdentity}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*

import zio.{IO, Scope, UIO, ZIO, ZLayer}

/** The toolchain half of a publish: pack the launcher, link the host client, prove the pair, then install. */
object DistStock:
  /** The live prover. A test provides its own [[PairProof]] through [[histos.HistosBuild.dist]]. */
  val live: ZLayer[Any, Nothing, PairProof] =
    ZLayer.succeed(Live)

  /** Ships in `modules` that name `.toolchain`. */
  def asked(graph: ProjectGraph, modules: List[PublishedModule]): List[Ship] =
    val names = modules.foldLeft(Set.empty[histos.version.ShipName])((seen, module) => seen + module.ship)
    graph.catalog.ships.filter(ship => names.contains(ship.name) && ship.toolchain.isDefined)

  /** Pack, link, and prove each asked ship. The files stay in the checkout. Nothing is installed or uploaded. */
  def prove(
      repo: Path,
      graph: ProjectGraph,
      ships: List[Ship],
      jdk: JdkRuntime,
      build: histos.HistosBuild,
      note: String => UIO[Unit],
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore & Scope, ToolchainPublishError, List[Placed]] =
    ZIO.foreach(ships)(ship => one(repo, graph, ship, jdk, build, note))

  /** `~/.histos/toolchains/<id>` and `.histos/toolchain`. `home` is the user home, not a hardcoded path. */
  def install(home: Path, repo: Path, placed: Placed): IO[ToolchainPublishError, Unit] =
    ZIO
      .fromEither(segment(placed.id))
      .flatMap: id =>
        val dir = home.resolve(".histos").resolve("toolchains").resolve(id)
        write(dir, repo, id, placed)

  def host(machine: Machine): Either[ToolchainPublishError, DistPlatform] =
    machine.host match
      case histos.toolchain.HostPlatform.MacosAarch64 => Right(DistPlatform.MacosAarch64)
      case histos.toolchain.HostPlatform.LinuxX86_64  => Right(DistPlatform.LinuxX86_64)
      case other                                      => Left(ToolchainPublishError.Unsupported(other.description))

  final case class Placed(id: String, platform: DistPlatform, jar: Path, client: Path, manifest: String)

  private object Live extends PairProof:
    def prove(attempt: PairAttempt): IO[PairError, Unit] =
      ZIO.scoped:
        for
          copy <- copyOf(attempt.checkout)
          _    <- note(copy, attempt)
        yield ()

    private def note(copy: Path, attempt: PairAttempt): IO[PairError, Unit] =
      run(attempt.client, copy, attempt.jar, attempt.wanted, List("version")).flatMap: printed =>
        val reported = first(printed).getOrElse("")
        PairProof.judge(
          attempt.wanted,
          reported,
          run(attempt.client, copy, attempt.jar, attempt.wanted, List("graph")).unit,
        )
  end Live

  private def one(
      repo: Path,
      graph: ProjectGraph,
      ship: Ship,
      jdk: JdkRuntime,
      build: histos.HistosBuild,
      note: String => UIO[Unit],
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore & Scope, ToolchainPublishError, Placed] =
    ship.toolchain match
      case None =>
        ZIO.fail(ToolchainPublishError.Upload(s"${ship.name.value} has no toolchain"))
      case Some(linked) =>
        for
          machine  <- Machine.load(repo).mapError(error => ToolchainPublishError.Unsupported(error.message))
          platform <- ZIO.fromEither(host(machine))
          identity <- ToolchainIdentity.discover(repo).flatMap {
            case Some(found) => ZIO.succeed(found)
            case None        => ZIO.fail(ToolchainPublishError.Upload("could not read the commit at HEAD"))
          }
          launcher <- ZIO.fromEither(module(graph, linked.launcher.value))
          _        <- note(s"packing ${linked.launcher.value}")
          packed   <- pack(repo, graph, jdk, machine, launcher)
          client = repo.resolve(DistLayout.builtClient(platform))
          _ <- note(s"linking ${platform.token}")
          _ <- link(repo, graph, jdk, machine, platform, linked.client.value, client)
          wanted  = s"histos ${identity.coordinate}"
          attempt = PairAttempt(wanted, packed, client, repo)
          proved  = ZIO.serviceWithZIO[PairProof](_.prove(attempt)).provideLayer(live >>> build.dist)
          _        <- ToolchainPublish.commit(platform, proved, ZIO.unit)
          manifest <- ZIO
            .attemptBlocking(manifestOf(identity.coordinate, platform, packed, client))
            .mapError(_ => ToolchainPublishError.Install(IoFailure(packed, IoFault.Read)))
        yield Placed(identity.coordinate, platform, packed, client, manifest)

  private def module(graph: ProjectGraph, name: String): Either[ToolchainPublishError, Module] =
    histos.ProjectName.from(name) match
      case Left(_)       => Left(ToolchainPublishError.Pack(AssembleError.NoModule(name)))
      case Right(parsed) =>
        graph.get(parsed) match
          case Some(found: Module) => Right(found)
          case _                   => Left(ToolchainPublishError.Pack(AssembleError.NoModule(name)))

  private def pack(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      machine: Machine,
      launcher: Module,
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore & Scope, ToolchainPublishError, Path] =
    opened(repo, graph)(
      Assemble
        .run(
          repo,
          graph,
          jdk,
          NativeOrigin.read(machine.env),
          launcher.name,
          AssemblyPath.output(repo, launcher),
          quiet,
        )
        .mapBoth(ToolchainPublishError.Pack(_), _.path)
    )

  private def link(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      machine: Machine,
      platform: DistPlatform,
      moduleName: String,
      destination: Path,
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore & Scope, ToolchainPublishError, Unit] =
    Runtimes
      .read(machine, graph.runnerTools.nodePath)
      .flatMap: runtimes =>
        opened(repo, graph)(
          Program
            .link(repo, graph, moduleName, destination, jdk, runtimes, quiet)
            .mapError(error => ToolchainPublishError.Link(platform, error))
            .unit
        )

  private def opened[A](repo: Path, graph: ProjectGraph)(
      body: ZIO[
        histos.resolve.Resolve & histos.cache.ContentStore & ResolutionService & RemoteCache,
        ToolchainPublishError,
        A,
      ]
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore & Scope, ToolchainPublishError, A] =
    for
      env    <- CacheConfig.processEnv
      remote <- CacheConfig.open(env).mapError(error => ToolchainPublishError.Upload(error.message))
      result <- body.provideSomeLayer[histos.resolve.Resolve & histos.cache.ContentStore](
        ZLayer.succeed(remote) ++ ResolutionService
          .layer(repo, graph)
          .mapError(error => ToolchainPublishError.Upload(error.message))
      )
    yield result

  private def quiet: CompileFeed => UIO[Unit] =
    _ => ZIO.unit

  private def manifestOf(version: String, platform: DistPlatform, jar: Path, client: Path): String =
    val jarSha    = DistLayout.sha256(Files.readAllBytes(jar))
    val clientSha = DistLayout.sha256(Files.readAllBytes(client))
    DistLayout.manifestJson(version, jarSha, List(platform -> clientSha))

  private def write(dir: Path, repo: Path, id: String, placed: Placed): IO[ToolchainPublishError, Unit] =
    val pointer = repo.resolve(".histos").resolve("toolchain")
    ZIO
      .attemptBlocking {
        Files.createDirectories(dir)
        Files.copy(placed.jar, dir.resolve("histos.jar"), StandardCopyOption.REPLACE_EXISTING)
        val client = dir.resolve("histos-client")
        Files.copy(placed.client, client, StandardCopyOption.REPLACE_EXISTING)
        Files.setPosixFilePermissions(client, java.nio.file.attribute.PosixFilePermissions.fromString("rwxr-xr-x"))
        Files.writeString(dir.resolve("manifest.json"), placed.manifest)
        Option(pointer.getParent).foreach(Files.createDirectories(_))
        Files.writeString(pointer, id + "\n")
        ()
      }
      .mapError(_ => ToolchainPublishError.Install(IoFailure(dir, IoFault.Write)))

  private def segment(id: String): Either[ToolchainPublishError, String] =
    if id.nonEmpty && id != "." && id != ".." && !id.contains('/') && !id.contains('\\') && !id.exists(_.isWhitespace)
    then Right(id)
    else Left(ToolchainPublishError.Install(IoFailure(Path.of(id), IoFault.Write)))

  private val skip = Set(".git", "target", ".histos", "dist")

  private def copyOf(from: Path): ZIO[Scope, PairError, Path] =
    ZIO.acquireRelease(
      ZIO
        .attemptBlocking {
          val dest  = Files.createTempDirectory("histos-pair")
          val root  = from.toAbsolutePath.normalize
          val walk  = Files.walk(root)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.foreach(path => copyOne(root, dest, path))
          dest
        }
        .mapError(thrown => PairError.Load(text(thrown)))
    )(dir => ZIO.attemptBlocking(deleteTree(dir)).ignore)

  private def copyOne(root: Path, dest: Path, path: Path): Unit =
    val relative = root.relativize(path)
    val dropped  = relative.iterator.asScala.exists(part => skip.contains(part.toString))
    if path != root && !dropped && !Files.isSymbolicLink(path) && Files.isRegularFile(path) then
      val target = dest.resolve(relative.toString)
      Option(target.getParent).foreach(Files.createDirectories(_))
      Files.copy(path, target, StandardCopyOption.REPLACE_EXISTING)

  private def deleteTree(root: Path): Unit =
    if Files.exists(root) then
      val walk  = Files.walk(root)
      val paths =
        try walk.iterator.asScala.toList
        finally walk.close()
      paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))

  private def run(
      client: Path,
      checkout: Path,
      jar: Path,
      wanted: String,
      args: List[String],
  ): IO[PairError, String] =
    ZIO
      .attemptBlocking {
        val process = new ProcessBuilder((client.toAbsolutePath.toString :: args)*)
          .directory(checkout.toFile)
          .redirectErrorStream(true)
        val env = process.environment()
        env.put("HISTOS_JAR", jar.toAbsolutePath.toString)
        env.put("HISTOS_VERSION", wanted)
        env.put("HISTOS_ROOT", checkout.toAbsolutePath.toString)
        val started = process.start()
        val printed = String(started.getInputStream.readAllBytes, StandardCharsets.UTF_8)
        val code    = started.waitFor()
        (code, printed)
      }
      .mapError(thrown => PairError.Exec(text(thrown)))
      .flatMap: (code, printed) =>
        if code == 0 then ZIO.succeed(printed)
        else ZIO.fail(PairError.Exec(first(printed).getOrElse(s"client exited $code")))

  private def first(text: String): Option[String] =
    text.linesIterator.filter(_.nonEmpty).toList match
      case line :: _ => Some(line)
      case Nil       => None

  private def text(thrown: Throwable): String =
    val message = thrown.getMessage
    if message == null || message.isEmpty then thrown.getClass.getSimpleName else message
end DistStock
