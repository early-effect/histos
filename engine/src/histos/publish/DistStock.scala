package histos.publish

import histos.assembly.{Assemble, AssembleError, AssemblyPath}
import histos.jdk.{JdkRuntime, Machine}
import histos.native.NativeOrigin
import histos.program.{ClientKit, Program}
import histos.resolve.ResolutionService
import histos.schedule.CacheConfig
import histos.toolchain.Runtimes
import histos.version.Ship
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.CompileFeed
import histos.{Module, ProjectGraph, ToolchainIdentity}

import java.nio.file.{Files, Path, StandardCopyOption}

import zio.{IO, Scope, UIO, ZIO, ZLayer}

/** The toolchain half of a publish: pack the launcher and compile the link kit. Nothing is linked. */
object DistStock:
  /** Ships in `modules` that name `.toolchain`. */
  def asked(graph: ProjectGraph, modules: List[PublishedModule]): List[Ship] =
    val names = modules.foldLeft(Set.empty[histos.version.ShipName])((seen, module) => seen + module.ship)
    graph.catalog.ships.filter(ship => names.contains(ship.name) && ship.toolchain.isDefined)

  /** Pack and compile each asked ship. The files stay in the checkout. Nothing is installed or uploaded. */
  def prove(
      repo: Path,
      graph: ProjectGraph,
      ships: List[Ship],
      jdk: JdkRuntime,
      note: String => UIO[Unit],
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore & Scope, ToolchainPublishError, List[Placed]] =
    ZIO.foreach(ships)(ship => one(repo, graph, ship, jdk, note))

  /** `~/.histos/toolchains/<id>` and `.histos/toolchain`. `home` is the user home, not a hardcoded path. */
  def install(home: Path, repo: Path, placed: Placed): IO[ToolchainPublishError, Unit] =
    ZIO
      .fromEither(segment(placed.id))
      .flatMap: id =>
        val dir = home.resolve(".histos").resolve("toolchains").resolve(id)
        write(dir, repo, id, placed)

  final case class Placed(id: String, jar: Path, kit: Path, manifest: String)

  private def one(
      repo: Path,
      graph: ProjectGraph,
      ship: Ship,
      jdk: JdkRuntime,
      note: String => UIO[Unit],
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore & Scope, ToolchainPublishError, Placed] =
    ship.toolchain match
      case None =>
        ZIO.fail(ToolchainPublishError.Upload(s"${ship.name.value} has no toolchain"))
      case Some(linked) =>
        for
          machine <- Machine.load(repo).mapError(error => ToolchainPublishError.Upload(error.message))
          identity <- ToolchainIdentity.discover(repo).flatMap {
            case Some(found) => ZIO.succeed(found)
            case None        => ZIO.fail(ToolchainPublishError.Upload("could not read the commit at HEAD"))
          }
          launcher <- ZIO.fromEither(module(graph, linked.launcher.value))
          _        <- note(s"packing ${linked.launcher.value}")
          packed   <- pack(repo, graph, jdk, machine, launcher)
          kit = repo.resolve(DistLayout.BuiltKit)
          _        <- note(s"compiling ${linked.client.value}")
          _        <- compileKit(repo, graph, jdk, machine, linked.client.value, kit)
          manifest <- ZIO
            .attemptBlocking(manifestOf(identity.coordinate, packed, kit))
            .mapError(_ => ToolchainPublishError.Install(IoFailure(packed, IoFault.Read)))
        yield Placed(identity.coordinate, packed, kit, manifest)

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

  private def compileKit(
      repo: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      machine: Machine,
      moduleName: String,
      destination: Path,
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore & Scope, ToolchainPublishError, Unit] =
    Runtimes.read(machine, graph.runnerTools.nodePath).flatMap: runtimes =>
      opened(repo, graph)(
        Program
          .exportKit(repo, graph, moduleName, destination, jdk, runtimes, quiet)
          .mapError(ToolchainPublishError.Kit(_))
          .unit
      )

  private def opened[A](repo: Path, graph: ProjectGraph)(
      body: ZIO[
        histos.resolve.Resolve & histos.cache.ContentStore & ResolutionService & histos.schedule.RemoteCache,
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

  private def manifestOf(version: String, jar: Path, kit: Path): String =
    val jarSha = DistLayout.sha256(Files.readAllBytes(jar))
    val kitSha = DistLayout.sha256(Files.readAllBytes(kit))
    DistLayout.manifestJson(version, jarSha, Some(DistLayout.kit(version) -> kitSha))

  private def write(dir: Path, repo: Path, id: String, placed: Placed): IO[ToolchainPublishError, Unit] =
    val pointer = repo.resolve(".histos").resolve("toolchain")
    ZIO
      .attemptBlocking {
        Files.createDirectories(dir)
        Files.copy(placed.jar, dir.resolve("histos.jar"), StandardCopyOption.REPLACE_EXISTING)
        Files.copy(placed.kit, dir.resolve(ClientKit.FileName), StandardCopyOption.REPLACE_EXISTING)
        List("histos-client", "client.stamp", "client.origin", "client.choice").foreach: name =>
          Files.deleteIfExists(dir.resolve(name))
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
end DistStock
