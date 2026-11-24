package histos.publish

import histos.central.{Publisher, SnapshotRepository}
import histos.jdk.{JdkRuntime, Machine}
import histos.plugin.ApiDigest
import histos.version.{Revision as ShipRevision, ShipName}
import histos.workspace.Workspace
import histos.zinc.CompileFeed
import histos.{PublishFault, PublishReceipt, PublishRequest, StagedDependency, StagedModule}
import histos.ProjectGraph

import java.nio.file.{Files, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Scope, UIO, ZIO, ZLayer}

/** Stage unsigned Maven files, then `provideLayer` the build's publisher. */
object PublishRun:
  final class Report(val lines: List[String])

  /** Compile the selected closure and compare it. No artifact is published and the snapshot pointer does not move. */
  def check(
      repo: Path,
      graph: ProjectGraph,
      tokens: List[String],
      jdk: JdkRuntime,
      env: Map[String, String],
      api: ApiDigest,
      scope: ProofScope,
      report: CompileFeed => UIO[Unit],
      note: String => UIO[Unit],
      plugins: Option[histos.load.PluginRuntime] = None,
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore, LocalPublishError, Report] =
    ZIO.scoped:
      for
        names  <- ZIO.fromEither(PublishSelect.resolve(graph, tokens)).tapError(error => note(error.message))
        staged <- stage(repo, graph, names, jdk, env, PublishWrite.Check, api, report, note, plugins, scope)
      yield Report(show(staged, Nil))

  def local(
      repo: Path,
      graph: ProjectGraph,
      build: histos.HistosBuild,
      tokens: List[String],
      jdk: JdkRuntime,
      env: Map[String, String],
      api: ApiDigest,
      scope: ProofScope,
      report: CompileFeed => UIO[Unit],
      note: String => UIO[Unit],
      plugins: Option[histos.load.PluginRuntime] = None,
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore & IvyRoot, LocalPublishError, Report] =
    ZIO.scoped:
      for
        root   <- ZIO.service[IvyRoot]
        names  <- ZIO.fromEither(PublishSelect.resolve(graph, tokens)).tapError(error => note(error.message))
        staged <- stage(repo, graph, names, jdk, env, PublishWrite.Local, api, report, note, plugins, scope)
        placed <- toolchain(repo, graph, staged, jdk, note)
        lines  <- localLines(repo, staged, ZLayer.succeed(root) >>> LocalStock.layer(note) >>> build.local)
        _      <- packLocal(repo, graph, staged, placed, note)
        _      <- localAssets(repo, graph, staged, placed)
      yield Report(show(staged, lines))

  def snapshots(
      repo: Path,
      graph: ProjectGraph,
      build: histos.HistosBuild,
      tokens: List[String],
      jdk: JdkRuntime,
      env: Map[String, String],
      api: ApiDigest,
      report: CompileFeed => UIO[Unit],
      note: String => UIO[Unit],
      plugins: Option[histos.load.PluginRuntime] = None,
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore, LocalPublishError, Report] =
    val layer = sonaLayer(env, build, note)
    ZIO.scoped:
      for
        publishing <- opened(layer)
        names      <- ZIO.fromEither(PublishSelect.resolve(graph, tokens)).tapError(error => note(error.message))
        _          <- ZIO.when(sonaSecrets(publishing))(
          ZIO.fromEither(PublishWhere.snapshots(env)).tapError(error => note(error.message)).unit
        )
        staged <- stage(
          repo,
          graph,
          names,
          jdk,
          env,
          PublishWrite.Snapshots,
          api,
          report,
          note,
          plugins,
          ProofScope.All,
        )
        _     <- toolchain(repo, graph, staged, jdk, note)
        plans <- ZIO
          .fromEither(DistChannel.plan(graph, staged.output.modules))
          .tapError(error => note(error.message))
        filled <- DistChannel.prepare(repo, env, plans).tapError(error => note(error.message))
        _      <- ZIO.fromEither(DistChannel.ready(repo, filled, plans)).tapError(error => note(error.message))
        lines  <- localLines(repo, staged, layer)
        _      <- ZIO.foreachDiscard(plans)(plan => note(s"uploading ${DistLayout.channel(plan.revision)}"))
        noted  <- DistChannel.upload(repo, filled, plans, lines).tapError(error => note(error.message))
      yield Report(show(staged, lines) ++ noted)

  def release(
      repo: Path,
      graph: ProjectGraph,
      build: histos.HistosBuild,
      tokens: List[String],
      jdk: JdkRuntime,
      env: Map[String, String],
      automatic: Boolean,
      api: ApiDigest,
      report: CompileFeed => UIO[Unit],
      note: String => UIO[Unit],
      plugins: Option[histos.load.PluginRuntime] = None,
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore, ReleaseError, ReleaseReport] =
    val layer = sonaLayer(env, build, note)
    ZIO.scoped:
      for
        publishing <- opened(layer)
        _          <- ZIO.when(sonaSecrets(publishing))(SonaStock.gate(env).tapError(error => note(error.message)))
        names      <- ZIO
          .fromEither(PublishSelect.resolve(graph, tokens))
          .tapError(error => note(error.message))
          .mapError(ReleaseError.Stage(_))
        staged <- stage(repo, graph, names, jdk, env, PublishWrite.Release, api, report, note, plugins, ProofScope.All)
          .mapError(
            ReleaseError.Stage(_)
          )
        _      <- toolchain(repo, graph, staged, jdk, note).mapError(ReleaseError.Stage(_))
        pieces <- releasePieces(repo, staged, automatic, layer)
      yield ReleaseReport(
        staged.output.refused ++ pieces.refused,
        staged.output.held ++ pieces.held,
        pieces.failed,
        pieces.deployed,
      )

  def promote(repo: Path, env: Map[String, String], id: String, note: String => UIO[Unit]): IO[ReleaseError, Unit] =
    ReleaseRun.promote(repo, env, id, note)

  /** The graph is read for the asset name. `publish drop` still takes only a deployment id. */
  def drop(
      repo: Path,
      graph: ProjectGraph,
      env: Map[String, String],
      id: String,
      note: String => UIO[Unit],
  ): IO[ReleaseError, Unit] =
    ReleaseRun.drop(repo, graph, env, id, note)

  /** The jar named on the assembly, for a ship whose deployment file says `VALIDATED`. */
  def assets(
      repo: Path,
      graph: ProjectGraph,
      tokens: List[String],
      env: Map[String, String],
  ): IO[AssetError, AssetReport] =
    AssetRun.publish(repo, graph, tokens, env)

  /** Compile the link kit for each staged toolchain ship. A ship with no `.toolchain` is skipped. The launcher jar is
    * packed later, from ivy.
    */
  private def toolchain(
      repo: Path,
      graph: ProjectGraph,
      staged: StagedTree,
      jdk: JdkRuntime,
      note: String => UIO[Unit],
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore & Scope, LocalPublishError, List[DistStock.Placed]] =
    DistStock
      .prove(repo, graph, DistStock.asked(graph, staged.output.modules), jdk, note)
      .mapError(error => LocalPublishError.Stock(error.message))

  /** `dist/install` is the local installer input: the manifest names the ivy coordinate `publish local` wrote. */
  private def localAssets(
      repo: Path,
      graph: ProjectGraph,
      staged: StagedTree,
      placed: List[DistStock.Placed],
  ): IO[LocalPublishError, Unit] =
    placed match
      case Nil => ZIO.unit
      case _   =>
        ZIO
          .fromEither(launcherModule(graph, staged))
          .flatMap: module =>
            ZIO.foreachDiscard(placed)(item => writeAssets(repo, item, module))

  private def launcherModule(
      graph: ProjectGraph,
      staged: StagedTree,
  ): Either[LocalPublishError, PublishedModule] =
    graph.catalog.ships.flatMap(_.toolchain.toList) match
      case Nil         => Left(LocalPublishError.Stock("no toolchain ship was packed"))
      case linked :: _ =>
        staged.output.modules.find(item => item.module == linked.launcher) match
          case Some(found) => Right(found)
          case None        => Left(LocalPublishError.Stock("publish local did not stage the launcher"))

  /** Ivy is already published. Resolve that coordinate and pack the toolchain jar, then write the same manifest
    * `dist/install` serves.
    */
  private def packLocal(
      repo: Path,
      graph: ProjectGraph,
      staged: StagedTree,
      placed: List[DistStock.Placed],
      note: String => UIO[Unit],
  ): ZIO[histos.resolve.Resolve & Scope, LocalPublishError, Unit] =
    placed match
      case Nil => ZIO.unit
      case _   =>
        for
          module  <- ZIO.fromEither(launcherModule(graph, staged))
          machine <- Machine.load(repo).mapError(error => LocalPublishError.Stock(error.message))
          _       <- ZIO.foreachDiscard(placed): item =>
            for
              work <- Workspace.work("histos-local-pack").mapError(error => LocalPublishError.Stock(error.message))
              jar = work.resolve("histos.jar")
              _ <- DistStock
                .pack(module, jar)
                .mapError(error => LocalPublishError.Stock(error.message))
              text <- ZIO
                .attemptBlocking(localManifest(item.id, module, item.kit))
                .mapError(_ => LocalPublishError.Io(item.kit.toString))
              _ <- DistStock
                .install(Path.of(machine.userHome), item.id, jar, item.kit, text)
                .mapError(error => LocalPublishError.Stock(error.message))
              _ <- note(s"HISTOS_PIN=${item.id}")
            yield ()
        yield ()

  /** The manifest names the ivy coordinate. The toolchain jar is built from that coordinate on this machine. */
  private def writeAssets(
      repo: Path,
      placed: DistStock.Placed,
      module: PublishedModule,
  ): IO[LocalPublishError, Unit] =
    val install = repo.resolve("dist").resolve("install")
    val name    = DistLayout.kit(placed.id)
    ZIO
      .attemptBlocking {
        Files.createDirectories(install)
        Files.copy(placed.kit, install.resolve(name), StandardCopyOption.REPLACE_EXISTING)
        val text = localManifest(placed.id, module, placed.kit)
        Files.writeString(install.resolve("manifest.json"), text)
        ()
      }
      .mapError(_ => LocalPublishError.Io(install.toString))

  private def localManifest(id: String, module: PublishedModule, kit: Path): String =
    val kitSha = DistLayout.sha256(Files.readAllBytes(kit))
    DistLayout.manifestJson(
      id,
      DistLayout.MavenPin(module.group.value, module.artifact, module.revision),
      Some(DistLayout.kit(id) -> kitSha),
      Some(DistLayout.IvyPin(module.group.value, module.artifact, module.revision)),
    )

  private def show(staged: StagedTree, lines: List[String]): List[String] =
    staged.output.notes ++ staged.output.skipped ++ staged.output.refused ++ staged.output.held ++ lines

  /** Idle refuses every revision. Workflow reads that stock. A command supplies `SonaStock` on Central's snapshots. */
  private def sonaLayer(
      env: Map[String, String],
      build: histos.HistosBuild,
      note: String => UIO[Unit],
  ): ZLayer[Any, Nothing, histos.Publishing] =
    ZLayer.succeed(SnapshotRepository.central) >>> SonaStock.layer(env, note) >>> build.sona

  private def opened(layer: ZLayer[Any, Nothing, histos.Publishing]): UIO[histos.Publishing] =
    ZIO.service[histos.Publishing].provideLayer(layer)

  private def sonaSecrets(publishing: histos.Publishing): Boolean =
    publishing.secretNames.toSet == histos.Sona.secrets.toSet

  private final case class StagedTree(root: Path, output: PublishOutput)

  private def stage(
      repo: Path,
      graph: ProjectGraph,
      names: List[PublishName],
      jdk: JdkRuntime,
      env: Map[String, String],
      write: PublishWrite,
      api: ApiDigest,
      report: CompileFeed => UIO[Unit],
      note: String => UIO[Unit],
      plugins: Option[histos.load.PluginRuntime],
      scope: ProofScope,
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore & Scope, LocalPublishError, StagedTree] =
    for
      _    <- ZIO.foreachDiscard(names)(name => note(s"building ${shown(name)}"))
      root <- ZIO.acquireRelease(
        ZIO
          .attemptBlocking(Files.createTempDirectory("histos-stage"))
          .mapError(_ => LocalPublishError.Io("histos-stage"))
      )(deleteTree)
      output <- LocalPublish
        .run(repo, graph, names, jdk, env, write, api, report, plugins, scope)
        .mapError {
          case error: LocalPublishError          => error
          case error: histos.zinc.StageRootError => LocalPublishError.Scoped(histos.testrun.TestFailures.root(error))
        }
        .tapError(error => note(error.message))
        .provideSome[histos.resolve.Resolve & histos.cache.ContentStore](Publisher.local(root))
      _ <- ZIO.foreachDiscard(output.modules)(module => note(s"staged ${module.artifact} ${module.revision}"))
    yield StagedTree(root, output)

  private def shown(name: PublishName): String =
    name match
      case PublishName.Ship(ship)           => ship.value
      case PublishName.Aggregate(aggregate) => aggregate.value
      case PublishName.Module(module)       => module.value

  private def localLines(
      checkout: Path,
      staged: StagedTree,
      layer: ZLayer[Any, Nothing, histos.Publishing],
  ): IO[LocalPublishError, List[String]] =
    ZIO
      .foreach(grouped(staged.output.modules))(modules => localGroup(checkout, staged.root, modules))
      .provideLayer(layer)
      .map(_.flatten)

  private def localGroup(
      checkout: Path,
      stage: Path,
      modules: List[PublishedModule],
  ): ZIO[histos.Publishing, LocalPublishError, List[String]] =
    modules match
      case Nil       => ZIO.succeed(Nil)
      case head :: _ =>
        ShipRevision.parse(head.revision) match
          case Left(_)         => ZIO.succeed(List(PublishFault.Unreadable(head.revision).message))
          case Right(revision) =>
            requestOf(revision, false, stage, checkout, modules) match
              case Left(fault)    => ZIO.fail(localFault(fault))
              case Right(request) =>
                ZIO
                  .serviceWithZIO[histos.Publishing](_.publish(request))
                  .foldZIO(
                    fault =>
                      fault match
                        case PublishFault.Version(_) => ZIO.succeed(List(fault.message))
                        case other                   => ZIO.fail(localFault(other)),
                    receipt => ZIO.succeed(receipt.lines.toList ++ receipt.failed.toList),
                  )

  private def releasePieces(
      checkout: Path,
      staged: StagedTree,
      automatic: Boolean,
      layer: ZLayer[Any, Nothing, histos.Publishing],
  ): IO[ReleaseError, Gather] =
    ZIO
      .foldLeft(grouped(staged.output.modules))(List.empty[Piece])((seen, modules) =>
        releaseGroup(checkout, staged.root, automatic, modules).map(seen :+ _)
      )
      .provideLayer(layer)
      .map(gather)

  private def releaseGroup(
      checkout: Path,
      stage: Path,
      automatic: Boolean,
      modules: List[PublishedModule],
  ): ZIO[histos.Publishing, ReleaseError, Piece] =
    modules match
      case Nil       => ZIO.succeed(Piece.Noted(Nil))
      case head :: _ =>
        ShipRevision.parse(head.revision) match
          case Left(_)         => ZIO.succeed(Piece.Refused(PublishFault.Unreadable(head.revision).message))
          case Right(revision) =>
            requestOf(revision, automatic, stage, checkout, modules) match
              case Left(fault)    => releaseFault(fault)
              case Right(request) =>
                ZIO
                  .serviceWithZIO[histos.Publishing](_.publish(request))
                  .foldZIO(releaseFault, receipt => classify(head.ship, head.revision, checkout, receipt))

  private def releaseFault(fault: PublishFault): IO[ReleaseError, Piece] =
    fault match
      case PublishFault.Credentials => ZIO.fail(ReleaseError.Credentials)
      case PublishFault.Rejected    => ZIO.fail(ReleaseError.Rejected)
      case PublishFault.Version(_)  => ZIO.succeed(Piece.Refused(fault.message))
      case other                    => ZIO.succeed(Piece.Failed(List(other.message)))

  private def classify(
      ship: ShipName,
      revision: String,
      checkout: Path,
      receipt: PublishReceipt,
  ): IO[ReleaseError, Piece] =
    val heldLine = s"${ship.value} $revision is already published"
    if receipt.failed.nonEmpty then ZIO.succeed(Piece.Failed(receipt.failed.toList))
    else if receipt.lines.contains(heldLine) then ZIO.succeed(Piece.Held(heldLine))
    else
      DeploymentFile
        .read(checkout, ship)
        .map:
          case Some(record) =>
            val deployed = Deployed(ship, record.id, record.state, record.version)
            if receipt.lines.contains(deployed.line) then Piece.Deployed(deployed)
            else Piece.Noted(receipt.lines.toList)
          case None => Piece.Noted(receipt.lines.toList)

  private def requestOf(
      revision: ShipRevision,
      automatic: Boolean,
      stage: Path,
      checkout: Path,
      modules: List[PublishedModule],
  ): Either[PublishFault, PublishRequest] =
    modules
      .foldLeft[Either[PublishFault, Chunk[StagedModule]]](Right(Chunk.empty)): (acc, module) =>
        acc.flatMap(seen => stageOne(module).map(seen :+ _))
      .map(staged => PublishRequest(revision, automatic, stage, checkout, staged))

  private def stageOne(module: PublishedModule): Either[PublishFault, StagedModule] =
    (module.jar, module.pom) match
      case (Some(jar), Some(pom)) =>
        Right(
          StagedModule(
            module.module,
            module.ship,
            module.group,
            module.artifact,
            module.revision,
            jar,
            pom,
            module.sources,
            module.javadoc,
            Chunk.fromIterable(
              module.dependencies.map(dependency =>
                StagedDependency(
                  dependency.group,
                  dependency.name.value,
                  dependency.version.value,
                  dependency.exclusions,
                  dependency.scope,
                )
              )
            ),
          )
        )
      case _ => Left(PublishFault.Write(module.artifact))

  private def localFault(fault: PublishFault): LocalPublishError =
    fault match
      case PublishFault.Credentials => LocalPublishError.Credentials
      case PublishFault.Write(path) => LocalPublishError.Io(path)
      case other                    => LocalPublishError.Stock(other.message)

  private def grouped(modules: List[PublishedModule]): List[List[PublishedModule]] =
    val order = modules.foldLeft(List.empty[(ShipName, String)]): (seen, module) =>
      val key = (module.ship, module.revision)
      if seen.contains(key) then seen else seen :+ key
    order.map: key =>
      modules.filter(module => module.ship == key._1 && module.revision == key._2)

  private enum Piece:
    case Refused(line: String)
    case Held(line: String)
    case Failed(lines: List[String])
    case Deployed(done: histos.publish.Deployed)
    case Noted(lines: List[String])

  private final case class Gather(
      refused: List[String],
      held: List[String],
      failed: List[String],
      deployed: List[Deployed],
  )

  private def gather(pieces: List[Piece]): Gather =
    pieces.foldLeft(Gather(Nil, Nil, Nil, Nil)): (acc, piece) =>
      piece match
        case Piece.Refused(line)  => acc.copy(refused = acc.refused :+ line)
        case Piece.Held(line)     => acc.copy(held = acc.held :+ line)
        case Piece.Failed(lines)  => acc.copy(failed = acc.failed ++ lines)
        case Piece.Deployed(done) => acc.copy(deployed = acc.deployed :+ done)
        case Piece.Noted(lines)   => acc.copy(held = acc.held ++ lines.filter(_.nonEmpty))

  private def deleteTree(dir: Path): UIO[Unit] =
    ZIO.attemptBlocking {
      if Files.exists(dir) then
        val walk  = Files.walk(dir)
        val paths =
          try walk.iterator().asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.ignore
end PublishRun
