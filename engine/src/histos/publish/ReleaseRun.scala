package histos.publish

import histos.central.{
  DeploymentId,
  DeploymentState,
  Fingerprint,
  Gav,
  Portal,
  PortalBase,
  PortalError,
  Preflight,
  PreflightError,
  Publisher,
  ReleaseMode,
  RepositorySecret,
}
import histos.jdk.JdkRuntime
import histos.version.ShipName
import histos.zinc.CompileFeed
import histos.ProjectGraph

import java.nio.file.{Files, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Scope, UIO, ZIO}

/** What one release invocation did. A failed ship is named and the others still publish. */
final case class ReleaseReport(
    refused: List[String],
    held: List[String],
    failed: List[String],
    deployed: List[Deployed],
):
  def lines: List[String] =
    refused ++ held ++ failed ++ deployed.map(_.line)

  def anyFailed: Boolean =
    failed.nonEmpty
end ReleaseReport

final case class Deployed(ship: ShipName, id: DeploymentId, state: DeploymentState, version: String):
  def line: String =
    DeploymentFile.token(state) match
      case Some(token) => s"${ship.value} ${id.value} $token"
      case None        => s"${ship.value} ${id.value}"

enum ReleaseError:
  case Credentials
  case Portal(error: PortalBase.Error)
  case Signer(error: SignerError)
  case Stage(error: LocalPublishError)
  case Bundle(error: BundleError)
  case Client(error: PortalError)
  case Record(ship: String)
  case Io(path: String)
  case Asset(error: AssetError)

  def message: String = this match
    case Credentials   => "SONATYPE_USERNAME and SONATYPE_PASSWORD are required"
    case Portal(error) => error.message
    case Signer(error) => error.message
    case Stage(error)  => error.message
    case Bundle(error) => error.message
    case Client(error) => error.message
    case Record(ship)  => s"$ship deployment record is not readable"
    case Io(path)      => s"could not write $path"
    case Asset(error)  => error.message
end ReleaseError

/** One staged release. `root` is the Maven directory. Upload has not run. */
private[publish] final case class ReleaseStage(
    root: Path,
    modules: List[PublishedModule],
    refused: List[String],
    held: List[String],
    skipped: List[String],
    signer: Signer,
    base: PortalBase,
    secret: RepositorySecret,
    fingerprint: Fingerprint,
    env: Map[String, String],
)

/** Stage, sign, and upload one bundle per ship. Secrets are checked before a socket is opened. */
private[publish] object ReleaseRun:
  def stage(
      repo: Path,
      graph: ProjectGraph,
      tokens: List[String],
      jdk: JdkRuntime,
      env: Map[String, String],
      report: CompileFeed => UIO[Unit],
  ): ZIO[histos.resolve.Resolve & histos.cache.ContentStore & Scope, ReleaseError, ReleaseStage] =
    for
      secret      <- ZIO.fromEither(credentials(env))
      signer      <- Signer.fromEnv(env).mapError(ReleaseError.Signer(_))
      fingerprint <- signer.fingerprint.mapError(ReleaseError.Signer(_))
      base        <- ZIO.fromEither(Preflight.portal(env).left.map(ReleaseError.Portal(_)))
      request     <- ZIO
        .fromEither(
          PublishSelect.resolve(graph, tokens, None, allowDirectory = false)
        )
        .mapError(ReleaseError.Stage(_))
      dir    <- ZIO.acquireRelease(directory("histos-release"))(deleteTree)
      output <- LocalPublish
        .run(repo, graph, request.names, jdk, env, PublishWrite.Release, report)
        .provideSome[histos.resolve.Resolve & histos.cache.ContentStore](Publisher.local(dir))
        .mapError(ReleaseError.Stage(_))
    yield ReleaseStage(
      dir,
      output.modules,
      output.refused,
      output.held,
      output.skipped,
      signer,
      base,
      secret,
      fingerprint,
      env,
    )

  def upload(repo: Path, staged: ReleaseStage, mode: ReleaseMode): IO[ReleaseError, ReleaseReport] =
    val ships = grouped(staged.modules)
    for
      decided <- ZIO.foreach(ships)(ship => hold(repo, ship))
      pending = decided.collect { case Decision.Upload(ship) => ship }
      sent <- deliver(repo, staged, pending, mode)
    yield ReleaseReport(
      staged.refused,
      staged.held ++ decided.collect { case Decision.Held(line) => line } ++ sent.held,
      decided.collect { case Decision.Failed(line) => line } ++ sent.failed,
      sent.deployed,
    )

  def promote(repo: Path, env: Map[String, String], raw: String): IO[ReleaseError, Unit] =
    prepared(env, raw).flatMap { (base, secret, id) =>
      use(base, secret, _.promote(id)) *> DeploymentFile.mark(repo, id, DeploymentState.Published)
    }

  /** Portal drop, then the local file, then the GitHub asset. The asset name and the tag's ship count are on the graph,
    * not in the deployment file. A missing file, or a ship with no asset, does not open a release socket. An asset
    * failure leaves the deployment dropped.
    */
  def drop(repo: Path, graph: ProjectGraph, env: Map[String, String], raw: String): IO[ReleaseError, Unit] =
    prepared(env, raw).flatMap { (base, secret, id) =>
      for
        _     <- use(base, secret, _.drop(id))
        taken <- DeploymentFile.remove(repo, id)
        _     <- erase(graph, env, taken)
      yield ()
    }

  private def prepared(
      env: Map[String, String],
      raw: String,
  ): IO[ReleaseError, (PortalBase, RepositorySecret, DeploymentId)] =
    for
      secret <- ZIO.fromEither(credentials(env))
      base   <- ZIO.fromEither(Preflight.portal(env).left.map(ReleaseError.Portal(_)))
      id     <- ZIO.fromEither(DeploymentId.from(raw).left.map(ReleaseError.Client(_)))
    yield (base, secret, id)

  private def erase(
      graph: ProjectGraph,
      env: Map[String, String],
      taken: Option[(ShipName, DeploymentFile.Record)],
  ): IO[ReleaseError, Unit] =
    taken match
      case None                 => ZIO.unit
      case Some((ship, record)) =>
        AssetRun.delete(graph, env, ship, record.version).mapError(ReleaseError.Asset(_))

  private def deliver(
      repo: Path,
      staged: ReleaseStage,
      ships: List[ShipBundle],
      mode: ReleaseMode,
  ): IO[ReleaseError, Sent] =
    ships match
      case Nil => ZIO.succeed(Sent(Nil, Nil, Nil))
      case _   =>
        for
          ready <- ZIO.foreach(ships)(ship => prepare(staged, ship))
          bodies = ready.collect { case Right(body) => body }
          failed = ready.collect { case Left(line) => line }
          sent <- bodies match
            case Nil => ZIO.succeed(Sent(Nil, failed, Nil))
            case _   =>
              ZIO
                .serviceWithZIO[Portal](portal => ZIO.foreach(bodies)(body => push(repo, staged, portal, body, mode)))
                .provide(Portal.layer(staged.base, staged.secret).mapError(ReleaseError.Client(_)))
                .map: rows =>
                  Sent(rows.flatMap(_.held), failed ++ rows.flatMap(_.failed), rows.flatMap(_.deployed))
        yield sent

  private def prepare(staged: ReleaseStage, ship: ShipBundle): UIO[Either[String, Body]] =
    ZIO
      .scoped {
        sign(staged.signer, ship) *> bundle(staged.root, ship)
      }
      .fold(error => Left(s"${ship.ship.value} ${error.message}"), body => Right(body))

  private def push(
      repo: Path,
      staged: ReleaseStage,
      portal: Portal,
      body: Body,
      mode: ReleaseMode,
  ): UIO[Sent] =
    val gavs = Chunk.fromIterable(
      body.ship.modules.map(module => Gav(module.group.value, module.artifact, module.revision))
    )
    Preflight
      .release(staged.env, gavs, staged.fingerprint, portal.upload(body.bytes, mode))
      .foldZIO(
        error => ZIO.succeed(sentOf(body.ship.ship, error)),
        deployment =>
          DeploymentFile
            .write(repo, body.ship.ship, deployment.id, deployment.state, body.ship.version)
            .fold(
              error => Sent(Nil, List(error.message), Nil),
              _ =>
                Sent(
                  Nil,
                  Nil,
                  List(Deployed(body.ship.ship, deployment.id, deployment.state, body.ship.version)),
                ),
            ),
      )

  private def sentOf(ship: ShipName, error: PreflightError): Sent =
    error match
      case PreflightError.Published(_) =>
        Sent(List(error.message), Nil, Nil)
      case PreflightError.Upload(PortalError.Failed(id)) =>
        Sent(Nil, List(s"${ship.value} deployment ${id.value} failed"), Nil)
      case other =>
        Sent(Nil, List(s"${ship.value} ${other.message}"), Nil)

  private def hold(repo: Path, ship: ShipBundle): UIO[Decision] =
    DeploymentFile
      .read(repo, ship.ship)
      .fold(
        error => Decision.Failed(error.message),
        {
          case Some(record) if record.state == DeploymentState.Published && record.version == ship.version =>
            Decision.Held(s"${ship.ship.value} ${ship.version} is already published")
          case _ => Decision.Upload(ship)
        },
      )

  private def sign(signer: Signer, ship: ShipBundle): IO[ReleaseError, Unit] =
    ZIO.foreachDiscard(ship.modules)(module => signModule(signer, module))

  private def signModule(signer: Signer, module: PublishedModule): IO[ReleaseError, Unit] =
    directoryOf(module) match
      case None      => ZIO.unit
      case Some(dir) =>
        filesUnder(dir).flatMap: files =>
          ZIO.foreachDiscard(files.filter(signable)): file =>
            for
              bytes <- readBytes(file)
              armor <- signer.sign(bytes).mapError(ReleaseError.Signer(_))
              _     <- writeString(asc(file), armor.text)
            yield ()

  private def bundle(stage: Path, ship: ShipBundle): ZIO[Scope, ReleaseError, Body] =
    for
      dir   <- directory("histos-ship")
      zip   <- file("histos-bundle")
      _     <- ZIO.foreachDiscard(ship.modules)(module => copyModule(stage, module, dir))
      _     <- Bundle.zip(dir, zip).mapError(ReleaseError.Bundle(_))
      bytes <- readBytes(zip)
    yield Body(ship, bytes)

  private def copyModule(stage: Path, module: PublishedModule, dest: Path): IO[ReleaseError, Unit] =
    directoryOf(module) match
      case None      => ZIO.fail(ReleaseError.Io(module.artifact))
      case Some(dir) =>
        filesUnder(dir).flatMap: files =>
          ZIO.foreachDiscard(files)(file => copyFile(stage, file, dest))

  private def copyFile(stage: Path, file: Path, dest: Path): IO[ReleaseError, Unit] =
    val relative = stage.relativize(file)
    val target   = dest.resolve(relative)
    ZIO
      .attemptBlocking {
        val parent = Option(target.getParent).getOrElse(dest)
        Files.createDirectories(parent)
        Files.copy(file, target, StandardCopyOption.REPLACE_EXISTING)
        ()
      }
      .mapError(_ => ReleaseError.Io(target.toString))

  private def directoryOf(module: PublishedModule): Option[Path] =
    module.jar.flatMap(path => Option(path.getParent))

  private def filesUnder(dir: Path): IO[ReleaseError, List[Path]] =
    ZIO
      .attemptBlocking {
        if !Files.isDirectory(dir) then Nil
        else
          val walk = Files.walk(dir)
          try walk.iterator().asScala.filter(path => Files.isRegularFile(path)).toList
          finally walk.close()
      }
      .mapError(_ => ReleaseError.Io(dir.toString))

  private def signable(path: Path): Boolean =
    val name = Option(path.getFileName).map(_.toString).getOrElse("")
    name.endsWith(".jar") || name.endsWith(".pom")

  private def asc(path: Path): Path =
    val name   = Option(path.getFileName).map(_.toString).getOrElse("artifact")
    val parent = Option(path.getParent).getOrElse(path)
    parent.resolve(s"$name.asc")

  private def grouped(modules: List[PublishedModule]): List[ShipBundle] =
    modules
      .groupBy(_.ship)
      .toList
      .sortBy((ship, _) => ship.value)
      .flatMap: (ship, members) =>
        members match
          case head :: _ => List(ShipBundle(ship, head.revision, members))
          case Nil       => Nil

  private def use(
      base: PortalBase,
      secret: RepositorySecret,
      body: Portal => IO[PortalError, Unit],
  ): IO[ReleaseError, Unit] =
    ZIO
      .serviceWithZIO[Portal](body)
      .mapError(ReleaseError.Client(_))
      .provide(Portal.layer(base, secret).mapError(ReleaseError.Client(_)))

  private def credentials(env: Map[String, String]): Either[ReleaseError, RepositorySecret] =
    PublishWhere.snapshots(env) match
      case Right(PublishWhere.Snapshots(user, password)) => Right(RepositorySecret(user, password))
      case _                                             => Left(ReleaseError.Credentials)

  private def readBytes(path: Path): IO[ReleaseError, Chunk[Byte]] =
    ZIO
      .attemptBlocking(Chunk.fromArray(Files.readAllBytes(path)))
      .mapError(_ => ReleaseError.Io(path.toString))

  private def writeString(path: Path, text: String): IO[ReleaseError, Unit] =
    ZIO.attemptBlocking(Files.writeString(path, text)).mapError(_ => ReleaseError.Io(path.toString)).unit

  private def directory(prefix: String): ZIO[Scope, ReleaseError, Path] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking(Files.createTempDirectory(prefix)).mapError(_ => ReleaseError.Io(prefix))
    )(deleteTree)

  private def file(prefix: String): ZIO[Scope, ReleaseError, Path] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking(Files.createTempFile(prefix, ".zip")).mapError(_ => ReleaseError.Io(prefix))
    )(path => ZIO.attemptBlocking(Files.deleteIfExists(path)).ignore)

  private def deleteTree(dir: Path): UIO[Unit] =
    ZIO.attemptBlocking {
      if Files.exists(dir) then
        val walk  = Files.walk(dir)
        val paths =
          try walk.iterator().asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.ignore

  private enum Decision:
    case Upload(ship: ShipBundle)
    case Held(line: String)
    case Failed(line: String)

  private final case class ShipBundle(ship: ShipName, version: String, modules: List[PublishedModule])

  private final case class Body(ship: ShipBundle, bytes: Chunk[Byte])

  private final case class Sent(held: List[String], failed: List[String], deployed: List[Deployed])
end ReleaseRun

/** `.histos/deployments/<ship>`. Not `.histos/releases`. */
private[publish] object DeploymentFile:
  final case class Record(id: DeploymentId, state: DeploymentState, version: String)

  def location(repo: Path, ship: ShipName): Path =
    repo.resolve(".histos").resolve("deployments").resolve(ship.value)

  def token(state: DeploymentState): Option[String] =
    state match
      case DeploymentState.Validated => Some("VALIDATED")
      case DeploymentState.Published => Some("PUBLISHED")
      case DeploymentState.Pending | DeploymentState.Validating | DeploymentState.Publishing | DeploymentState.Failed =>
        None

  def write(
      repo: Path,
      ship: ShipName,
      id: DeploymentId,
      state: DeploymentState,
      version: String,
  ): IO[ReleaseError, Unit] =
    token(state) match
      case None             => ZIO.fail(ReleaseError.Record(ship.value))
      case Some(stateToken) =>
        val path = location(repo, ship)
        val text = s"id ${id.value}\nstate $stateToken\nversion $version\n"
        ZIO
          .attemptBlocking {
            val parent = Option(path.getParent).getOrElse(path)
            Files.createDirectories(parent)
            Files.writeString(path, text)
          }
          .mapError(_ => ReleaseError.Io(path.toString))
          .unit

  def read(repo: Path, ship: ShipName): IO[ReleaseError, Option[Record]] =
    val path = location(repo, ship)
    ZIO.attemptBlocking(Files.isRegularFile(path)).mapError(_ => ReleaseError.Io(path.toString)).flatMap {
      case false => ZIO.succeed(None)
      case true  =>
        ZIO
          .attemptBlocking(Files.readString(path))
          .mapError(_ => ReleaseError.Io(path.toString))
          .flatMap(text => ZIO.fromEither(parse(ship, text)))
    }

  def mark(repo: Path, id: DeploymentId, state: DeploymentState): IO[ReleaseError, Unit] =
    find(repo, id).flatMap:
      case None               => ZIO.unit
      case Some(ship, record) =>
        write(repo, ship, id, state, record.version)

  def remove(repo: Path, id: DeploymentId): IO[ReleaseError, Option[(ShipName, Record)]] =
    find(repo, id).flatMap:
      case None               => ZIO.succeed(None)
      case Some(ship, record) =>
        val path = location(repo, ship)
        ZIO
          .attemptBlocking(Files.deleteIfExists(path))
          .mapError(_ => ReleaseError.Io(path.toString))
          .as(Some(ship -> record))

  private def parse(ship: ShipName, text: String): Either[ReleaseError, Option[Record]] =
    text.stripLineEnd.split("\n", -1).toList match
      case List(idLine, stateLine, versionLine) =>
        row(ship, idLine, stateLine, versionLine).map(Some(_))
      case _ => Left(ReleaseError.Record(ship.value))

  private def row(
      ship: ShipName,
      idLine: String,
      stateLine: String,
      versionLine: String,
  ): Either[ReleaseError, Record] =
    for
      id <- field(ship, idLine, "id").flatMap(raw =>
        DeploymentId.from(raw).left.map(_ => ReleaseError.Record(ship.value))
      )
      token   <- field(ship, stateLine, "state")
      state   <- stateOf(ship, token)
      version <- field(ship, versionLine, "version")
    yield Record(id, state, version)

  private def field(ship: ShipName, line: String, name: String): Either[ReleaseError, String] =
    val prefix = s"$name "
    if line.startsWith(prefix) && line.length > prefix.length then Right(line.substring(prefix.length))
    else Left(ReleaseError.Record(ship.value))

  private def stateOf(ship: ShipName, token: String): Either[ReleaseError, DeploymentState] =
    token match
      case "VALIDATED" => Right(DeploymentState.Validated)
      case "PUBLISHED" => Right(DeploymentState.Published)
      case _           => Left(ReleaseError.Record(ship.value))

  private def find(repo: Path, id: DeploymentId): IO[ReleaseError, Option[(ShipName, Record)]] =
    val root = repo.resolve(".histos").resolve("deployments")
    ZIO.attemptBlocking(Files.isDirectory(root)).mapError(_ => ReleaseError.Io(root.toString)).flatMap {
      case false => ZIO.succeed(None)
      case true  =>
        listed(root).flatMap: paths =>
          ZIO.foldLeft(paths)(None: Option[(ShipName, Record)]): (found, path) =>
            found match
              case Some(_) => ZIO.succeed(found)
              case None    =>
                shipOf(path) match
                  case None       => ZIO.succeed(None)
                  case Some(ship) =>
                    read(repo, ship).map:
                      case Some(record) if record.id == id => Some(ship -> record)
                      case _                               => None
    }

  private def listed(root: Path): IO[ReleaseError, List[Path]] =
    ZIO
      .attemptBlocking {
        val stream = Files.list(root)
        try stream.iterator().asScala.toList
        finally stream.close()
      }
      .mapError(_ => ReleaseError.Io(root.toString))

  private def shipOf(path: Path): Option[ShipName] =
    val name = Option(path.getFileName).map(_.toString).getOrElse("")
    ShipName.from(name).toOption
end DeploymentFile
