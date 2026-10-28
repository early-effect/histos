package histos.publish

import histos.central.DeploymentState
import histos.plugin.RepoPath
import histos.version.{Ship, ShipName}
import histos.{Module, ProjectGraph}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, UIO, ZIO}

/** What one `publish assets` invocation uploaded. A failed ship is named. The others still upload. */
final case class AssetReport(failed: List[String], uploaded: List[String]):
  def lines: List[String] =
    failed ++ uploaded

  def anyFailed: Boolean =
    failed.nonEmpty
end AssetReport

/** Upload each named asset whose deployment file says `VALIDATED`. Missing secrets fail before a socket. */
object AssetRun:
  def publish(
      repo: Path,
      graph: ProjectGraph,
      tokens: List[String],
      env: Map[String, String],
  ): IO[AssetError, AssetReport] =
    for
      asked  <- ZIO.fromEither(select(graph, tokens))
      judged <- ZIO.foreach(asked)(row => judge(repo, graph, row))
      ready  = judged.flatMap(_.files)
      failed = judged.flatMap(_.failed)
      sent <- deliver(env, ready)
    yield AssetReport(failed ++ sent.failed, sent.uploaded)

  private def deliver(env: Map[String, String], ready: List[Pending]): IO[AssetError, Sent] =
    ready match
      case Nil => ZIO.succeed(Sent(Nil, Nil))
      case _   =>
        for
          access <- ZIO.fromEither(accessOf(env))
          bases  <- ZIO.fromEither(basesOf(env))
          sent   <- send(access, bases, ready)
        yield sent

  private def send(access: Access, bases: Bases, ready: List[Pending]): IO[AssetError, Sent] =
    ZIO
      .serviceWithZIO[Assets] { assets =>
        ZIO.foreach(ready): file =>
          assets
            .upload(file.tag, file.name, file.bytes)
            .fold(
              error => Left(s"${file.ship.value} ${error.message}"),
              _ => Right(s"${file.ship.value} ${file.name}"),
            )
      }
      .provide(Assets.layer(bases.api, bases.uploads, access.token, access.repository))
      .map: rows =>
        Sent(rows.collect { case Left(line) => line }, rows.collect { case Right(line) => line })

  private def select(graph: ProjectGraph, tokens: List[String]): Either[AssetError, List[Asked]] =
    if tokens.isEmpty then Right(graph.catalog.ships.filter(ship => assetsOf(graph, ship).nonEmpty).map(Asked.Quiet(_)))
    else
      tokens.foldLeft[Either[AssetError, List[Asked]]](Right(Nil)): (acc, token) =>
        acc.flatMap: seen =>
          shipOf(graph, token) match
            case None                                                => Left(AssetError.UnknownShip(token))
            case Some(ship) if seen.exists(_.ship.name == ship.name) =>
              Left(AssetError.Duplicate(token))
            case Some(ship) => Right(seen :+ Asked.Named(ship))

  private def judge(repo: Path, graph: ProjectGraph, asked: Asked): UIO[Judged] =
    val ship    = asked.ship
    val version = versionOf(ship)
    val files   = assetsOf(graph, ship)
    files match
      case Nil =>
        asked match
          case Asked.Named(_) => ZIO.succeed(Judged.failed(s"${ship.name.value} has no asset"))
          case Asked.Quiet(_) => ZIO.succeed(Judged.skip)
      case _ =>
        clash(files.map(_._1)) match
          case Some(name) => ZIO.succeed(Judged.failed(s"${ship.name.value} names two assets called $name"))
          case None       =>
            DeploymentFile
              .read(repo, ship.name)
              .foldZIO(
                error => ZIO.succeed(Judged.failed(error.message)),
                record => decide(repo, graph, asked, version, files, record),
              )

  private def decide(
      repo: Path,
      graph: ProjectGraph,
      asked: Asked,
      version: String,
      files: List[(String, RepoPath)],
      record: Option[DeploymentFile.Record],
  ): UIO[Judged] =
    val ship = asked.ship.name.value
    record match
      case None =>
        asked match
          case Asked.Named(_) => ZIO.succeed(Judged.failed(s"$ship has no recorded deployment"))
          case Asked.Quiet(_) => ZIO.succeed(Judged.skip)
      case Some(found) if found.state != DeploymentState.Validated =>
        asked match
          case Asked.Named(_) =>
            val token = DeploymentFile.token(found.state).getOrElse("recorded")
            ZIO.succeed(Judged.failed(s"$ship is $token, not VALIDATED"))
          case Asked.Quiet(_) => ZIO.succeed(Judged.skip)
      case Some(found) if found.version != version =>
        asked match
          case Asked.Named(_) =>
            ZIO.succeed(Judged.failed(s"$ship deployment records ${found.version}, not $version"))
          case Asked.Quiet(_) => ZIO.succeed(Judged.skip)
      case Some(found) =>
        val tag = tagOf(graph.catalog.ships.length, asked.ship.name, found.version)
        readAll(repo, asked.ship.name, tag, files)

  private def readAll(
      repo: Path,
      ship: ShipName,
      tag: String,
      files: List[(String, RepoPath)],
  ): UIO[Judged] =
    ZIO
      .foldLeft(files)(Right(Nil): Either[String, List[Pending]]): (acc, file) =>
        acc match
          case left @ Left(_) => ZIO.succeed(left)
          case Right(have)    =>
            readOne(repo, ship, tag, file).map:
              case Left(line)     => Left(line)
              case Right(pending) => Right(have :+ pending)
      .map:
        case Left(line)  => Judged.failed(line)
        case Right(have) => Judged.ready(have)

  private def readOne(
      repo: Path,
      ship: ShipName,
      tag: String,
      file: (String, RepoPath),
  ): UIO[Either[String, Pending]] =
    val (name, relative) = file
    val path             = repo.resolve(relative.value).normalize
    ZIO
      .attemptBlocking(Files.isRegularFile(path))
      .foldZIO(
        _ => ZIO.succeed(Left(s"${ship.value} could not read ${relative.value}")),
        {
          case false => ZIO.succeed(Left(s"${ship.value} ${relative.value} is not a file"))
          case true  =>
            ZIO
              .attemptBlocking(Chunk.fromArray(Files.readAllBytes(path)))
              .fold(
                _ => Left(s"${ship.value} could not read ${relative.value}"),
                bytes => Right(Pending(ship, tag, name, bytes)),
              )
        },
      )

  private def assetsOf(graph: ProjectGraph, ship: Ship): List[(String, RepoPath)] =
    ship.members.toList.flatMap: name =>
      graph.get(name) match
        case Some(module: Module) => module.asset.toList.map(path => fileName(path) -> path)
        case _                    => Nil

  private def fileName(path: RepoPath): String =
    val raw = path.value
    val at  = raw.lastIndexOf('/')
    if at < 0 then raw else raw.substring(at + 1)

  private def clash(names: List[String]): Option[String] =
    names.groupBy(identity).collectFirst { case (name, items) if items.length > 1 => name }

  private def shipOf(graph: ProjectGraph, token: String): Option[Ship] =
    ShipName.from(token).toOption.flatMap(name => graph.catalog.get(name))

  private def versionOf(ship: Ship): String =
    ship match
      case Ship.Manual(_, version, _, _, _) => version.value
      case Ship.Line(_, number, _, _, _)    => number.render

  /** One ship in the catalog is `v<number>`. More than one is `<ship>/v<number>`. */
  private def tagOf(ships: Int, ship: ShipName, version: String): String =
    if ships > 1 then s"${ship.value}/v$version" else s"v$version"

  private def accessOf(env: Map[String, String]): Either[AssetError, Access] =
    val token = env.get("GITHUB_TOKEN").filter(_.nonEmpty)
    val repo  = env.get("GITHUB_REPOSITORY").map(_.trim).filter(_.nonEmpty)
    (token, repo) match
      case (Some(secret), Some(name)) => AssetRepo.from(name).map(parsed => Access(AssetToken(secret), parsed))
      case _                          => Left(AssetError.Credentials)

  private def basesOf(env: Map[String, String]): Either[AssetError, Bases] =
    val api    = text(env.get("HISTOS_GITHUB"))
    val upload = text(env.get("HISTOS_GITHUB_UPLOAD"))
    (api, upload) match
      case (None, None)      => Right(Bases(AssetBase.github, AssetBase.uploads))
      case (Some(raw), None) =>
        AssetBase.from(raw).map(base => Bases(base, base))
      case (None, Some(raw)) =>
        AssetBase.from(raw).map(base => Bases(AssetBase.github, base))
      case (Some(apiRaw), Some(uploadRaw)) =>
        for
          apiBase    <- AssetBase.from(apiRaw)
          uploadBase <- AssetBase.from(uploadRaw)
        yield Bases(apiBase, uploadBase)

  private def text(value: Option[String]): Option[String] =
    value.map(_.trim).filter(_.nonEmpty)

  private enum Asked:
    case Named(chosen: Ship)
    case Quiet(chosen: Ship)

    def ship: Ship = this match
      case Named(chosen) => chosen
      case Quiet(chosen) => chosen
  end Asked

  private final case class Pending(ship: ShipName, tag: String, name: String, bytes: Chunk[Byte])

  private final case class Judged(failed: List[String], files: List[Pending])

  private object Judged:
    def failed(line: String): Judged        = Judged(List(line), Nil)
    def ready(files: List[Pending]): Judged = Judged(Nil, files)
    val skip: Judged                        = Judged(Nil, Nil)

  private final case class Access(token: AssetToken, repository: AssetRepo)
  private final case class Bases(api: AssetBase, uploads: AssetBase)
  private final case class Sent(failed: List[String], uploaded: List[String])
end AssetRun
