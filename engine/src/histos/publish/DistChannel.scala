package histos.publish

import histos.plugin.RepoPath
import histos.version.{Revision, ShipName}
import histos.{Module, ProjectGraph, ProjectName}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, IO, ZIO}

/** One snapshot coordinate whose channel file, manifest, and jar go on the `dist` release. */
final case class ChannelPlan(
    ship: ShipName,
    module: ProjectName,
    group: String,
    artifact: String,
    revision: String,
    asset: RepoPath,
    clients: List[DistPlatform],
)

/** The shim reads `channel-<version>` from the existing `dist` release for a pin that ends in `-SNAPSHOT`. The body is
  * the timestamp Maven stored for that coordinate. The manifest, the jar, and any clients use that same timestamp. A
  * stored commit coordinate uploads `channel-<ship>-<sha>-SNAPSHOT` and does not move the pointer channel. A ship that
  * names `.toolchain` reads each linked binary from [[DistLayout.builtClient]]. A platform this publish did not link is
  * omitted. None at all is refused before a token.
  */
object DistChannel:
  val Tag: String = "dist"

  /** Asset publishes that end in `-SNAPSHOT`: the pointer, and the stored `<ship>-<sha>-SNAPSHOT`. A ship with two
    * assets is refused when one of those modules published either. The pointer channel is planned only when this
    * publish wrote the pointer.
    */
  def plan(graph: ProjectGraph, modules: List[PublishedModule]): Either[LocalPublishError, List[ChannelPlan]] =
    val ships = modules.foldLeft(List.empty[ShipName]): (seen, module) =>
      if seen.contains(module.ship) then seen else seen :+ module.ship
    ships.foldLeft[Either[LocalPublishError, List[ChannelPlan]]](Right(Nil)): (acc, ship) =>
      acc.flatMap: seen =>
        one(graph, ship, modules.filter(_.ship == ship)).map(seen ++ _)

  /** The asset file and each client, then GitHub credentials. An empty plan checks neither and does not read `gh` or
    * `origin`.
    */
  def prepare(
      repo: Path,
      env: Map[String, String],
      plans: List[ChannelPlan],
      readToken: zio.UIO[Option[String]] = AssetAccess.ghToken,
      remote: Path => Option[String] = AssetAccess.origin,
  ): zio.IO[LocalPublishError, Map[String, String]] =
    if plans.isEmpty then ZIO.succeed(env)
    else
      ZIO
        .fromEither(assets(repo, plans))
        .flatMap: _ =>
          AssetAccess
            .complete(repo, env, readToken, remote)
            .mapError(error => LocalPublishError.Stock(error.message))

  /** The asset file, each client, and the release credentials. An empty plan checks neither. */
  def ready(repo: Path, env: Map[String, String], plans: List[ChannelPlan]): Either[LocalPublishError, Unit] =
    val files = assets(repo, plans)
    files.flatMap: _ =>
      if plans.isEmpty then Right(())
      else
        for
          _ <- AssetAccess.access(env).left.map(error => LocalPublishError.Stock(error.message))
          _ <- AssetAccess.bases(env).left.map(error => LocalPublishError.Stock(error.message))
        yield ()

  /** Replace the timestamped files, then the channel. The receipt line is `group artifact revision unique`. */
  def upload(
      repo: Path,
      env: Map[String, String],
      plans: List[ChannelPlan],
      lines: List[String],
  ): IO[LocalPublishError, List[String]] =
    plans match
      case Nil => ZIO.succeed(Nil)
      case _   =>
        for
          access <- ZIO.fromEither(AssetAccess.access(env).left.map(error => LocalPublishError.Stock(error.message)))
          bases  <- ZIO.fromEither(AssetAccess.bases(env).left.map(error => LocalPublishError.Stock(error.message)))
          noted  <- ZIO.foreach(plans)(plan => send(repo, access, bases, plan, lines))
        yield noted

  /** The jar, each client, the manifest, and last the channel body, in upload order. `coordinate` is the pin
    * (`<ship>-SNAPSHOT` or `<ship>-<sha>-SNAPSHOT`). The channel goes last so it never names a manifest that is not up
    * yet. With no clients the manifest's `clients` is empty and the shim runs `java -jar`.
    */
  def files(
      coordinate: String,
      revision: String,
      jar: Chunk[Byte],
      clients: List[(DistPlatform, Chunk[Byte])],
  ): List[(String, Chunk[Byte])] =
    val shas     = clients.map((platform, bytes) => platform -> DistLayout.sha256(bytes.toArray))
    val manifest = DistLayout.manifestJson(revision, DistLayout.sha256(jar.toArray), shas)
    List(DistLayout.jar(revision) -> jar) ++
      clients.map((platform, bytes) => DistLayout.client(revision, platform) -> bytes) ++
      List(
        DistLayout.manifest(revision)  -> text(manifest),
        DistLayout.channel(coordinate) -> text(revision + "\n"),
      )

  private def one(
      graph: ProjectGraph,
      ship: ShipName,
      modules: List[PublishedModule],
  ): Either[LocalPublishError, List[ChannelPlan]] =
    val assets   = assetMembers(graph, ship)
    val clients  = snapshotClients(graph, ship)
    val channels = modules.filter(module => channel(module.revision))
    assets match
      case Nil                 => Right(Nil)
      case (name, path) :: Nil =>
        channels
          .filter(_.module == name)
          .foldLeft[Either[LocalPublishError, List[ChannelPlan]]](Right(Nil)): (acc, hit) =>
            acc.flatMap: seen =>
              if seen.exists(_.revision == hit.revision) then
                Left(
                  LocalPublishError.Stock(
                    s"${ship.value} ${hit.module.value} published more than one ${hit.revision}"
                  )
                )
              else Right(seen :+ ChannelPlan(ship, name, hit.group.value, hit.artifact, hit.revision, path, clients))
      case _ =>
        val named = assets.map(_._1).toSet
        if channels.exists(module => named.contains(module.module)) then
          Left(LocalPublishError.Stock(s"${ship.value} names two assets"))
        else Right(Nil)

  private def assetMembers(graph: ProjectGraph, ship: ShipName): List[(ProjectName, RepoPath)] =
    graph.catalog.get(ship) match
      case None      => Nil
      case Some(row) =>
        row.members.toList.flatMap: name =>
          graph.get(name) match
            case Some(module: Module) => module.asset.toList.map(name -> _)
            case _                    => Nil

  private def snapshotClients(graph: ProjectGraph, ship: ShipName): List[DistPlatform] =
    graph.catalog.get(ship).flatMap(_.toolchain) match
      case Some(_) => DistPlatform.values.toList
      case None    => Nil

  private def channel(revision: String): Boolean =
    Revision.parse(revision) match
      case Right(Revision.Pointer(_) | Revision.Snapshot(_)) => true
      case _                                                 => false

  private def assets(repo: Path, plans: List[ChannelPlan]): Either[LocalPublishError, Unit] =
    plans.foldLeft[Either[LocalPublishError, Unit]](Right(())): (acc, plan) =>
      acc.flatMap(_ => present(repo, plan))

  private def present(repo: Path, plan: ChannelPlan): Either[LocalPublishError, Unit] =
    isFile(repo.resolve(plan.asset.value).normalize).flatMap:
      case true  => clientsPresent(repo, plan)
      case false =>
        Left(
          LocalPublishError.Stock(
            s"${plan.asset.value} is not a file. ./histos assembly writes the install jar."
          )
        )

  private def clientsPresent(repo: Path, plan: ChannelPlan): Either[LocalPublishError, Unit] =
    linked(repo, plan).map(_ => ())

  /** The clients this publish linked. The ship asked, and every file is absent: refuse before a token. One machine
    * links the platforms it can, and the manifest names those.
    */
  private def linked(repo: Path, plan: ChannelPlan): Either[LocalPublishError, List[DistPlatform]] =
    plan.clients match
      case Nil                  => Right(Nil)
      case asked @ (first :: _) =>
        val gathered =
          asked.foldLeft[Either[LocalPublishError, List[DistPlatform]]](Right(Nil)): (acc, platform) =>
            acc.flatMap: found =>
              val relative = Path.of(DistLayout.builtClient(platform))
              isFile(repo.resolve(relative).normalize).map: exists =>
                if exists then found :+ platform else found
        gathered.flatMap:
          case Nil   => Left(LocalPublishError.MissingClient(plan.ship, Path.of(DistLayout.builtClient(first))))
          case found => Right(found)

  private def isFile(path: Path): Either[LocalPublishError, Boolean] =
    try Right(Files.isRegularFile(path))
    catch case _: java.io.IOException => Left(LocalPublishError.Read(path))

  private def send(
      repo: Path,
      access: AssetAccess.Access,
      bases: AssetAccess.Bases,
      plan: ChannelPlan,
      lines: List[String],
  ): IO[LocalPublishError, String] =
    for
      unique    <- ZIO.fromEither(timestamp(plan, lines))
      jar       <- bytes(repo.resolve(plan.asset.value).normalize)
      platforms <- ZIO.fromEither(linked(repo, plan))
      clients   <- ZIO.foreach(platforms)(platform =>
        bytes(repo.resolve(DistLayout.builtClient(platform)).normalize).map(platform -> _)
      )
      _ <- replace(access, bases, plan, files(plan.revision, unique, jar, clients))
    yield s"${plan.ship.value} dist ${DistLayout.channel(plan.revision)} $unique"

  private def timestamp(plan: ChannelPlan, lines: List[String]): Either[LocalPublishError, String] =
    lines.iterator.flatMap(line => receipt(plan, line)).nextOption() match
      case Some(unique) if LauncherPin.validRevision(plan.revision, unique) => Right(unique)
      case Some(unique)                                                     =>
        Left(LocalPublishError.Stock(s"$unique is not a timestamp of ${plan.revision}"))
      case None =>
        Left(LocalPublishError.Stock(s"${plan.artifact} ${plan.revision} did not publish a timestamp"))

  /** `group artifact revision unique`, the snapshot receipt from [[PublishedModule.rendered]]. */
  private def receipt(plan: ChannelPlan, line: String): Option[String] =
    line.split(" ", -1) match
      case Array(group, artifact, revision, unique)
          if group == plan.group && artifact == plan.artifact && revision == plan.revision =>
        Some(unique)
      case _ => None

  private def bytes(path: Path): IO[LocalPublishError, Chunk[Byte]] =
    ZIO
      .attemptBlocking(Chunk.fromArray(Files.readAllBytes(path)))
      .mapError(_ => LocalPublishError.Read(path))

  private def replace(
      access: AssetAccess.Access,
      bases: AssetAccess.Bases,
      plan: ChannelPlan,
      files: List[(String, Chunk[Byte])],
  ): IO[LocalPublishError, Unit] =
    ZIO
      .serviceWithZIO[Assets](assets => ZIO.foreachDiscard(files)((name, bytes) => assets.replace(Tag, name, bytes)))
      .provide(Assets.layer(bases.api, bases.uploads, access.token, access.repository))
      .mapError(error => LocalPublishError.Stock(s"${plan.ship.value} ${error.message}"))

  private def text(value: String): Chunk[Byte] =
    Chunk.fromArray(value.getBytes(StandardCharsets.UTF_8))
end DistChannel
