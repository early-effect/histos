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
)

/** The shim reads `channel-<version>` from the existing `dist` release for a pin that ends in `-SNAPSHOT`. The body
  * is the timestamp Maven stored for that coordinate. The manifest and the jar use that same timestamp. A stored
  * commit coordinate uploads `channel-<ship>-<sha>-SNAPSHOT` and does not move the pointer channel.
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

  /** The asset file, then GitHub credentials. An empty plan checks neither and does not read `gh` or `origin`. */
  def prepare(
      repo: Path,
      env: Map[String, String],
      plans: List[ChannelPlan],
      readToken: zio.UIO[Option[String]] = AssetAccess.ghToken,
      remote: Path => Option[String] = AssetAccess.origin,
  ): zio.IO[LocalPublishError, Map[String, String]] =
    if plans.isEmpty then ZIO.succeed(env)
    else
      ZIO.fromEither(assets(repo, plans)).flatMap: _ =>
        AssetAccess
          .complete(repo, env, readToken, remote)
          .mapError(error => LocalPublishError.Stock(error.message))

  /** The asset file and the release credentials. An empty plan checks neither. */
  def ready(repo: Path, env: Map[String, String], plans: List[ChannelPlan]): Either[LocalPublishError, Unit] =
    val files = assets(repo, plans)
    files.flatMap: _ =>
      if plans.isEmpty then Right(())
      else
        for
          _ <- AssetAccess.access(env).left.map(error => LocalPublishError.Stock(error.message))
          _ <- AssetAccess.bases(env).left.map(error => LocalPublishError.Stock(error.message))
        yield ()

  /** Replace the channel, then the timestamped manifest and jar. The receipt line is `group artifact revision unique`.
    */
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

  /** Channel body, manifest, and jar. `coordinate` is the pin (`<ship>-SNAPSHOT` or `<ship>-<sha>-SNAPSHOT`). `clients`
    * is empty. The shim falls back to `java -jar`.
    */
  def files(coordinate: String, revision: String, jar: Chunk[Byte]): List[(String, Chunk[Byte])] =
    val manifest = DistLayout.manifestJson(revision, DistLayout.sha256(jar.toArray), Nil, None)
    List(
      DistLayout.channel(coordinate) -> text(revision + "\n"),
      DistLayout.manifest(revision) -> text(manifest),
      DistLayout.jar(revision) -> jar,
    )

  private def one(
      graph: ProjectGraph,
      ship: ShipName,
      modules: List[PublishedModule],
  ): Either[LocalPublishError, List[ChannelPlan]] =
    val assets   = assetMembers(graph, ship)
    val channels = modules.filter(module => channel(module.revision))
    assets match
      case Nil                 => Right(Nil)
      case (name, path) :: Nil =>
        channels.filter(_.module == name).foldLeft[Either[LocalPublishError, List[ChannelPlan]]](Right(Nil)):
          (acc, hit) =>
            acc.flatMap: seen =>
              if seen.exists(_.revision == hit.revision) then
                Left(
                  LocalPublishError.Stock(
                    s"${ship.value} ${hit.module.value} published more than one ${hit.revision}"
                  )
                )
              else Right(seen :+ ChannelPlan(ship, name, hit.group.value, hit.artifact, hit.revision, path))
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

  private def channel(revision: String): Boolean =
    Revision.parse(revision) match
      case Right(Revision.Pointer(_) | Revision.Snapshot(_)) => true
      case _                                                 => false

  private def assets(repo: Path, plans: List[ChannelPlan]): Either[LocalPublishError, Unit] =
    plans.foldLeft[Either[LocalPublishError, Unit]](Right(())): (acc, plan) =>
      acc.flatMap(_ => present(repo, plan))

  private def present(repo: Path, plan: ChannelPlan): Either[LocalPublishError, Unit] =
    val path = repo.resolve(plan.asset.value).normalize
    val file =
      try Right(Files.isRegularFile(path))
      catch case _: java.io.IOException => Left(LocalPublishError.Read(path))
    file.flatMap:
      case true  => Right(())
      case false =>
        Left(
          LocalPublishError.Stock(
            s"${plan.asset.value} is not a file. ./histos assembly writes the install jar."
          )
        )

  private def send(
      repo: Path,
      access: AssetAccess.Access,
      bases: AssetAccess.Bases,
      plan: ChannelPlan,
      lines: List[String],
  ): IO[LocalPublishError, String] =
    for
      unique <- ZIO.fromEither(timestamp(plan, lines))
      jar    <- read(repo, plan)
      _      <- replace(access, bases, plan, files(plan.revision, unique, jar))
    yield s"${plan.ship.value} dist ${DistLayout.channel(plan.revision)} $unique"

  private def timestamp(plan: ChannelPlan, lines: List[String]): Either[LocalPublishError, String] =
    lines.iterator.flatMap(line => receipt(plan, line)).nextOption() match
      case Some(unique) if LauncherPin.validRevision(plan.revision, unique) => Right(unique)
      case Some(unique)                                                     =>
        Left(LocalPublishError.Stock(s"$unique is not a timestamp of ${plan.revision}"))
      case None                                                             =>
        Left(LocalPublishError.Stock(s"${plan.artifact} ${plan.revision} did not publish a timestamp"))

  /** `group artifact revision unique`, the snapshot receipt from [[PublishedModule.rendered]]. */
  private def receipt(plan: ChannelPlan, line: String): Option[String] =
    line.split(" ", -1) match
      case Array(group, artifact, revision, unique)
          if group == plan.group && artifact == plan.artifact && revision == plan.revision =>
        Some(unique)
      case _ => None

  private def read(repo: Path, plan: ChannelPlan): IO[LocalPublishError, Chunk[Byte]] =
    val path = repo.resolve(plan.asset.value).normalize
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
