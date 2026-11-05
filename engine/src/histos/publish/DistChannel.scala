package histos.publish

import histos.plugin.RepoPath
import histos.version.{Revision, ShipName}
import histos.{Module, ProjectGraph, ProjectName}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, IO, ZIO}

/** One default-branch pointer whose asset jar is published on the `dist` release. */
final case class ChannelPlan(
    ship: ShipName,
    module: ProjectName,
    group: String,
    artifact: String,
    pointer: String,
    asset: RepoPath,
)

/** The shim reads `channel-<ship>-SNAPSHOT` from the existing `dist` release. The body is the timestamp Maven stored
  * for the module that names the asset. The manifest and the jar use that same timestamp. A stored commit coordinate
  * does not move the channel.
  */
object DistChannel:
  val Tag: String = "dist"

  /** Pointer publishes of an asset module. A ship with two assets is refused when one of those modules published a
    * pointer. A stored `<ship>-<sha>-SNAPSHOT` coordinate is not a pointer.
    */
  def plan(graph: ProjectGraph, modules: List[PublishedModule]): Either[LocalPublishError, List[ChannelPlan]] =
    val ships = modules.foldLeft(List.empty[ShipName]): (seen, module) =>
      if seen.contains(module.ship) then seen else seen :+ module.ship
    ships.foldLeft[Either[LocalPublishError, List[ChannelPlan]]](Right(Nil)): (acc, ship) =>
      acc.flatMap: seen =>
        one(graph, ship, modules.filter(_.ship == ship)).map(seen ++ _)

  /** The asset file and the release credentials. An empty plan checks neither. */
  def ready(repo: Path, env: Map[String, String], plans: List[ChannelPlan]): Either[LocalPublishError, Unit] =
    val files = plans.foldLeft[Either[LocalPublishError, Unit]](Right(())): (acc, plan) =>
      acc.flatMap(_ => present(repo, plan))
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

  /** Channel body, manifest, and jar. `clients` is empty. The shim falls back to `java -jar`. */
  def files(pointer: String, revision: String, jar: Chunk[Byte]): List[(String, Chunk[Byte])] =
    val manifest = DistLayout.manifestJson(revision, DistLayout.sha256(jar.toArray), Nil, None)
    List(
      DistLayout.channel(pointer) -> text(revision + "\n"),
      DistLayout.manifest(revision) -> text(manifest),
      DistLayout.jar(revision) -> jar,
    )

  private def one(
      graph: ProjectGraph,
      ship: ShipName,
      modules: List[PublishedModule],
  ): Either[LocalPublishError, List[ChannelPlan]] =
    val assets   = assetMembers(graph, ship)
    val pointers = modules.filter(module => pointer(module.revision))
    assets match
      case Nil                    => Right(Nil)
      case (name, path) :: Nil    =>
        pointers.filter(_.module == name) match
          case Nil          => Right(Nil)
          case hit :: Nil   =>
            Right(List(ChannelPlan(ship, name, hit.group.value, hit.artifact, hit.revision, path)))
          case hit :: _     =>
            Left(LocalPublishError.Stock(s"${ship.value} ${hit.module.value} published more than one ${hit.revision}"))
      case _                      =>
        val named = assets.map(_._1).toSet
        if pointers.exists(module => named.contains(module.module)) then
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

  private def pointer(revision: String): Boolean =
    Revision.parse(revision) match
      case Right(Revision.Pointer(_)) => true
      case _                          => false

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
      _      <- replace(access, bases, plan, files(plan.pointer, unique, jar))
    yield s"${plan.ship.value} dist ${DistLayout.channel(plan.pointer)} $unique"

  private def timestamp(plan: ChannelPlan, lines: List[String]): Either[LocalPublishError, String] =
    lines.iterator.flatMap(line => receipt(plan, line)).nextOption() match
      case Some(unique) if LauncherPin.validRevision(plan.pointer, unique) => Right(unique)
      case Some(unique)                                                    =>
        Left(LocalPublishError.Stock(s"$unique is not a timestamp of ${plan.pointer}"))
      case None                                                            =>
        Left(LocalPublishError.Stock(s"${plan.artifact} ${plan.pointer} did not publish a timestamp"))

  /** `group artifact revision unique`, the snapshot receipt from [[PublishedModule.rendered]]. */
  private def receipt(plan: ChannelPlan, line: String): Option[String] =
    line.split(" ", -1) match
      case Array(group, artifact, revision, unique)
          if group == plan.group && artifact == plan.artifact && revision == plan.pointer =>
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
