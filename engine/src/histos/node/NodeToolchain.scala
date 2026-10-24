package histos.node

import histos.jdk.Machine
import histos.archive.ArchivePin
import histos.toolchain.{
  LocalOverride,
  LocalOverrideError,
  RuntimeFiles,
  RuntimeIdentity
}
import histos.workspace.IoFailure

import java.nio.file.Path

import zio.{IO, ZIO, ZLayer}

/** The lock that was read and the decision it produced. Printing is
  * [[NodeToolchain.text]].
  */
final case class NodeSurvey(
    lock: NodeLock,
    resolution: NodeResolution,
    identity: Option[RuntimeIdentity],
    archive: Option[ArchivePin]
)

final case class NodeRuntime(binary: NodeBinary, identity: RuntimeIdentity)

final case class NodeToolchain(stager: NodeStage):
  def resolve(machine: Machine): IO[NodeMiss, NodeRuntime] =
    NodeToolchain.resolveWith(machine, stager.stage)

object NodeToolchain:
  val layer: ZLayer[NodeStage, Nothing, NodeToolchain] =
    ZLayer.fromFunction(NodeToolchain.apply)

  /** Read the Node decision. Does not download. A missing lock is an error: the
    * caller already knows Js needs Node.
    */
  def explain(machine: Machine): IO[NodeError, NodeSurvey] =
    for
      explicit <- readOverride(machine.repo)
      pin <- readLock(machine.repo).someOrFail(
        NodeError.NoLock(lockPath(machine.repo))
      )
      found <- survey(machine, pin, explicit)
    yield found

  private def survey(
      machine: Machine,
      lock: NodeLock,
      explicit: Option[Path]
  ): IO[NodeError, NodeSurvey] =
    for
      archive <-
        if explicit.isEmpty then
          ZIO
            .fromEither(lock.archive(machine.host))
            .mapError(NodeError.Archive(_))
            .map(Some(_))
        else ZIO.none
      found <- resolution(lock, explicit)
      identity <- found match
        case NodeResolution.Selected(chosen, _) =>
          RuntimeFiles
            .identity(chosen.home)
            .mapError(NodeError.Io(_))
            .map(Some(_))
        case NodeResolution.OverrideRejected(_, _) => ZIO.none
        case _                                     =>
          ZIO.succeed(archive.map(pin => RuntimeIdentity.Locked(pin.sha256)))
    yield NodeSurvey(lock, found, identity, archive)

  /** The `node` a Js launch runs: the one selected, or the locked archive
    * staged into the cache.
    */

  def resolve(machine: Machine): IO[NodeMiss, NodeRuntime] =
    resolveWith(machine, NodeStage.stage)

  private def resolveWith(
      machine: Machine,
      stage: (NodeLock, Machine) => IO[NodeStageError, NodeHome]
  ): IO[NodeMiss, NodeRuntime] =
    for
      explicit <- readOverride(machine.repo).mapError(NodeMiss.Unresolved(_))
      pin <- readLock(machine.repo).mapError(NodeMiss.Unresolved(_))
      runtime <- pin match
        case None       => bootstrap(machine, explicit)
        case Some(lock) =>
          explicit match
            case None =>
              for
                archive <- ZIO
                  .fromEither(lock.archive(machine.host))
                  .mapError(error =>
                    NodeMiss.Unresolved(NodeError.Archive(error))
                  )
                home <- stage(lock, machine).mapError(error =>
                  NodeMiss.Unresolved(NodeError.Stage(error))
                )
                binary <- NodeProbe
                  .binary(home.path)
                  .mapError(NodeMiss.Unresolved(_))
              yield NodeRuntime(binary, RuntimeIdentity.Locked(archive.sha256))
            case Some(_) =>
              survey(machine, lock, explicit)
                .mapError(NodeMiss.Unresolved(_))
                .flatMap: found =>
                  found.resolution match
                    case NodeResolution.Selected(chosen, _) =>
                      for
                        binary <- NodeProbe
                          .binary(chosen.home)
                          .mapError(NodeMiss.Unresolved(_))
                        identity <- RuntimeFiles
                          .identity(chosen.home)
                          .mapError(error =>
                            NodeMiss.Unresolved(NodeError.Io(error))
                          )
                      yield NodeRuntime(binary, identity)
                    case NodeResolution.OverrideRejected(reason, _) =>
                      ZIO.fail(
                        NodeMiss.Rejected(
                          machine.repo.resolve(".histos/node.local"),
                          reason
                        )
                      )
                    case _ => ZIO.fail(NodeMiss.Unresolved(NodeError.NoRuntime))
    yield runtime

  private def bootstrap(
      machine: Machine,
      explicit: Option[Path]
  ): IO[NodeMiss, NodeRuntime] =
    for
      path <- explicit match
        case Some(path) => ZIO.succeed(path)
        case None       =>
          val name =
            if machine.os == histos.jdk.Os.Windows then "node.exe" else "node"
          RuntimeFiles
            .firstOnPath(machine.env.get("PATH"), List(name))
            .mapError(error => NodeMiss.Unresolved(NodeError.Locations(error)))
            .map(_.flatMap(RuntimeFiles.homeOf))
            .someOrFail(NodeMiss.Unresolved(NodeError.NoRuntime))
      home <- NodeProbe.home(path).mapError(NodeMiss.Unresolved(_))
      _ <- ZIO
        .fail(NodeMiss.Rejected(path, NodeReject.NoBinary))
        .unless(home.hasBinary)
      _ <- ZIO
        .fromEither(home.version)
        .mapError(error =>
          NodeMiss.Rejected(path, NodeReject.UnreadableVersion(error))
        )
      binary <- NodeProbe.binary(path).mapError(NodeMiss.Unresolved(_))
      identity <- RuntimeFiles
        .identity(path)
        .mapError(failure => NodeMiss.Unresolved(NodeError.Io(failure)))
    yield NodeRuntime(binary, identity)

  def text(survey: NodeSurvey): String =
    render(survey.lock, survey.resolution, survey.archive)

  def render(
      lock: NodeLock,
      resolution: NodeResolution,
      archive: Option[ArchivePin]
  ): String =
    val tail = resolution match
      case NodeResolution.Provision(_, _) =>
        archive.fold("explicit local runtime")(pin =>
          s"archive ${pin.source.render}"
        )
      case _ => s"lock ${lock.version.value}"
    Node.report(resolution) + "\n" + tail

  private def resolution(
      lock: NodeLock,
      explicit: Option[Path]
  ): IO[NodeError, NodeResolution] =
    explicit match
      case None       => ZIO.succeed(NodeResolution.Provision(lock.major, Nil))
      case Some(path) =>
        NodeProbe
          .locate(Some(path))
          .map(location =>
            Node.resolve(
              lock.major,
              NodeView(
                location,
                NodeLocated.Unset,
                NodeManagerSlot.Absent,
                NodeManagerSlot.Absent,
                NodeManagerSlot.Absent,
                Nil
              )
            )
          )

  private def readLock(repo: Path): IO[NodeError, Option[NodeLock]] =
    RuntimeFiles
      .readLock(lockPath(repo))(NodeLock.parse)
      .mapError:
        case failure: IoFailure   => NodeError.Io(failure)
        case error: NodeLockError => NodeError.Lock(error)

  private def readOverride(repo: Path): IO[NodeError, Option[Path]] =
    LocalOverride
      .read(repo.resolve(".histos/node.local"))
      .mapError:
        case failure: IoFailure       => NodeError.Io(failure)
        case LocalOverrideError.Empty =>
          NodeError.Override(NodeOverrideError.Empty)
        case LocalOverrideError.Multiple =>
          NodeError.Override(NodeOverrideError.Multiple)
        case LocalOverrideError.Relative(raw) =>
          NodeError.Override(NodeOverrideError.Relative(raw))
        case LocalOverrideError.Invalid(raw) =>
          NodeError.Override(NodeOverrideError.Invalid(raw))

  private def lockPath(repo: Path): Path = repo.resolve(".histos/node.lock")
