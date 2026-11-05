package histos.node

import histos.jdk.Machine
import histos.toolchain.{LocalOverride, LocalOverrideError, RuntimeFiles, RuntimeIdentity}
import histos.workspace.{IoFailure, IoFault}
import histos.NodePath

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

final case class NodeRuntime(
    binary: NodeBinary,
    version: NodeVersion,
    identity: RuntimeIdentity,
)

object NodeToolchain:
  /** Scala.js Wasm needs Node 25, so every Js launch uses that floor. */
  val Floor: NodeVersion =
    NodeVersion.trusted("25", NodeMajor.trusted(25))

  /** Absolute path of a `node` binary. Set, it wins over the build, `node.local`, and `PATH`. */
  val Key: String = "HISTOS_NODE"

  def explain(machine: Machine, nodePath: Option[NodePath]): IO[NodeMiss, NodeSurvey] =
    resolve(machine, nodePath).map(runtime => NodeSurvey(runtime.binary, runtime.version, runtime.identity))

  def text(survey: NodeSurvey): String =
    s"node ${survey.version.value} ${survey.binary.path}"

  /** `HISTOS_NODE`, then a [[NodePath]] on the build, then the home in `node.local`, then `node` on `PATH`. A named
    * binary that is missing, not executable, or older than [[Floor]] fails. Histos does not fetch Node and does not try
    * the next source.
    */
  def resolve(machine: Machine, nodePath: Option[NodePath]): IO[NodeMiss, NodeRuntime] =
    machine.env.get(Key) match
      case Some(raw) =>
        ZIO
          .fromEither(NodePath.from(raw))
          .mapError(error => NodeMiss.Refused(NodeSource.Environment, error))
          .flatMap(chosen => acceptBinary(Path.of(chosen.value), NodeSource.Environment))
      case None =>
        nodePath match
          case Some(chosen) => acceptBinary(Path.of(chosen.value), NodeSource.Config)
          case None         => inherited(machine)

  private def inherited(machine: Machine): IO[NodeMiss, NodeRuntime] =
    readOverride(machine.repo)
      .mapError(NodeMiss.Unresolved(_))
      .flatMap:
        case Some(home) => acceptHome(home, NodeSource.Local)
        case None       => onPath(machine)

  private def onPath(machine: Machine): IO[NodeMiss, NodeRuntime] =
    val name = "node"
    RuntimeFiles
      .firstOnPath(machine.env.get("PATH"), List(name))
      .mapError(error => NodeMiss.Unresolved(NodeError.Locations(error)))
      .flatMap:
        case Some(binary) =>
          RuntimeFiles.homeOf(binary) match
            case Some(home) => acceptHome(home, NodeSource.Path)
            case None       => ZIO.fail(NodeMiss.Absent(Floor, NodeSource.Path))
        case None => ZIO.fail(NodeMiss.Absent(Floor, NodeSource.Path))

  private def acceptBinary(path: Path, source: NodeSource): IO[NodeMiss, NodeRuntime] =
    ready(path, source).flatMap: binary =>
      adopt(binary, source, placed(binary))

  private def acceptHome(home: Path, source: NodeSource): IO[NodeMiss, NodeRuntime] =
    ready(home.resolve("bin").resolve("node"), source).flatMap: binary =>
      adopt(binary, source, home)

  private def adopt(binary: Path, source: NodeSource, home: Path): IO[NodeMiss, NodeRuntime] =
    for
      version <- NodeProbe
        .version(binary)
        .mapError:
          case NodeError.Version(_, error) =>
            NodeMiss.Rejected(binary, NodeReject.UnreadableVersion(error), source)
          case other => NodeMiss.Unresolved(other)
      _ <- ZIO
        .fail(NodeMiss.TooOld(Floor, version, home, source))
        .unless(histos.toolchain.VersionOrder.atLeast(version.value, Floor.value, stripJavaOne = false))
      identity <- RuntimeFiles
        .identity(home, version.value)
        .mapError(failure => NodeMiss.Unresolved(NodeError.Io(failure)))
    yield NodeRuntime(NodeBinary(binary), version, identity)

  /** A `bin/node` layout keeps the home. Any other file is its own identity. */
  private def placed(binary: Path): Path =
    Option(binary.getParent)
      .filter(parent => Option(parent.getFileName).exists(_.toString == "bin"))
      .flatMap(parent => Option(parent.getParent))
      .getOrElse(binary)

  private def ready(path: Path, source: NodeSource): IO[NodeMiss, Path] =
    ZIO
      .attemptBlocking(Files.exists(path))
      .mapError(_ => NodeMiss.Unresolved(NodeError.Io(IoFailure(path, IoFault.Stat))))
      .flatMap:
        case false => ZIO.fail(NodeMiss.Unusable(path, source, NodeFileFault.Missing))
        case true  =>
          ZIO
            .attemptBlocking(path.toRealPath().normalize)
            .mapError(_ => NodeMiss.Unresolved(NodeError.Io(IoFailure(path, IoFault.Stat))))
            .flatMap: canonical =>
              ZIO
                .attemptBlocking((Files.isRegularFile(canonical), Files.isExecutable(canonical)))
                .mapError(_ => NodeMiss.Unresolved(NodeError.Io(IoFailure(canonical, IoFault.Stat))))
                .flatMap:
                  case (false, _)    => ZIO.fail(NodeMiss.Unusable(path, source, NodeFileFault.NotAFile))
                  case (true, false) => ZIO.fail(NodeMiss.Unusable(path, source, NodeFileFault.NotExecutable))
                  case (true, true)  => ZIO.succeed(canonical)

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
end NodeToolchain
