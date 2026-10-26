package histos.node

import histos.jdk.{Machine, Os}
import histos.toolchain.{LocalOverride, LocalOverrideError, RuntimeFiles, RuntimeIdentity}
import histos.workspace.IoFailure

import java.nio.file.Path

import zio.{IO, ZIO}

/** The Node a Js launch will run. */
final case class NodeSurvey(
    binary: NodeBinary,
    version: NodeVersion,
    identity: RuntimeIdentity,
)

final case class NodeRuntime(
    binary: NodeBinary,
    version: NodeVersion,
    identity: RuntimeIdentity,
)

object NodeToolchain:
  /** Scala.js Wasm needs Node 25, so every Js launch uses that floor. */
  val Floor: NodeVersion =
    NodeVersion.trusted("25", NodeMajor.trusted(25))

  def explain(machine: Machine): IO[NodeMiss, NodeSurvey] =
    resolve(machine).map(runtime => NodeSurvey(runtime.binary, runtime.version, runtime.identity))

  def text(survey: NodeSurvey): String =
    s"node ${survey.version.value} ${survey.binary.path}"

  /** `node` on `PATH`, or the home in `node.local`. A missing binary or one older than [[Floor]] fails. Histos does not
    * fetch Node.
    */
  def resolve(machine: Machine): IO[NodeMiss, NodeRuntime] =
    readOverride(machine.repo)
      .mapError(NodeMiss.Unresolved(_))
      .flatMap:
        case Some(path) => accept(path)
        case None       =>
          val name = if machine.os == Os.Windows then "node.exe" else "node"
          RuntimeFiles
            .firstOnPath(machine.env.get("PATH"), List(name))
            .mapError(error => NodeMiss.Unresolved(NodeError.Locations(error)))
            .flatMap:
              case Some(binary) =>
                RuntimeFiles.homeOf(binary) match
                  case Some(home) => accept(home)
                  case None       => ZIO.fail(NodeMiss.Absent(Floor))
              case None => ZIO.fail(NodeMiss.Absent(Floor))

  private def accept(path: Path): IO[NodeMiss, NodeRuntime] =
    for
      home <- NodeProbe.home(path).mapError(NodeMiss.Unresolved(_))
      _    <- ZIO
        .fail(NodeMiss.Rejected(path, NodeReject.NoBinary))
        .unless(home.hasBinary)
      version <- ZIO
        .fromEither(home.version)
        .mapError(error => NodeMiss.Rejected(path, NodeReject.UnreadableVersion(error)))
      _ <- ZIO
        .fail(NodeMiss.TooOld(Floor, version, path))
        .unless(version.atLeast(Floor))
      binary   <- NodeProbe.binary(path).mapError(NodeMiss.Unresolved(_))
      identity <- RuntimeFiles
        .identity(path, version.value)
        .mapError(failure => NodeMiss.Unresolved(NodeError.Io(failure)))
    yield NodeRuntime(binary, version, identity)

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
