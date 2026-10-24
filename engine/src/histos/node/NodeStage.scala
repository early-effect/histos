package histos.node

import histos.archive.{
  ArchiveRequest,
  ArchiveStage,
  ArchiveStageError,
  ExtractFailure
}
import histos.jdk.{Machine, Sha256}
import histos.workspace.IoFailure

import java.nio.file.Path

import zio.{IO, ZIO, ZLayer}
import histos.toolchain.ToolArchiveError

enum NodeStageError:
  case UnknownArchive
  case Selection(error: ToolArchiveError)
  case Checksum(expected: Sha256, actual: Sha256)
  case UnsafePath(entry: String)
  case NoHome
  case Version(path: Path, error: NodeVersionError)
  case Extract(archive: Path, failure: ExtractFailure)
  case Io(failure: IoFailure)
  case Archive(error: ArchiveStageError)
  case Probe(error: NodeError)

  def message: String = this match
    case Selection(error) => error.message
    case UnknownArchive   => "node archive is not a zip, a tar.gz, or a tar.xz"
    case Checksum(expected, actual) =>
      s"node checksum was ${actual.value}, lock has ${expected.value}"
    case UnsafePath(entry) => s"node archive entry escapes the cache: $entry"
    case NoHome => "node archive has no runtime of the exact locked version"
    case Version(path, error) =>
      s"could not read the Node version at $path: ${error.message}"
    case Extract(archive, failure) => failure.message(archive)
    case Io(failure)               => failure.message
    case Archive(error)            => error.message
    case Probe(error)              => error.message

final case class NodeStage(archives: ArchiveStage):
  def stage(lock: NodeLock, machine: Machine): IO[NodeStageError, NodeHome] =
    ZIO
      .fromEither(NodeStage.request(lock, machine))
      .mapError(NodeStageError.Selection(_))
      .flatMap(request =>
        archives
          .stage(request)(NodeStage.homeOf(_, lock))
          .mapError(NodeStage.failure)
      )

object NodeStage:
  val layer: ZLayer[ArchiveStage, Nothing, NodeStage] =
    ZLayer.fromFunction(NodeStage.apply)

  def stage(lock: NodeLock, machine: Machine): IO[NodeStageError, NodeHome] =
    ZIO
      .fromEither(request(lock, machine))
      .mapError(NodeStageError.Selection(_))
      .flatMap(request =>
        ArchiveStage.live(request)(homeOf(_, lock)).mapError(failure)
      )

  private def request(
      lock: NodeLock,
      machine: Machine
  ): Either[ToolArchiveError, ArchiveRequest] =
    lock
      .archive(machine.host)
      .map(pin =>
        ArchiveRequest(
          pin.source,
          machine.cache.resolve("nodes").resolve(pin.sha256.value),
          pin.sha256
        )
      )

  private def homeOf(
      unpack: Path,
      lock: NodeLock
  ): IO[NodeStageError, NodeHome] =
    NodeProbe
      .homes(unpack)
      .mapError(stageProbe)
      .flatMap: homes =>
        homes.find(home =>
          home.hasBinary && home.version.contains(lock.version)
        ) match
          case Some(home) => zio.ZIO.succeed(home)
          case None       =>
            val unreadable = homes.collectFirst:
              case NodeHome(path, true, Left(error)) => path -> error
            unreadable match
              case Some((path, error)) =>
                NodeProbe
                  .binary(path)
                  .mapError(stageProbe)
                  .flatMap(binary =>
                    zio.ZIO.fail(NodeStageError.Version(binary.path, error))
                  )
              case None => zio.ZIO.fail(NodeStageError.NoHome)

  private def failure(
      error: ArchiveStageError | NodeStageError
  ): NodeStageError = error match
    case ArchiveStageError.UnknownArchive => NodeStageError.UnknownArchive
    case ArchiveStageError.Checksum(expected, actual) =>
      NodeStageError.Checksum(expected, actual)
    case ArchiveStageError.UnsafePath(entry) => NodeStageError.UnsafePath(entry)
    case ArchiveStageError.Extract(archive, failure) =>
      NodeStageError.Extract(archive, failure)
    case ArchiveStageError.Io(failure) => NodeStageError.Io(failure)
    case error: ArchiveStageError      => NodeStageError.Archive(error)
    case error: NodeStageError         => error

  private def stageProbe(error: NodeError): NodeStageError =
    error match
      case NodeError.Io(failure)          => NodeStageError.Io(failure)
      case NodeError.Version(path, error) => NodeStageError.Version(path, error)
      case other                          => NodeStageError.Probe(other)
