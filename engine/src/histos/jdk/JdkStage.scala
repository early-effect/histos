package histos.jdk

import histos.archive.*
import histos.workspace.IoFailure

import java.nio.file.Path

import zio.{IO, ZIO, ZLayer}
import histos.toolchain.ToolArchiveError

enum StageError:
  case UnknownArchive
  case Selection(error: ToolArchiveError)
  case Checksum(expected: Sha256, actual: Sha256)
  case UnsafePath(entry: String)
  case NoHome
  case Version(path: Path, error: ReleaseError)
  case Extract(archive: Path, failure: ExtractFailure)
  case Io(failure: IoFailure)
  case Archive(error: ArchiveStageError)
  case Probe(error: JdkError)

  def message: String = this match
    case Selection(error) => error.message
    case UnknownArchive   => "jdk archive is not a zip, a tar.gz, or a tar.xz"
    case Checksum(expected, actual) =>
      s"jdk checksum was ${actual.value}, lock has ${expected.value}"
    case UnsafePath(entry)    => s"jdk archive entry escapes the cache: $entry"
    case Version(path, error) => s"JDK release at $path: ${error.message}"
    case NoHome => "jdk archive has no runtime of the exact locked version"
    case Extract(archive, failure) => failure.message(archive)
    case Io(failure)               => failure.message
    case Archive(error)            => error.message
    case Probe(error)              => error.message

final case class JdkStage(archives: ArchiveStage):
  def stage(lock: JdkLock, machine: Machine): IO[StageError, JdkHome] =
    ZIO
      .fromEither(JdkStage.request(lock, machine))
      .mapError(StageError.Selection(_))
      .flatMap(request =>
        archives
          .stage(request)(JdkStage.homeOf(_, lock))
          .mapError(JdkStage.failure)
      )

object JdkStage:
  val layer: ZLayer[ArchiveStage, Nothing, JdkStage] =
    ZLayer.fromFunction(JdkStage.apply)

  def stage(lock: JdkLock, machine: Machine): IO[StageError, JdkHome] =
    ZIO
      .fromEither(request(lock, machine))
      .mapError(StageError.Selection(_))
      .flatMap(request =>
        ArchiveStage.live(request)(homeOf(_, lock)).mapError(failure)
      )

  private def request(
      lock: JdkLock,
      machine: Machine
  ): Either[ToolArchiveError, ArchiveRequest] =
    lock
      .archive(machine.host)
      .map(pin =>
        ArchiveRequest(
          pin.source,
          machine.cache.resolve("jdks").resolve(pin.sha256.value),
          pin.sha256
        )
      )

  private def homeOf(unpack: Path, lock: JdkLock): IO[StageError, JdkHome] =
    Jdk
      .homes(unpack)
      .mapError(stageProbe)
      .flatMap: homes =>
        homes.find(home =>
          home.hasJavac && home.version.contains(lock.version)
        ) match
          case Some(home) => zio.ZIO.succeed(home)
          case None       =>
            val unreadable = homes.collectFirst:
              case JdkHome(path, true, Left(error)) => path -> error
            unreadable match
              case Some((path, error)) =>
                zio.ZIO.fail(StageError.Version(path.resolve("release"), error))
              case None => zio.ZIO.fail(StageError.NoHome)

  private def failure(error: ArchiveStageError | StageError): StageError =
    error match
      case ArchiveStageError.UnknownArchive => StageError.UnknownArchive
      case ArchiveStageError.Checksum(expected, actual) =>
        StageError.Checksum(expected, actual)
      case ArchiveStageError.UnsafePath(entry) => StageError.UnsafePath(entry)
      case ArchiveStageError.Extract(archive, failure) =>
        StageError.Extract(archive, failure)
      case ArchiveStageError.Io(failure) => StageError.Io(failure)
      case error: ArchiveStageError      => StageError.Archive(error)
      case error: StageError             => error

  private def stageProbe(error: JdkError): StageError =
    error match
      case JdkError.Io(failure) => StageError.Io(failure)
      case other                => StageError.Probe(other)
