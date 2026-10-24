package histos.archive

import heddle.client.{Client, ClientError, TargetError}
import heddle.http.{Request, Status, Url}
import heddle.server.BodyBytes
import histos.cache.Scratch
import histos.jdk.{Sha256, Sha256Error}
import histos.toolchain.RuntimeFiles
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, LinkOption, Path}
import scala.util.Try

import zio.{IO, Scope, ZIO, ZLayer}
import zio.stream.ZStream

enum DownloadFailure:
  case Transport(error: ClientError)
  case Status(status: heddle.http.Status)
  case Body(error: ClientError)
  case RedirectMissing
  case RedirectSyntax(raw: String)
  case RedirectInvalid(raw: String, error: HttpsUrlError)
  case RedirectLimit

  def message: String = this match
    case Transport(error)            => error.message
    case Status(status)              => s"HTTP ${status.code}"
    case Body(error)                 => error.message
    case RedirectMissing             => "redirect has no Location header"
    case RedirectSyntax(raw)         => s"redirect URI is malformed: $raw"
    case RedirectInvalid(raw, error) => s"redirect $raw: ${error.message}"
    case RedirectLimit               => "too many archive redirects"

enum ArchiveStageError:
  case Client(source: ArchiveSource, error: ClientError)
  case Download(url: HttpsUrl, failure: DownloadFailure)
  case UnknownArchive
  case Source(error: ArchiveSourceError)
  case Checksum(expected: Sha256, actual: Sha256)
  case CorruptInstallation(expected: Sha256, actual: Sha256)
  case Manifest(path: Path, error: Sha256Error)
  case UnsafePath(entry: String)
  case UnsafeLink(entry: String, target: String)
  case Extract(archive: Path, failure: ExtractFailure)
  case Io(failure: IoFailure)
  case Budget(error: ArchiveBudgetError)
  case Cache(error: RuntimeCacheError)

  def message: String = this match
    case Client(source, error) =>
      s"archive client for ${source.render}: ${error.message}"
    case Download(url, failure) =>
      s"could not download ${url.value}: ${failure.message}"
    case Source(error)  => error.message
    case UnknownArchive => "archive is not a zip, a tar.gz, or a tar.xz"
    case Checksum(expected, actual) =>
      s"archive checksum was ${actual.value}, lock has ${expected.value}"
    case CorruptInstallation(expected, actual) =>
      s"installed runtime checksum was ${actual.value}, expected ${expected.value}"
    case Manifest(path, error) => s"runtime manifest $path: ${error.message}"
    case UnsafePath(entry)     =>
      s"archive entry escapes or conflicts with the installation: $entry"
    case UnsafeLink(entry, target) =>
      s"archive link is unsafe: $entry -> $target"
    case Extract(archive, failure) => failure.message(archive)
    case Io(failure)               => failure.message
    case Budget(error)             => error.message
    case Cache(error)              => error.message

final case class ArchiveRequest(
    source: ArchiveSource,
    root: Path,
    sha256: Sha256
)

final case class ArchiveStage(
    client: Client,
    decoder: ArchiveDecoder,
    budget: ArchiveBudget = ArchiveBudget.default
):
  def stage[E, A](request: ArchiveRequest)(
      validate: Path => IO[E, A]
  ): IO[ArchiveStageError | E, A] =
    val archive = request.root.resolve("archive")
    val unpack = request.root.resolve("unpack")
    for
      _ <- RuntimeCache
        .prepare(request.root)
        .mapError(ArchiveStageError.Cache(_))
      _ <- place(request, archive)
      installed <- exists(unpack)
      _ <-
        if installed then ZIO.unit
        else
          ZIO.scoped:
            for
              fresh <- Scratch.directory(
                unpack,
                ArchiveStageError.Io(IoFailure(unpack, IoFault.Create))
              )
              _ <- ArchiveExtract(decoder, budget)
                .unpack(archive, fresh)
                .mapError:
                  case ArchiveStageError.Extract(
                        _,
                        ExtractFailure.UnknownArchive
                      ) =>
                    ArchiveStageError.UnknownArchive
                  case other => other
              _ <- validate(fresh)
              hash <- RuntimeFiles
                .contentBounded(fresh, Set.empty, budget)
                .mapError(contentFailure)
              _ <- Workspace
                .writeString(
                  fresh.resolve(ArchiveStage.ManifestName),
                  hash.value
                )
                .mapError(ArchiveStageError.Io(_))
              _ <- Scratch.place(
                fresh,
                unpack,
                ArchiveStageError.Io(IoFailure(unpack, IoFault.Write))
              )
            yield ()
      _ <- RuntimeCache
        .installation(unpack)
        .mapError(ArchiveStageError.Cache(_))
      _ <- verifyInstallation(unpack)
      result <- validate(unpack)
    yield result

  private def place(
      request: ArchiveRequest,
      archive: Path
  ): IO[ArchiveStageError, Unit] =
    exists(archive).flatMap:
      case true  => verified(archive, request.sha256)
      case false =>
        ZIO.scoped:
          for
            scratch <- Scratch.file(
              archive,
              ArchiveStageError.Io(IoFailure(archive, IoFault.Write))
            )
            stream <- request.source match
              case ArchiveSource.Local(path) =>
                ZIO.succeed(
                  RuntimeFiles.bytes(path).mapError(ArchiveStageError.Io(_))
                )
              case ArchiveSource.File(url) =>
                url.path
                  .mapError(ArchiveStageError.Source(_))
                  .map(path =>
                    RuntimeFiles.bytes(path).mapError(ArchiveStageError.Io(_))
                  )
              case ArchiveSource.Remote(url) => follow(url, 5)
            actual <- RuntimeFiles
              .writeHashed(
                budget.download(stream),
                scratch
              )
              .mapError:
                case failure: IoFailure        => ArchiveStageError.Io(failure)
                case error: ArchiveBudgetError =>
                  ArchiveStageError.Budget(error)
                case error: ArchiveStageError => error
            _ <- checksum(request.sha256, actual)
            _ <- Scratch.place(
              scratch,
              archive,
              ArchiveStageError.Io(IoFailure(archive, IoFault.Write))
            )
            _ <- verified(archive, request.sha256)
          yield ()

  private def follow(
      url: HttpsUrl,
      remaining: Int
  ): ZIO[Scope, ArchiveStageError, ZStream[Any, ArchiveStageError, Byte]] =
    for
      target <- ZIO
        .fromEither(Url.decode(url.value))
        .mapError(error =>
          ArchiveStageError.Download(
            url,
            DownloadFailure.Transport(
              ClientError.InvalidTarget(url.value, TargetError.Malformed(error))
            )
          )
        )
      response <- client
        .streaming(Request.get(target))
        .mapError(error =>
          ArchiveStageError.Download(url, DownloadFailure.Transport(error))
        )
      stream <- response.status match
        case Status.Ok =>
          ZIO.succeed(
            response.body.toStream.mapError(error =>
              ArchiveStageError.Download(
                url,
                DownloadFailure.Body(ClientError.BodyFailed(error))
              )
            )
          )
        case status if Set(301, 302, 303, 307, 308).contains(status.code) =>
          for
            _ <- ZIO
              .fail(
                ArchiveStageError.Download(url, DownloadFailure.RedirectLimit)
              )
              .when(remaining == 0)
            location <- ZIO
              .fromOption(response.header("Location"))
              .orElseFail(
                ArchiveStageError.Download(url, DownloadFailure.RedirectMissing)
              )
            resolved <- ZIO
              .fromEither(
                Try(
                  java.net.URI(url.value).resolve(location).toASCIIString
                ).toEither
              )
              .mapError(_ =>
                ArchiveStageError
                  .Download(url, DownloadFailure.RedirectSyntax(location))
              )
            next <- ZIO
              .fromEither(HttpsUrl.from(resolved))
              .mapError(error =>
                ArchiveStageError.Download(
                  url,
                  DownloadFailure.RedirectInvalid(location, error)
                )
              )
            stream <- follow(next, remaining - 1)
          yield stream
        case status =>
          ZIO.fail(
            ArchiveStageError.Download(url, DownloadFailure.Status(status))
          )
    yield stream

  private def exists(path: Path): IO[ArchiveStageError, Boolean] =
    ZIO
      .attemptBlocking(Files.exists(path, LinkOption.NOFOLLOW_LINKS))
      .mapError(_ => ArchiveStageError.Io(IoFailure(path, IoFault.Stat)))

  private def verified(
      path: Path,
      expected: Sha256
  ): IO[ArchiveStageError, Unit] =
    ZIO
      .attemptBlocking(Files.size(path))
      .mapError(_ => ArchiveStageError.Io(IoFailure(path, IoFault.Stat)))
      .flatMap(size =>
        ZIO
          .fail(
            ArchiveStageError.Budget(
              ArchiveBudgetError
                .Exceeded(ArchiveLimit.DownloadBytes, budget.downloadBytes)
            )
          )
          .when(size > budget.downloadBytes)
      ) *>
      RuntimeFiles
        .sha256(path)
        .mapError(ArchiveStageError.Io(_))
        .flatMap(checksum(expected, _))

  private def checksum(
      expected: Sha256,
      actual: Sha256
  ): IO[ArchiveStageError, Unit] =
    ZIO
      .fail(ArchiveStageError.Checksum(expected, actual))
      .unless(actual == expected)
      .unit

  private def contentFailure(
      error: IoFailure | ArchiveBudgetError
  ): ArchiveStageError = error match
    case error: IoFailure          => ArchiveStageError.Io(error)
    case error: ArchiveBudgetError => ArchiveStageError.Budget(error)

  private def verifyInstallation(unpack: Path): IO[ArchiveStageError, Unit] =
    val manifest = unpack.resolve(ArchiveStage.ManifestName)
    for
      text <- Workspace.readString(manifest).mapError(ArchiveStageError.Io(_))
      expected <- ZIO
        .fromEither(Sha256.from(text))
        .mapError(ArchiveStageError.Manifest(manifest, _))
      actual <- RuntimeFiles
        .contentBounded(unpack, Set(manifest), budget)
        .mapError(contentFailure)
      _ <- ZIO
        .fail(ArchiveStageError.CorruptInstallation(expected, actual))
        .unless(actual == expected)
    yield ()

object ArchiveStage:
  private[histos] val ManifestName = ".histos-runtime.sha256"

  val layer: ZLayer[Client & ArchiveDecoder, Nothing, ArchiveStage] =
    ZLayer.fromZIO(for
      client <- ZIO.service[Client]
      decoder <- ZIO.service[ArchiveDecoder]
    yield ArchiveStage(client, decoder))

  def live[E, A](
      request: ArchiveRequest
  )(validate: Path => IO[E, A]): IO[ArchiveStageError | E, A] =
    ZIO
      .serviceWithZIO[ArchiveStage](_.stage(request)(validate))
      .provideLayer(
        ((ZLayer.succeed(
          Client.Config.default
            .copy(maxBodyBytes = BodyBytes(1024L * 1024 * 1024))
        ) >>> Client.layer)
          .mapError(error =>
            ArchiveStageError.Client(request.source, error)
          ) ++ CommonsArchiveDecoder.layer) >>> layer
      )
