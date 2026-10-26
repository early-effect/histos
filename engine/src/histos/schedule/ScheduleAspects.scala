package histos.schedule

import java.nio.ByteBuffer
import java.nio.charset.{CharsetDecoder, CodingErrorAction, StandardCharsets}

import zio.{Chunk, IO, UIO, ZIO}

/** Cache, manifest, and remote behavior applied around a scheduled step. */
private[schedule] object ScheduleAspects:
  def cached(
      cache: ActionCache,
      action: Action,
      digest: Digest,
  ): IO[ScheduleError[Nothing], Option[String]] =
    cache
      .get(digest)
      .mapError(ScheduleError.Cache(_))
      .flatMap:
        case None        => ZIO.none
        case Some(value) =>
          declared(action, value).map(ok => if ok then Some(value) else None)

  def declared(action: Action, stored: String): UIO[Boolean] =
    action.policy match
      case Policy.Core                        => ZIO.succeed(true)
      case Policy.Plugin(_, _, root, outputs) =>
        if outputs.isEmpty then ZIO.succeed(true)
        else
          root match
            case None      => ZIO.succeed(false)
            case Some(dir) => PluginManifest.accept(stored, dir, outputs)

  def remote(
      remote: RemoteCache,
      action: Action,
      digest: Digest,
  ): IO[ScheduleError[Nothing], Option[String]] =
    remote.mode match
      case RemoteMode.Off                     => ZIO.none
      case RemoteMode.Read | RemoteMode.Write =>
        remote
          .find(digest)
          .mapError(ScheduleError.Remote(_))
          .flatMap:
            case None        => ZIO.none
            case Some(bytes) =>
              decode(digest, bytes)
                .mapError(ScheduleError.Remote(_))
                .flatMap: text =>
                  declared(action, text).map(ok => if ok then Some(text) else None)

  def upload(
      remote: RemoteCache,
      verdict: Verdict,
      digest: Digest,
      value: String,
  ): IO[ScheduleError[Nothing], Unit] =
    (remote.mode, verdict) match
      case (RemoteMode.Write, Verdict.Send) =>
        val bytes = Chunk.fromArray(value.getBytes(StandardCharsets.UTF_8))
        remote.store(digest, bytes).mapError(ScheduleError.Remote(_))
      case _ => ZIO.unit

  private def decode(
      action: Digest,
      bytes: Chunk[Byte],
  ): IO[RemoteError, String] =
    ZIO
      .attempt(decoder.decode(ByteBuffer.wrap(bytes.toArray)).toString)
      .mapError(_ => RemoteError.Text(action))

  private def decoder: CharsetDecoder =
    StandardCharsets.UTF_8.newDecoder
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
end ScheduleAspects
