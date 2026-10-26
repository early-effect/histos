package histos.client

import histos.bsp.Frame
import histos.mcp.Notice
import histos.server.{Call, Feed, Message, ServerError, ServerIo}

import java.io.InputStream
import java.net.UnixDomainSocketAddress
import java.nio.channels.{Channels, SocketChannel}
import java.nio.file.Path

import zio.json.*
import zio.{Chunk, IO, UIO, ZIO}

/** Frames from one call. The terminal frame is [[Feed.Done]]. Run, observation, and diagnostics ride on that result.
  */
object Remote:
  final case class Heard(feeds: Chunk[Feed])

  enum Event:
    case More(feed: Feed)
    case Stop(feed: Feed)

  def hear(repo: Path, socket: Path, call: Call): IO[ServerError, Heard] =
    ZIO.scoped {
      for
        channel <- ZIO.acquireRelease(connect(socket))(channel => ZIO.attempt(channel.close()).ignore)
        input  = Channels.newInputStream(channel)
        output = Channels.newOutputStream(channel)
        _     <- Frame.write(output, call.toJson).mapError(written(socket))
        first <- Frame.read(input).mapError(read(socket))
        _     <- ZIO.fromEither(ready(first))
        heard <- collect(repo, input, socket, Chunk.empty)
      yield heard
    }

  private[histos] def event(payload: String): Either[ServerError, Event] =
    payload.fromJson[Message] match
      case Right(Message.Event(event)) => Right(Event.More(Feed.Event(event)))
      case Right(Message.Done(result)) => Right(Event.Stop(Feed.Done(result)))
      case _                           => Left(ServerError.Protocol)

  private def collect(
      repo: Path,
      input: InputStream,
      socket: Path,
      acc: Chunk[Feed],
  ): IO[ServerError, Heard] =
    Frame.read(input).mapError(read(socket)).flatMap {
      case None          => ZIO.fail(ServerError.Protocol)
      case Some(payload) =>
        event(payload) match
          case Left(error)             => ZIO.fail(error)
          case Right(Event.More(feed)) =>
            note(repo, feed) *> collect(repo, input, socket, acc :+ feed)
          case Right(Event.Stop(feed)) =>
            ZIO.succeed(Heard(acc :+ feed))
    }

  private def note(repo: Path, feed: Feed): UIO[Unit] =
    feed match
      case Feed.Event(event) => Notice.show(repo, event)
      case Feed.Done(_)      => ZIO.unit

  private def connect(socket: Path): IO[ServerError, SocketChannel] =
    ZIO
      .attemptBlocking(SocketChannel.open(UnixDomainSocketAddress.of(socket)))
      .mapError(_ => ServerError.Unreachable(socket))

  private def ready(frame: Option[String]): Either[ServerError, Unit] =
    frame match
      case Some(payload) =>
        payload.fromJson[Message] match
          case Right(Message.Ready) => Right(())
          case _                    => Left(ServerError.Protocol)
      case None => Left(ServerError.Protocol)

  private def written(socket: Path)(error: Frame.FrameError): ServerError =
    ServerError.Io(ServerIo.Write(socket))

  private def read(socket: Path)(error: Frame.FrameError): ServerError =
    ServerError.Io(ServerIo.Read(socket))
end Remote
