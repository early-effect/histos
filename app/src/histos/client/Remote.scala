package histos.client

import histos.bsp.Frame
import histos.mcp.Notice
import histos.report.Observation
import histos.server.{Call, Feed, Message, ServerError, ServerIo}

import java.io.InputStream
import java.net.UnixDomainSocketAddress
import java.nio.channels.{Channels, SocketChannel}
import java.nio.file.Path

import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, IO, UIO, ZIO}

/** The exit frame carries the minted run id and the observation. `Client.talk` drops both.
  */
object Remote:
  final case class Heard(
      feeds: Chunk[Feed],
      run: Option[String],
      observation: Option[Observation],
  )

  enum Event:
    case More(feed: Feed)
    case Stop(
        feed: Feed,
        run: Option[String],
        observation: Option[Observation],
    )

  def hear(socket: Path, call: Call): IO[ServerError, Heard] =
    ZIO.scoped {
      for
        channel <- ZIO.acquireRelease(connect(socket))(channel => ZIO.attempt(channel.close()).ignore)
        input  = Channels.newInputStream(channel)
        output = Channels.newOutputStream(channel)
        _     <- Frame.write(output, call.toJson).mapError(written(socket))
        first <- Frame.read(input).mapError(read(socket))
        _     <- ZIO.fromEither(ready(first))
        heard <- collect(input, socket, Chunk.empty)
      yield heard
    }

  private[histos] def event(payload: String): Either[ServerError, Event] =
    payload.fromJson[Json] match
      case Right(Json.Obj(fields)) =>
        fields.toMap.get("Exit") match
          case Some(Json.Obj(body)) => stop(body.toMap)
          case Some(_)              => Left(ServerError.Protocol)
          case None                 => message(payload)
      case _ => message(payload)

  private def stop(body: Map[String, Json]): Either[ServerError, Event] =
    (body.get("code"), body.get("err")) match
      case (Some(code), Some(Json.Str(err))) =>
        for
          n           <- whole(code)
          run         <- text(body.get("run"))
          observation <- tally(body.get("observation"))
        yield Event.Stop(Feed.Exit(n, err), run, observation)
      case _ => Left(ServerError.Protocol)

  private def whole(json: Json): Either[ServerError, Int] =
    json match
      case Json.Num(value) =>
        value.toString.toIntOption match
          case Some(n) => Right(n)
          case None    => Left(ServerError.Protocol)
      case _ => Left(ServerError.Protocol)

  private def text(json: Option[Json]): Either[ServerError, Option[String]] =
    json match
      case None                  => Right(None)
      case Some(Json.Str(value)) => Right(Some(value))
      case Some(_)               => Left(ServerError.Protocol)

  private def tally(
      json: Option[Json]
  ): Either[ServerError, Option[Observation]] =
    json match
      case None        => Right(None)
      case Some(value) =>
        value.toJson.fromJson[Observation] match
          case Right(found) => Right(Some(found))
          case Left(_)      => Left(ServerError.Protocol)

  private def message(payload: String): Either[ServerError, Event] =
    payload.fromJson[Message] match
      case Right(Message.Out(text))       => Right(Event.More(Feed.Out(text)))
      case Right(Message.Err(text))       => Right(Event.More(Feed.Err(text)))
      case Right(Message.Exit(code, err)) =>
        Right(Event.Stop(Feed.Exit(code, err), None, None))
      case _ => Left(ServerError.Protocol)

  private def collect(
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
            note(feed) *> collect(input, socket, acc :+ feed)
          case Right(Event.Stop(feed, run, observation)) =>
            ZIO.succeed(Heard(acc :+ feed, run, observation))
    }

  private def note(feed: Feed): UIO[Unit] =
    feed match
      case Feed.Out(text)  => Notice.out(text)
      case Feed.Err(text)  => Notice.err(text)
      case Feed.Exit(_, _) => ZIO.unit

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
