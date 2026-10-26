package histos.mcp

import heddle.mcp.Session
import heddle.mcp.protocol.Message

import zio.{UIO, ZIO}
import zio.json.*
import zio.json.ast.Json

/** One CLI line, on the request that is running the tool. Outside a request this sends nothing. */
object Notice:
  val Method = "notifications/message"
  val Logger = "histos"

  /** MCP logging level. The wire token is `info` or `error`, not a derived object. */
  enum Level:
    case Info, Error

    def token: String = this match
      case Info  => "info"
      case Error => "error"
  end Level

  object Level:
    given JsonCodec[Level] =
      JsonCodec.string.transformOrFail(
        {
          case "info"  => Right(Level.Info)
          case "error" => Right(Level.Error)
          case other   => Left(s"unknown level $other")
        },
        _.token,
      )

  final case class LogLine(level: Level, logger: String, data: String) derives JsonCodec

  def out(text: String): UIO[Unit] = line(Level.Info, text)

  def err(text: String): UIO[Unit] = line(Level.Error, text)

  private def line(level: Level, text: String): UIO[Unit] =
    if text.isEmpty then ZIO.unit
    else publish(LogLine(level, Logger, text))

  /** Heddle's notification params are a `Json.Obj`. This is the only conversion. */
  private def publish(note: LogLine): UIO[Unit] =
    note.toJsonAST match
      case Right(obj: Json.Obj) => Session.publish(Message.Notification(Method, obj))
      case other                => ZIO.dieMessage(s"a log line encodes as an object, got $other")
end Notice
