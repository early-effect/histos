package histos.acp

import zio.json.*
import zio.json.ast.Json

/** The ACP handshake. Production capabilities follow the MCP tools. This answers `initialize` only. */
object Acp:
  val ProtocolVersion: Int = 1

  def answer(line: String): String =
    line.fromJson[Json] match
      case Left(_)     => rpcError(Json.Null, "parse error")
      case Right(json) =>
        val id = json match
          case Json.Obj(fields) => fields.collectFirst { case ("id", value) => value }.getOrElse(Json.Null)
          case _                => Json.Null
        method(json) match
          case Some("initialize") =>
            Json.Obj(
              "jsonrpc" -> Json.Str("2.0"),
              "id"      -> id,
              "result"  -> Json.Obj(
                "protocolVersion" -> Json.Num(ProtocolVersion),
                "capabilities"    -> Json.Obj(),
              ),
            ).toJson
          case Some(other) => rpcError(id, s"acp does not handle $other")
          case None        => rpcError(id, "acp requires a method")

  private def method(json: Json): Option[String] =
    json match
      case Json.Obj(fields) =>
        fields.collectFirst:
          case ("method", Json.Str(name)) => name
      case _ => None

  private def rpcError(id: Json, message: String): String =
    Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id"      -> id,
      "error"   -> Json.Obj("code" -> Json.Num(-32601), "message" -> Json.Str(message)),
    ).toJson
end Acp
