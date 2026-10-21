package histos.acp

import histos.cache.ContentStore
import histos.client.Session
import histos.jdk.Machine
import histos.load.Loader
import histos.mcp.HistosMcp
import histos.resolve.Resolve
import histos.server.Call

import java.nio.file.Path

import zio.ZIO
import zio.json.*
import zio.json.ast.Json

/** Newline JSON-RPC. The commands are the MCP tools, and each one returns CLI text. */
object Acp:
  val ProtocolVersion: Int = 1

  def answer(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Session.Desk,
      line: String
  ): ZIO[Loader & Resolve & ContentStore, Nothing, String] =
    line.fromJson[Json] match
      case Left(_) => ZIO.succeed(rpcError(Json.Null, "parse error"))
      case Right(json) =>
        val id = field(json, "id").getOrElse(Json.Null)
        method(json) match
          case Some("initialize") => ZIO.succeed(initialized(id))
          case Some(name) if Session.commands.contains(name) =>
            command(repo, apiJar, machine, desk, id, name, field(json, "params").getOrElse(Json.Obj()))
          case Some(other) => ZIO.succeed(rpcError(id, s"acp does not handle $other"))
          case None        => ZIO.succeed(rpcError(id, "acp requires a method"))

  private def command(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Session.Desk,
      id: Json,
      name: String,
      params: Json
  ): ZIO[Loader & Resolve & ContentStore, Nothing, String] =
    val asked = name match
      case "graph" | "toolchain" => Right(if name == "graph" then Call.Graph else Call.Toolchain)
      case "compile"             => params.as[HistosMcp.CompileArgs].map(args => Call.Compile(args.projects))
      case "test"                =>
        params.as[HistosMcp.TestArgs].map(args => Call.Test(args.projects, args.patterns, args.quick))
      case "publish" =>
        params.as[HistosMcp.PublishArgs].map(args => Call.Publish(args.repository, args.projects))
      case "run" =>
        params.as[HistosMcp.RunArgs].flatMap(args => Session.runCall(args.id, args.platform))
      case "cancel" => params.as[HistosMcp.CancelArgs].map(args => Call.Cancel(args.id))
      case other    => Left(s"acp does not handle $other")
    asked match
      case Left(message) => ZIO.succeed(value(id, message))
      case Right(call) =>
        Session
          .perform(repo, apiJar, machine, desk, call)
          .fold(err => value(id, err), text => value(id, text))

  private def initialized(id: Json): String =
    val names = Json.Arr(Session.commands.map(Json.Str(_))*)
    Json
      .Obj(
        "jsonrpc" -> Json.Str("2.0"),
        "id"      -> id,
        "result"  -> Json.Obj(
          "protocolVersion" -> Json.Num(ProtocolVersion),
          "capabilities"    -> Json.Obj("commands" -> names)
        )
      )
      .toJson

  private def value(id: Json, text: String): String =
    Json
      .Obj(
        "jsonrpc" -> Json.Str("2.0"),
        "id"      -> id,
        "result"  -> Json.Obj("value" -> Json.Str(text))
      )
      .toJson

  private def method(json: Json): Option[String] =
    field(json, "method").collect { case Json.Str(name) => name }

  private def field(json: Json, name: String): Option[Json] =
    json match
      case Json.Obj(fields) =>
        fields.collectFirst { case (`name`, value) => value }
      case _ => None

  private def rpcError(id: Json, message: String): String =
    Json
      .Obj(
        "jsonrpc" -> Json.Str("2.0"),
        "id"      -> id,
        "error"   -> Json.Obj(
          "code"    -> Json.Num(-32601),
          "message" -> Json.Str(message)
        )
      )
      .toJson
end Acp
