package histos.server

import zio.json.*
import zio.json.ast.Json

/** `.histos/server.json`. `assembly` is missing when the server did not record a checksum. */
final case class ServerFile(
    version: String,
    fingerprint: String,
    socket: String,
    assembly: Option[String],
)

object ServerFile:
  def render(file: ServerFile): String =
    val fields =
      List(
        "version"     -> Json.Str(file.version),
        "fingerprint" -> Json.Str(file.fingerprint),
        "socket"      -> Json.Str(file.socket),
      ) ++ file.assembly.toList.map(value => "assembly" -> Json.Str(value))
    Json.Obj(fields*).toJson

  def parse(text: String): Option[ServerFile] =
    text.fromJson[Json].toOption.flatMap(_.asObject).flatMap { obj =>
      for
        version     <- string(obj, "version")
        fingerprint <- string(obj, "fingerprint")
        socket      <- string(obj, "socket")
      yield ServerFile(version, fingerprint, socket, string(obj, "assembly"))
    }

  private def string(obj: Json.Obj, key: String): Option[String] =
    obj.get(key).collect { case Json.Str(value) if value.nonEmpty => value }
end ServerFile
