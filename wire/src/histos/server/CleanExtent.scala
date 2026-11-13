package histos.server

import zio.json.*

/** What `histos clean` deletes. `Products` is the work tree. `Full` also forgets this repo in the content store. */
enum CleanExtent:
  case Products, Full

  def token: String = this match
    case Products => "products"
    case Full     => "full"
end CleanExtent

object CleanExtent:
  given JsonCodec[CleanExtent] =
    JsonCodec.string.transformOrFail(
      raw => from(raw).left.map(_.message),
      _.token,
    )

  def from(raw: String): Either[CleanExtentError, CleanExtent] =
    CleanExtent.values.find(_.token == raw).toRight(CleanExtentError.Unknown(raw))
end CleanExtent

enum CleanExtentError:
  case Unknown(raw: String)

  def message: String = this match
    case Unknown(raw) => s"'$raw' is not a clean extent"
end CleanExtentError
