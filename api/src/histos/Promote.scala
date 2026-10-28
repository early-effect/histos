package histos

/** How a push to main fills the main index. Rebuild compiles and writes that index. CopyIndex copies the pull-request
  * index and does not compile. The build file writes `rebuild` or `copy-index`. Omission is rebuild.
  */
enum Promote:
  case Rebuild
  case CopyIndex

  def token: String = this match
    case Rebuild   => "rebuild"
    case CopyIndex => "copy-index"
end Promote

object Promote:
  def from(raw: String): Either[PromoteError, Promote] =
    raw match
      case "rebuild"    => Right(Promote.Rebuild)
      case "copy-index" => Right(Promote.CopyIndex)
      case other        => Left(PromoteError.Unknown(other))

enum PromoteError:
  case Unknown(raw: String)

  def message: String = this match
    case Unknown(raw) => s"'$raw' is not a promote strategy. A strategy is rebuild or copy-index."
end PromoteError
