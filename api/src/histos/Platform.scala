package histos

/** A compilation and run target. The command token is `jvm`, `js`, or `native`. */
enum Platform:
  case Jvm, Js, Native

  def token: String = this match
    case Jvm    => "jvm"
    case Js     => "js"
    case Native => "native"

object Platform:
  def from(raw: String): Either[PlatformError, Platform] =
    raw match
      case "jvm"    => Right(Jvm)
      case "js"     => Right(Js)
      case "native" => Right(Native)
      case other    => Left(PlatformError.Unknown(other))

export Platform.{Jvm, Js, Native}

enum PlatformError:
  case Unknown(raw: String)

  def message: String = this match
    case Unknown(raw) => s"'$raw' is not a platform (jvm, js, or native)"
