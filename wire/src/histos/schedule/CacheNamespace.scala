package histos.schedule

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

import histos.version.{PullRequestError, PullRequestNumber}

/** The `dev/<name>` segment. One segment of letters, digits, `-`, or `_`. */
opaque type DevName = String

object DevName:
  given CanEqual[DevName, DevName] = CanEqual.derived

  inline def apply(inline raw: String): DevName = ${
    DevNameMacro.literal('raw)
  }

  def from(raw: String): Either[DevNameError, DevName] =
    if segment(raw) then Right(trusted(raw)) else Left(DevNameError.Segment(raw))

  extension (name: DevName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): DevName = raw

  private def segment(raw: String): Boolean =
    raw.nonEmpty && raw.forall(char => char.isLetterOrDigit || char == '-' || char == '_')
end DevName

enum DevNameError:
  case Segment(raw: String)

  def message: String = this match
    case Segment(raw) =>
      s"'$raw' is not a dev cache name. A name is one segment of letters, digits, '-' or '_'."
end DevNameError

private object DevNameMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[DevName] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "DevName(...) takes a literal. Use DevName.from at runtime.",
          raw,
        )
      case Some(value) =>
        DevName.from(value) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_) => '{ DevName.trusted(${ Expr(value) }) }

/** Action-index prefix. The blob path has none. */
enum CacheNamespace:
  case Main
  case PullRequest(number: PullRequestNumber)
  case Dev(name: DevName)

  def segments: List[String] = this match
    case Main                => List("main")
    case PullRequest(number) => List("pr", number.value.toString)
    case Dev(name)           => List("dev", name.value)

  /** Where `find` looks, in order. `store` writes this value and ignores the rest. */
  def reads: List[CacheNamespace] = this match
    case Main           => List(this)
    case PullRequest(_) => List(this, Main)
    case Dev(_)         => List(this, Main)

  def token: String = segments.mkString("/")
end CacheNamespace

object CacheNamespace:
  /** Env var the generated test step sets. Other jobs leave it unset. The remote URI is separate configuration. */
  val Key: String = "HISTOS_CACHE_NAMESPACE"

  def from(raw: String): Either[NamespaceError, CacheNamespace] =
    raw match
      case "main" => Right(Main)
      case other  =>
        other.split("/", -1).toList match
          case "pr" :: number :: Nil =>
            PullRequestNumber.parse(number).left.map(NamespaceError.Number(_)).map(PullRequest(_))
          case "dev" :: name :: Nil =>
            DevName.from(name).left.map(NamespaceError.Name(_)).map(Dev(_))
          case _ =>
            Left(NamespaceError.Shape(raw))
end CacheNamespace

enum NamespaceError:
  case Shape(raw: String)
  case Number(error: PullRequestError)
  case Name(error: DevNameError)

  def message: String = this match
    case Shape(raw) =>
      s"'$raw' is not a cache namespace. A namespace is main, pr/<n>, or dev/<name>."
    case Number(error) => error.message
    case Name(error)   => error.message
end NamespaceError
