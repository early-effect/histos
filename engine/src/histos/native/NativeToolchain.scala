package histos.native

import histos.{RepositoryUrl, RepositoryUrlError}

/** The Scala Native revision the linker fetches. Empty and whitespace are not revisions. */
opaque type NativeRevision = String

object NativeRevision:
  given CanEqual[NativeRevision, NativeRevision] = CanEqual.derived

  val Release: NativeRevision = trusted("0.5.12")

  def from(raw: String): Either[NativeToolchainError, NativeRevision] =
    if raw.isEmpty then Left(NativeToolchainError.EmptyRevision)
    else if raw.exists(_.isWhitespace) then Left(NativeToolchainError.BadRevision(raw))
    else Right(trusted(raw))

  private[native] def trusted(raw: String): NativeRevision = raw

  extension (revision: NativeRevision) def value: String = revision

/** Where the linker fetches Scala Native. The build file does not name this. */
enum NativeSource:
  case Central
  case IvyLocal
  case Maven(url: RepositoryUrl)

  def label: String = this match
    case Central     => "Maven Central"
    case IvyLocal    => "ivy2 local"
    case Maven(url)  => url.value

/** Stable Central unless `HISTOS_NATIVE` names an origin and a revision. */
final case class NativeToolchain(source: NativeSource, revision: NativeRevision)

object NativeToolchain:
  val Release: NativeToolchain = NativeToolchain(NativeSource.Central, NativeRevision.Release)

  val Key: String = "HISTOS_NATIVE"

  def fromEnv(env: Map[String, String]): Either[NativeToolchainError, NativeToolchain] =
    env.get(Key) match
      case None        => Right(Release)
      case Some(raw)   => parse(raw)

  def parse(raw: String): Either[NativeToolchainError, NativeToolchain] =
    if raw.isEmpty then Left(NativeToolchainError.Empty)
    else
      val at = raw.lastIndexOf('@')
      if at < 0 then Left(NativeToolchainError.Origin(raw))
      else
        val origin = raw.substring(0, at)
        val revision = raw.substring(at + 1)
        for
          parsed <- NativeRevision.from(revision)
          source <- sourceOf(origin)
        yield NativeToolchain(source, parsed)

  private def sourceOf(raw: String): Either[NativeToolchainError, NativeSource] =
    if raw == "ivy-local" then Right(NativeSource.IvyLocal)
    else if raw.startsWith("file:") then
      RepositoryUrl.from(raw) match
        case Left(error) => Left(NativeToolchainError.Repository(error))
        case Right(url)  => Right(NativeSource.Maven(url))
    else Left(NativeToolchainError.Origin(raw))

enum NativeToolchainError:
  case Empty
  case EmptyRevision
  case BadRevision(raw: String)
  case Origin(raw: String)
  case Repository(error: RepositoryUrlError)

  def message: String = this match
    case Empty              => "HISTOS_NATIVE is empty"
    case EmptyRevision      => "HISTOS_NATIVE names no revision"
    case BadRevision(raw)   => s"'$raw' is not a Scala Native revision"
    case Origin(raw)        => s"'$raw' is not HISTOS_NATIVE. Use ivy-local@revision or file:/path@revision"
    case Repository(error)  => error.message
