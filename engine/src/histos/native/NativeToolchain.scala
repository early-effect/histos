package histos.native

import histos.{NativeVersion, RepositoryUrl, RepositoryUrlError}

/** Where the linker fetches the Scala Native version the module named. The
  * build file does not name this.
  */
enum NativeSource:
  case Central
  case IvyLocal
  case Maven(url: RepositoryUrl)

  def label: String = this match
    case Central    => "Maven Central"
    case IvyLocal   => "ivy2 local"
    case Maven(url) => url.value

/** A module's Scala Native revision, fetched from the machine's origin. */
final case class NativeToolchain(source: NativeSource, revision: NativeVersion)

object NativeToolchain:
  /** The revision a native module uses when it does not name one. */
  val ReleaseRevision: NativeVersion =
    histos.ToolCatalog.default.native.revision

  def revision(named: Option[NativeVersion]): NativeVersion =
    named.getOrElse(ReleaseRevision)

  val Release: NativeToolchain =
    NativeToolchain(NativeSource.Central, ReleaseRevision)

  val Key: String = "HISTOS_NATIVE"

  def of(
      revision: NativeVersion,
      origin: NativeOrigin
  ): Either[NativeToolchainError, NativeToolchain] =
    origin match
      case NativeOrigin.Chosen(source) =>
        Right(NativeToolchain(source, revision))
      case NativeOrigin.Refused(error) => Left(error)

  /** Unset is Central. `ivy-local` or `file:/path` replaces the origin. The
    * revision stays on the module.
    */
  def source(
      env: Map[String, String]
  ): Either[NativeToolchainError, NativeSource] =
    env.get(Key) match
      case None      => Right(NativeSource.Central)
      case Some(raw) => parse(raw)

  def parse(raw: String): Either[NativeToolchainError, NativeSource] =
    if raw.isEmpty then Left(NativeToolchainError.Empty)
    else if raw.contains("@") then Left(NativeToolchainError.Revision(raw))
    else if raw == "ivy-local" then Right(NativeSource.IvyLocal)
    else if raw.startsWith("file:") then
      RepositoryUrl.from(raw) match
        case Left(error) => Left(NativeToolchainError.Repository(error))
        case Right(url)  => Right(NativeSource.Maven(url))
    else Left(NativeToolchainError.Origin(raw))

/** `HISTOS_NATIVE` as one command read it from the machine. A refusal fails
  * only the Native cells that fetch, so a graph without Native does not care
  * what it says.
  */
enum NativeOrigin:
  case Chosen(source: NativeSource)
  case Refused(error: NativeToolchainError)

object NativeOrigin:
  val Central: NativeOrigin = Chosen(NativeSource.Central)

  def read(env: Map[String, String]): NativeOrigin =
    NativeToolchain.source(env).fold(Refused(_), Chosen(_))

enum NativeToolchainError:
  case Empty
  case Origin(raw: String)
  case Revision(raw: String)
  case Repository(error: RepositoryUrlError)

  def message: String = this match
    case Empty       => "HISTOS_NATIVE is empty"
    case Origin(raw) =>
      s"'$raw' is not HISTOS_NATIVE. Use ivy-local or file:/path"
    case Revision(raw) =>
      s"'$raw' puts a revision in HISTOS_NATIVE. Name nativeVersion on the module"
    case Repository(error) => error.message
