package histos.native

import histos.{NativeVersion, RepositoryUrl, RepositoryUrlError}

/** Where the linker fetches the Scala Native version the module named. The build file does not name this. */
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
  val ReleaseRevision: NativeVersion = NativeVersion("0.5.12")

  val Release: NativeToolchain = NativeToolchain(NativeSource.Central, ReleaseRevision)

  val Key: String = "HISTOS_NATIVE"

  def resolve(revision: NativeVersion, env: Map[String, String]): Either[NativeToolchainError, NativeToolchain] =
    source(env).map(origin => NativeToolchain(origin, revision))

  /** Unset is Central. `ivy-local` or `file:/path` replaces the origin. The revision stays on the module. */
  def source(env: Map[String, String]): Either[NativeToolchainError, NativeSource] =
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

enum NativeToolchainError:
  case Empty
  case Origin(raw: String)
  case Revision(raw: String)
  case Repository(error: RepositoryUrlError)

  def message: String = this match
    case Empty             => "HISTOS_NATIVE is empty"
    case Origin(raw)       => s"'$raw' is not HISTOS_NATIVE. Use ivy-local or file:/path"
    case Revision(raw)     => s"'$raw' puts a revision in HISTOS_NATIVE. Name nativeVersion on the module"
    case Repository(error) => error.message
