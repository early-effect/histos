package histos

import _root_.scala.annotation.publicInBinary

/** A `file:` or `https:` repository root. An empty string is not a url. */
opaque type RepositoryUrl = String

object RepositoryUrl:
  given CanEqual[RepositoryUrl, RepositoryUrl] = CanEqual.derived

  inline def apply(inline raw: String): RepositoryUrl = ${ RepositoryUrlMacro.literal('raw) }

  def from(raw: String): Either[RepositoryUrlError, RepositoryUrl] =
    if valid(raw) then Right(trusted(raw)) else Left(RepositoryUrlError.Bad(raw))

  extension (url: RepositoryUrl) def value: String = url

  private def valid(raw: String): Boolean =
    val https = raw.startsWith("https://") && raw.length > "https://".length
    val file  = raw.startsWith("file:") && raw.length > "file:".length
    (https || file) && !raw.exists(_.isWhitespace)

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(raw: String): RepositoryUrl = raw
end RepositoryUrl

enum RepositoryUrlError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a repository url"
end RepositoryUrlError

/** A repository the build asks. `IvyLocal` is not one of these. */
enum Resolver:
  case Central
  case Maven(url: RepositoryUrl)

/** The default when a build lists none: Maven Central, and nothing else. */
val central: Resolver = Resolver.Central
