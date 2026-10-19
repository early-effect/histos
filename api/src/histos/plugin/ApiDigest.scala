package histos.plugin

import _root_.scala.annotation.publicInBinary

/** Sha256 of the `histos-api` jar bytes. Not the `0.0.1-SNAPSHOT` product label. */
opaque type ApiDigest = String

object ApiDigest:
  given CanEqual[ApiDigest, ApiDigest] = CanEqual.derived

  /** One line in the plugin jar: the hex digest of the api jar that plugin was compiled against. */
  val resource: String = "META-INF/histos/version"

  inline def apply(inline raw: String): ApiDigest = ${ PluginMacros.apiDigest('raw) }

  def from(raw: String): Either[ApiDigestError, ApiDigest] =
    Tokens.sha256(raw) match
      case Some(hex) => Right(trusted(hex))
      case None      => Left(ApiDigestError.Bad(raw))

  extension (digest: ApiDigest) def value: String = digest

  @publicInBinary private[histos] def trusted(raw: String): ApiDigest = raw

enum ApiDigestError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a sha256"

/** Sha256 of declared content: a file, a directory, or a module's class directories. Not the api jar. */
opaque type ContentHash = String

object ContentHash:
  given CanEqual[ContentHash, ContentHash] = CanEqual.derived

  inline def apply(inline raw: String): ContentHash = ${ PluginMacros.contentHash('raw) }

  def from(raw: String): Either[ContentHashError, ContentHash] =
    Tokens.sha256(raw) match
      case Some(hex) => Right(trusted(hex))
      case None      => Left(ContentHashError.Bad(raw))

  extension (hash: ContentHash) def value: String = hash

  @publicInBinary private[histos] def trusted(raw: String): ContentHash = raw

enum ContentHashError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not a sha256"
