package histos.version

import _root_.scala.annotation.publicInBinary

/** A full git commit, stored as 40 lowercase hex characters. Shorter input is not a commit. */
opaque type CommitSha = String

object CommitSha:
  given CanEqual[CommitSha, CommitSha] = CanEqual.derived

  /** A literal (`CommitSha("0123...")`), rejected at compile time when it is not 40 hex characters. */
  inline def apply(inline raw: String): CommitSha = ${ CommitShaMacro.literal('raw) }

  def from(raw: String): Either[CommitShaError, CommitSha] =
    if raw.isEmpty then Left(CommitShaError.Empty)
    else if raw.length == 40 && raw.forall(isHex) then Right(trusted(raw.toLowerCase))
    else Left(CommitShaError.NotHex(raw))

  extension (sha: CommitSha)
    def value: String = sha

    /** The 12-character abbreviation registries store. The sha is already lowercase. */
    def abbreviate: AbbrevSha = AbbrevSha.trusted(sha.take(12))
  end extension

  @publicInBinary private[histos] def trusted(raw: String): CommitSha = raw

  private def isHex(char: Char): Boolean =
    char.isDigit || (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F')
end CommitSha

enum CommitShaError:
  case Empty
  case NotHex(raw: String)

  def message: String = this match
    case Empty       => "a commit sha is not empty"
    case NotHex(raw) => s"'$raw' is not a 40-hex commit"
end CommitShaError

/** Exactly 12 lowercase hex characters. This is the stored abbreviation, not a short sha. */
opaque type AbbrevSha = String

object AbbrevSha:
  given CanEqual[AbbrevSha, AbbrevSha] = CanEqual.derived

  def from(raw: String): Either[AbbrevShaError, AbbrevSha] =
    if raw.isEmpty then Left(AbbrevShaError.Empty)
    else if raw.length < 12 && raw.forall(lowerHex) then Left(AbbrevShaError.TooShort(raw))
    else if raw.length == 12 && raw.forall(lowerHex) then Right(trusted(raw))
    else if raw.length == 12 && raw.forall(isHex) then Left(AbbrevShaError.NotLower(raw))
    else Left(AbbrevShaError.NotAbbrev(raw))

  extension (sha: AbbrevSha) def value: String = sha

  /** Spliced from a [[CommitSha]]. A `CommitSha` is 40 lowercase hex, so the first 12 are an abbreviation. */
  @publicInBinary private[histos] def trusted(raw: String): AbbrevSha = raw

  private def lowerHex(char: Char): Boolean =
    char.isDigit || (char >= 'a' && char <= 'f')

  private def isHex(char: Char): Boolean =
    lowerHex(char) || (char >= 'A' && char <= 'F')
end AbbrevSha

enum AbbrevShaError:
  case Empty
  case TooShort(raw: String)
  case NotLower(raw: String)
  case NotAbbrev(raw: String)

  def message: String = this match
    case Empty         => "an abbreviated sha is not empty"
    case TooShort(raw) =>
      s"'$raw' is shorter than 12 hex characters. A snapshot sha is the 12-character abbreviation of the commit."
    case NotLower(raw)  => s"'$raw' is not lowercase. A stored snapshot sha is 12 lowercase hex characters."
    case NotAbbrev(raw) => s"'$raw' is not a 12-hex abbreviation"
end AbbrevShaError
