package histos

import _root_.scala.annotation.publicInBinary

/** A Maven group. Dotted segments, each starting with a letter. `..` is not a group. */
opaque type Organization = String

object Organization:
  given CanEqual[Organization, Organization] = CanEqual.derived

  private val Segment = """[A-Za-z][A-Za-z0-9_-]*""".r

  inline def apply(inline raw: String): Organization = ${ OrganizationMacro.literal('raw) }

  def from(raw: String): Either[OrganizationError, Organization] =
    val parts = raw.split("\\.", -1)
    if raw.nonEmpty && parts.forall(Segment.matches) then Right(trusted(raw))
    else Left(OrganizationError.Bad(raw))

  extension (organization: Organization)
    def value: String = organization
    def segments: List[String] = organization.split("\\.", -1).toList

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(raw: String): Organization = raw

enum OrganizationError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not an organization"
