package histos

import _root_.scala.annotation.publicInBinary

/** The Maven artifact id before a Scala or platform suffix. `histos-api` publishes as `histos-api_3`. */
opaque type ArtifactBase = String

object ArtifactBase:
  given CanEqual[ArtifactBase, ArtifactBase] = CanEqual.derived

  private val Name = """[A-Za-z][A-Za-z0-9_.-]*""".r

  inline def apply(inline raw: String): ArtifactBase = ${ ArtifactBaseMacro.literal('raw) }

  def from(raw: String): Either[ArtifactBaseError, ArtifactBase] =
    if raw.nonEmpty && !raw.contains("..") && Name.matches(raw) then Right(trusted(raw))
    else Left(ArtifactBaseError.Bad(raw))

  extension (name: ArtifactBase) def value: String = name

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(raw: String): ArtifactBase = raw

enum ArtifactBaseError:
  case Bad(raw: String)

  def message: String = this match
    case Bad(raw) => s"'$raw' is not an artifact name"
