package histos

import _root_.scala.annotation.publicInBinary

/** A path inside a class directory. The same grammar `src/main/resources` already uses: no empty name, no leading
  * slash, no backslash, and no `.` or `..` segment.
  */
opaque type ResourceName = String

object ResourceName:
  given CanEqual[ResourceName, ResourceName] = CanEqual.derived

  inline def apply(inline raw: String): ResourceName = ${ ResourceNameMacro.literal('raw) }

  def from(raw: String): Either[ResourceNameError, ResourceName] =
    if raw.isEmpty then Left(ResourceNameError.Empty)
    else if raw.startsWith("/") || raw.contains('\\') then Left(ResourceNameError.Escapes(raw))
    else
      val parts = raw.split("/", -1).toList
      if parts.exists(part => part.isEmpty || part == "." || part == "..") then Left(ResourceNameError.Escapes(raw))
      else Right(trusted(raw))

  extension (name: ResourceName) def value: String = name

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(raw: String): ResourceName = raw
end ResourceName

enum ResourceNameError:
  case Empty
  case Escapes(raw: String)

  def message: String = this match
    case Empty        => "a resource name is not empty"
    case Escapes(raw) => s"'$raw' is not a resource name"
end ResourceNameError
