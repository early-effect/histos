package histos.plugin

import _root_.scala.annotation.publicInBinary

/** JVM binary name of the plugin's module class, including the trailing `$`. */
opaque type EntryName = String

object EntryName:
  given CanEqual[EntryName, EntryName] = CanEqual.derived

  inline def apply(inline raw: String): EntryName = ${ PluginMacros.entryName('raw) }

  def from(raw: String): Either[EntryNameError, EntryName] =
    if Tokens.entry(raw) then Right(trusted(raw))
    else if raw.isEmpty then Left(EntryNameError.Empty)
    else Left(EntryNameError.Bad(raw))

  def fromClass(cls: Class[?]): Either[EntryNameError, EntryName] =
    from(cls.getName)

  extension (name: EntryName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): EntryName = raw
end EntryName

enum EntryNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "an entry name is not empty"
    case Bad(raw) => s"'$raw' is not an entry name"
end EntryNameError
