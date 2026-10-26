package histos.plugin

import _root_.scala.annotation.publicInBinary

/** Text a plugin stores on a module. One non-empty line. Richer settings are encoded by the plugin. */
opaque type NoteText = String

object NoteText:
  given CanEqual[NoteText, NoteText] = CanEqual.derived

  inline def apply(inline raw: String): NoteText = ${ PluginMacros.noteText('raw) }

  def from(raw: String): Either[NoteTextError, NoteText] =
    if Tokens.plain(raw) then Right(trusted(raw))
    else if raw.isEmpty then Left(NoteTextError.Empty)
    else Left(NoteTextError.Bad(raw))

  extension (text: NoteText) def value: String = text

  @publicInBinary private[histos] def trusted(raw: String): NoteText = raw
end NoteText

enum NoteTextError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty  => "note text is not empty"
    case Bad(_) => "note text is one line"
end NoteTextError

/** The plugin's own non-empty message. It is not an exception message. */
opaque type PluginDetail = String

object PluginDetail:
  given CanEqual[PluginDetail, PluginDetail] = CanEqual.derived

  inline def apply(inline raw: String): PluginDetail = ${ PluginMacros.pluginDetail('raw) }

  def from(raw: String): Either[PluginDetailError, PluginDetail] =
    if Tokens.plain(raw) then Right(trusted(raw))
    else if raw.isEmpty then Left(PluginDetailError.Empty)
    else Left(PluginDetailError.Bad(raw))

  extension (detail: PluginDetail) def value: String = detail

  @publicInBinary private[histos] def trusted(raw: String): PluginDetail = raw
end PluginDetail

enum PluginDetailError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty  => "a plugin detail is not empty"
    case Bad(_) => "a plugin detail is one line"
end PluginDetailError

/** A declared text input. Non-empty, and it contains no whitespace. */
opaque type InputKey = String

object InputKey:
  given CanEqual[InputKey, InputKey] = CanEqual.derived

  inline def apply(inline raw: String): InputKey = ${ PluginMacros.inputKey('raw) }

  def from(raw: String): Either[InputKeyError, InputKey] =
    if Tokens.inputKey(raw) then Right(trusted(raw))
    else if raw.isEmpty then Left(InputKeyError.Empty)
    else Left(InputKeyError.Bad(raw))

  extension (key: InputKey) def value: String = key

  @publicInBinary private[histos] def trusted(raw: String): InputKey = raw
end InputKey

enum InputKeyError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "an input key is not empty"
    case Bad(raw) => s"'$raw' is not an input key"
end InputKeyError

/** An environment variable the task declares. A letter or `_`, then letters, digits, or `_`. */
opaque type EnvName = String

object EnvName:
  given CanEqual[EnvName, EnvName] = CanEqual.derived

  inline def apply(inline raw: String): EnvName = ${ PluginMacros.envName('raw) }

  def from(raw: String): Either[EnvNameError, EnvName] =
    if Tokens.env(raw) then Right(trusted(raw))
    else if raw.isEmpty then Left(EnvNameError.Empty)
    else Left(EnvNameError.Bad(raw))

  extension (name: EnvName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): EnvName = raw
end EnvName

enum EnvNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "an environment name is not empty"
    case Bad(raw) => s"'$raw' is not an environment name"
end EnvNameError
