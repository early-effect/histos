package histos.plugin

import _root_.scala.annotation.publicInBinary

/** Dot-separated plugin id. Each segment starts with a letter. The rest is a letter, a digit, or a hyphen. */
opaque type PluginId = String

object PluginId:
  given CanEqual[PluginId, PluginId] = CanEqual.derived

  inline def apply(inline raw: String): PluginId = ${ PluginMacros.pluginId('raw) }

  def from(raw: String): Either[PluginIdError, PluginId] =
    if Tokens.pluginId(raw) then Right(trusted(raw))
    else if raw.isEmpty then Left(PluginIdError.Empty)
    else Left(PluginIdError.Bad(raw))

  extension (id: PluginId) def value: String = id

  /** Spliced by the literal macro. Source code cannot call it. */
  @publicInBinary private[histos] def trusted(raw: String): PluginId = raw

enum PluginIdError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a plugin id is not empty"
    case Bad(raw) => s"'$raw' is not a plugin id"
