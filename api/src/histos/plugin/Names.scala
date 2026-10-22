package histos.plugin

import _root_.scala.annotation.publicInBinary

/** A task's local name. A letter, then letters, digits, or hyphens. No slash and no dot. */
opaque type TaskName = String

object TaskName:
  given CanEqual[TaskName, TaskName] = CanEqual.derived

  inline def apply(inline raw: String): TaskName = ${ PluginMacros.taskName('raw) }

  def from(raw: String): Either[TaskNameError, TaskName] =
    Tokens.parseSimple(raw, TaskNameError.Empty, TaskNameError.Bad(_)).map(trusted)

  extension (name: TaskName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): TaskName = raw

enum TaskNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a task name is not empty"
    case Bad(raw) => s"'$raw' is not a task name"

/** A root command a plugin contributes. Same token rule as a task name, and still a different type. */
opaque type CommandName = String

object CommandName:
  given CanEqual[CommandName, CommandName] = CanEqual.derived

  /** Root words the built-in grammar owns, plus `acp`, held until ACP returns. A plugin command cannot use one. */
  val reserved: Set[CommandName] = Set(
    trusted("version"),
    trusted("graph"),
    trusted("run"),
    trusted("test"),
    trusted("testOnly"),
    trusted("testQuick"),
    trusted("compile"),
    trusted("assembly"),
    trusted("publish"),
    trusted("mcp"),
    trusted("acp"),
    trusted("bsp"),
    trusted("server"),
    trusted("toolchain"),
    trusted("cache"),
    trusted("resolve"),
    trusted("workflow"),
    trusted("reload"),
    trusted("reboot"),
  )

  inline def apply(inline raw: String): CommandName = ${ PluginMacros.commandName('raw) }

  def from(raw: String): Either[CommandNameError, CommandName] =
    Tokens.parseSimple(raw, CommandNameError.Empty, CommandNameError.Bad(_)).map(trusted)

  extension (name: CommandName)
    def value: String = name
    def isReserved: Boolean = reserved.contains(name)

  @publicInBinary private[histos] def trusted(raw: String): CommandName = raw

enum CommandNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a command name is not empty"
    case Bad(raw) => s"'$raw' is not a command name"

/** A CI job name. Same token rule as a task name. `test`, `versions`, and `workflow` are reserved. */
opaque type JobName = String

object JobName:
  given CanEqual[JobName, JobName] = CanEqual.derived

  val reserved: Set[JobName] = Set(
    trusted("test"),
    trusted("versions"),
    trusted("workflow"),
  )

  inline def apply(inline raw: String): JobName = ${ PluginMacros.jobName('raw) }

  def from(raw: String): Either[JobNameError, JobName] =
    Tokens.parseSimple(raw, JobNameError.Empty, JobNameError.Bad(_)).map(trusted)

  extension (name: JobName)
    def value: String = name
    def isReserved: Boolean = reserved.contains(name)

  @publicInBinary private[histos] def trusted(raw: String): JobName = raw

enum JobNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a job name is not empty"
    case Bad(raw) => s"'$raw' is not a job name"

/** A declared output's name. Same token rule as a task name. */
opaque type OutputName = String

object OutputName:
  given CanEqual[OutputName, OutputName] = CanEqual.derived

  inline def apply(inline raw: String): OutputName = ${ PluginMacros.outputName('raw) }

  def from(raw: String): Either[OutputNameError, OutputName] =
    Tokens.parseSimple(raw, OutputNameError.Empty, OutputNameError.Bad(_)).map(trusted)

  extension (name: OutputName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): OutputName = raw

enum OutputNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "an output name is not empty"
    case Bad(raw) => s"'$raw' is not an output name"

/** A positional command parameter. Same token rule as a task name. */
opaque type ParamName = String

object ParamName:
  given CanEqual[ParamName, ParamName] = CanEqual.derived

  inline def apply(inline raw: String): ParamName = ${ PluginMacros.paramName('raw) }

  def from(raw: String): Either[ParamNameError, ParamName] =
    Tokens.parseSimple(raw, ParamNameError.Empty, ParamNameError.Bad(_)).map(trusted)

  extension (name: ParamName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): ParamName = raw

enum ParamNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a parameter name is not empty"
    case Bad(raw) => s"'$raw' is not a parameter name"

/** A boolean flag, without the leading dashes. Same token rule as a task name. */
opaque type FlagName = String

object FlagName:
  given CanEqual[FlagName, FlagName] = CanEqual.derived

  inline def apply(inline raw: String): FlagName = ${ PluginMacros.flagName('raw) }

  def from(raw: String): Either[FlagNameError, FlagName] =
    Tokens.parseSimple(raw, FlagNameError.Empty, FlagNameError.Bad(_)).map(trusted)

  extension (name: FlagName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): FlagName = raw

enum FlagNameError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a flag name is not empty"
    case Bad(raw) => s"'$raw' is not a flag name"

/** A note key on one module. Same token rule as a task name. */
opaque type NoteKey = String

object NoteKey:
  given CanEqual[NoteKey, NoteKey] = CanEqual.derived

  inline def apply(inline raw: String): NoteKey = ${ PluginMacros.noteKey('raw) }

  def from(raw: String): Either[NoteKeyError, NoteKey] =
    Tokens.parseSimple(raw, NoteKeyError.Empty, NoteKeyError.Bad(_)).map(trusted)

  extension (key: NoteKey) def value: String = key

  @publicInBinary private[histos] def trusted(raw: String): NoteKey = raw

enum NoteKeyError:
  case Empty
  case Bad(raw: String)

  def message: String = this match
    case Empty    => "a note key is not empty"
    case Bad(raw) => s"'$raw' is not a note key"
