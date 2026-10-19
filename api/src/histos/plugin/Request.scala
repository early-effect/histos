package histos.plugin

import histos.ProjectName

import java.nio.file.Path

/** One note `Projects` kept. The runner sees only the notes of the plugin that is running. */
final case class ModuleNote(plugin: PluginId, key: NoteKey, value: NoteText)

final case class ModuleView(
    name: ProjectName,
    baseDirectory: String,
    notes: List[ModuleNote],
)

final case class NamedText(name: ParamName, value: String)

final case class PluginArgs(texts: List[NamedText], flags: Set[FlagName]):
  def get(name: ParamName): Option[String] =
    texts.collectFirst:
      case NamedText(key, value) if key == name => value

  def flag(name: FlagName): Boolean =
    flags.contains(name)

  def require(name: ParamName): Either[PluginRunError, String] =
    get(name) match
      case Some(value) => Right(value)
      case None        => Left(PluginRunError.MissingArg(name))

object PluginArgs:
  val empty: PluginArgs = PluginArgs(Nil, Set.empty)

/** A declared input after the engine hashed it. `Env` carries the raw value. The manifest does not. */
enum MaterialInput:
  case File(path: RepoPath, sha256: ContentHash)
  case Directory(path: RepoPath, sha256: ContentHash)
  case Module(name: ProjectName, sha256: ContentHash)
  case Note(module: ProjectName, key: NoteKey, value: NoteText)
  case Text(name: InputKey, value: String)
  case Env(name: EnvName, value: String)

final case class WrittenFile(name: OutputName, path: RepoPath)

final case class PluginOutput(text: String, files: List[WrittenFile])

final case class RunRequest(
    id: PluginTaskId,
    repo: Path,
    modules: List[ModuleView],
    args: PluginArgs,
    inputs: List[MaterialInput],
)

/** The contribution stored on the graph. Not the trait, the classloader, or a function. */
final case class RegisteredPlugin(
    id: PluginId,
    entry: EntryName,
    contribution: Contribution,
)
