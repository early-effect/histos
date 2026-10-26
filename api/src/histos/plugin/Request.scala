package histos.plugin

import histos.{Platform, ProjectName, ScalaVersion}

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
end PluginArgs

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

/** Classpath of one cell after `TaskDep.Compile`.
  *
  * `compile` is the main class directory, upstream class directories, the JVM standard library, and compile-scope jars.
  * `test` is that list. When the module has test sources, the test class directory is first and test-scope jars follow.
  */
final case class CellClasspath(
    module: ProjectName,
    version: ScalaVersion,
    platform: Platform,
    compile: List[Path],
    test: List[Path],
)

final case class RunRequest(
    id: PluginTaskId,
    repo: Path,
    modules: List[ModuleView],
    args: PluginArgs,
    inputs: List[MaterialInput],
    compiled: List[CellClasspath] = Nil,
)

/** The contribution stored on the graph. Not the trait, the classloader, or a function. */
final case class RegisteredPlugin(
    id: PluginId,
    entry: EntryName,
    contribution: Contribution,
)
