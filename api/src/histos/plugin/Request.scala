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

final case class WrittenDirectory(name: OutputName, path: RepoPath)

final case class PluginOutput(text: String, files: List[WrittenFile], directories: List[WrittenDirectory] = Nil)

/** Invocation-owned staging paths, not the final published repository paths. */
enum OutputDestination:
  case File(override val name: OutputName, path: Path)
  case Directory(override val name: OutputName, path: Path)

  def name: OutputName

  def kind: OutputKind = this match
    case File(_, _)      => OutputKind.File
    case Directory(_, _) => OutputKind.Directory
end OutputDestination

final case class OutputDestinations(values: List[OutputDestination]):
  def file(name: OutputName): Either[PluginRunError, Path] =
    values.find(_.name == name) match
      case Some(OutputDestination.File(_, path)) => Right(path)
      case Some(other) => Left(PluginRunError.OutputKindMismatch(name, OutputKind.File, other.kind))
      case None        => Left(PluginRunError.MissingDestination(name))

  def directory(name: OutputName): Either[PluginRunError, Path] =
    values.find(_.name == name) match
      case Some(OutputDestination.Directory(_, path)) => Right(path)
      case Some(other) => Left(PluginRunError.OutputKindMismatch(name, OutputKind.Directory, other.kind))
      case None        => Left(PluginRunError.MissingDestination(name))
end OutputDestinations

object OutputDestinations:
  val empty: OutputDestinations = OutputDestinations(Nil)

/** Classpath of one cell after `TaskDep.Compile` or `TaskDep.CompileScoped`.
  *
  * `compile` contains main classes/resources and main dependencies. Test-complete compilation supplies `test`, with
  * test classes/resources and test-only dependencies before the main classpath. Main-only compilation leaves `test`
  * empty. `scope` records the completed stage.
  */
final case class CellClasspath(
    module: ProjectName,
    version: ScalaVersion,
    platform: Platform,
    compile: List[Path],
    test: List[Path],
    scope: SourceSet = SourceSet.Test,
):
  def stage: histos.CompilationStage = histos.CompilationStage(histos.Cell(module, version, platform), scope)

/** The script `TaskDep.Link` wrote. */
final case class LinkedScript(
    module: ProjectName,
    version: ScalaVersion,
    script: Path,
    optimize: histos.JsOptimize = histos.JsOptimize.Fast,
)

final case class RunRequest(
    id: PluginTaskId,
    repo: Path,
    modules: List[ModuleView],
    args: PluginArgs,
    inputs: List[MaterialInput],
    invocation: InvocationContext,
    events: PluginEvents.Invocation,
    compiled: List[CellClasspath] = Nil,
    linked: List[LinkedScript] = Nil,
    destinations: OutputDestinations = OutputDestinations.empty,
):
  def file(name: OutputName): Either[PluginRunError, Path]      = destinations.file(name)
  def directory(name: OutputName): Either[PluginRunError, Path] = destinations.directory(name)

/** What `use` registered. `contribution` is the data the graph keeps. `plugin` is the value `contribute` ran on, and
  * the runtime calls `run` on it. The second parameter list keeps the value out of equality, so two evaluations of one
  * build still produce equal graphs.
  */
final case class RegisteredPlugin(
    id: PluginId,
    contribution: Contribution,
)(val plugin: HistosPlugin)
