package histos.plugin

import histos.doctor.DoctorCheck
import histos.{JsOptimize, Platform, PlatformSelection, ProjectName, RootKind, ScalaVersion}

/** Data copied onto the project graph. Not an effect. */
final case class Contribution(
    tasks: List[DeclaredTask],
    commands: List[CommandSpec],
    jobs: List[CiJob],
    checks: List[DoctorCheck] = Nil,
    preflight: List[DoctorCheck] = Nil,
)

final case class DeclaredTask(
    name: TaskName,
    seal: Seal,
    needs: Set[Need],
    inputs: List[DeclaredInput],
    outputs: List[DeclaredOutput],
    dependsOn: List[TaskDep],
)

enum DeclaredInput:
  case File(path: RepoPath)
  case Directory(path: RepoPath)
  case Module(name: ProjectName, version: Option[ScalaVersion], platform: Option[Platform])
  case Note(module: ProjectName, key: NoteKey)
  case Text(name: InputKey, value: String)
  case Env(name: EnvName)

enum OutputKind:
  case File
  case Directory

enum DeclaredOutput:
  case File(override val name: OutputName, override val path: RepoPath)
  case Directory(override val name: OutputName, override val path: RepoPath)

  def name: OutputName
  def path: RepoPath
  def kind: OutputKind = this match
    case File(_, _)      => OutputKind.File
    case Directory(_, _) => OutputKind.Directory
end DeclaredOutput

object DeclaredOutput:
  /** The original declaration owns one file. Directory ownership must be explicit. */
  def apply(name: OutputName, path: RepoPath): DeclaredOutput = File(name, path)

enum TaskDep:
  /** Test-complete: both main and test classpaths are supplied to the task. */
  case Compile(module: ProjectName, version: Option[ScalaVersion], platform: Option[Platform])

  /** Compile exactly this scope. Test necessarily includes its corresponding Main stage. */
  case CompileScoped(module: ProjectName, version: Option[ScalaVersion], platform: Option[Platform], scope: SourceSet)
  case Same(name: TaskName)

  /** Link this JavaScript cell and put the script on the run request. */
  case Link(module: ProjectName, version: Option[ScalaVersion], optimize: Option[JsOptimize] = None)

  /** Run this task first. It does not add sources to a module. */
  case Generate(name: TaskName)

  /** This task's output directories are source roots for `module`. It must not also [[Compile]] that module. */
  case Emit(module: ProjectName)

  /** One named File or Directory supplies this exact scope and platform selection. */
  case EmitScoped(
      module: ProjectName,
      scope: SourceSet,
      platforms: PlatformSelection,
      kind: RootKind,
      output: OutputName,
  )
end TaskDep

final case class CommandSpec(
    name: CommandName,
    params: List[CliParam],
    task: TaskName,
    summary: PluginDetail,
)

enum CliParam:
  case Text(name: ParamName)
  case OptionalText(name: ParamName)
  case Flag(name: FlagName)

/** How a header coordinate is linked. `Api` is a real case and loading it is refused. */
enum LinkStrategy:
  case ExactBinary
  case Source
  case BinaryOrSource
  case Api

/** Which bytes actually defined the plugin classes. */
enum PluginLinked:
  case FromBinary(api: ApiDigest)
  case FromSource(published: ApiDigest, running: ApiDigest)
