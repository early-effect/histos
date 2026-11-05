package histos.plugin

import histos.doctor.DoctorCheck
import histos.{Platform, ProjectName, ScalaVersion}

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

final case class DeclaredOutput(name: OutputName, path: RepoPath)

enum TaskDep:
  case Compile(module: ProjectName, version: Option[ScalaVersion], platform: Option[Platform])
  case Same(name: TaskName)

  /** Link this JavaScript cell and put the script on the run request. */
  case Link(module: ProjectName, version: Option[ScalaVersion])
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
