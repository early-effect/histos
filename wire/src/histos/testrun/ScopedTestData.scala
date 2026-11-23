package histos.testrun

import histos.*
import histos.plugin.{OutputKind, OutputName, PluginTaskId}
import histos.step.StepId
import histos.workspace.IoFailure
import histos.zinc.CompileError
import java.nio.file.Path
import zio.NonEmptyChunk
import zio.json.JsonCodec
import histos.report.WireJson.given
import ScopedStepJson.given

private object ScopedStepJson:
  given JsonCodec[histos.step.Phase] = JsonCodec.derived
  given JsonCodec[StepId]            = JsonCodec.derived

enum TestSelectionError derives JsonCodec:
  case Selection(error: SelectionError)
  case UnknownProject(name: ProjectName)
  case InvalidProjectAggregateMember(aggregate: ProjectName, member: ProjectName)
  case Aggregate(error: TestAggregateError)

enum TestAggregateError derives JsonCodec:
  case UnsupportedNative(module: ProjectName)
  case MissingMember(owner: ProjectName, member: ProjectName)
  case NonModuleMember(owner: ProjectName, member: ProjectName)
  case SelfMember(owner: ProjectName)
  case DuplicateMember(owner: ProjectName, member: ProjectName)
  case Overlap(firstOwner: ProjectName, secondOwner: ProjectName, member: ProjectName)

/** Wire-safe failure metadata, not an execution graph. */
enum TestFailureNode derives JsonCodec:
  case Compile(stage: CompilationStage)
  case Generate(task: PluginTaskId)
  case Link(cell: Cell, optimize: JsOptimize)

enum TestNodeFailure derives JsonCodec:
  case Unknown, Missing, Wrong, Dependency

enum ScopedTestFailure derives JsonCodec:
  case Selection(error: TestSelectionError)
  case DuplicateSuite(name: SuiteName, modules: NonEmptyChunk[ProjectName])
  case Declaration(error: BuildError)
  case Node(failure: TestNodeFailure, node: TestFailureNode)
  case MissingUpstream(stage: CompilationStage, module: ProjectName)
  case MissingCell(task: PluginTaskId, module: ProjectName, version: Option[ScalaVersion], platform: Option[Platform])
  case Skipped(steps: NonEmptyChunk[StepId])
  case UnknownStage(stage: CompilationStage)
  case RootContract(error: BuildError)
  case InvalidPath(raw: String)
  case MissingRoot(path: Path, kind: OutputKind)
  case RootLink(path: Path)
  case Escaped(path: Path)
  case MissingGenerated(task: PluginTaskId, output: OutputName)
  case DuplicateGenerated(task: PluginTaskId, output: OutputName)
  case OutputKindMismatch(task: PluginTaskId, output: OutputName, expected: OutputKind, actual: OutputKind)
  case Io(error: IoFailure)
  case Source(error: CompileError)
  case Resource(error: AssetError)
  case SourceCollision(entry: String, paths: List[Path])
  case ResourceCollision(name: ResourceName, paths: List[Path])

  def message: String = this match
    case DuplicateSuite(name, modules) =>
      s"duplicate test suite ${name.value}: ${modules.toList.map(_.value).mkString(", ")}"
    case Selection(error)                             => s"invalid test selection: $error"
    case Declaration(error)                           => error.message
    case RootContract(error)                          => error.message
    case Node(failure, node)                          => s"test build node $failure: $node"
    case MissingUpstream(stage, module)               => s"$stage has no upstream Test cell for ${module.value}"
    case MissingCell(task, module, version, platform) =>
      s"$task requires a missing cell: ${module.value} $version $platform"
    case Skipped(steps)                   => s"test build stages skipped: ${steps.toList.mkString(", ")}"
    case UnknownStage(stage)              => s"unknown test build stage: $stage"
    case InvalidPath(raw)                 => s"invalid test root path: $raw"
    case MissingRoot(path, kind)          => s"missing test root $kind: $path"
    case RootLink(path)                   => s"test root is a symbolic link: $path"
    case Escaped(path)                    => s"test root escapes the repository: $path"
    case MissingGenerated(task, output)   => s"missing generated test root: $task ${output.value}"
    case DuplicateGenerated(task, output) => s"duplicate generated test root: $task ${output.value}"
    case OutputKindMismatch(task, output, expected, actual) => s"$task ${output.value} requires $expected, not $actual"
    case Io(error)                                          => error.message
    case Source(error)                                      => error.message
    case Resource(error)                                    => error.message
    case SourceCollision(entry, paths)  => s"test source collision at $entry: ${paths.mkString(", ")}"
    case ResourceCollision(name, paths) => s"test resource collision at ${name.value}: ${paths.mkString(", ")}"
end ScopedTestFailure
