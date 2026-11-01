package histos.testrun

import histos.node.NodeMiss
import histos.resolve.ResolveError
import histos.select.SelectError
import histos.process.ChildError
import histos.zinc.{CompileError, GraphCompileError}
import histos.pack.MainClassError
import histos.{Platform, ProjectName, ScalaVersion, TestMain, TestProvider}
import java.nio.file.Path

enum ModuleTestError:
  case Tool(
      module: ProjectName,
      version: ScalaVersion,
      error: histos.toolchain.PlatformToolError,
  )
  case Target(error: SelectError)
  case Compile(error: GraphCompileError)
  case Lock(error: ResolveError)
  case LockRead(path: Path)
  case Sources(module: ProjectName, version: ScalaVersion, error: CompileError)
  case MissingSources(module: ProjectName, main: TestMain)
  case MissingProvider(module: ProjectName, provider: TestProvider)
  case ProviderDependency(module: ProjectName, provider: TestProvider)
  case NoneDiscovered(module: ProjectName, provider: TestProvider)
  case NoneMatched(patterns: List[SuitePattern])
  case Mixed(module: ProjectName)
  case Process(module: ProjectName, version: ScalaVersion, error: ChildError)
  case Main(error: MainClassError)
  case Absent(module: ProjectName)
  case MissingUpstream(
      module: ProjectName,
      dependency: ProjectName,
      version: ScalaVersion,
  )
  case Work(path: Path)
  case Root(path: Path)
  case Runner(path: Path)
  case LinkFailed(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      detail: String,
  )
  case NoNode(module: ProjectName, version: ScalaVersion, miss: NodeMiss)
  case Stamp(error: StampError)
  case Record(error: QuickError)
  case Pattern(error: SuitePatternError)
  case Api(module: ProjectName, version: ScalaVersion, platform: Platform)

  def message: String = this match
    case Tool(_, _, error)               => error.message
    case Target(error)                   => error.message
    case Compile(error)                  => error.message
    case Lock(error)                     => error.message
    case LockRead(path)                  => s"could not read $path"
    case Sources(module, version, error) =>
      s"'${module.value}' ${version.value} failed to compile tests\n${error.message}"
    case MissingSources(module, main) =>
      s"'${module.value}' names ${main.value} and has no test sources"
    case MissingProvider(module, provider) =>
      s"'${module.value}' runs ${provider.framework.value} and has no test sources"
    case ProviderDependency(module, provider) =>
      val library = provider.library
      s"'${module.value}' runs ${provider.framework.value} and testDependencies do not include ${library.organization}::${library.name}"
    case NoneDiscovered(module, provider) =>
      s"'${module.value}' ran ${provider.framework.value} and it found no tests"
    case NoneMatched(patterns) =>
      s"no tests matched ${patterns.map(_.value).mkString(" ")}"
    case Mixed(module) =>
      s"'${module.value}' names a test main and a test provider"
    case Process(module, version, error) =>
      s"'${module.value}' ${version.value} did not start\n${error.message}"
    case Main(error)    => error.message
    case Absent(module) =>
      s"'${module.value}' was compiled and is not a module in the graph"
    case MissingUpstream(module, dependency, version) =>
      s"'${module.value}' depends on '${dependency.value}', which has no ${version.value} classes"
    case Work(path)                                    => s"could not prepare the test work directory $path"
    case Root(path)                                    => s"could not read test roots under $path"
    case Runner(path)                                  => s"could not write the test provider runner at $path"
    case LinkFailed(module, version, platform, detail) =>
      s"'${module.value}' ${version.value} ${platform.token} failed to link\n$detail"
    case NoNode(module, version, miss) =>
      s"'${module.value}' ${version.value} js has no Node to run on: ${miss.message}"
    case Stamp(error)                   => error.message
    case Record(error)                  => error.message
    case Pattern(error)                 => error.message
    case Api(module, version, platform) =>
      s"'${module.value}' ${version.value} ${platform.token} has no readable API digest"
end ModuleTestError
