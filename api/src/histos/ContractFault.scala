package histos

import histos.plugin.{OutputKind, OutputName, PluginTaskId, RepoPath}

enum BuildNode:
  case Compile(stage: CompilationStage)
  case Link(cell: Cell)
  case Task(id: PluginTaskId)

enum ContractFault:
  case RunDefault(module: ProjectName, platform: Platform)
  case RootPlatform(module: ProjectName, platform: Platform)
  case AggregateOverlap(first: ProjectName, second: ProjectName, member: ProjectName)
  case DuplicateOutput(task: PluginTaskId, name: OutputName)
  case OutputOverlap(first: PluginTaskId, second: PluginTaskId, path: RepoPath)
  case EmittedOutput(task: PluginTaskId, name: OutputName)
  case EmittedOutputKind(task: PluginTaskId, name: OutputName, expected: OutputKind, actual: OutputKind)
  case Cycle(path: List[BuildNode])
  case TestModule(name: ProjectName)
  case TestPlatform(module: ProjectName, platform: Platform)
  case TestSelection(error: SelectionError)
  case Parallelism(max: Int)

  def message: String = this match
    case RunDefault(module, platform)            => s"${module.value} does not build its default run platform $platform"
    case RootPlatform(module, platform)          => s"${module.value} does not build root platform $platform"
    case AggregateOverlap(first, second, member) =>
      s"native test aggregates ${first.value} and ${second.value} both include ${member.value}"
    case DuplicateOutput(task, name)        => s"${task.render} declares output ${name.value} twice"
    case OutputOverlap(first, second, path) =>
      s"${first.render} and ${second.render} overlap output ownership at ${path.value}"
    case EmittedOutput(task, name) => s"${task.render} emitted output ${name.value} is not declared"
    case EmittedOutputKind(task, name, expected, actual) =>
      s"${task.render} emitted output ${name.value} is $actual, not $expected"
    case Cycle(path)                    => s"build stage cycle ${path.mkString(" -> ")}"
    case TestModule(name)               => s"test selection ${name.value} is not a module"
    case TestPlatform(module, platform) => s"test selection ${module.value} does not build $platform"
    case TestSelection(error)           => error.message
    case Parallelism(max)               => s"a parallelism max is at least 1, not $max"
end ContractFault
