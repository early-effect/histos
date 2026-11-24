package histos.testrun

import histos.build.{BuildDagError, BuildNode, ExecutionError}
import histos.zinc.StageRootError

private[histos] object TestFailures:
  def apply(error: ModuleTest.Error): ModuleTestError = error match
    case error: ModuleTestError    => error
    case error: TestSelectionError => ModuleTestError.Scoped(ScopedTestFailure.Selection(error))
    case error: ExecutionError     =>
      error match
        case ExecutionError.Compile(error) => ModuleTestError.Compile(error)
        case ExecutionError.Test(error)    => error
        case ExecutionError.Plugin(error)  => ModuleTestError.Plugin(error)
        case ExecutionError.Link(error)    => ModuleTestError.Link(error)
        case ExecutionError.Roots(error)   => ModuleTestError.Scoped(root(error))
        case ExecutionError.Graph(error)   =>
          ModuleTestError.Scoped(graph(error))
        case ExecutionError.MissingProduct(found) =>
          ModuleTestError.Scoped(ScopedTestFailure.Node(TestNodeFailure.Missing, node(found)))
        case ExecutionError.WrongProduct(found) =>
          ModuleTestError.Scoped(ScopedTestFailure.Node(TestNodeFailure.Wrong, node(found)))
        case ExecutionError.Dependency(found) =>
          ModuleTestError.Scoped(ScopedTestFailure.Node(TestNodeFailure.Dependency, node(found)))
        case ExecutionError.Skipped(steps) => ModuleTestError.Scoped(ScopedTestFailure.Skipped(steps))

  def graph(error: BuildDagError): ScopedTestFailure = error match
    case BuildDagError.Declaration(error)             => ScopedTestFailure.Declaration(error)
    case BuildDagError.UnknownNode(found)             => ScopedTestFailure.Node(TestNodeFailure.Unknown, node(found))
    case BuildDagError.MissingUpstream(stage, module) => ScopedTestFailure.MissingUpstream(stage, module)
    case BuildDagError.MissingCell(task, module, version, platform) =>
      ScopedTestFailure.MissingCell(task, module, version, platform)

  def node(value: BuildNode): TestFailureNode = value match
    case BuildNode.Compile(stage)   => TestFailureNode.Compile(stage)
    case BuildNode.Generate(task)   => TestFailureNode.Generate(task)
    case BuildNode.Link(cell, mode) => TestFailureNode.Link(cell, mode)

  def root(error: StageRootError): ScopedTestFailure = error match
    case StageRootError.UnknownStage(stage)    => ScopedTestFailure.UnknownStage(stage)
    case StageRootError.Contract(error)        => ScopedTestFailure.RootContract(error)
    case StageRootError.InvalidPath(raw)       => ScopedTestFailure.InvalidPath(raw)
    case StageRootError.Missing(path, kind)    => ScopedTestFailure.MissingRoot(path, kind)
    case StageRootError.Link(path)             => ScopedTestFailure.RootLink(path)
    case StageRootError.Escaped(path)          => ScopedTestFailure.Escaped(path)
    case StageRootError.MissingGenerated(id)   => ScopedTestFailure.MissingGenerated(id.task, id.output)
    case StageRootError.DuplicateGenerated(id) => ScopedTestFailure.DuplicateGenerated(id.task, id.output)
    case StageRootError.OutputKindMismatch(id, expected, actual) =>
      ScopedTestFailure.OutputKindMismatch(id.task, id.output, expected, actual)
    case StageRootError.Io(error)                      => ScopedTestFailure.Io(error)
    case StageRootError.Source(error)                  => ScopedTestFailure.Source(error)
    case StageRootError.Resource(error)                => ScopedTestFailure.Resource(error)
    case StageRootError.SourceCollision(entry, paths)  => ScopedTestFailure.SourceCollision(entry, paths)
    case StageRootError.ResourceCollision(name, paths) => ScopedTestFailure.ResourceCollision(name, paths)
end TestFailures
