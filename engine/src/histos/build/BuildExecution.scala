package histos.build

import histos.cache.ContentStore
import histos.jdk.Sha256
import histos.js.{JsLinkError, JsLinked}
import histos.load.PluginFault
import histos.plugin.{PluginOutput, PluginTaskId, SourceSet}
import histos.resolve.{ResolutionService, Resolve}
import histos.schedule.{ActionCache, Histos, Outcome, RemoteCache, StepFailure}
import histos.step.{HermeticStep, Need, OutputFile, Phase, Step, StepId, StepInfo, Stored, StoredError}
import histos.testrun.ModuleTestError
import histos.zinc.{
  CompileFeed,
  CompileMemo,
  CompileSession,
  CompiledCell,
  CompilerStarts,
  Compilers,
  GeneratedLocation,
  GeneratedRoots,
  GraphCompileError,
  StageRootError,
}
import histos.{Cell, CompilationStage, JsOptimize}

import java.nio.file.Path

import zio.{Chunk, NonEmptyChunk, Ref, Scope, UIO, ZEnvironment, ZIO}

/** The compile and injected plugin/link leaves retain their own adapters and typed failures. */
type BuildEnv = Resolve & ResolutionService & ContentStore & ActionCache & RemoteCache & CompilerStarts & Compilers &
  CompileMemo

final case class StageProduct(
    stage: CompilationStage,
    compiled: CompiledCell,
    analysis: Path,
    api: Sha256,
    implementation: Sha256,
    classpath: List[Path],
):
  def classes: Path = compiled.classes

final case class ExecutionRequest(
    session: CompileSession,
    roots: List[BuildNode],
    report: CompileFeed => UIO[Unit] = _ => ZIO.unit,
)

enum ExecutionError:
  case Graph(error: BuildDagError)
  case Roots(error: StageRootError)
  case Compile(error: GraphCompileError)
  case Test(error: ModuleTestError)
  case Plugin(error: PluginFault)
  case Link(error: JsLinkError)
  case MissingProduct(node: BuildNode)
  case WrongProduct(node: BuildNode)
  case Dependency(node: BuildNode)
  case Skipped(steps: NonEmptyChunk[StepId])

final case class GeneratedTask[+E](output: PluginOutput, read: ZIO[Scope, E | ExecutionError, List[GeneratedLocation]])
final case class TaskProduct(output: PluginOutput, locations: List[GeneratedLocation])

enum ExecutionProduct:
  case Compiled(value: StageProduct)
  case Generated(task: PluginTaskId, value: TaskProduct)
  case Linked(cell: Cell, optimize: JsOptimize, value: JsLinked)

final class ExecutionProducts private[build] (private val values: Map[BuildNode, ExecutionProduct]):
  def nodes: Set[BuildNode] = values.keySet

  def require(stage: CompilationStage): Either[ExecutionError, StageProduct] =
    val node = BuildNode.Compile(stage)
    values.get(node) match
      case Some(ExecutionProduct.Compiled(value)) => Right(value)
      case Some(_)                                => Left(ExecutionError.WrongProduct(node))
      case None                                   => Left(ExecutionError.MissingProduct(node))

  def require(task: PluginTaskId): Either[ExecutionError, TaskProduct] =
    val node = BuildNode.Generate(task)
    values.get(node) match
      case Some(ExecutionProduct.Generated(_, value)) => Right(value)
      case Some(_)                                    => Left(ExecutionError.WrongProduct(node))
      case None                                       => Left(ExecutionError.MissingProduct(node))

  def require(cell: Cell, optimize: JsOptimize): Either[ExecutionError, JsLinked] =
    val node = BuildNode.Link(cell, optimize)
    values.get(node) match
      case Some(ExecutionProduct.Linked(_, _, value)) => Right(value)
      case Some(_)                                    => Left(ExecutionError.WrongProduct(node))
      case None                                       => Left(ExecutionError.MissingProduct(node))

  def generatedRoots: GeneratedRoots = GeneratedRoots.Provided(values.values.toList.collect {
    case ExecutionProduct.Generated(_, value) => value.locations
  }.flatten)

  def stages: Map[CompilationStage, StageProduct] =
    values.values.collect { case ExecutionProduct.Compiled(value) => value.stage -> value }.toMap

  private[build] def visible(nodes: Set[BuildNode]): ExecutionProducts =
    new ExecutionProducts(values.filter((node, _) => nodes.contains(node)))

  private[build] def add(node: BuildNode, product: ExecutionProduct): Either[ExecutionError, ExecutionProducts] =
    val valid = node match
      case BuildNode.Compile(stage) =>
        product match
          case ExecutionProduct.Compiled(value) =>
            stage == value.stage && stage.cell == Cell(
              value.compiled.module,
              value.compiled.version,
              value.compiled.platform,
            )
          case ExecutionProduct.Generated(_, _) | ExecutionProduct.Linked(_, _, _) => false
      case BuildNode.Generate(task) =>
        product match
          case ExecutionProduct.Generated(found, value) => task == found && value.locations.forall(_.id.task == task)
          case ExecutionProduct.Compiled(_) | ExecutionProduct.Linked(_, _, _) => false
      case BuildNode.Link(cell, optimize) =>
        product match
          case ExecutionProduct.Linked(found, mode, _)                         => cell == found && optimize == mode
          case ExecutionProduct.Compiled(_) | ExecutionProduct.Generated(_, _) => false
    if valid then Right(new ExecutionProducts(values.updated(node, product)))
    else Left(ExecutionError.WrongProduct(node))
end ExecutionProducts

object ExecutionProducts:
  val empty: ExecutionProducts = new ExecutionProducts(Map.empty)

/** Leaves receive only completed products. They never recursively enter a compile or plugin executor. */
final case class ExecutionLeaves[R, E](
    main: (ExecutionRequest, CompilationStage, ExecutionProducts) => ZIO[R & Scope, E | ExecutionError, StageProduct],
    test: (ExecutionRequest, CompilationStage, ExecutionProducts) => ZIO[R & Scope, E | ExecutionError, StageProduct],
    generate: (ExecutionRequest, PluginTaskId, ExecutionProducts) => ZIO[R & Scope, E | ExecutionError, GeneratedTask[
      E
    ]],
    link: (ExecutionRequest, Cell, JsOptimize, ExecutionProducts) => ZIO[R & Scope, E | ExecutionError, JsLinked],
)

/** Histos shares node signals and bounds the whole invocation. Products remain typed, not serialized step text. */
final class BuildExecution[R, E](leaves: ExecutionLeaves[R, E], scheduler: Histos):
  import BuildExecution.given
  private type NodeStep = HermeticStep[R, E | ExecutionError, Unit]

  def ensure(request: ExecutionRequest): ZIO[R & Scope, E | ExecutionError, ExecutionProducts] =
    for
      plan <- ZIO.fromEither(BuildDag.plan(request.session.graph, request.roots)).mapError(ExecutionError.Graph(_))
      leaseScope <- ZIO.scope
      products   <- Ref.make(ExecutionProducts.empty)
      steps <- ZIO.fromEither(plan.nodes.foldLeft[Either[ExecutionError, Map[BuildNode, NodeStep]]](Right(Map.empty)) {
        (acc, node) =>
          acc.flatMap { built =>
            plan.dependencies(node).left.map(ExecutionError.Graph(_)).flatMap { dependencies =>
              lookup(dependencies, built).map { previous =>
                val effect = for
                  allowed   <- ZIO.fromEither(plan.prerequisites(node)).mapError(ExecutionError.Graph(_))
                  completed <- products.get.map(_.visible(allowed))
                  product   <- runLeaf(request, node, completed, leaseScope)
                  _         <- products
                    .modify { state =>
                      state.add(node, product) match
                        case Left(error) => (Left(error), state)
                        case Right(next) => (Right(()), next)
                    }
                    .flatMap(ZIO.fromEither)
                yield ()
                val info = infoOf(node)
                val step = combine(previous) match
                  case None           => Step.hermetic[Unit](info)(effect)
                  case Some(upstream) => upstream.andThenHermetic[Unit](info)(_ => effect)
                built.updated(node, step)
              }
            }
          }
      })
      roots <- ZIO.fromEither(lookup(plan.roots, steps))
      _     <- combine(roots) match
        case None       => ZIO.unit
        case Some(goal) =>
          ZIO.scoped[R] {
            scheduler.start(goal).flatMap(_.outcome).flatMap {
              case Outcome.Done(_)          => ZIO.unit
              case Outcome.Failed(failures) =>
                failures.toChunk.collectFirst { case StepFailure.Failed(_, cause) => cause } match
                  case Some(cause) => ZIO.failCause(cause)
                  case None        =>
                    ZIO.fail(ExecutionError.Skipped(failures.map {
                      case StepFailure.Failed(id, _)  => id
                      case StepFailure.Skipped(id, _) => id
                    }))
            }
          }
      completed <- products.get
    yield completed

  private def lookup(nodes: List[BuildNode], steps: Map[BuildNode, NodeStep]): Either[ExecutionError, List[NodeStep]] =
    nodes.foldRight[Either[ExecutionError, List[NodeStep]]](Right(Nil)) { (node, rest) =>
      steps.get(node).toRight(ExecutionError.MissingProduct(node)).flatMap(step => rest.map(step :: _))
    }

  private def combine(steps: List[NodeStep]): Option[NodeStep] =
    steps match
      case Nil          => None
      case head :: tail => Some(tail.foldLeft(head)((left, right) => left.zip(right).map(_ => ())))

  private def runLeaf(
      request: ExecutionRequest,
      node: BuildNode,
      completed: ExecutionProducts,
      leaseScope: Scope,
  ): ZIO[R, E | ExecutionError, ExecutionProduct] =
    node match
      case BuildNode.Compile(stage) =>
        val compile = stage.scope match
          case SourceSet.Main => leaves.main
          case SourceSet.Test => leaves.test
        ZIO.scoped[R](compile(request, stage, completed)).map(ExecutionProduct.Compiled(_))
      case BuildNode.Generate(task) =>
        ZIO.scoped[R](leaves.generate(request, task, completed)).flatMap { generated =>
          generated.read
            .provideEnvironment(ZEnvironment(leaseScope))
            .map(locations => ExecutionProduct.Generated(task, TaskProduct(generated.output, locations)))
        }
      case BuildNode.Link(cell, mode) =>
        ZIO.scoped[R](leaves.link(request, cell, mode, completed)).map(ExecutionProduct.Linked(cell, mode, _))

  private def infoOf(node: BuildNode): StepInfo =
    val id = node match
      case BuildNode.Compile(stage) =>
        val phase = stage.scope match
          case SourceSet.Main => Phase.Compile
          case SourceSet.Test => Phase.TestCompile
        StepId.Cell(stage.cell.module, stage.cell.version, stage.cell.platform, phase)
      case BuildNode.Generate(task)   => StepId.Plugin(task)
      case BuildNode.Link(cell, mode) =>
        StepId.Work(s"link ${cell.module.value} ${cell.version.value} ${cell.platform.token} $mode")
    StepInfo(id, Chunk.empty, Set(Need.Cpu))
end BuildExecution

object BuildExecution:
  private given Stored[Unit] with
    def encode(value: Unit): Chunk[Byte]                      = Chunk.empty
    def decode(bytes: Chunk[Byte]): Either[StoredError, Unit] =
      if bytes.isEmpty then Right(()) else Left(StoredError.Malformed)
    def outputs(value: Unit): Chunk[OutputFile] = Chunk.empty
