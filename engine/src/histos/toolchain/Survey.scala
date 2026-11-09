package histos.toolchain

import histos.jdk.{JdkRead, JdkSurvey, Machine, Toolchain}
import histos.native.{Clang, ClangMiss, ClangProbe, NativeDeclaration, NativeProject}
import histos.node.{Node, NodeMiss, NodeSource, NodeSurvey, NodeToolchain}
import histos.ProjectGraph

import zio.{IO, UIO, ZIO}

/** What `compile` would use: the JDK, Node when the graph has Js, and clang when it has Native.
  */
final case class Survey(
    jdk: JdkSurvey,
    node: Option[NodeSurvey],
    clang: Option[Clang],
    jdkFrom: String,
    nodeFrom: Option[String],
)

/** Node was not required, or it was probed. */
enum NodeSlot:
  case Idle
  case Ready(survey: NodeSurvey, source: NodeSource)
  case Failed(error: NodeMiss)

/** Clang was not required, or it was probed. A missing or split declaration is a failure, not a skip of the other
  * tools.
  */
enum ClangSlot:
  case Idle
  case Ready(clang: Clang)
  case Failed(message: String)

/** JDK, Node, and clang, including the ones that failed. */
final case class Collected(jdk: JdkRead, node: NodeSlot, clang: ClangSlot):
  def text: String =
    s"$jdkLine\n$nodeLine\n$clangLine"

  def failureText: String =
    failures.mkString("\n")

  def ok: Boolean = failures.isEmpty

  def toSurvey: Option[Survey] =
    if !ok then None
    else
      jdk match
        case JdkRead.Failed(_, _)            => None
        case JdkRead.Chosen(runtime, source) =>
          val nodeSurvey = node match
            case NodeSlot.Ready(found, _) => Some(found)
            case _                        => None
          val from = node match
            case NodeSlot.Ready(_, sourceName) => Some(NodeToolchain.origin(sourceName))
            case _                             => None
          val clangSurvey = clang match
            case ClangSlot.Ready(found) => Some(found)
            case _                      => None
          Some(
            Survey(
              JdkSurvey(runtime.home, runtime.version, runtime.identity),
              nodeSurvey,
              clangSurvey,
              source.label,
              from,
            )
          )

  private def failures: List[String] =
    val jdkText = jdk match
      case JdkRead.Failed(error, source) => List(Toolchain.failure(error, source))
      case JdkRead.Chosen(_, _)          => Nil
    val nodeText = node match
      case NodeSlot.Failed(error) => List(NodeToolchain.failure(error))
      case _                      => Nil
    val clangText = clang match
      case ClangSlot.Failed(message) => List(message)
      case _                         => Nil
    jdkText ++ nodeText ++ clangText

  private def jdkLine: String = jdk match
    case JdkRead.Chosen(runtime, source) =>
      s"jdk ${runtime.version.value} ${runtime.home} from ${source.label}"
    case JdkRead.Failed(error, source) =>
      Toolchain.failure(error, source)

  private def nodeLine: String = node match
    case NodeSlot.Idle                  => "node not required"
    case NodeSlot.Ready(survey, source) =>
      s"${NodeToolchain.text(survey)} from ${NodeToolchain.origin(source)}"
    case NodeSlot.Failed(error) => NodeToolchain.failure(error)

  private def clangLine: String = clang match
    case ClangSlot.Idle            => "clang not required"
    case ClangSlot.Ready(found)    => Survey.clangLine(found)
    case ClangSlot.Failed(message) => message
end Collected

object Survey:
  /** Probes JDK, Node, and clang. A failure of one does not skip the others. */
  def collect(machine: Machine, graph: ProjectGraph): UIO[Collected] =
    for
      jdkFiber   <- Toolchain.read(machine).fork
      nodeFiber  <- nodeOf(machine, graph).fork
      clangFiber <- clangOf(machine, graph).fork
      jdk        <- jdkFiber.join
      node       <- nodeFiber.join
      clang      <- clangFiber.join
    yield Collected(jdk, node, clang)

  def read(machine: Machine, graph: ProjectGraph): IO[SurveyError, Survey] =
    collect(machine, graph).flatMap: found =>
      found.toSurvey match
        case Some(survey) => ZIO.succeed(survey)
        case None         => ZIO.fail(SurveyError.Incomplete(found.text, found.failureText))

  /** The lines `histos toolchain` prints. Each tool is one line, including a tool the graph does not need. */
  def text(survey: Survey): String =
    val jdkLine  = s"${Toolchain.text(survey.jdk)} from ${survey.jdkFrom}"
    val nodeLine = survey.node match
      case None        => "node not required"
      case Some(found) =>
        s"${NodeToolchain.text(found)} from ${survey.nodeFrom.getOrElse("PATH")}"
    val clang = survey.clang match
      case None        => "clang not required"
      case Some(found) => clangLine(found)
    s"$jdkLine\n$nodeLine\n$clang"

  /** Lines already decided when a later toolchain fails. An incomplete survey is every line, not the prefix. */
  def shown(error: SurveyError): String = error match
    case SurveyError.Jdk(_)                 => ""
    case SurveyError.Node(jdk, _)           => Toolchain.text(jdk)
    case SurveyError.Undeclared(reached, _) => shown(reached)
    case SurveyError.Split(reached, _, _)   => shown(reached)
    case SurveyError.Clang(reached, _)      => shown(reached)
    case SurveyError.Incomplete(report, _)  => report

  def clangProblem(error: ClangMiss): String =
    s"${error.message} Histos uses the clang and clang++ pair on PATH and does not fetch a compiler."

  def clangLine(clang: Clang): String =
    s"chosen clang ${clang.version.value} ${clang.target.value} ${clang.clang} from PATH"

  private def nodeOf(machine: Machine, graph: ProjectGraph): UIO[NodeSlot] =
    if !Node.required(graph) then ZIO.succeed(NodeSlot.Idle)
    else
      NodeToolchain
        .choose(machine, graph.runnerTools.nodePath)
        .either
        .map:
          case Right(choice) =>
            NodeSlot.Ready(
              NodeSurvey(choice.runtime.binary, choice.runtime.version, choice.runtime.identity),
              choice.source,
            )
          case Left(error) => NodeSlot.Failed(error)

  private def clangOf(machine: Machine, graph: ProjectGraph): UIO[ClangSlot] =
    NativeProject.declaration(graph) match
      case NativeDeclaration.NotNative =>
        ZIO.succeed(ClangSlot.Idle)
      case NativeDeclaration.Missing(name) =>
        ZIO.succeed(ClangSlot.Failed(s"${NativeDeclaration.Missing(name).message} clang is resolved from PATH."))
      case NativeDeclaration.Disagree(left, right) =>
        ZIO.succeed(
          ClangSlot.Failed(s"${NativeDeclaration.Disagree(left, right).message} clang is resolved from PATH.")
        )
      case NativeDeclaration.Major(major) =>
        ClangProbe
          .resolve(major, machine.env)
          .either
          .map:
            case Right(clang) => ClangSlot.Ready(clang)
            case Left(error)  => ClangSlot.Failed(clangProblem(error))

  private def shown(reached: Reached): String =
    reached.node match
      case None       => Toolchain.text(reached.jdk)
      case Some(node) =>
        s"${Toolchain.text(reached.jdk)}\n${NodeToolchain.text(node)}"
end Survey
