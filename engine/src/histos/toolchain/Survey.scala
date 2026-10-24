package histos.toolchain

import histos.jdk.{JdkSurvey, Machine, Toolchain, ToolchainError}
import histos.native.{
  Clang,
  ClangMiss,
  ClangProbe,
  NativeDeclaration,
  NativeProject
}
import histos.node.{Node, NodeMiss, NodeSurvey, NodeToolchain}
import histos.{ProjectGraph, ProjectName}

import zio.{IO, ZIO}

/** What `compile` would use: the JDK, Node when the graph has Js, and clang
  * when it has Native.
  */
final case class Survey(
    jdk: JdkSurvey,
    node: Option[NodeSurvey],
    clang: Option[Clang]
)

/** A JDK decision, and a Node decision when the graph asked for one, before
  * clang is resolved.
  */
final case class Reached(jdk: JdkSurvey, node: Option[NodeSurvey])

enum SurveyError:
  case Jdk(error: ToolchainError)
  case Node(jdk: JdkSurvey, error: NodeMiss)
  case Undeclared(reached: Reached, name: ProjectName)
  case Split(reached: Reached, left: ProjectName, right: ProjectName)
  case Clang(reached: Reached, error: ClangMiss)

  def message: String = this match
    case Jdk(error)            => error.message
    case Node(_, error)        => error.message
    case Undeclared(_, name)   => NativeDeclaration.Missing(name).message
    case Split(_, left, right) =>
      NativeDeclaration.Disagree(left, right).message
    case Clang(_, error) => error.message

object Survey:
  def read(machine: Machine, graph: ProjectGraph): IO[SurveyError, Survey] =
    Toolchain
      .explain(machine)
      .mapError(SurveyError.Jdk(_))
      .flatMap: jdk =>
        val node =
          if !Node.required(graph) then ZIO.succeed(None)
          else
            NodeToolchain
              .explain(machine)
              .mapBoth(
                error => SurveyError.Node(jdk, error),
                found => Some(found)
              )
        node.flatMap: found =>
          val reached = Reached(jdk, found)
          NativeProject.declaration(graph) match
            case NativeDeclaration.NotNative =>
              ZIO.succeed(Survey(jdk, found, None))
            case NativeDeclaration.Missing(name) =>
              ZIO.fail(SurveyError.Undeclared(reached, name))
            case NativeDeclaration.Disagree(left, right) =>
              ZIO.fail(SurveyError.Split(reached, left, right))
            case NativeDeclaration.Major(major) =>
              ClangProbe
                .resolve(major, machine.env)
                .mapBoth(
                  error => SurveyError.Clang(reached, error),
                  clang => Survey(jdk, found, Some(clang))
                )

  /** The lines `histos toolchain` prints. */
  def text(survey: Survey): String =
    survey.clang match
      case None        => shown(Reached(survey.jdk, survey.node))
      case Some(clang) =>
        s"${shown(Reached(survey.jdk, survey.node))}\n${clangLine(clang)}"

  /** Lines already decided when a later toolchain fails. A JDK failure has
    * printed nothing.
    */
  def shown(error: SurveyError): String = error match
    case SurveyError.Jdk(_)                 => ""
    case SurveyError.Node(jdk, _)           => Toolchain.text(jdk)
    case SurveyError.Undeclared(reached, _) => shown(reached)
    case SurveyError.Split(reached, _, _)   => shown(reached)
    case SurveyError.Clang(reached, _)      => shown(reached)

  private def shown(reached: Reached): String =
    reached.node match
      case None       => Toolchain.text(reached.jdk)
      case Some(node) =>
        s"${Toolchain.text(reached.jdk)}\n${NodeToolchain.text(node)}"

  private def clangLine(clang: Clang): String =
    s"chosen clang ${clang.version.value} ${clang.target.value} ${clang.clang}"
