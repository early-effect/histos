package histos.toolchain

import histos.jdk.{JdkSurvey, ToolchainError}
import histos.native.{ClangMiss, NativeDeclaration}
import histos.node.{NodeMiss, NodeSurvey}
import histos.{ProjectName}

/** A JDK decision, and a Node decision when the graph asked for one, before clang is resolved.
  */
final case class Reached(jdk: JdkSurvey, node: Option[NodeSurvey])

enum SurveyError:
  case Jdk(error: ToolchainError)
  case Node(jdk: JdkSurvey, error: NodeMiss)
  case Undeclared(reached: Reached, name: ProjectName)
  case Split(reached: Reached, left: ProjectName, right: ProjectName)
  case Clang(reached: Reached, error: ClangMiss)

  /** JDK, Node, and clang were all probed. `report` is every line. `detail` is the failures. */
  case Incomplete(report: String, detail: String)

  def message: String = this match
    case Jdk(error)            => error.message
    case Node(_, error)        => error.message
    case Undeclared(_, name)   => NativeDeclaration.Missing(name).message
    case Split(_, left, right) =>
      NativeDeclaration.Disagree(left, right).message
    case Clang(_, error)       => error.message
    case Incomplete(_, detail) => detail
end SurveyError
