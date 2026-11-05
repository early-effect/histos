package histos

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

/** A Node major the test job asks the runner to install. `25` is the floor, not `25.0.0`. */
opaque type NodeFloor = Int

object NodeFloor:
  given CanEqual[NodeFloor, NodeFloor] = CanEqual.derived

  /** A literal (`NodeFloor(25)`), rejected at compile time when it is not positive. */
  inline def apply(inline raw: Int): NodeFloor = ${ NodeFloorMacro.literal('raw) }

  def from(raw: Int): Either[NodeFloorError, NodeFloor] =
    if raw > 0 then Right(trusted(raw)) else Left(NodeFloorError.NotPositive(raw))

  extension (floor: NodeFloor) def value: Int = floor

  @publicInBinary private[histos] def trusted(raw: Int): NodeFloor = raw
end NodeFloor

enum NodeFloorError:
  case NotPositive(raw: Int)

  def message: String = this match
    case NotPositive(raw) => s"$raw is not a Node major"
end NodeFloorError

/** Tools a generated test job installs, and the Node binary a Js launch prefers.
  *
  * `node` is the major the runner installs. `nodePath`, when set, is the absolute `node` binary used after
  * `HISTOS_NODE` and before `node.local` and `PATH`. Empty installs nothing and names no binary.
  */
final case class RunnerTools(
    node: Option[NodeFloor],
    llvm: Option[ToolRevision],
    playwright: Option[ToolRevision],
    nodePath: Option[NodePath] = None,
)

object RunnerTools:
  val empty: RunnerTools = RunnerTools(None, None, None, None)

private object NodeFloorMacro:
  def literal(raw: Expr[Int])(using Quotes): Expr[NodeFloor] =
    import quotes.reflect.report
    raw.value match
      case None =>
        report.errorAndAbort("NodeFloor(...) takes a literal. Use NodeFloor.from at runtime.", raw)
      case Some(major) =>
        NodeFloor.from(major) match
          case Left(error) => report.errorAndAbort(error.message, raw)
          case Right(_)    => '{ NodeFloor.trusted(${ Expr(major) }) }
