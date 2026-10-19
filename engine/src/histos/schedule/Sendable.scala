package histos.schedule

import histos.plugin.{Need, Seal}
import histos.task.TaskKind

/** Who runs the action. A closure is the build file. A plugin is sendable only when it says so. */
enum Runner:
  case Compile
  case Inspect
  case Versions
  case Test
  case Publish
  case Closure
  case Plugin(seal: Seal)

enum Hold:
  case Inspect
  case Versions
  case Test
  case Publish
  case Closure
  case Plugin
  case Needs(needs: Set[Need])

enum Verdict:
  case Send
  case Stay(reason: Hold)

object Sendable:
  /** Core compile may leave. Everything else stays, and a need vetoes a compile or a hermetic plugin. */
  def decide(runner: Runner, needs: Set[Need]): Verdict =
    if needs.nonEmpty then Verdict.Stay(Hold.Needs(needs))
    else
      runner match
        case Runner.Compile               => Verdict.Send
        case Runner.Plugin(Seal.Hermetic) => Verdict.Send
        case Runner.Plugin(Seal.Open)     => Verdict.Stay(Hold.Plugin)
        case Runner.Closure               => Verdict.Stay(Hold.Closure)
        case Runner.Test                  => Verdict.Stay(Hold.Test)
        case Runner.Publish               => Verdict.Stay(Hold.Publish)
        case Runner.Inspect               => Verdict.Stay(Hold.Inspect)
        case Runner.Versions              => Verdict.Stay(Hold.Versions)

  def ofAction(action: Action): Verdict =
    action.policy match
      case Policy.Core =>
        val runner = action.kind match
          case TaskKind.Compile  => Runner.Compile
          case TaskKind.Inspect  => Runner.Inspect
          case TaskKind.Versions => Runner.Versions
          case TaskKind.Plugin   => Runner.Plugin(Seal.Open)
        decide(runner, Set.empty)
      case Policy.Plugin(seal, needs, _, _) =>
        decide(Runner.Plugin(seal), needs)
