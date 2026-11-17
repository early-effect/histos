package histos.plugin

import histos.lock.ExecutionKind

import zio.{IO, ZLayer}

/** Facts the loading strategy may read. It cannot waive an integrity or boundary check. */
final case class PluginLoadContext(
    publication: PublicationFact,
    binding: BindingMatch,
    recipe: RecipeFact,
    closure: ClosureFact,
    offline: OfflineSelection,
    execution: ExecutionKind,
    invocation: InvocationContext,
    events: PluginEvents.Invocation,
)

enum PublicationFact:
  case Rebuildable
  case BinaryOnly
  case Missing

enum BindingMatch:
  case Exact
  case Drift

enum RecipeFact:
  case Present
  case Missing

enum ClosureFact:
  case Valid
  case Invalid

enum PluginLoadPlan:
  case UseExactBinary
  case Rebuild
  case Reject(refusal: PluginRefusal)

enum PluginRefusal:
  case NotExact
  case MissingRecipe

  def message: String = this match
    case NotExact      => "plugin binding is not exact"
    case MissingRecipe => "plugin recipe is missing"
end PluginRefusal

enum PluginStrategyError:
  case Configuration(detail: PluginDetail)
  case Policy(detail: PluginDetail)

  def message: String = this match
    case Configuration(detail) => detail.value
    case Policy(detail)        => detail.value
end PluginStrategyError

trait PluginLoadingStrategy:
  def plan(context: PluginLoadContext): IO[PluginStrategyError, PluginLoadPlan]

object PluginLoading:
  val exactOrRebuild: ZLayer[Any, Nothing, PluginLoadingStrategy] =
    ZLayer.succeed(BuiltIn(LoadingMode.ExactOrRebuild))

  val rebuildAlways: ZLayer[Any, Nothing, PluginLoadingStrategy] =
    ZLayer.succeed(BuiltIn(LoadingMode.RebuildAlways))

  val exactBinaryOnly: ZLayer[Any, Nothing, PluginLoadingStrategy] =
    ZLayer.succeed(BuiltIn(LoadingMode.ExactBinaryOnly))

  private enum LoadingMode:
    case ExactOrRebuild
    case RebuildAlways
    case ExactBinaryOnly

  private final class BuiltIn(mode: LoadingMode) extends PluginLoadingStrategy:
    def plan(context: PluginLoadContext): IO[PluginStrategyError, PluginLoadPlan] =
      zio.ZIO.succeed:
        mode match
          case LoadingMode.RebuildAlways   => PluginLoadPlan.Rebuild
          case LoadingMode.ExactBinaryOnly =>
            context.binding match
              case BindingMatch.Exact => PluginLoadPlan.UseExactBinary
              case BindingMatch.Drift => PluginLoadPlan.Reject(PluginRefusal.NotExact)
          case LoadingMode.ExactOrRebuild =>
            context.binding match
              case BindingMatch.Exact => PluginLoadPlan.UseExactBinary
              case BindingMatch.Drift =>
                context.publication match
                  case PublicationFact.Rebuildable => PluginLoadPlan.Rebuild
                  case PublicationFact.BinaryOnly  => PluginLoadPlan.Reject(PluginRefusal.NotExact)
                  case PublicationFact.Missing     => PluginLoadPlan.Reject(PluginRefusal.MissingRecipe)
end PluginLoading
