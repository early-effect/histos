package histos.plugin

import zio.{Scope, ZIO, ZLayer}

/** Which scope one acquire dies with. The environment `Scope` of `run` is always [[ResourceLife.Invocation]].
  */
enum ResourceLife:
  /** Released when `run` returns, including on interrupt. */
  case Invocation

  /** Released when this generation of plugin classes is retired, and when the process shuts down. */
  case Generation
end ResourceLife

/** The scopes a plugin task can acquire into. A test supplies its own. The loader builds the live one.
  */
trait PluginScopes:
  def apply(life: ResourceLife): Scope

object PluginScopes:
  def live(generation: Scope): ZLayer[Scope, Nothing, PluginScopes] =
    ZLayer.fromZIO(ZIO.serviceWith[Scope](invocation => Paired(invocation, generation): PluginScopes))

  private final class Paired(invocation: Scope, generation: Scope) extends PluginScopes:
    def apply(life: ResourceLife): Scope = life match
      case ResourceLife.Invocation => invocation
      case ResourceLife.Generation => generation
end PluginScopes
