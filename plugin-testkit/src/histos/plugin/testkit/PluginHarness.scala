package histos.plugin.testkit

import histos.lock.{LockContext, LockDecision, LockStrategy, LockStrategyError}
import histos.plugin.{
  ActivatedBuildServices,
  ApiDigest,
  BuildContext,
  ContentHash,
  GenerationId,
  GenerationPluginEvent,
  HistosPlugin,
  InvocationContext,
  InvocationPluginEvent,
  Plan,
  PluginArgs,
  PluginLoading,
  PluginEventError,
  PluginEvents,
  PluginOutput,
  PluginRunError,
  PluginScopes,
  PluginTaskId,
  ResourceLife,
  RunRequest,
  ServiceActivationError,
  ServiceSetupError,
  TaskName,
}

import java.nio.file.Path

import zio.{IO, Queue, Scope, UIO, ZEnvironment, ZIO, ZLayer}

/** One generation, then one caller at a time. Construction does not see the caller. */
object PluginHarness:
  def caller(context: InvocationContext): UIO[Caller] =
    Queue.unbounded[InvocationPluginEvent].map(Caller(context, _))

  def activate[S, E <: ServiceSetupError](
      plan: Plan[S, E]
  ): ZIO[Scope, ServiceActivationError | E, ActivatedBuildServices] =
    ZIO.serviceWithZIO[Scope]: scope =>
      plan.activate.provide(
        ZLayer.succeed[BuildContext](generation),
        ZLayer.succeed[PluginEvents.Generation](GenerationSink),
        ZLayer.succeed[LockStrategy](CoreLocks),
        PluginLoading.exactOrRebuild,
        ZLayer.succeed(scope),
      )

  def run(plugin: HistosPlugin, caller: Caller): ZIO[Scope, PluginRunError, PluginOutput] =
    ZIO.serviceWithZIO[Scope]: generationScope =>
      ZIO.scoped:
        ZIO.serviceWithZIO[Scope]: invocation =>
          val scopes  = Scopes(invocation, generationScope)
          val request = RunRequest(
            id = PluginTaskId(plugin.id, TaskName("run")),
            repo = Path.of("."),
            modules = Nil,
            args = PluginArgs.empty,
            inputs = Nil,
            invocation = caller.context,
            events = caller.events,
          )
          plugin.run(request).provideEnvironment(ZEnvironment[Scope](invocation).add[PluginScopes](scopes))

  final class Caller(val context: InvocationContext, queue: Queue[InvocationPluginEvent]):
    val events: PluginEvents.Invocation = new PluginEvents.Invocation:
      def emit(event: InvocationPluginEvent): IO[PluginEventError, Unit] =
        queue.offer(event).unit

    def recorded: UIO[List[InvocationPluginEvent]] =
      queue.takeAll.map(_.toList)
  end Caller

  private inline val digest = "0000000000000000000000000000000000000000000000000000000000000000"

  private val generation: BuildContext = new BuildContext:
    def generation: GenerationId  = GenerationId(digest)
    def repository: String        = "fixture"
    def declarations: ContentHash = ContentHash(digest)
    def host: ApiDigest           = ApiDigest(digest)

  private object GenerationSink extends PluginEvents.Generation:
    def emit(event: GenerationPluginEvent): IO[PluginEventError, Unit] =
      ZIO.unit

  private final class Scopes(invocation: Scope, generation: Scope) extends PluginScopes:
    def apply(life: ResourceLife): Scope = life match
      case ResourceLife.Invocation => invocation
      case ResourceLife.Generation => generation

  private object CoreLocks extends LockStrategy:
    def decide(context: LockContext): IO[LockStrategyError, LockDecision] =
      ZIO.succeed(LockDecision.UsePinned)
end PluginHarness
