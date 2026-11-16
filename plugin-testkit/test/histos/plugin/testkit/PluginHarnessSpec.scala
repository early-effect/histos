package histos.plugin.testkit

import histos.Resolution
import histos.lock.*
import histos.plugin.*

import zio.{Promise, Ref, Scope, ZIO, ZLayer}
import zio.test.*

object PluginHarnessSpec extends ZIOSpecDefault:
  private val policy: LockStrategy = new LockStrategy:
    def decide(context: LockContext): zio.IO[LockStrategyError, LockDecision] =
      ZIO.succeed:
        context.execution match
          case ExecutionKind.Local => LockDecision.OfferRepair
          case ExecutionKind.Ci    => LockDecision.Frozen

  def spec = suite("plugin harness")(
    test("missingLock is locked graph discovery with no revision") {
      val local = LockHarness.missingLock(ExecutionKind.Local)
      val ci    = LockHarness.missingLock(ExecutionKind.Ci)
      for
        localDecision <- policy.decide(local)
        ciDecision    <- policy.decide(ci)
      yield assertTrue(
        local.phase == LockPhase.Graph,
        local.stage == LockStage.Discovery,
        local.revision == LockRevisionState.Absent,
        local.intent == ResolutionIntent.Select(Resolution.Lock),
        local.invocation.command == ci.invocation.command,
        local.execution == ExecutionKind.Local,
        ci.execution == ExecutionKind.Ci,
        localDecision == LockDecision.OfferRepair,
        ciDecision == LockDecision.Frozen,
      )
    },
    test("two callers share one service and see their own command and events") {
      ZIO.scoped:
        for
          arrived <- Ref.make(0)
          release <- Promise.make[Nothing, Unit]
          count   <- Ref.make(0)
          first = ZLayer.fromZIO:
            count.update(_ + 1) *> ZIO.succeed(Shared(arrived, release))
          second = ZLayer.fromZIO:
            count.update(_ + 1) *> ZIO.succeed(Shared(arrived, release))
          built  <- PluginHarness.activate(BuildServices.default.add[Shared](first).tasks[Shared](second))
          plugin <- built.tasks match
            case one :: Nil => ZIO.succeed(one)
            case _          => ZIO.fail(PluginRunError.Failed(PluginDetail("expected one task")))
          local         <- PluginHarness.caller(localCaller)
          ci            <- PluginHarness.caller(ciCaller)
          localFiber    <- PluginHarness.run(plugin, local).fork
          ciFiber       <- PluginHarness.run(plugin, ci).fork
          localOut      <- localFiber.join
          ciOut         <- ciFiber.join
          localEvents   <- local.recorded
          ciEvents      <- ci.recorded
          constructions <- count.get
        yield assertTrue(
          constructions == 1,
          localOut.text == "local:alpha:local",
          ciOut.text == "ci:beta:ci",
          localEvents == List(InvocationPluginEvent.Message(PluginDetail("local:alpha:local"))),
          ciEvents == List(InvocationPluginEvent.Message(PluginDetail("ci:beta:ci"))),
        )
    },
  )

  private val localCaller: InvocationContext =
    InvocationContext(
      id = InvocationId.derive("local"),
      client = ClientId("local"),
      command = CommandIdentity.Other(CommandToken("alpha")),
      execution = ExecutionKind.Local,
      interaction = Interaction.Offers,
      offline = OfflineSelection.Online,
    )

  private val ciCaller: InvocationContext =
    InvocationContext(
      id = InvocationId.derive("ci"),
      client = ClientId("ci"),
      command = CommandIdentity.Other(CommandToken("beta")),
      execution = ExecutionKind.Ci,
      interaction = Interaction.Unattended,
      offline = OfflineSelection.Offline,
    )
end PluginHarnessSpec

final class Shared(arrived: Ref[Int], release: Promise[Nothing, Unit]) extends HistosPlugin:
  val id = PluginId("shared")

  def contribute: Contribution = Contribution(Nil, Nil, Nil)

  def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
    val name = request.invocation.command match
      case CommandIdentity.Other(token) => token.value
      case _                            => "other"
    val kind = request.invocation.execution match
      case ExecutionKind.Local => "local"
      case ExecutionKind.Ci    => "ci"
    val rendered = s"${request.invocation.client.value}:$name:$kind"
    for
      n <- arrived.updateAndGet(_ + 1)
      _ <- ZIO.when(n == 2)(release.succeed(()))
      _ <- release.await
      _ <- PluginDetail.from(rendered) match
        case Right(detail) =>
          request.events
            .emit(InvocationPluginEvent.Message(detail))
            .mapError(_ => PluginRunError.Failed(detail))
        case Left(_) => ZIO.fail(PluginRunError.Failed(PluginDetail("unreadable")))
    yield PluginOutput(rendered, Nil)
end Shared
