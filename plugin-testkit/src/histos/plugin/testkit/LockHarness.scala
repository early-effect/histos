package histos.plugin.testkit

import histos.Resolution
import histos.lock.{
  DeclarationDrift,
  ExecutionKind,
  LockContext,
  LockDiff,
  LockPhase,
  LockRevisionState,
  LockStage,
  NecessaryScope,
}
import histos.plugin.{
  ClientId,
  CommandIdentity,
  CommandToken,
  Interaction,
  InvocationContext,
  InvocationId,
  InvocationPluginEvent,
  OfflineSelection,
  PluginDetail,
  PluginEventError,
  PluginEvents,
  ResolutionIntent,
}

import zio.{IO, ZIO}

/** Locked graph discovery with no revision. The command stays put when the execution kind changes. */
object LockHarness:
  def missingLock(execution: ExecutionKind): LockContext =
    LockContext(
      phase = LockPhase.Graph,
      stage = LockStage.Discovery,
      execution = execution,
      intent = ResolutionIntent.Select(Resolution.Lock),
      revision = LockRevisionState.Absent,
      drift = DeclarationDrift.Same,
      scope = NecessaryScope(PluginDetail("missing lock"), LockDiff(Nil)),
      proposal = None,
      invocation = InvocationContext(
        id = InvocationId("0000000000000000000000000000000000000000000000000000000000000000"),
        client = ClientId("lock-fixture"),
        command = CommandIdentity.Other(CommandToken("fixture")),
        execution = execution,
        interaction = Interaction.Unattended,
        offline = OfflineSelection.Online,
      ),
      events = Quiet,
    )

  private object Quiet extends PluginEvents.Invocation:
    def emit(event: InvocationPluginEvent): IO[PluginEventError, Unit] =
      ZIO.unit
end LockHarness
