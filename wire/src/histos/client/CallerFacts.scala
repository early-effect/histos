package histos.client

import histos.lock.{CallIdentity, ExecutionKind, InvocationEnvelope, LockMode}
import histos.plugin.{ClientId, Interaction, InvocationContext, InvocationId, OfflineSelection}
import histos.server.Call

import zio.{UIO, ZIO}

/** Caller facts read where the command starts. The daemon, the coordinator, and a plugin task consume the envelope. */
object CallerFacts:
  val cli: ClientId      = ClientId("cli")
  val native: ClientId   = ClientId("native")
  val mcp: ClientId      = ClientId("mcp")
  val bsp: ClientId      = ClientId("bsp")
  val detached: ClientId = ClientId("server-client")

  /** Truthy `CI`, or `GITHUB_ACTIONS` equal to true. The same rule the upgrade notice uses, read on the client. */
  def execution(env: Map[String, String]): ExecutionKind =
    if truthy(env.get("CI")) || env.get("GITHUB_ACTIONS").exists(_.trim.equalsIgnoreCase("true")) then ExecutionKind.Ci
    else ExecutionKind.Local

  def interaction(prompts: Boolean): Interaction =
    if prompts then Interaction.Offers else Interaction.Unattended

  def envelope(
      client: ClientId,
      call: Call,
      execution: ExecutionKind,
      interaction: Interaction,
      lock: Option[LockMode],
  ): UIO[InvocationEnvelope] =
    fresh.map: id =>
      InvocationEnvelope(
        InvocationContext(
          id,
          client,
          CallIdentity.identity(call),
          execution,
          interaction,
          OfflineSelection.Online,
        ),
        call,
        lock,
      )

  /** A reboot or a test harness. No environment and no terminal. */
  def detached(call: Call): UIO[InvocationEnvelope] =
    envelope(detached, call, ExecutionKind.Local, Interaction.Unattended, None)

  def cli(call: Call, lock: Option[LockMode], prompts: Boolean): UIO[InvocationEnvelope] =
    observed(cli, call, prompts, lock)

  def mcp(call: Call): UIO[InvocationEnvelope] =
    observed(mcp, call, prompts = false, None)

  def bsp(call: Call): UIO[InvocationEnvelope] =
    observed(bsp, call, prompts = false, None)

  def native(call: Call, env: Map[String, String], prompts: Boolean): UIO[InvocationEnvelope] =
    envelope(native, call, execution(env), interaction(prompts), None)

  private def observed(
      client: ClientId,
      call: Call,
      prompts: Boolean,
      lock: Option[LockMode],
  ): UIO[InvocationEnvelope] =
    zio.System.envs
      .orElseSucceed(Map.empty)
      .flatMap(env => envelope(client, call, execution(env), interaction(prompts), lock))

  /** A fresh id for this process. `UUID.randomUUID` needs `SecureRandom`, which the native client cannot link. */
  private val ticks = new java.util.concurrent.atomic.AtomicLong(System.nanoTime())

  private def fresh: UIO[InvocationId] =
    ZIO.succeed(InvocationId.derive(ticks.incrementAndGet().toString + ":" + System.nanoTime().toString))

  private def truthy(raw: Option[String]): Boolean =
    raw.map(_.trim.toLowerCase) match
      case Some("1" | "true" | "yes") => true
      case _                          => false
end CallerFacts
