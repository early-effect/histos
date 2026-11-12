package histos.supply

import zio.{UIO, ULayer, ZIO, ZLayer}

/** Whether a person at a terminal can answer doctor's prompt before it changes a GitHub setting. */
trait Terminal:
  def attached: UIO[Boolean]

object Terminal:
  /** The JVM's console, which is absent when stdin or stdout is not a terminal. */
  val live: ULayer[Terminal] = ZLayer.succeed(Console)

  /** An MCP call's stdin and stdout are the protocol, so no person can answer there. */
  val detached: ULayer[Terminal] = ZLayer.succeed(Detached)

  private object Console extends Terminal:
    def attached: UIO[Boolean] = ZIO.succeed(Option(java.lang.System.console()).isDefined)

  private object Detached extends Terminal:
    def attached: UIO[Boolean] = ZIO.succeed(false)
end Terminal
