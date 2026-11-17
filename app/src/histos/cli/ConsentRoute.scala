package histos.cli

import histos.doctor.Noted
import histos.lock.{CallIdentity, LockMode, LockOffer, LockReply, LockTransactionError}
import histos.server.Call

/** Where consent is answered. A prompt exists only on a CLI whose input and output are both a terminal. */
enum ClientSurface:
  case Cli(prompts: Boolean)
  case Mcp(negotiated: Boolean)
  case Bsp(negotiated: Boolean)

enum ConsentDecision:
  /** The invoking terminal may ask. The daemon does not. */
  case Prompt

  /** The client already negotiated a structured reply. */
  case Structured

  /** No negotiation. A diagnosis, never a prompt. */
  case Diagnosis(noted: Noted)

  /** Redirected CLI. The summary and both rerun commands, and [[histos.lock.LockTransactionError.ConsentRequired]]. */
  case Required(
      error: LockTransactionError.ConsentRequired,
      summary: List[String],
      update: String,
      local: String,
  )
end ConsentDecision

object ConsentRoute:
  def decide(surface: ClientSurface, offer: LockOffer, call: Call): ConsentDecision =
    surface match
      case ClientSurface.Cli(true)  => ConsentDecision.Prompt
      case ClientSurface.Cli(false) => required(offer, call)
      case ClientSurface.Mcp(true)  => ConsentDecision.Structured
      case ClientSurface.Bsp(true)  => ConsentDecision.Structured
      case ClientSurface.Mcp(false) => diagnosis(offer, call)
      case ClientSurface.Bsp(false) => diagnosis(offer, call)

  def required(offer: LockOffer, call: Call): ConsentDecision.Required =
    ConsentDecision.Required(
      LockTransactionError.ConsentRequired(offer.proposal),
      CallIdentity.summary(offer),
      CallIdentity.rerun(call, LockMode.Update),
      CallIdentity.rerun(call, LockMode.Local),
    )

  def diagnosis(offer: LockOffer, call: Call): ConsentDecision.Diagnosis =
    val lines        = CallIdentity.summary(offer)
    val update       = CallIdentity.rerun(call, LockMode.Update)
    val local        = CallIdentity.rerun(call, LockMode.Local)
    val (head, rest) = lines match
      case Nil           => ("Lock repair required", "")
      case first :: tail => (first, tail.mkString("\n"))
    val explain =
      s"$rest\nThis client has not negotiated consent.\n$update\n$local"
    ConsentDecision.Diagnosis(
      Noted.error(
        "lock.consent",
        head,
        explain,
        Noted.command(update),
        Noted.command(local),
      )
    )

  /** A tty answer. `y`, `Y`, and `yes` accept. Anything else declines. This is not [[histos.server.RunInput]]. */
  def reply(offer: LockOffer, line: String): LockReply =
    line.trim match
      case "y" | "Y" | "yes" => LockReply.Accept(offer.proposal, offer.base)
      case _                 => LockReply.Decline(offer.proposal)
end ConsentRoute
