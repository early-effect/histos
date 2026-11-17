package histos.mcp

import histos.cli.{ClientSurface, ConsentDecision, ConsentRoute}
import histos.lock.LockOffer
import histos.server.Call

/** MCP consent is a diagnosis until the client negotiates a structured reply. The pipe is not a terminal. */
object McpConsent:
  def unnegotiated(offer: LockOffer, call: Call): ConsentDecision =
    ConsentRoute.decide(ClientSurface.Mcp(false), offer, call)
