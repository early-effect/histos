package histos.bsp

import histos.cli.{ClientSurface, ConsentDecision, ConsentRoute}
import histos.lock.LockOffer
import histos.server.Call

/** BSP consent is a diagnosis until the client negotiates a structured reply. */
object BspConsent:
  val updateHint: String = "histos resolve --update"

  def unnegotiated(offer: LockOffer, call: Call): ConsentDecision =
    ConsentRoute.decide(ClientSurface.Bsp(false), offer, call)
end BspConsent
