package histos.workflow

import histos.version.CommitSha

/** The rendering ship the laws hand to a workflow render. */
object RenderingLaw:
  val ship: RenderingShip = RenderingShip(CommitSha.trusted("0123456789abcdef0123456789abcdef01234567"))
