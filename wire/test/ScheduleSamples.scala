package histos.schedule

import histos.WireSamples.hex

object ScheduleSamples:
  val compile: ActionId  = ActionId.trusted("compile")
  val link: ActionId     = ActionId.trusted("link")
  val sources: InputName = InputName.trusted("sources")
  val digest: Digest     = Digest.trusted(hex)
