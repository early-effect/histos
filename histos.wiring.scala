import histos.plugin.*

object HistosWiring:
  def layer = wire[ci.LinkClient.type]
