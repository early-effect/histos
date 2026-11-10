package histos.load

/** Calls `HistosWiring.layer` and reads [[histos.plugin.Plugins]]. A requirement that is still open fails this compile.
  */
private[load] object WiringSource:
  val label: String = "WiringBuilt.scala"

  val generated: String =
    """object WiringBuilt extends histos.plugin.WiringBuilt:
      |  type Failure = Any
      |  def build: zio.ZIO[zio.Scope, Any, histos.plugin.Plugins] =
      |    zio.ZIO.scoped:
      |      HistosWiring.layer.build.map(env => env.get[histos.plugin.Plugins])
      |""".stripMargin
end WiringSource
