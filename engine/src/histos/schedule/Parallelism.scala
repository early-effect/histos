package histos.schedule

import zio.{ULayer, ZLayer}

/** How many steps of each resource class may run at once. */
final case class Parallelism(cpu: Int, io: Int, process: Int)

object Parallelism:
  def default: Parallelism =
    val processors = java.lang.Runtime.getRuntime.availableProcessors
    Parallelism(cpu = processors.max(1), io = 16, process = 8)

  val live: ULayer[Parallelism] = ZLayer.succeed(default)
end Parallelism
