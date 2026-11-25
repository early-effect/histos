package histos.schedule

import zio.{ULayer, ZLayer}

/** How many steps of each resource class may run at once. */
final case class Parallelism(cpu: Int, io: Int, process: Int)

object Parallelism:
  /** One permit per core for each class. The classes do not share a pool, so a compile does not wait on a download.
    */
  def default: Parallelism =
    val processors = java.lang.Runtime.getRuntime.availableProcessors.max(1)
    Parallelism(cpu = processors, io = processors, process = processors)

  val live: ULayer[Parallelism] = ZLayer.succeed(default)
end Parallelism
