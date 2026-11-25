package histos.schedule

import zio.{ULayer, ZLayer}

/** How many steps of each resource class may run at once. */
final case class Parallelism(cpu: Int, io: Int, process: Int)

object Parallelism:
  /** One permit per core for each class, lowered when the build names a ceiling. The classes do not share a pool, so a
    * compile does not wait on a download.
    */
  def of(max: Option[histos.ParallelismMax]): Parallelism =
    val processors = java.lang.Runtime.getRuntime.availableProcessors.max(1)
    val width      = max.fold(processors)(cap => processors.min(cap.value))
    Parallelism(cpu = width, io = width, process = width)

  def default: Parallelism = of(None)

  val live: ULayer[Parallelism] = ZLayer.succeed(default)
end Parallelism
