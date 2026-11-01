package histos.schedule

import zio.{ULayer, ZLayer}

/** How many steps of each resource class may run at once. */
final case class Parallelism(cpu: Int, io: Int, process: Int)

object Parallelism:
  /** Compile steps take a CPU permit. Four compilers per core, and at least four, so independent cells share the
    * machine.
    */
  def default: Parallelism =
    val processors = java.lang.Runtime.getRuntime.availableProcessors.max(1)
    Parallelism(cpu = (processors * 4).max(4), io = 16, process = 8)

  val live: ULayer[Parallelism] = ZLayer.succeed(default)
end Parallelism
