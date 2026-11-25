package histos.schedule

import histos.ParallelismMax
import histos.suite.HistosSpec

import zio.test.*

object ParallelismSpec extends HistosSpec:
  private val processors = java.lang.Runtime.getRuntime.availableProcessors.max(1)

  def spec = suite("parallelism")(
    test("omission is one permit per core") {
      val open = Parallelism.of(None)
      assertTrue(
        open.cpu == processors,
        open.io == processors,
        open.process == processors,
        open == Parallelism.default,
      )
    },
    test("a ceiling lowers every class and does not raise it past the core count") {
      val low  = Parallelism.of(Some(ParallelismMax.trusted(1)))
      val high = Parallelism.of(Some(ParallelismMax.trusted(processors + 8)))
      assertTrue(
        low.cpu == 1,
        low.io == 1,
        low.process == 1,
        high.cpu == processors,
        high.io == processors,
        high.process == processors,
      )
    },
  )
end ParallelismSpec
