package histos.suite

import zio.*
import zio.internal.stacktracer.SourceLocation
import zio.test.*

trait HistosSpec extends ZIOSpecDefault:
  override def aspects: Chunk[TestAspectAtLeastR[Environment & TestEnvironment]] =
    super.aspects :+ TestAspect.timed

  override def suite[In](label: String)(specs: In*)(implicit
      suiteConstructor: SuiteConstructor[In],
      sourceLocation: SourceLocation,
      trace: Trace,
  ): Spec[suiteConstructor.OutEnvironment, suiteConstructor.OutError] =
    zio.test.suite(label)(specs*) @@ SuiteWall(label)
end HistosSpec
