package histos.suite

import zio.*
import zio.test.*

trait IntegrationSpec extends HistosSpec:
  override def aspects: Chunk[TestAspectAtLeastR[Environment & TestEnvironment]] =
    super.aspects :+ TestAspect.tag(IntegrationSpec.Tag)

object IntegrationSpec:
  val Tag: String = "integration"
