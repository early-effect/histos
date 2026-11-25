package lib
import zio.*
import zio.test.*
object UpstreamSpec extends ZIOSpecDefault:
  def spec = test("must not execute") {
    ZIO
      .attempt(
        java.nio.file.Files
          .writeString(java.nio.file.Path.of(sys.env.getOrElse("HISTOS_REPO", "."), "upstream-counter"), "ran")
      )
      .as(assertTrue(false))
  }
