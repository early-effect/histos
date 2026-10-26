package histos

import zio.test.*

object ProviderLibrarySpec extends ZIOSpecDefault:
  def spec = suite("provider library")(
    test("a literal is the library it names, and a runtime string keeps the typed error"):
      val named  = ProviderLibrary("dev.zio", "zio-test-sbt")
      val parsed = ProviderLibrary.from("dev.zio", "zio-test-sbt")
      assertTrue(
        parsed == Right(named),
        named.organization == "dev.zio",
        named.name == "zio-test-sbt",
        parsed.exists(library => library.organization == "dev.zio" && library.name == "zio-test-sbt"),
        ProviderLibrary.from("", "zio") == Left(ProviderLibraryError.Empty),
        ProviderLibrary.from("dev.zio", "zio test") == Left(ProviderLibraryError.Bad("dev.zio", "zio test")),
      )
  )
