package histos.doctor

import zio.test.*

object SuggestSpec extends ZIOSpecDefault:
  def spec = suite("suggestions")(
    test("a command typo within two edits is named, and a farther one is not"):
      val names = List("compile", "test", "testOnly", "graph", "help")
      assertTrue(
        Suggest.closest("compil", names).contains("compile"),
        Suggest.closest("tst", names).contains("test"),
        Suggest.near("zzzz", names).isEmpty,
        Suggest.near("test", names).isEmpty,
      )
    ,
    test("a colon coordinate suggests the binary form"):
      assertTrue(
        Suggest.binary("dev.zio", "zio", "2.1.27").exists(_.contains("dev.zio::zio:2.1.27")),
        Suggest.binary("dev.zio", "zio_3", "2.1.27").isEmpty,
      )
    ,
  )
end SuggestSpec
