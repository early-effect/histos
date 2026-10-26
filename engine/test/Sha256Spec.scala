package histos.jdk

import zio.test.*

object Sha256Spec extends ZIOSpecDefault:
  private val hex = "ab" * 32

  def spec = suite("sha256")(
    test("a literal is the trimmed lowercase digest"):
      val literal = Sha256("abababababababababababababababababababababababababababababababab")
      val spaced  = Sha256(" ABABABABABABABABABABABABABABABABABABABABABABABABABABABABABABABAB ")
      val parsed  = Sha256.from(" ABABABABABABABABABABABABABABABABABABABABABABABABABABABABABABABAB ")
      assertTrue(
        parsed == Right(spaced),
        spaced == literal,
        literal.value == hex,
        spaced.value == hex,
        Sha256.from(hex) == Right(literal),
      )
    ,
    test("a short or empty digest is rejected"):
      assertTrue(
        Sha256.from("abc") == Left(Sha256Error.Bad("abc")),
        Sha256.from("") == Left(Sha256Error.Bad("")),
      ),
  )
end Sha256Spec
