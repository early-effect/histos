package histos

import zio.test.*

object ClangMajorSpec extends histos.SuiteClock:
  def spec = suite("clang major")(
    test("a positive literal is that major, and zero or negative is NotPositive"):
      assertTrue(
        ClangMajor(21).value == 21,
        ClangMajor.from(21).map(_.value) == Right(21),
        ClangMajor.from(0) == Left(ClangError.NotPositive(0)),
        ClangMajor.from(-1) == Left(ClangError.NotPositive(-1)),
      )
  )
