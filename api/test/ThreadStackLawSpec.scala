package histos

import _root_.scala.compiletime.testing.typeCheckErrors

import zio.test.*

object ThreadStackLawSpec extends ZIOSpecDefault:
  def spec = suite("thread stack laws")(
    test("a thread stack is a byte count, or a count of k or m, from 64k to 1024m"):
      assertTrue(
        List("65536", "64k", "8m", "16m", "1024m", "1048576k").map(raw => ThreadStack.from(raw).map(_.value)) ==
          List("65536", "64k", "8m", "16m", "1024m", "1048576k").map(Right(_)),
        ThreadStack.from("") == Left(ThreadStackError.Empty),
        ThreadStack.from("8M") == Left(ThreadStackError.Malformed("8M")),
        ThreadStack.from("8 m") == Left(ThreadStackError.Malformed("8 m")),
        ThreadStack.from("8g") == Left(ThreadStackError.Malformed("8g")),
        ThreadStack.from("-8m") == Left(ThreadStackError.Malformed("-8m")),
        ThreadStack.from("m") == Left(ThreadStackError.Malformed("m")),
        ThreadStack.from("65535") == Left(ThreadStackError.OutOfRange("65535")),
        ThreadStack.from("63k") == Left(ThreadStackError.OutOfRange("63k")),
        ThreadStack.from("1025m") == Left(ThreadStackError.OutOfRange("1025m")),
        ThreadStack.from("99999999999999999999m") == Left(ThreadStackError.OutOfRange("99999999999999999999m")),
        ThreadStack.from("8m") == Right(ThreadStack.default),
      )
    ,
    test("a literal compiles only when the parser accepts it, and names the parser's refusal"):
      assertTrue(
        ThreadStack("16m").value == "16m",
        typeCheckErrors("""histos.ThreadStack("8g")""").map(_.message) ==
          List("'8g' is not a thread stack: a byte count, or a count of k or m, such as 8m"),
        typeCheckErrors("""histos.ThreadStack("1k")""").map(_.message) ==
          List("'1k' is outside the 64k to 1024m a thread stack can be"),
        typeCheckErrors("""histos.ThreadStack("")""").map(_.message) == List("a thread stack is not empty"),
      ),
  )
