package histos

import _root_.scala.compiletime.testing.typeCheckErrors

import zio.test.*

object LiteralSpec extends ZIOSpecDefault:
  private val literals = suite("literals")(
    test("a full Scala version is a pin"):
      assertTrue(
        scala"3.9.0".value == "3.9.0",
        scala"2.13.16" == ScalaVersion("2.13.16"),
        ScalaVersion.from("3.9.0-RC1") == Right(scala"3.9.0-RC1"),
        ScalaVersion.from("3") == Left(ScalaVersionError.NotFull("3")),
        ScalaVersion.from("2.13") == Left(ScalaVersionError.NotFull("2.13")),
        ScalaVersion.from("") == Left(ScalaVersionError.Empty),
      ),
    test("a series is rejected when the build file compiles"):
      val errors = typeCheckErrors("""scala"2.13"""")
      assertTrue:
        errors match
          case e :: _ => e.message.contains("2.13")
          case Nil    => false
    ,
    test("a dependency literal keeps its cross kind"):
      val binary = Dep.from("dev.zio::zio:2.1.26")
      assertTrue(
        dep"com.example:lib:1.2.3" == Dep("com.example:lib:1.2.3"),
        binary == Right(dep"dev.zio::zio:2.1.26"),
        dep"org.scala-lang:::scala3-library:3.9.0".kind == DepKind.Full,
        binary.exists(_.kind == DepKind.Binary),
        Dep.from("nope") == Left(DepError.BadShape("nope")),
        Dep.from("") == Left(DepError.Empty),
        Dep.from("a::b::c") == Left(DepError.BadShape("a::b::c")),
      ),
    test("a broken dependency literal does not compile"):
      val errors = typeCheckErrors("""dep"nope"""")
      assertTrue:
        errors match
          case e :: _ => e.message.contains("nope")
          case Nil    => false
    ,
  )

  def spec = literals
