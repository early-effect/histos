package histos

import _root_.scala.compiletime.testing.typeCheckErrors

import histos.version.{ShipNumber, ShipNumberError}

import zio.test.*

object LiteralSpec extends ZIOSpecDefault:
  private val literals = suite("literals")(
    test("a maven repository is a file or https url"):
      val errors = typeCheckErrors("""maven"http://example.com"""")
      assertTrue(
        maven"file:///repo" == Resolver.Maven(RepositoryUrl("file:///repo")),
        maven"https://repo.example/maven" == Resolver.Maven(RepositoryUrl("https://repo.example/maven")),
        RepositoryUrl.from("http://example.com") == Left(RepositoryUrlError.Bad("http://example.com")),
        errors match
          case e :: _ => e.message.contains("http://example.com")
          case Nil    => false,
      )
    ,
    test("an organization is a dotted group"):
      val errors = typeCheckErrors("""organization"com..example"""")
      assertTrue(
        organization"com.example".value == "com.example",
        organization"com.example" == Organization("com.example"),
        Organization.from("com..example") == Left(OrganizationError.Bad("com..example")),
        Organization.from("com/example") == Left(OrganizationError.Bad("com/example")),
        errors match
          case e :: _ => e.message.contains("com..example")
          case Nil    => false,
      )
    ,
    test("a full Scala version is a pin"):
      assertTrue(
        scala"3.9.0".value == "3.9.0",
        scala"2.13.16" == ScalaVersion("2.13.16"),
        ScalaVersion.from("3.9.0-RC1") == Right(scala"3.9.0-RC1"),
        ScalaVersion.from("3") == Left(ScalaVersionError.NotFull("3")),
        ScalaVersion.from("2.13") == Left(ScalaVersionError.NotFull("2.13")),
        ScalaVersion.from("") == Left(ScalaVersionError.Empty),
      )
    ,
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
      )
    ,
    test("a ship number rejects leading zeros and short forms"):
      val errors = typeCheckErrors("""ship"01.2.3"""")
      assertTrue(
        ship"1.4.2" == ShipNumber("1.4.2"),
        ship"0.1.0".render == "0.1.0",
        ShipNumber.from("1.2") == Left(ShipNumberError.NotAShip("1.2")),
        ShipNumber.from("01.2.3") == Left(ShipNumberError.NotAShip("01.2.3")),
        ShipNumber.from("1.02.3") == Left(ShipNumberError.NotAShip("1.02.3")),
        ShipNumber.from("") == Left(ShipNumberError.Empty),
        errors match
          case e :: _ => e.message.contains("01.2.3")
          case Nil    => false,
      )
    ,
    test("Ship.next rejects a bad name, a negative component, and a short commit"):
      val name     = typeCheckErrors("""Ship("1histos").next(0, 2, 0)""")
      val negative = typeCheckErrors("""Ship("histos").next(-1, 2, 0)""")
      val sha      = typeCheckErrors("""Ship("histos").next(0, 2, 0).commit("abc")""")
      val pin      = Ship("histos").next(0, 2, 0)
      val released = pin.released
      assertTrue(
        pin.pointer.revision == "0.2.0-SNAPSHOT",
        pin.commit("0123456789abcdef0123456789abcdef01234567").revision == "0.2.0-0123456789ab-SNAPSHOT",
        released.revision == "0.2.0",
        pin.pointer.dep(organization"rocks.earlyeffect", artifact"histos-api").revision == "0.2.0-SNAPSHOT",
        released.dep(organization"rocks.earlyeffect", artifact"histos").revision == "0.2.0",
        name match
          case e :: _ => e.message.contains("1histos") && e.message.contains("Ship(\"histos\")")
          case Nil    => false,
        negative match
          case e :: _ => e.message.contains("major is -1") && e.message.contains("next(0, 2, 0)")
          case Nil    => false,
        sha match
          case e :: _ => e.message.contains("abc") && e.message.contains("40-hex")
          case Nil    => false,
        typeCheckErrors(
          "val pin = Ship(\"histos\").next(0, 2, 0).pointer; pin.revisionValue"
        ).nonEmpty,
      )
    ,
    test("a broken dependency literal does not compile"):
      val errors = typeCheckErrors("""dep"nope"""")
      assertTrue:
        errors match
          case e :: _ => e.message.contains("nope")
          case Nil    => false
    ,
    test("a non-string hole does not typecheck, and a string hole takes no arguments"):
      val scalaHole = typeCheckErrors("""scala"${1}"""")
      val depHole   = typeCheckErrors("""dep"${1}"""")
      val scalaText = typeCheckErrors("""scala"${"3.9.0"}"""")
      val depText   = typeCheckErrors("""dep"${"dev.zio::zio:2.1.26"}"""")
      assertTrue(
        scala"3.9.0".value == "3.9.0",
        dep"dev.zio::zio:2.1.26" == Dep("dev.zio::zio:2.1.26"),
        scalaHole match
          case e :: _ => e.message.nonEmpty && !e.message.contains("takes no arguments")
          case Nil    => false,
        depHole match
          case e :: _ => e.message.nonEmpty && !e.message.contains("takes no arguments")
          case Nil    => false,
        scalaText match
          case e :: _ => e.message.contains("takes no arguments")
          case Nil    => false,
        depText match
          case e :: _ => e.message.contains("takes no arguments")
          case Nil    => false,
      ),
  )

  def spec = literals
end LiteralSpec
