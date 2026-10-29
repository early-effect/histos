package histos.testrun

import histos.jdk.Sha256

import zio.test.*

/** Laws read off the stamp. The class bytes stay out of it. A class the suite does not reach is not in it. */
object StampAlgebraSpec extends histos.SuiteClock:
  private val cover = SuiteStamp.Cover(Nil, Nil, "jdk", Nil, Nil)

  def spec = suite("stamps")(
    test("a missing suite has no stamp, and its module class is the suite"):
      val companion = Map("demo.Foo$" -> SuiteStamp.Print("fp", Set.empty))
      val absent    = SuiteStamp.digest("demo.Foo", Map.empty, cover)
      val present   = SuiteStamp.digest("demo.Foo", companion, cover)
      assertTrue(
        absent == Right(None),
        stamped(present),
      )
    ,
    test("the stamp follows references, and an unreferenced class stays out"):
      val base = Map(
        "demo.Foo"   -> SuiteStamp.Print("foo", Set("demo.Lib$")),
        "demo.Lib"   -> SuiteStamp.Print("lib", Set.empty),
        "demo.Other" -> SuiteStamp.Print("other", Set.empty),
      )
      val found   = SuiteStamp.digest("demo.Foo", base, cover)
      val library = SuiteStamp.digest("demo.Foo", base.updated("demo.Lib", SuiteStamp.Print("lib2", Set.empty)), cover)
      val other   =
        SuiteStamp.digest("demo.Foo", base.updated("demo.Other", SuiteStamp.Print("other2", Set.empty)), cover)
      assertTrue(stamped(found), found == other, moved(found, library))
    ,
    test("jar order stays, and flag order moves the stamp"):
      val index   = Map("demo.Foo" -> SuiteStamp.Print("foo", Set.empty))
      val jars    = cover.copy(jars = List("b", "a"))
      val flipped = cover.copy(jars = List("a", "b"))
      val flags   = cover.copy(flags = List("-X", "-Y"))
      val swapped = cover.copy(flags = List("-Y", "-X"))
      val found   = SuiteStamp.digest("demo.Foo", index, jars)
      assertTrue(
        found == SuiteStamp.digest("demo.Foo", index, flipped),
        moved(SuiteStamp.digest("demo.Foo", index, flags), SuiteStamp.digest("demo.Foo", index, swapped)),
      ),
  )

  private def stamped(got: Either[StampError, Option[Sha256]]): Boolean =
    got match
      case Right(Some(_)) => true
      case _              => false

  private def moved(left: Either[StampError, Option[Sha256]], right: Either[StampError, Option[Sha256]]): Boolean =
    (left, right) match
      case (Right(Some(one)), Right(Some(other))) => one != other
      case _                                      => false
end StampAlgebraSpec
