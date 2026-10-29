package histos.testrun

import zio.test.*

object SuitePatternSpec extends histos.SuiteClock:
  private val patterns = suite("suite pattern")(
    test("a prefix matches each suite and not another package"):
      val pattern = SuitePattern.from("com.acme.Foo*")
      assertTrue:
        pattern match
          case Right(found) =>
            SuitePattern.allows(List(found), "com.acme.FooA") &&
            SuitePattern.allows(List(found), "com.acme.FooB") &&
            SuitePattern.allows(List(found), "com.acme.FooA$") &&
            !SuitePattern.allows(List(found), "com.acme.Bar") &&
            !SuitePattern.allows(List(found), "com.other.FooA")
          case Left(_) => false
    ,
    test("a package pattern crosses dots, and a question mark is one character"):
      val packaged = SuitePattern.from("com.acme.*")
      val one      = SuitePattern.from("com.acme.Foo?")
      assertTrue:
        (packaged, one) match
          case (Right(wide), Right(mark)) =>
            SuitePattern.allows(List(wide), "com.acme.FooA") &&
            SuitePattern.allows(List(wide), "com.acme.bar.Baz") &&
            SuitePattern.allows(List(mark), "com.acme.FooA") &&
            SuitePattern.allows(List(mark), "com.acme.FooB") &&
            !SuitePattern.allows(List(mark), "com.acme.FooAB")
          case _ => false
    ,
    test("an exact class does not match a different class"):
      val found = SuitePattern.from("histos.testrun.ProviderNoteSpec")
      assertTrue:
        found match
          case Right(pattern) =>
            SuitePattern.allows(List(pattern), "histos.testrun.ProviderNoteSpec") &&
            SuitePattern.allows(List(pattern), "histos.testrun.ProviderNoteSpec$") &&
            !SuitePattern.allows(List(pattern), "histos.load.SpecularFixtureSpec") &&
            !SuitePattern.allows(List(pattern), "histos.load.SpecularFixtureSpec$")
          case Left(_) => false
    ,
    test("an exact name does not match a longer one"):
      val found = SuitePattern.from("com.acme.FooA")
      assertTrue(
        found match
          case Right(pattern) =>
            SuitePattern.allows(List(pattern), "com.acme.FooA") &&
            !SuitePattern.allows(List(pattern), "com.acme.FooAB")
          case Left(_) => false
        ,
        SuitePattern.from("") == Left(SuitePatternError.Empty),
        SuitePattern.allows(Nil, "com.acme.FooA"),
      ),
  )

  def spec = patterns
end SuitePatternSpec
