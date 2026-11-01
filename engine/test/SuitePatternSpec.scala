package histos.testrun

import zio.test.*

object SuitePatternSpec extends ZIOSpecDefault:
  private val patterns = suite("suite pattern")(
    test("a prefix matches each suite and not another package"):
      val pattern = SuitePattern.from("com.acme.Foo*")
      assertTrue:
        pattern match
          case Right(found) =>
            SuiteMatch.allows(List(found), "com.acme.FooA") &&
            SuiteMatch.allows(List(found), "com.acme.FooB") &&
            SuiteMatch.allows(List(found), "com.acme.FooA$") &&
            !SuiteMatch.allows(List(found), "com.acme.Bar") &&
            !SuiteMatch.allows(List(found), "com.other.FooA")
          case Left(_) => false
    ,
    test("a package pattern crosses dots, and a question mark is one character"):
      val packaged = SuitePattern.from("com.acme.*")
      val one      = SuitePattern.from("com.acme.Foo?")
      assertTrue:
        (packaged, one) match
          case (Right(wide), Right(mark)) =>
            SuiteMatch.allows(List(wide), "com.acme.FooA") &&
            SuiteMatch.allows(List(wide), "com.acme.bar.Baz") &&
            SuiteMatch.allows(List(mark), "com.acme.FooA") &&
            SuiteMatch.allows(List(mark), "com.acme.FooB") &&
            !SuiteMatch.allows(List(mark), "com.acme.FooAB")
          case _ => false
    ,
    test("an exact class does not match a different class"):
      val found = SuitePattern.from("histos.testrun.ProviderNoteSpec")
      assertTrue:
        found match
          case Right(pattern) =>
            SuiteMatch.allows(List(pattern), "histos.testrun.ProviderNoteSpec") &&
            SuiteMatch.allows(List(pattern), "histos.testrun.ProviderNoteSpec$") &&
            !SuiteMatch.allows(List(pattern), "histos.load.SpecularFixtureSpec") &&
            !SuiteMatch.allows(List(pattern), "histos.load.SpecularFixtureSpec$")
          case Left(_) => false
    ,
    test("an exact name does not match a longer one"):
      val found = SuitePattern.from("com.acme.FooA")
      assertTrue(
        found match
          case Right(pattern) =>
            SuiteMatch.allows(List(pattern), "com.acme.FooA") &&
            !SuiteMatch.allows(List(pattern), "com.acme.FooAB")
          case Left(_) => false
        ,
        SuitePattern.from("") == Left(SuitePatternError.Empty),
        SuiteMatch.allows(Nil, "com.acme.FooA"),
      ),
  )

  def spec = patterns
end SuitePatternSpec
