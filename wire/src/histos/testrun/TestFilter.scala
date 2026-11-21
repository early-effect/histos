package histos.testrun

import _root_.scala.annotation.publicInBinary
import _root_.scala.quoted.*

/** A ZIO Test label substring. Any term keeps a test. */
opaque type TestTerm = String

object TestTerm:
  given CanEqual[TestTerm, TestTerm] = CanEqual.derived

  inline def apply(inline raw: String): TestTerm = ${
    TestTermMacro.literal('raw)
  }

  def from(raw: String): Either[TestTermError, TestTerm] =
    if raw.isEmpty then Left(TestTermError.Empty)
    else if raw.exists(_.isWhitespace) then Left(TestTermError.Blank(raw))
    else if raw.startsWith("-") then Left(TestTermError.Flag(raw))
    else Right(trusted(raw))

  extension (term: TestTerm) def value: String = term

  @publicInBinary private[histos] def trusted(raw: String): TestTerm = raw
end TestTerm

enum TestTermError:
  case Empty
  case Blank(raw: String)
  case Flag(raw: String)

  def message: String = this match
    case Empty      => "a test term is not empty"
    case Blank(raw) => s"'$raw' is not a test term"
    case Flag(raw)  => s"'$raw' is not a test term"
end TestTermError

/** An exact ZIO Test tag. */
opaque type TagName = String

object TagName:
  given CanEqual[TagName, TagName] = CanEqual.derived

  inline def apply(inline raw: String): TagName = ${
    TagNameMacro.literal('raw)
  }

  def from(raw: String): Either[TagNameError, TagName] =
    if raw.isEmpty then Left(TagNameError.Empty)
    else if raw.exists(_.isWhitespace) then Left(TagNameError.Blank(raw))
    else if raw.startsWith("-") then Left(TagNameError.Flag(raw))
    else Right(trusted(raw))

  extension (name: TagName) def value: String = name

  @publicInBinary private[histos] def trusted(raw: String): TagName = raw
end TagName

enum TagNameError:
  case Empty
  case Blank(raw: String)
  case Flag(raw: String)

  def message: String = this match
    case Empty      => "a tag name is not empty"
    case Blank(raw) => s"'$raw' is not a tag name"
    case Flag(raw)  => s"'$raw' is not a tag name"
end TagNameError

/** Which tests inside an already selected suite to run. Empty is every test in that suite. */
final case class TestFilter(
    tests: List[TestTerm],
    tags: List[TagName],
    ignoredTags: List[TagName],
):
  /** `-t`, then `-tags`, then `-ignore-tags`. The command line's flag order does not change this. */
  def argv: List[String] =
    tests.flatMap(term => List("-t", term.value)) ++
      tags.flatMap(tag => List("-tags", tag.value)) ++
      ignoredTags.flatMap(tag => List("-ignore-tags", tag.value))

object TestFilter:
  val empty: TestFilter = TestFilter(Nil, Nil, Nil)

  val ArgCount: String  = "HISTOS_RUNNER_ARG_COUNT"
  val ArgPrefix: String = "HISTOS_RUNNER_ARG_"

  def argv(filter: TestFilter): List[String] = filter.argv

  /** One entry per argv element, in argv order. The count is present when the filter is empty. */
  def environment(filter: TestFilter): Map[String, String] =
    val args    = filter.argv
    val indexed = args.zipWithIndex.map: (arg, index) =>
      s"$ArgPrefix$index" -> arg
    Map(ArgCount -> args.length.toString) ++ indexed
end TestFilter

private object TestTermMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[TestTerm] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "TestTerm(...) takes a literal. Use TestTerm.from at runtime.",
          raw,
        )
      case Some(value) =>
        TestTerm.from(value) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_) => '{ TestTerm.trusted(${ Expr(value) }) }

private object TagNameMacro:
  def literal(raw: Expr[String])(using Quotes): Expr[TagName] =
    raw.value match
      case None =>
        quotes.reflect.report.errorAndAbort(
          "TagName(...) takes a literal. Use TagName.from at runtime.",
          raw,
        )
      case Some(value) =>
        TagName.from(value) match
          case Left(error) =>
            quotes.reflect.report.errorAndAbort(error.message, raw)
          case Right(_) => '{ TagName.trusted(${ Expr(value) }) }
