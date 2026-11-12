package histos

import histos.report.{Diagnostic, ModuleTally, Observation, ObserveError, Rejection, Report, Severity, Tally, WireJson}
import histos.testrun.{TestStatus, TestedCell}
import histos.zinc.{CompileProblem, CompileSeverity, PositionBase, SourceCount}
import zio.json.*
import zio.test.*
import zio.{Cause, Chunk, Config}

import WireJson.given

object ReportLawSpec extends ZIOSpecDefault:
  import WireSamples.*

  private val count  = SourceCount(3, 1)
  private val tested = TestedCell(core, scala3, Platform.Jvm, TestStatus.Failed(2))

  private val problem = CompileProblem(
    "bad",
    Some(other),
    CompileSeverity.Error,
    Some(1),
    Some(2),
    Some(3),
    Some(4),
    PositionBase.Dotty,
  )
  private val logLine = CompileProblem(
    "log",
    None,
    CompileSeverity.Info,
    None,
    None,
    None,
    None,
    PositionBase.Zinc,
  )

  def spec = suite("report laws")(
    test("a report keeps its lines, tally, diagnostics, tests, and program"):
      val placed = Diagnostic(Some("A.scala"), Some(1), Some(2), Some(3), Some(4), Severity.Error, "bad")
      val bare   = Diagnostic(None, None, None, None, None, Severity.Warn, "note")
      val tally  = ModuleTally(core.value, scala3.value, "jvm", Some(3), Some(1), Some(9))
      val seen   = Observation(Chunk(tally), Some(3), Some(1), Some(9), 1)
      val full   = Report(
        Chunk("out"),
        Chunk("err"),
        Some("demo"),
        Some(seen),
        Chunk(placed, bare),
        Chunk(tested),
        Some(0),
      )
      val empty = Report.empty.toJson
      assertTrue(
        full.toJson.fromJson[Report] == Right(full),
        empty.fromJson[Report] == Right(Report.empty),
        empty == """{"out":[],"err":[],"diagnostics":[],"tests":[]}""",
      )
    ,
    test("an empty command help keeps its name list on the document"):
      val help = Rejection.CommandHelp(Nil)
      assertTrue(help.toJson == """{"CommandHelp":{"names":[]}}""")
    ,
    test("a tally records classes only with sources, and only four failures name a cell"):
      val full                                         = Tally.recorded(core, scala3, Platform.Js, Some(count), Some(9))
      val uncounted                                    = Tally.recorded(core, scala3, Platform.Js, Some(count), None)
      val unknown                                      = Tally.recorded(core, scala3, Platform.Native, None, Some(9))
      val errors                                       = CodecSamples.graphErrors
      def blank(platform: String): Option[ModuleTally] =
        Some(ModuleTally(core.value, scala3.value, platform, None, None, None))
      assertTrue(
        full == ModuleTally(core.value, scala3.value, "js", Some(3), Some(1), Some(9)),
        uncounted.compiled.contains(3),
        uncounted.classes.isEmpty,
        unknown.compiled.isEmpty,
        unknown.skipped.isEmpty,
        unknown.classes.isEmpty,
        unknown.platform == "native",
        EnumLaw.covered(errors),
        errors.map(Tally.failed) == List(
          None,
          None,
          None,
          None,
          None,
          blank("jvm"),
          blank("js"),
          None,
          None,
          None,
          None,
          None,
          blank("jvm"),
          blank("jvm"),
          None,
        ),
      )
    ,
    test("merging observations sums counts, and a missing count poisons that side"):
      val row    = Tally.recorded(core, scala3, Platform.Jvm, Some(count), Some(9))
      val left   = Observation(Chunk(row), Some(3), Some(1), Some(9), 1)
      val right  = Observation(Chunk.empty, Some(4), Some(2), Some(5), 2)
      val poison = Observation(Chunk.empty, None, None, None, 1)
      val both   = Tally.merge(Some(left), Some(right))
      val lost   = Tally.merge(Some(left), Some(poison))
      assertTrue(
        Tally.merge(None, None).isEmpty,
        Tally.merge(None, Some(left)).contains(left),
        Tally.merge(Some(left), None).contains(left),
        both.exists(item =>
          item.compiled.contains(7) && item.skipped.contains(3) && item.classes.contains(14) &&
            item.moduleCount == 3 && item.modules.length == 1
        ),
        lost.exists(item =>
          item.compiled.isEmpty && item.skipped.isEmpty && item.classes.isEmpty && item.moduleCount == 2
        ),
        Tally.combine(List(Some(left), None, Some(right))).exists(_.compiled.contains(7)),
      )
    ,
    test("a config error round-trips, and a failed cause keeps its message"):
      val invalid     = Config.Error.InvalidData(Chunk("jdk"), "bad")
      val missing     = Config.Error.MissingData(Chunk.empty, "gone")
      val unsupported = Config.Error.Unsupported(Chunk("node"), "no")
      val quiet       = List(
        invalid,
        missing,
        unsupported,
        Config.Error.And(invalid, missing),
        Config.Error.Or(missing, unsupported),
        Config.Error.SourceUnavailable(Chunk("home"), "down", Cause.empty),
      )
      val blown: Config.Error =
        Config.Error.SourceUnavailable(Chunk("home"), "down", Cause.fail(new IllegalStateException("boom")))
      val died: Config.Error =
        Config.Error.SourceUnavailable(Chunk("home"), "down", Cause.die(new IllegalStateException("died")))
      val blownJson = blown.toJson
      val diedJson  = died.toJson
      val again     = blownJson.fromJson[Config.Error]
      val diedAgain = diedJson.fromJson[Config.Error]
      assertTrue(quiet.map(error => error.toJson.fromJson[Config.Error]) == quiet.map(Right(_))) &&
      assertTrue(again.map(_.toJson) == Right(blownJson), diedAgain.map(_.toJson) == Right(diedJson)) &&
      causeMessage(again, _.failureOption, "boom") &&
      causeMessage(diedAgain, _.dieOption, "died")
    ,
    test("a cons round-trips and refuses an empty document"):
      val cons = ::("a", Nil)
      assertTrue(
        cons.toJson.fromJson[::[String]] == Right(cons),
        "[]".fromJson[::[String]].isLeft,
      )
    ,
    JsonLaw.values("a compile problem keeps a path, and a log line keeps none", List(problem, logLine)),
    JsonLaw.enumLaw(
      "every observe error round-trips",
      List(
        ObserveError.Missing("tally.json"),
        ObserveError.Unreadable("tally.json"),
        ObserveError.Malformed("tally.json"),
      ),
    ),
  )

  private def causeMessage(
      error: Either[String, Config.Error],
      pick: Cause[Throwable] => Option[Throwable],
      text: String,
  ): TestResult =
    error match
      case Right(Config.Error.SourceUnavailable(_, _, cause)) =>
        assertTrue(pick(cause).map(_.getMessage).contains(text))
      case other =>
        assertTrue(other == Left(text))
end ReportLawSpec
