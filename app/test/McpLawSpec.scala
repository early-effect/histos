package histos.mcp

import heddle.mcp.protocol.ToolName
import histos.cache.CacheCleanMode
import histos.cli.{Grammar, Operation}
import histos.format.{FormatMode, FormatTargets}
import histos.report.Rejection
import histos.server.Call
import histos.task.{RunId, TaskError}
import histos.testrun.{SuitePattern, SuitePatternError, TestMode}
import histos.ProjectName

import zio.NonEmptyChunk
import zio.json.*
import zio.test.*

object McpLawSpec extends ZIOSpecDefault:
  def spec = suite("mcp laws")(
    test("compile and clean parse project names and publish passes the tokens through"):
      val api = ProjectName("api")
      ProjectName.from("1api") match
        case Left(error) =>
          assertTrue(
            HistosMcp.compileCall(List("api")) == Right(Call.Compile(List(api))),
            HistosMcp.compileCall(Nil) == Right(Call.Compile(Nil)),
            HistosMcp.cleanCall(List("api")) == Right(Call.Clean(List(api))),
            HistosMcp.compileCall(List("1api")) == Left(Rejection.ProjectName(error)),
            HistosMcp.cleanCall(List("1api")) == Left(Rejection.ProjectName(error)),
            HistosMcp.publishLocalCall(List("1api")) == Right(Call.PublishLocal(List("1api"))),
            HistosMcp.publishSnapshotsCall(List("1api")) == Right(Call.PublishSnapshots(List("1api"))),
          )
        case Right(_) => assertTrue(false)
    ,
    test("test defaults to run and rejects a bad mode or a bad pattern"):
      SuitePattern.from("*") match
        case Left(_)     => assertTrue(false)
        case Right(glob) =>
          TestMode.from("nope") match
            case Left(error) =>
              assertTrue(
                HistosMcp.testCall(List("api"), List("*"), None) == Right(
                  Call.Test(List(ProjectName("api")), List(glob), TestMode.Run)
                ),
                HistosMcp.testCall(Nil, Nil, Some("quick")) == Right(Call.Test(Nil, Nil, TestMode.Quick)),
                HistosMcp.testCall(Nil, Nil, Some("nope")) == Left(Rejection.TestMode(error)),
                HistosMcp.testCall(Nil, List(""), None) == Left(Rejection.SuitePattern(SuitePatternError.Empty)),
              )
            case Right(_) => assertTrue(false)
    ,
    test("cache defaults to remove and cancel rejects an empty id"):
      CacheCleanMode.from("nope") match
        case Left(mode) =>
          RunId.from("") match
            case Left(id) =>
              RunId.from("run-1") match
                case Right(run) =>
                  assertTrue(
                    HistosMcp.cacheCall(None) == Right(Call.CacheClean(CacheCleanMode.Remove)),
                    HistosMcp.cacheCall(Some("dry-run")) == Right(Call.CacheClean(CacheCleanMode.DryRun)),
                    HistosMcp.cacheCall(Some("nope")) == Left(Rejection.CacheClean(mode)),
                    HistosMcp.cancelCall("") == Left(Rejection.RunId(id)),
                    id == TaskError.EmptyId,
                    HistosMcp.cancelCall("run-1") == Right(Call.Cancel(run)),
                  )
                case Left(_) => assertTrue(false)
            case Right(_) => assertTrue(false)
        case Right(_) => assertTrue(false)
    ,
    test("preview parses the operation and cleanFull rejects a project"):
      ProjectName.from("1api") match
        case Left(error) =>
          assertTrue(
            HistosMcp.previewCall(args("compile", projects = List("api"))) == Right(
              Call.PreviewCompile(List(ProjectName("api")))
            ),
            HistosMcp.previewCall(args("compile", projects = List("1api"))) == Left(Rejection.ProjectName(error)),
            HistosMcp.previewCall(args("test", projects = List("api"))) == Right(
              Call.PreviewTest(List(ProjectName("api")))
            ),
            HistosMcp.previewCall(args("publishLocal", projects = List("1api"))) == Right(
              Call.PreviewPublishLocal(List("1api"))
            ),
            HistosMcp.previewCall(args("publishSnapshots", projects = List("1api"))) == Right(
              Call.PreviewPublishSnapshots(List("1api"))
            ),
            HistosMcp.previewCall(args("clean")) == Right(Call.PreviewClean(Nil)),
            HistosMcp.previewCall(args("clean", projects = List("api"))) == Right(
              Call.PreviewClean(List(ProjectName("api")))
            ),
            HistosMcp.previewCall(args("cleanFull", projects = List("api"))) == Left(Rejection.PreviewProjects),
            HistosMcp.previewCall(args("cleanFull")) == Right(Call.PreviewCleanFull),
            HistosMcp.previewCall(args("run", id = "")) == Left(Rejection.MissingRunName),
            HistosMcp.previewCall(args("run")) == Right(Call.PreviewRun("api", None)),
            HistosMcp.previewCall(args("ship")) == Left(Rejection.PreviewOperation("ship")),
          )
        case Right(_) => assertTrue(false)
    ,
    test("fmt sends the same call as the command line, and refuses a bad mode, a bad path, or two scopes"):
      val api  = ProjectName.trusted("api")
      val file = java.nio.file.Path.of("api/src/A.scala")
      def fmt(projects: List[String] = Nil, files: List[String] = Nil, mode: Option[String] = None) =
        HistosMcp.fmtCall(HistosMcp.FmtArgs(projects, files, mode))
      for
        build   <- Grammar.parse(List("fmt")).map(Operation.operation)
        checked <- Grammar.parse(List("fmt", "--check", "api")).map(Operation.operation)
        filed   <- Grammar.parse(List("fmt", "--file", "api/src/A.scala")).map(Operation.operation)
      yield assertTrue(
        fmt() == Right(Call.Format(FormatTargets.Build, FormatMode.Write)),
        fmt() == build,
        fmt(List("api"), mode = Some("check")) == checked,
        fmt(files = List("api/src/A.scala")) == filed,
        filed == Right(Call.Format(FormatTargets.Files(NonEmptyChunk(file)), FormatMode.Write)),
        fmt(mode = Some("write")) == Right(Call.Format(FormatTargets.Build, FormatMode.Write)),
        fmt(mode = Some("diff")) == Left(Rejection.FormatMode("diff")),
        fmt(List("api"), List("api/src/A.scala")) == Left(Rejection.FormatTargets),
        fmt(files = List("/etc/passwd")) == Left(Rejection.FormatPath("/etc/passwd")),
        fmt(List("1api")).isLeft,
        HistosMcp.fmtCall(HistosMcp.FmtArgs()).map(Call.encode).flatMap(_.fromJson[Call]) ==
          Right(Call.Format(FormatTargets.Build, FormatMode.Write)),
        Call.encode(Call.Format(FormatTargets.Projects(NonEmptyChunk(api)), FormatMode.Check)).fromJson[Call] ==
          Right(Call.Format(FormatTargets.Projects(NonEmptyChunk(api)), FormatMode.Check)),
      )
    ,
    test("the tool list is the twenty five commands and does not include portal or bsp"):
      val names = Set(
        ToolName("graph"),
        ToolName("run"),
        ToolName("cancel"),
        ToolName("compile"),
        ToolName("test"),
        ToolName("publishLocal"),
        ToolName("publishSnapshots"),
        ToolName("toolchain"),
        ToolName("version"),
        ToolName("assembly"),
        ToolName("clean"),
        ToolName("cleanFull"),
        ToolName("resolve"),
        ToolName("workflow"),
        ToolName("fmt"),
        ToolName("reload"),
        ToolName("reboot"),
        ToolName("cachePath"),
        ToolName("cacheList"),
        ToolName("cacheClean"),
        ToolName("launcherUpdate"),
        ToolName("watch"),
        ToolName("preview"),
        ToolName("observe"),
        ToolName("doctor"),
      )
      assertTrue(HistosMcp.toolNames == names, names.size == 25)
    ,
    test("a notice level is info or error"):
      val other = "\"warn\"".fromJson[Notice.Level]
      assertTrue(
        "\"info\"".fromJson[Notice.Level] == Right(Notice.Level.Info),
        "\"error\"".fromJson[Notice.Level] == Right(Notice.Level.Error),
        Notice.Level.Info.toJson.fromJson[Notice.Level] == Right(Notice.Level.Info),
        other match
          case Left(reason) => reason.contains("unknown level warn")
          case Right(_)     => false,
      ),
  )

  private def args(operation: String, id: String = "api", projects: List[String] = Nil): HistosMcp.PreviewArgs =
    HistosMcp.PreviewArgs(operation, id, projects)
end McpLawSpec
