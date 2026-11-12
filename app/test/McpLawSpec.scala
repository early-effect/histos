package histos.mcp

import heddle.mcp.protocol.{ListToolsResult, ProtocolVersion, RequestMeta, ToolName}
import histos.cache.{CacheCleanMode, CacheCleanModeError, ContentStore}
import histos.cli.{Grammar, Operation}
import histos.client.Session
import histos.format.{FormatMode, FormatTargets}
import histos.load.Loader
import histos.publish.IvyRoot
import histos.report.Rejection
import histos.resolve.Resolve
import histos.server.{Call, Calls}
import histos.task.{RunId, TaskError}
import histos.testrun.{SuitePattern, SuitePatternError, TestMode, TestModeError}
import histos.{ProjectName, ProjectNameError}
import slayer.stubbed

import java.nio.file.Path

import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.{NonEmptyChunk, Scope, ZIO, ZLayer}

object McpLawSpec extends ZIOSpecDefault:
  private val api     = ProjectName("api")
  private val badName = Rejection.ProjectName(ProjectNameError.Bad("1api"))

  def spec = suite("mcp laws")(
    test("compile and clean parse project names and publish passes the tokens through"):
      assertTrue(
        HistosMcp.compileCall(List("api")) == Right(Call.Compile(List(api))),
        HistosMcp.compileCall(Nil) == Right(Call.Compile(Nil)),
        HistosMcp.cleanCall(List("api")) == Right(Call.Clean(List(api))),
        HistosMcp.compileCall(List("1api")) == Left(badName),
        HistosMcp.cleanCall(List("1api")) == Left(badName),
        HistosMcp.publishLocalCall(List("1api")) == Right(Call.PublishLocal(List("1api"))),
        HistosMcp.publishSnapshotsCall(List("1api")) == Right(Call.PublishSnapshots(List("1api"))),
      )
    ,
    test("test defaults to run and rejects a bad mode or a bad pattern"):
      assertTrue(
        HistosMcp.testCall(List("api"), List("*"), None) == Right(
          Call.Test(List(api), List(SuitePattern("*")), TestMode.Run)
        ),
        HistosMcp.testCall(Nil, Nil, Some("quick")) == Right(Call.Test(Nil, Nil, TestMode.Quick)),
        HistosMcp.testCall(Nil, Nil, Some("nope")) == Left(Rejection.TestMode(TestModeError.Unknown("nope"))),
        HistosMcp.testCall(Nil, List(""), None) == Left(Rejection.SuitePattern(SuitePatternError.Empty)),
      )
    ,
    test("cache defaults to remove and cancel rejects an empty id"):
      assertTrue(
        HistosMcp.cacheCall(None) == Right(Call.CacheClean(CacheCleanMode.Remove)),
        HistosMcp.cacheCall(Some("dry-run")) == Right(Call.CacheClean(CacheCleanMode.DryRun)),
        HistosMcp.cacheCall(Some("nope")) == Left(Rejection.CacheClean(CacheCleanModeError.Unknown("nope"))),
        HistosMcp.cancelCall("") == Left(Rejection.RunId(TaskError.EmptyId)),
        HistosMcp.cancelCall("run-1") == Right(Call.Cancel(RunId("run-1"))),
      )
    ,
    test("preview parses the operation and cleanFull rejects a project"):
      assertTrue(
        HistosMcp.previewCall(args("compile", projects = List("api"))) == Right(Call.PreviewCompile(List(api))),
        HistosMcp.previewCall(args("compile", projects = List("1api"))) == Left(badName),
        HistosMcp.previewCall(args("test", projects = List("api"))) == Right(Call.PreviewTest(List(api))),
        HistosMcp.previewCall(args("publishLocal", projects = List("1api"))) == Right(
          Call.PreviewPublishLocal(List("1api"))
        ),
        HistosMcp.previewCall(args("publishSnapshots", projects = List("1api"))) == Right(
          Call.PreviewPublishSnapshots(List("1api"))
        ),
        HistosMcp.previewCall(args("clean")) == Right(Call.PreviewClean(Nil)),
        HistosMcp.previewCall(args("clean", projects = List("api"))) == Right(Call.PreviewClean(List(api))),
        HistosMcp.previewCall(args("cleanFull", projects = List("api"))) == Left(Rejection.PreviewProjects),
        HistosMcp.previewCall(args("cleanFull")) == Right(Call.PreviewCleanFull),
        HistosMcp.previewCall(args("run", id = "")) == Left(Rejection.MissingRunName),
        HistosMcp.previewCall(args("run")) == Right(Call.PreviewRun("api", None)),
        HistosMcp.previewCall(args("ship")) == Left(Rejection.PreviewOperation("ship")),
      )
    ,
    test("fmt sends the same call as the command line, and refuses a bad mode, a bad path, a bad name, or two scopes"):
      val file = Path.of("api/src/A.scala")
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
        checked == Right(Call.Format(FormatTargets.Projects(NonEmptyChunk(api)), FormatMode.Check)),
        fmt(files = List("api/src/A.scala")) == filed,
        filed == Right(Call.Format(FormatTargets.Files(NonEmptyChunk(file)), FormatMode.Write)),
        fmt(mode = Some("write")) == Right(Call.Format(FormatTargets.Build, FormatMode.Write)),
        fmt(mode = Some("diff")) == Left(Rejection.FormatMode("diff")),
        fmt(List("api"), List("api/src/A.scala")) == Left(Rejection.FormatTargets),
        fmt(files = List("/etc/passwd")) == Left(Rejection.FormatPath("/etc/passwd")),
        fmt(List("1api")) == Left(badName),
      )
    ,
    test("tools/list names every tool the server registers, and neither portal nor bsp is one"):
      val listing = Json.Obj(
        "jsonrpc" -> Json.Str("2.0"),
        "id"      -> Json.Num(1),
        "method"  -> Json.Str("tools/list"),
        "params"  -> Json.Obj(
          "_meta" -> Json.Obj(
            RequestMeta.VersionKey    -> Json.Str(ProtocolVersion.Current.value),
            RequestMeta.ClientCapsKey -> Json.Obj(),
          )
        ),
      )
      val program =
        for
          desk     <- Session.desk
          mcp      <- ZIO.fromEither(HistosMcp.tools(Path.of("repo"), Path.of("api.jar"), Calls.idleMachine, desk))
          answered <- mcp.handle(listing).someOrFail("tools/list had no answer")
          listed   <- ZIO.fromEither(
            answered.toJson
              .fromJson[Json.Obj]
              .flatMap(_.get("result").toRight("no result"))
              .flatMap(_.as[ListToolsResult])
          )
          names = listed.tools.map(_.name).toSet
        yield assertTrue(
          names == HistosMcp.toolNames,
          !names.contains(ToolName("portal")),
          !names.contains(ToolName("bsp")),
        )
      program.provideSomeLayer[Scope](
        stubbed[Loader] ++ stubbed[Resolve] ++ stubbed[ContentStore] ++
          ZLayer.succeed(IvyRoot.at(Path.of("ivy-never-written")))
      )
    ,
    test("a notice level is sent as info or error"):
      assertTrue(
        Notice.Level.Info.toJson == "\"info\"",
        Notice.Level.Error.toJson == "\"error\"",
      ),
  )

  private def args(operation: String, id: String = "api", projects: List[String] = Nil): HistosMcp.PreviewArgs =
    HistosMcp.PreviewArgs(operation, id, projects)
end McpLawSpec
