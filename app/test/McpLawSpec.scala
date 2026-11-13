package histos.mcp

import heddle.mcp.{McpBuildError, ToolError}
import heddle.mcp.protocol.{
  CallToolResult,
  ContentBlock,
  ListToolsResult,
  ProtocolVersion,
  RequestMeta,
  Tool,
  ToolName,
}
import histos.cache.{CacheCleanMode, CacheCleanModeError, ContentStore}
import histos.central.{KeyServer, ReleaseFeed}
import histos.cli.{Grammar, Operation}
import histos.client.Session
import histos.doctor.GhAuth
import histos.format.{FormatMode, FormatTargets}
import histos.load.{LoadedBuild, Loader}
import histos.publish.IvyRoot
import histos.report.{ObserveError, Rejection}
import histos.resolve.Resolve
import histos.server.Call
import histos.supply.{GitRead, Net}
import histos.task.{RunId, TaskError}
import histos.testrun.{SuitePattern, SuitePatternError, TestMode, TestModeError}
import histos.select.SelectError
import histos.workspace.Workspace
import histos.{
  HistosBuild,
  LawMachine,
  LawPlugins,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ProjectNameError,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
}
import slayer.{any, stub, stubbed}

import java.nio.file.Path

import zio.json.*
import zio.json.ast.Json
import zio.test.*
import zio.{NonEmptyChunk, Scope, ZIO, ZLayer, durationInt}

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
      val rows: List[(HistosMcp.PreviewArgs, Either[Rejection, Call])] = List(
        HistosMcp.PreviewArgs("compile", "api", List("api"))           -> Right(Call.PreviewCompile(List(api))),
        HistosMcp.PreviewArgs("compile", "api", List("1api"))          -> Left(badName),
        HistosMcp.PreviewArgs("test", "api", List("api"))              -> Right(Call.PreviewTest(List(api))),
        HistosMcp.PreviewArgs("publishLocal", "api", List("1api"))     -> Right(Call.PreviewPublishLocal(List("1api"))),
        HistosMcp.PreviewArgs("publishSnapshots", "api", List("1api")) -> Right(
          Call.PreviewPublishSnapshots(List("1api"))
        ),
        HistosMcp.PreviewArgs("clean", "api", Nil)             -> Right(Call.PreviewClean(Nil)),
        HistosMcp.PreviewArgs("clean", "api", List("api"))     -> Right(Call.PreviewClean(List(api))),
        HistosMcp.PreviewArgs("cleanFull", "api", List("api")) -> Left(Rejection.PreviewProjects),
        HistosMcp.PreviewArgs("cleanFull", "api", Nil)         -> Right(Call.PreviewCleanFull),
        HistosMcp.PreviewArgs("run", "", Nil)                  -> Left(Rejection.MissingRunName),
        HistosMcp.PreviewArgs("run", "api", Nil)               -> Right(Call.PreviewRun("api", None)),
        HistosMcp.PreviewArgs("ship", "api", Nil)              -> Left(Rejection.PreviewOperation("ship")),
      )
      val misses = rows.collect { case (args, want) if HistosMcp.previewCall(args) != want => args }
      assertTrue(misses == Nil)
    ,
    test("fmt sends the same call as the command line, and refuses a bad mode, a bad path, a bad name, or two scopes"):
      val file = Path.of("api/src/A.scala")

      def fmt(projects: List[String], files: List[String], mode: Option[String]) =
        HistosMcp.fmtCall(HistosMcp.FmtArgs(projects, files, mode))
      for
        build   <- Grammar.parse(List("fmt")).map(Operation.operation)
        checked <- Grammar.parse(List("fmt", "--check", "api")).map(Operation.operation)
        filed   <- Grammar.parse(List("fmt", "--file", "api/src/A.scala")).map(Operation.operation)
      yield assertTrue(
        fmt(Nil, Nil, None) == Right(Call.Format(FormatTargets.Build, FormatMode.Write)),
        fmt(Nil, Nil, None) == build,
        fmt(List("api"), Nil, Some("check")) == checked,
        checked == Right(Call.Format(FormatTargets.Projects(NonEmptyChunk(api)), FormatMode.Check)),
        fmt(Nil, List("api/src/A.scala"), None) == filed,
        filed == Right(Call.Format(FormatTargets.Files(NonEmptyChunk(file)), FormatMode.Write)),
        fmt(Nil, Nil, Some("write")) == Right(Call.Format(FormatTargets.Build, FormatMode.Write)),
        fmt(Nil, Nil, Some("diff")) == Left(Rejection.FormatMode("diff")),
        fmt(List("api"), List("api/src/A.scala"), None) == Left(Rejection.FormatTargets),
        fmt(Nil, List("/etc/passwd"), None) == Left(Rejection.FormatPath("/etc/passwd")),
        fmt(List("1api"), Nil, None) == Left(badName),
      )
    ,
    test("tools/list names every tool the server registers, and neither portal nor bsp is one"):
      listed.map: tools =>
        val names = tools.map(_.name).toSet
        assertTrue(
          names == HistosMcp.toolNames,
          !names.contains(ToolName("portal")),
          !names.contains(ToolName("bsp")),
        )
    ,
    test("a title in the tool schemas and the errors the tools fail with names one shape"):
      import histos.client.ClientJson.given
      import histos.report.WireJson.given
      import histos.report.WireSchema.given
      val errors = List(
        summon[ToolError[Session.CallProblem]].shape.outputSchema,
        summon[ToolError[ObserveError]].shape.outputSchema,
      )
      listed.map: tools =>
        val documents = tools.flatMap(tool => tool.inputSchema :: tool.outputSchema.toList) ++ errors
        val shapes    = documents.flatMap(titled).groupMap((title, _) => title)((_, shape) => shape)
        val clashes   = shapes.collect { case (title, found) if found.distinct.size > 1 => title }
        assertTrue(clashes.toList.sorted == Nil, shapes.contains("CallProblem"))
    ,
    test("doctor launches the named modules, and a bad name or an unknown module is a typed refusal"):
      val core = Module(
        ProjectName("core"),
        Platforms(Platform.Jvm, Nil),
        ScalaVersions(ScalaVersion("3.10.0"), Nil),
        Nil,
        Nil,
        SourcePos("histos.scala", 1, 1),
      )
      val graph = ProjectGraph.trusted(List(core))
      assertTrue(
        HistosMcp.launched(graph, Nil) == Right(Nil),
        HistosMcp.launched(graph, List("core")) == Right(List(core)),
        HistosMcp.launched(graph, List("1api", "nope")) == Left(ProjectNameError.Bad("1api")),
        HistosMcp.launched(graph, List("nope")) == Left(SelectError.Unknown(ProjectName("nope"), List("core"))),
        HistosMcp.notLaunched(ProjectNameError.Bad("1api")) == HistosMcp.DoctorText("'1api' is not a project name"),
        HistosMcp.notLaunched(SelectError.Unknown(ProjectName("nope"), Nil)) == HistosMcp.DoctorText(
          "'nope' is not a project\nThe name is not a project in this graph.\nfix  histos graph"
        ),
      )
    ,
    test("a doctor fix's command reads nothing, and what it prints is in the tool's answer and never on stdout"):
      val core = Module(
        ProjectName("core"),
        Platforms(Platform.Jvm, Nil),
        ScalaVersions(ScalaVersion("3.10.0"), Nil),
        Nil,
        Nil,
        SourcePos("histos.scala", 1, 1),
      )
      val graph   = ProjectGraph.trusted(List(core))
      val program =
        for
          repo <- Workspace.work("mcp-doctor-fix")
          script = repo.resolve("histos")
          _ <- Workspace.writeString(
            script,
            "#!/bin/sh\necho \"fix said $*\"\necho \"fix warned\" >&2\nif read line; then echo \"fix read $line\"; else echo \"fix read nothing\"; fi\n",
          )
          _ <- ZIO.attemptBlocking(script.toFile.setExecutable(true)).orDie
          _ <- stub[Loader](_.load(any[Path], any[Path])):
            ZIO.succeed(LoadedBuild(graph, LawPlugins.none, HistosBuild.fixed(graph)))
          _        <- stub[GitRead](_.origin(any[Path]))(ZIO.none)
          _        <- stub[GhAuth](_.token(any[Option[String]]))(ZIO.none)
          desk     <- Session.desk
          mcp      <- ZIO.fromEither(HistosMcp.tools(repo, Path.of("api.jar"), LawMachine.idle, desk))
          answered <- mcp.handle(doctorFix).someOrFail("tools/call had no answer")
          text <- ZIO.fromEither(
            answered.toJson
              .fromJson[Json.Obj]
              .flatMap(_.get("result").toRight("no result"))
              .flatMap(_.as[CallToolResult])
          )
        yield
          val shown = text.content.toList.collect { case ContentBlock.Text(said, _) => said }.mkString
          assertTrue(
            !text.failed,
            shown.contains("ran: ./histos workflow --update"),
            shown.contains("  fix said workflow --update"),
            shown.contains("  fix warned"),
            shown.contains("  fix read nothing"),
          )
      program.provideSomeLayer[Scope](
        stubbed[Loader] ++ stubbed[Resolve] ++ stubbed[ContentStore] ++
          ZLayer.succeed(IvyRoot.at(Path.of("ivy-never-written"))) ++ stubbed[ReleaseFeed] ++ stubbed[GitRead] ++
          stubbed[Net] ++ stubbed[GhAuth] ++ stubbed[KeyServer]
      )
    // A fix that read the protocol's stdin would wait for it, and the doctor bounds each check on the live clock.
    @@ TestAspect.timeout(60.seconds) @@ TestAspect.withLiveClock
    ,
    test("a notice level is sent as info or error"):
      assertTrue(
        Notice.Level.Info.toJson == "\"info\"",
        Notice.Level.Error.toJson == "\"error\"",
      ),
  )

  /** The tools a built server answers `tools/list` with. No tool runs. */
  private def listed: ZIO[Scope, NonEmptyChunk[McpBuildError] | String, List[Tool]] =
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
        mcp      <- ZIO.fromEither(HistosMcp.tools(Path.of("repo"), Path.of("api.jar"), LawMachine.idle, desk))
        answered <- mcp.handle(listing).someOrFail("tools/list had no answer")
        result   <- ZIO.fromEither(
          answered.toJson
            .fromJson[Json.Obj]
            .flatMap(_.get("result").toRight("no result"))
            .flatMap(_.as[ListToolsResult])
        )
      yield result.tools.toList
    program
      .provideSomeLayer[Scope](
        stubbed[Loader] ++ stubbed[Resolve] ++ stubbed[ContentStore] ++
          ZLayer.succeed(IvyRoot.at(Path.of("ivy-never-written"))) ++ stubbed[ReleaseFeed] ++ stubbed[GitRead] ++
          stubbed[Net] ++ stubbed[GhAuth] ++ stubbed[KeyServer]
      )

  /** `tools/call` of doctor with `--fix` on the supply chain, whose Actions author names `./histos workflow --update`. */
  private val doctorFix = Json.Obj(
    "jsonrpc" -> Json.Str("2.0"),
    "id"      -> Json.Num(2),
    "method"  -> Json.Str("tools/call"),
    "params"  -> Json.Obj(
      "name"      -> Json.Str("doctor"),
      "arguments" -> Json.Obj("fix" -> Json.Bool(true), "area" -> Json.Str("supply-chain")),
      "_meta"     -> Json.Obj(
        RequestMeta.VersionKey    -> Json.Str(ProtocolVersion.Current.value),
        RequestMeta.ClientCapsKey -> Json.Obj(),
      ),
    ),
  )

  /** Every titled type in `json`, with its schema. */
  private def titled(json: Json): List[(String, Json)] =
    json match
      case Json.Obj(fields) =>
        fields.toList.flatMap:
          case ("title", Json.Str(title)) => List((title, json))
          case ("oneOf", Json.Arr(arms))  => arms.toList.flatMap(arm)
          case (_, value)                 => titled(value)
      case Json.Arr(items) => items.toList.flatMap(titled)
      case _               => Nil

  /** One case of a sum: an object whose one property is the case, titled with the case's name. Only the types the case
    * holds are titled types.
    */
  private def arm(json: Json): List[(String, Json)] =
    json match
      case Json.Obj(fields) =>
        fields.toList.flatMap:
          case ("properties", Json.Obj(cases)) => cases.toList.flatMap((_, held) => untitled(held))
          case (_, value)                      => titled(value)
      case other => titled(other)

  private def untitled(json: Json): List[(String, Json)] =
    json match
      case Json.Obj(fields) =>
        fields.toList.flatMap:
          case ("title", _) => Nil
          case (_, value)   => titled(value)
      case other => titled(other)
end McpLawSpec
