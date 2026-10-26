package histos.report

import heddle.endpoint.{Schema, SchemaDoc, SchemaField}
import histos.clean.Products
import histos.cli.{Cli, CliExit, CliParseError, CompileConsole}
import histos.client.{Remote, Session}
import histos.client.Session.SessionError
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256}
import histos.load.{LoadError, Loader}
import histos.server.{Call, Calls, Feed, Message, Say, ServerError}
import histos.task.RunId
import histos.toolchain.RuntimeIdentity
import histos.workspace.Workspace
import histos.zinc.{CompileFeed, CompiledCell}
import histos.{Jvm, ProjectName, Projects, ScalaVersion, module}
import histos.cache.ContentStore
import histos.resolve.{
  LockedArtifact,
  Repository,
  RepositoryIo,
  Repositories,
  Resolve,
  ResolveCell,
  ResolveError,
  ToolRequest,
}

import java.nio.file.Path

import zio.json.*
import zio.test.*
import zio.{Chunk, Fiber, Ref, Scope, ZIO, ZLayer}

object ReportSpec extends ZIOSpecDefault:
  private val version  = ScalaVersion("3.10.0")
  private val repoRoot = Path.of("/repo")

  def spec = suite("report")(
    test("a report is the tally json, and a chunk is an array"):
      val report = Report(
        Chunk("compiled", "done"),
        Chunk.empty,
        Some("run-1"),
        Some(sample),
        Chunk(warned),
        Chunk.empty,
        None,
      )
      val json    = report.toJson
      val missing =
        """{"exit":0,"out":[],"err":[],"run":null,"observation":null}"""
      assertTrue(
        json.fromJson[Report] == Right(report),
        sample.toJson.fromJson[Observation] == Right(sample),
        json.contains(""""out":["compiled","done"]"""),
        json.contains(""""run":"run-1""""),
        json.contains(""""severity":"warn""""),
        !json.contains(""""Warn""""),
        !json.contains(""""exit""""),
        missing.fromJson[Report].isLeft,
        severityTokens,
      )
    ,
    test("spoken keeps the minted id when the transcript names another"):
      val carried = Report.empty.copy(run = Some("minted"))
      val found   = Session.spoken(
        List(Feed.Out("run other"), Feed.Done(CallResult.Reported(carried)))
      )
      assertTrue(
        found == Right(CallResult.Reported(carried.copy(out = Chunk("run other"))))
      )
    ,
    test("a warning stays on the report and the terminal frame is success"):
      val warning = "core/src/Core.scala:4:2: warn: unused"
      val carried = Report.empty.copy(diagnostics = Chunk(warned))
      val spoken  = Session.spoken(
        List(Feed.Out("ok"), Feed.Err(warning), Feed.Done(CallResult.Reported(carried)))
      )
      for
        result    <- ZIO.fromEither(spoken)
        delivered <- Session.deliver(ZIO.succeed(result))
      yield assertTrue(
        CliExit.from(Call.Compile(Nil), CallResult.Reported(carried)) == CliExit.Success,
        delivered.out == Chunk("ok"),
        delivered.err == Chunk(warning),
        delivered.diagnostics == Chunk(warned),
      )
    ,
    test("a reported failure is the tool value, and a fault fails the tool"):
      val line   = "core/src/Core.scala:4:1: error: missing"
      val report = Report(
        Chunk("out"),
        Chunk(line),
        None,
        Some(sample),
        Chunk.empty,
        Chunk.empty,
        None,
      )
      val fault = BuildFault.Interrupted
      for
        kept    <- Session.deliver(ZIO.succeed(CallResult.Reported(report))).either
        failed  <- Session.deliver(ZIO.succeed(CallResult.Fault(fault))).either
        dropped <- Session.deliver(ZIO.fail(SessionError.DidNotFinish)).either
      yield assertTrue(
        kept == Right(report),
        failed == Left(Session.CallProblem.Fault(fault)),
        dropped == Left(Session.CallProblem.Transport(SessionError.DidNotFinish)),
      )
    ,
    test("tracked passes the minted id and does not read it from the line"):
      for
        runs  <- Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]])
        feeds <- Ref.make(Chunk.empty[Feed])
        say = Say.record(feeds)
        held <- Ref.make(Option.empty[String])
        _    <- Calls.tracked(runs, say)(
          say.run.flatMap(id => held.set(id) *> Calls.finish(say, CallResult.reported()))
        )
        id    <- held.get
        run   <- say.run
        chunk <- feeds.get
        found = Session.spoken(chunk.toList)
      yield assertTrue(
        id == run,
        (id, found) match
          case (Some(value), Right(result)) =>
            result == CallResult.Reported(
              Report(
                Chunk(s"run $value"),
                Chunk.empty,
                Some(value),
                None,
                Chunk.empty,
                Chunk.empty,
                None,
              )
            )
          case _ => false,
      )
    ,
    test("a done frame keeps the run id and the tally"):
      val reported = CallResult.Reported(
        Report(
          Chunk.empty,
          Chunk.empty,
          Some("minted"),
          Some(sample),
          Chunk(warned),
          Chunk.empty,
          None,
        )
      )
      val fault = CallResult.Fault(BuildFault.Interrupted)
      val json  = Say.wire(Message.Done(reported))
      val plain = Say.wire(Message.Done(fault))
      val out   = Say.wire(Message.Out("hi"))
      assertTrue(
        json.fromJson[Message] == Right(Message.Done(reported)),
        json.contains(""""severity":"warn""""),
        !json.contains(""""Exit""""),
        Remote.event(json) == Right(Remote.Event.Stop(Feed.Done(reported))),
        Remote.event(plain) == Right(Remote.Event.Stop(Feed.Done(fault))),
        Remote.event(out) == Right(Remote.Event.More(Feed.Out("hi"))),
      )
    ,
    test("a different server version is not a report"):
      ZIO.scoped {
        for
          repo  <- Workspace.work("histos-report-version")
          loads <- Ref.make(0)
          _     <- Workspace.writeString(
            repo.resolve(".histos").resolve("server.json"),
            """{"version":"other","fingerprint":"abc","socket":"/tmp/nope"}""",
          )
          desk   <- Session.desk
          result <- Session
            .perform(
              repo,
              Path.of("api.jar"),
              Calls.idleMachine,
              desk,
              Call.Version,
            )
            .provideSome[Scope](observeLayers(repo, loads))
            .either
          seen <- loads.get
        yield assertTrue(
          result == Left(SessionError.DifferentVersion),
          seen == 0,
        )
      }
    ,
    test("version is a report with no run id"):
      ZIO.scoped {
        for
          repo   <- Workspace.work("histos-report-plain")
          loads  <- Ref.make(0)
          desk   <- Session.desk
          result <- Session
            .perform(
              repo,
              Path.of("api.jar"),
              Calls.idleMachine,
              desk,
              Call.Version,
            )
            .provideSome[Scope](observeLayers(repo, loads))
          seen <- loads.get
        yield assertTrue(
          result match
            case CallResult.Reported(report) =>
              CliExit.from(Call.Version, result) == CliExit.Success &&
              report.run.isEmpty &&
              report.observation.isEmpty &&
              report.diagnostics.isEmpty &&
              report.tests.isEmpty &&
              report.program.isEmpty &&
              report.out == Chunk(Cli.VersionText) &&
              report.err.isEmpty
            case CallResult.Fault(_) => false
          ,
          seen == 0,
        )
      }
    ,
    test("a finished tally is the file a later read sees"):
      ZIO.scoped {
        for
          repo  <- Workspace.work("histos-report-file")
          saved <- remember(repo)
          read  <- Stored.read(repo)
        yield assertTrue(
          read == saved,
          read.classes.isEmpty,
          read.compiled.isEmpty,
          !read.toJson.contains(""""classes":4"""),
          !read.toJson.contains(""""classes":9"""),
        )
      }
    ,
    test("a missing tally is a typed error, and garbage is not a tally"):
      ZIO.scoped {
        for
          repo    <- Workspace.work("histos-report-missing")
          missing <- Stored.read(repo).either
          _       <- Workspace.writeString(Products.observation(repo), "nope")
          garbage <- Stored.read(repo).either
          path = Products.observation(repo).toString
        yield assertTrue(
          missing match
            case Left(ObserveError.Missing(found)) =>
              found == path &&
              ObserveError.Missing(found).message == s"no observation at $path"
            case _ => false
          ,
          garbage == Left(ObserveError.Malformed(path)),
        )
      }
    ,
    test("a compile that enters no module replaces the stored tally"):
      ZIO.scoped {
        for
          repo  <- Workspace.work("histos-report-compile")
          _     <- Stored.keep(repo, Some(sample))
          built <- ZIO.fromEither(Projects(module("core", Jvm)(version)))
          feeds <- Ref.make(Chunk.empty[Feed])
          say = Say.record(feeds)
          _ <- Calls
            .compile(repo, built, Nil, jdk, Calls.idleMachine, say)
            .provide(ZLayer.succeed(DeadResolve))
          read        <- Stored.read(repo).either
          observation <- say.observation
          chunk       <- feeds.get
        yield assertTrue(
          read == Left(
            ObserveError.Missing(Products.observation(repo).toString)
          ),
          observation.isEmpty,
          chunk.exists {
            case Feed.Done(result) =>
              CliExit.from(Call.Compile(Nil), result) != CliExit.Success
            case _ => false
          },
        )
      }
    ,
    test("observe prints the file and does not compile"):
      ZIO.scoped {
        for
          repo  <- Workspace.work("histos-report-observe")
          loads <- Ref.make(0)
          _     <- Stored.keep(repo, Some(sample))
          done  <- Cli
            .execute(List("observe"), repo, Path.of("api.jar"))
            .provideSome[Scope](observeLayers(repo, loads))
          out  <- TestConsole.output
          seen <- loads.get
        yield assertTrue(
          done.exit == CliExit.Success,
          done.out == sample.toJson,
          out.mkString.contains(sample.toJson),
          seen == 0,
        )
      }
    ,
    test("observe with no file prints the missing line and does not compile"):
      ZIO.scoped {
        for
          repo  <- Workspace.work("histos-report-observe-missing")
          loads <- Ref.make(0)
          done  <- Cli
            .execute(List("observe"), repo, Path.of("api.jar"))
            .provideSome[Scope](observeLayers(repo, loads))
          err  <- TestConsole.outputErr
          seen <- loads.get
          path = Products.observation(repo).toString
        yield assertTrue(
          done.exit == CliExit.TaskFailed,
          done.err == s"no observation at $path",
          err.mkString.contains(s"no observation at $path"),
          seen == 0,
        )
      }
    ,
    test("observe is reserved, and watch of it is usage"):
      ZIO.scoped {
        for
          repo  <- Workspace.work("histos-report-watch")
          loads <- Ref.make(0)
          bad   <- Cli.parse(List("observe", "--bad")).either
          done  <- Cli
            .execute(List("~", "observe"), repo, Path.of("api.jar"))
            .provideSome[Scope](observeLayers(repo, loads))
          seen <- loads.get
        yield assertTrue(
          bad match
            case Left(CliParseError.UnknownRoot) => false
            case Left(_)                         => true
            case Right(_)                        => false
          ,
          done.exit == CliExit.Usage,
          done.err.contains("usage: histos"),
          seen == 0,
        )
      },
  )

  private def remember(repo: Path): ZIO[
    Any,
    histos.workspace.IoFailure | histos.ProjectNameError,
    Observation,
  ] =
    for
      name    <- ZIO.fromEither(ProjectName.from("core"))
      console <- CompileConsole.open(repo)
      _       <- console.hear(
        CompileFeed.Done(
          CompiledCell(
            name,
            version,
            Jvm,
            repoRoot.resolve("core"),
            4,
            None,
            changed = true,
            sources = None,
            classesWritten = Some(9),
          )
        )
      )
      found <- console.observation
      _     <- Stored.keep(repo, found)
      saved <- found match
        case Some(value) => ZIO.succeed(value)
        case None        =>
          ZIO.fail(
            histos.workspace.IoFailure(
              Products.observation(repo),
              histos.workspace.IoFault.Write,
            )
          )
    yield saved

  private def severityTokens: Boolean =
    summon[Schema[Diagnostic]].doc match
      case SchemaDoc.Object(_, fields, _) =>
        fields.exists:
          case SchemaField("severity", SchemaDoc.Enum(_, values), _) =>
            values == List("error", "warn", "info")
          case _ => false
      case _ => false

  private val warned = Diagnostic(
    Some("core/src/Core.scala"),
    Some(4),
    Some(2),
    None,
    None,
    Severity.Warn,
    "unused",
  )

  private val sample = Observation(
    Chunk(ModuleTally("core", "3.10.0", "jvm", Some(2), Some(1), Some(4))),
    Some(2),
    Some(1),
    Some(4),
    1,
  )

  private val jdk = JdkRuntime(
    Path.of("jdk"),
    JdkVersion.trusted("25.0.5", JdkMajor.trusted(25)),
    RuntimeIdentity.Local(
      Path.of("jdk"),
      Sha256.of(Chunk.fromArray(Array[Byte](1))),
    ),
  )

  private final class Counting(loads: Ref[Int]) extends Loader:
    def scan(repo: Path) = ZIO.fail(LoadError.MissingFile(repo))
    def loadLocked(
        repo: Path,
        apiJar: Path,
        lock: histos.resolve.DependencyLock,
    ) =
      loads.update(_ + 1) *> ZIO.fail(LoadError.MissingFile(repo))
    def load(repo: Path, apiJar: Path) =
      loads.update(_ + 1) *> ZIO.fail(LoadError.MissingFile(repo))
    def buildClasspath(apiJar: Path)          = ZIO.succeed(Nil)
    def reportBuild(repo: Path, apiJar: Path) =
      ZIO.fail(LoadError.MissingFile(apiJar))

  private def observeLayers(repo: Path, loads: Ref[Int]) =
    ZLayer.succeed[Loader](Counting(loads)) ++
      ZLayer.succeed(ContentStore.in(repo.resolve("cache"))) ++
      ZLayer.succeed(Calls.idleMachine) ++
      Resolve.live(repo.resolve("resolve"))

  private object DeadResolve extends Resolve:
    private val failed: zio.IO[ResolveError, Nothing] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))
    def resolveTool(request: ToolRequest)                                 = failed
    def resolve(cell: ResolveCell)                                        = failed
    def fetch(artifact: LockedArtifact)                                   = failed
    def resolvePlugin(dependency: histos.Dep, repositories: Repositories) =
      failed
    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository,
    ) = failed
end ReportSpec
