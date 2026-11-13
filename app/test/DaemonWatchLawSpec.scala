package histos.server

import histos.cache.CacheCleanMode
import histos.plugin.{
  Contribution,
  DeclaredInput,
  DeclaredOutput,
  DeclaredTask,
  HistosPlugin,
  OutputName,
  PluginArgs,
  PluginId,
  RegisteredPlugin,
  RepoPath,
  RunRequest,
  Seal,
  TaskName,
}
import histos.central.DeploymentId
import histos.format.{FormatMode, FormatTargets}
import histos.publish.PublishToken
import histos.schedule.CacheNamespace
import histos.task.RunId
import histos.testrun.TestMode
import histos.version.{Catalog, Ship, ShipName, ShipNumber}
import histos.workflow.WorkflowAction
import histos.{Module, Platform, Platforms, ProjectGraph, ProjectName, ScalaVersion, ScalaVersions, SourcePos}

import java.nio.file.Path

import zio.{Promise, Ref, Semaphore, ZIO, durationInt}
import zio.test.*

object DaemonWatchLawSpec extends ZIOSpecDefault:
  private val api       = ProjectName("api")
  private val runId     = RunId.trusted("run-1")
  private val repo      = Path.of("repo")
  private val buildFile = repo.resolve("histos.scala")

  def spec = suite("daemon and watch laws")(
    test("class directory writers are the calls that occupy the permit"):
      val bad = outputs.collect { case (label, call, writes) if Daemon.writesOutputs(call) != writes => label }
      assertTrue(bad == Nil)
    ,
    test("a format and a compile in one checkout run one after the other"):
      for
        permit  <- Semaphore.make(1)
        heard   <- Ref.make(List.empty[String])
        entered <- Promise.make[Nothing, Unit]
        format  <- Daemon
          .lease(permit, Call.Format(FormatTargets.Build, FormatMode.Write))(_ =>
            entered.succeed(()) *> ZIO.sleep(1.second) *> heard.update(_ :+ "format")
          )
          .fork
        _       <- entered.await
        compile <- Daemon.lease(permit, Call.Compile(Nil))(_ => heard.update(_ :+ "compile")).fork
        _       <- TestClock.adjust(1.second)
        _       <- format.join *> compile.join
        order   <- heard.get
      yield assertTrue(order == List("format", "compile"))
    ,
    test("a publish watch follows the selection the preview shows"):
      val bad = selections.flatMap: tokens =>
        val named     = tokens.map(PublishToken.trusted)
        val local     = Watching.roots(repo, Call.PublishLocal(named), shipped)
        val snapshots = Watching.roots(repo, Call.PublishSnapshots(tokens), shipped)
        val shown     = Watching.roots(repo, Call.PreviewPublishLocal(named), shipped)
        val shownSnap = Watching.roots(repo, Call.PreviewPublishSnapshots(tokens), shipped)
        List(
          Option.when(local != shown)(s"publishLocal ${tokens.mkString(" ")}"),
          Option.when(snapshots != shownSnap)(s"publish snapshots ${tokens.mkString(" ")}"),
        ).flatten
      assertTrue(
        bad == Nil,
        Watching.roots(repo, Call.PublishLocal(List(PublishToken("api"))), shipped) == onlyApi,
        Watching.roots(repo, Call.PublishSnapshots(List("lib")), shipped) == both,
      )
    ,
    test("watch roots follow the named modules and one chosen module adds the shared sources"):
      val bad = roots.collect {
        case (label, call, want) if Watching.roots(repo, call, graph) != want => label
      }
      assertTrue(bad == Nil)
    ,
    test("a plugin task watches its declared inputs and excludes its declared outputs"):
      val watched = Watching.watched(repo, Call.Task("docs/site", PluginArgs.empty), pluginGraph)
      assertTrue(
        watched.paths == List(
          repo.resolve("docs"),
          repo.resolve("notes").resolve("src"),
          repo.resolve("notes").resolve("test"),
        ),
        watched.excluded == Set(repo.resolve("docs").resolve("dev-stamp")),
        Watching.roots(repo, Call.Task("docs/quiet", PluginArgs.empty), pluginGraph) == List(buildFile),
        Watching.roots(repo, Call.Task("missing/site", PluginArgs.empty), pluginGraph) == List(buildFile),
      ),
  )

  private val core = ProjectName("core")

  private def module(name: ProjectName): Module =
    Module(
      name,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion("3.3.4"), Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )

  private def graph: ProjectGraph =
    ProjectGraph.trusted(List(module(api), module(core)))

  private val tool           = ProjectName("tool")
  private val lib: Ship.Line = Ship.Line(ShipName("lib"), ShipNumber("0.1.0"), ::(api, List(core)))

  /** `lib` ships api and core. `tool` is in no ship. */
  private def shipped: ProjectGraph =
    ProjectGraph.trusted(
      List(module(api), module(core), module(tool)),
      histos.Splits.aggregate,
      Catalog.Lines(::(lib, Nil)),
      None,
      Nil,
    )

  private val selections: List[List[String]] =
    List(Nil, List("api"), List("lib"), List("tool"), List("api", "core"), List("missing"), List("lib", "missing"))

  private val docs = PluginId.trusted("docs")

  private object Unrun extends HistosPlugin:
    def id: PluginId             = docs
    def contribute: Contribution = Contribution(Nil, Nil, Nil)
    def run(request: RunRequest) = ZIO.dieMessage("watching never runs a plugin")

  private def pluginGraph: ProjectGraph =
    ProjectGraph.trusted(
      List(module(ProjectName("notes"))),
      histos.Splits.aggregate,
      histos.version.Catalog.empty,
      None,
      Nil,
      List(
        RegisteredPlugin(
          docs,
          Contribution(
            List(
              DeclaredTask(
                TaskName.trusted("site"),
                Seal.Open,
                Set.empty,
                List(
                  DeclaredInput.Directory(RepoPath.trusted("docs")),
                  DeclaredInput.Module(ProjectName("notes"), None, None),
                ),
                List(DeclaredOutput(OutputName.trusted("stamp"), RepoPath.trusted("docs/dev-stamp"))),
                Nil,
              ),
              DeclaredTask(TaskName.trusted("quiet"), Seal.Open, Set.empty, Nil, Nil, Nil),
            ),
            Nil,
            Nil,
          ),
        )(Unrun)
      ),
    )

  private def owned(name: String): List[Path] =
    val base = repo.resolve(name)
    List(base.resolve("src"), base.resolve("test"))

  private val shared   = List(repo.resolve("src"), repo.resolve("test"))
  private val both     = buildFile :: owned("api") ::: owned("core")
  private val onlyApi  = buildFile :: owned("api") ::: shared
  private val fileOnly = List(buildFile)

  private def roots: List[(String, Call, List[Path])] = List(
    ("compile all", Call.Compile(Nil), both),
    ("compile one", Call.Compile(List(api)), onlyApi),
    ("test all", Call.Test(Nil, Nil, TestMode.Run), both),
    ("test one", Call.Test(List(api), Nil, TestMode.Quick), onlyApi),
    ("preview compile one", Call.PreviewCompile(List(api)), onlyApi),
    ("preview test all", Call.PreviewTest(Nil, Nil, TestMode.Run), both),
    ("run one", Call.Run("api", None), onlyApi),
    ("run not a project", Call.Run("1api", None), fileOnly),
    ("run unknown", Call.Run("other", None), fileOnly),
    ("preview run unknown", Call.PreviewRun("other", Some(Platform.Jvm)), fileOnly),
    ("publish local unknown", Call.PublishLocal(List(PublishToken("missing"))), fileOnly),
    ("publish local empty", Call.PublishLocal(Nil), both),
    ("preview publish empty", Call.PreviewPublishLocal(Nil), both),
    ("preview publish unknown", Call.PreviewPublishLocal(List(PublishToken("missing"))), fileOnly),
    ("preview snapshots unknown", Call.PreviewPublishSnapshots(List("missing")), fileOnly),
    ("clean empty", Call.Clean(Nil, CleanExtent.Products), both),
    ("clean one", Call.Clean(List(api), CleanExtent.Products), onlyApi),
    ("preview clean one", Call.PreviewClean(List(api), CleanExtent.Products), onlyApi),
    ("graph", Call.Graph, both),
    ("version", Call.Version, both),
  )

  private def outputs: List[(String, Call, Boolean)] = List(
    ("compile", Call.Compile(Nil), true),
    ("test", Call.Test(Nil, Nil, TestMode.Run), true),
    ("publish local", Call.PublishLocal(Nil), true),
    ("publish snapshots", Call.PublishSnapshots(Nil), true),
    ("publish release", Call.PublishRelease(Nil, false), true),
    ("run", Call.Run("api", None), true),
    ("assembly", Call.Assembly(None), true),
    ("clean empty", Call.Clean(Nil, CleanExtent.Products), true),
    ("clean named", Call.Clean(List(api), CleanExtent.Products), true),
    ("clean full", Call.Clean(Nil, CleanExtent.Full), true),
    ("format", Call.Format(FormatTargets.Build, FormatMode.Check), true),
    ("publish promote", Call.PublishPromote(DeploymentId("id")), false),
    ("publish drop", Call.PublishDrop(DeploymentId("drop")), false),
    ("publish assets", Call.PublishAssets(Nil), false),
    ("graph", Call.Graph, false),
    ("task", Call.Task("demo/pack", PluginArgs.empty), false),
    ("cancel", Call.Cancel(runId), false),
    ("resolve", Call.Resolve(ResolveAction.Verify), false),
    ("workflow", Call.Workflow(WorkflowAction.Check), false),
    ("reload", Call.Reload, false),
    ("reboot", Call.Reboot(Successor(None, None)), false),
    ("version", Call.Version, false),
    ("cache path", Call.CachePath, false),
    ("cache list", Call.CacheList, false),
    ("cache clean", Call.CacheClean(CacheCleanMode.Remove), false),
    ("cache promote", Call.CachePromote(CacheNamespace.Main, CacheNamespace.Main), false),
    ("watch", Call.Watch(Call.Version, WatchPolicy.After), false),
    ("bsp", Call.Bsp, false),
    ("preview compile", Call.PreviewCompile(Nil), false),
    ("preview test", Call.PreviewTest(Nil, Nil, TestMode.Run), false),
    ("preview publish local", Call.PreviewPublishLocal(Nil), false),
    ("preview publish snapshots", Call.PreviewPublishSnapshots(Nil), false),
    ("preview clean empty", Call.PreviewClean(Nil, CleanExtent.Products), false),
    ("preview clean named", Call.PreviewClean(List(api), CleanExtent.Products), false),
    ("preview clean full", Call.PreviewClean(Nil, CleanExtent.Full), false),
    ("preview run", Call.PreviewRun("api", None), false),
  )
end DaemonWatchLawSpec
