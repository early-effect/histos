package histos.server

import histos.assembly.AssembleError
import histos.build.BuildEvent
import histos.cache.{CacheCleanMode, ContentStore}
import histos.format.{FormatMode, FormatTargets}
import histos.load.{LoadError, LoadedBuild, Loader}
import histos.plugin.PluginArgs
import histos.program.ProgramError
import histos.central.DeploymentId
import histos.publish.IvyRoot
import histos.report.{BuildFault, CallResult, Rejection}
import histos.resolve.{ResolutionService, Resolve, ResolveError}
import histos.schedule.CacheNamespace
import histos.select.SelectError
import histos.task.{RunId, TaskError}
import histos.testrun.{ModuleTestError, SuitePatternError, TestMode}
import histos.workflow.WorkflowAction
import histos.zinc.{CompileError, CompileMemo, Compilers, GraphCompileError}
import histos.{LawMachine, Platform, ProjectName, ScalaVersion}
import slayer.{stub, stubbed}

import java.nio.file.Path
import java.time.Instant

import zio.*
import zio.test.*

object PerformLawSpec extends ZIOSpecDefault:
  private val api     = ProjectName("api")
  private val repo    = Path.of("repo")
  private val jar     = Path.of("api.jar")
  private val version = "fixture"
  private val pin     = Path.of("dependencies.lock")
  private val runId   = RunId.trusted("run-1")

  private val lock   = GraphCompileError.Lock(ResolveError.NoLock(pin))
  private val missed = GraphCompileError.NoRoot(api)
  private val cells  = GraphCompileError.Cells(NonEmptyChunk.single(missed))
  private val ran    =
    GraphCompileError.Failed(api, ScalaVersion("3.3.4"), Platform.Jvm, CompileError.NoSources)
  private val target = GraphCompileError.Target(SelectError.Unknown(api, Nil))

  def spec = suite("perform laws")(
    test("a call that reads the adopted graph is the only one that needs it"):
      val bad = needed.collect { case (label, call, need) if Perform.needsGraph(call) != need => label }
      assertTrue(bad == Nil)
    ,
    test("publish keeps the compile fault, and every other call reports a cell that ran and faults one that never did"):
      val bad = jdkRows.collect {
        case (call, error, want) if Perform.jdkMissed(call, error) != want =>
          s"${call.productPrefix} ${error.productPrefix}"
      }
      assertTrue(bad == Nil)
    ,
    test("a cell that ran is a report and a request that never started is a fault"):
      val noLock = ResolveError.NoLock(pin)
      assertTrue(
        Faults.compileStopped(cells) == None,
        Faults.compileStopped(ran) == None,
        Faults.compileStopped(lock) == Some(BuildFault.Resolve(noLock)),
        Faults.compileStopped(missed) == Some(BuildFault.Compile(missed)),
        Faults.testStopped(ModuleTestError.NoneMatched(Nil)) == None,
        Faults.testStopped(ModuleTestError.Pattern(SuitePatternError.Empty)) ==
          Some(BuildFault.Pattern(SuitePatternError.Empty)),
        Faults.testStopped(ModuleTestError.Lock(noLock)) == Some(BuildFault.Resolve(noLock)),
        Faults.testStopped(ModuleTestError.Compile(cells)) == None,
        Faults.testStopped(ModuleTestError.Target(SelectError.Unknown(api, Nil))) ==
          Some(BuildFault.Select(SelectError.Unknown(api, Nil))),
        Faults.testStopped(ModuleTestError.Absent(api)) == Some(BuildFault.Test(ModuleTestError.Absent(api))),
        Faults.programStopped(ProgramError.Absent("api")) == Some(BuildFault.Program(ProgramError.Absent("api"))),
        Faults.programStopped(ProgramError.Lock(noLock)) == Some(BuildFault.Resolve(noLock)),
        Faults.programStopped(ProgramError.Compile(cells)) == None,
        Faults.assembleStopped(AssembleError.NoModule("api")) ==
          Some(BuildFault.Assemble(AssembleError.NoModule("api"))),
        Faults.assembleStopped(AssembleError.Resolve(noLock)) == Some(BuildFault.Resolve(noLock)),
        Faults.assembleStopped(AssembleError.Compile(cells)) == None,
      )
    ,
    test("a loaded fault finishes before the call and a missing graph rejects a reader"):
      val fault   = BuildFault.Load(LoadError.MissingFile(repo.resolve("histos.scala")))
      val reject  = Chunk(Message.Done(CallResult.Fault(BuildFault.Rejected(Rejection.RunFailed))))
      val program =
        for
          loaded <- drive(Left(fault), Call.Version, quietEdges)
          missed <- ZIO.foreach(readers)(call => drive(Right(None), call, quietEdges))
        yield
          val bad = readers.zip(missed).collect { case (call, heard) if heard != reject => call.productPrefix }
          assertTrue(loaded == Chunk(Message.Done(CallResult.Fault(fault))), bad == Nil)
      program.provideSomeLayer[TestEnvironment](services)
    ,
    test("version prints the fixture and does not ask for a graph"):
      val program =
        for heard <- drive(Right(None), Call.Version, quietEdges)
        yield assertTrue(
          heard == Chunk(
            Message.Event(BuildEvent.Out(s"$version\nserver $version\nno local assembly")),
            Message.Done(CallResult.reported()),
          )
        )
      program.provideSomeLayer[TestEnvironment](services)
    ,
    test("watch receives the inner call and reload reboot and bsp stay on their edges"):
      val inner   = Call.Compile(Nil)
      val program =
        for
          seen <- Ref.make(Option.empty[(Call, WatchPolicy)])
          hit  <- Ref.make(List.empty[String])
          watch = Perform.Edges[Any](
            (call, policy, say) => seen.set(Some(call -> policy)) *> say(Message.Event(BuildEvent.Out("watch"))),
            die("reload"),
            _ => die("reboot"),
            die("bsp"),
            ZIO.unit,
          )
          watched <- drive(Right(None), Call.Watch(inner, WatchPolicy.After), watch)
          found   <- seen.get
          _       <- drive(Right(None), Call.Reload, edge(hit, "reload"))
          _       <- drive(Right(None), Call.Reboot(successor), edge(hit, "reboot"))
          _       <- drive(Right(None), Call.Bsp, edge(hit, "bsp"))
          order   <- hit.get
        yield assertTrue(
          found == Some(inner -> WatchPolicy.After),
          watched == Chunk(Message.Event(BuildEvent.Out("watch"))),
          order == List("reload", "reboot", "bsp"),
        )
      program.provideSomeLayer[TestEnvironment](services)
    ,
    test("cache path list and clean do not ask for a graph"):
      val program =
        for
          _ <- stub[ContentStore](_.root)(Path.of("cas"))
          _ <- stub[ContentStore](_.list)(ZIO.succeed(Nil))
          _ <- stub[ContentStore](
            _.clean(slayer.any[Instant], slayer.any[java.time.Duration], slayer.any[CacheCleanMode])
          ) { (now: Instant, retention: java.time.Duration, mode: CacheCleanMode) =>
            if now == Instant.EPOCH && retention == ContentStore.Retention && mode == CacheCleanMode.DryRun then
              ZIO.succeed(Nil)
            else ZIO.die(new IllegalStateException(s"clean $now $retention $mode"))
          }
          path  <- drive(Right(None), Call.CachePath, quietEdges)
          list  <- drive(Right(None), Call.CacheList, quietEdges)
          clean <- drive(Right(None), Call.CacheClean(CacheCleanMode.DryRun), quietEdges)
        yield assertTrue(
          path == Chunk(Message.Event(BuildEvent.Out("cas")), Message.Done(CallResult.reported())),
          list == Chunk(Message.Done(CallResult.reported())),
          clean == Chunk(
            Message.Event(BuildEvent.Out("would remove nothing")),
            Message.Done(CallResult.reported()),
          ),
        )
      program.provideSomeLayer[TestEnvironment](services)
    ,
    test("cancel of an unknown id does not boot an edge"):
      val program =
        for heard <- drive(Right(None), Call.Cancel(runId), quietEdges)
        yield assertTrue(
          heard == Chunk(Message.Done(CallResult.Fault(BuildFault.Task(TaskError.UnknownRun(runId)))))
        )
      program.provideSomeLayer[TestEnvironment](services),
  )

  /** Nothing here publishes, so the ivy root is never written. */
  private def services =
    stubbed[Resolve] ++ stubbed[Loader] ++ stubbed[ContentStore] ++ stubbed[Compilers] ++ stubbed[CompileMemo] ++
      stubbed[ResolutionService] ++ stubbed[histos.format.Formatters] ++
      ZLayer.succeed(IvyRoot.at(Path.of("ivy-never-written")))

  private def die(name: String): ZIO[Any, ServerError, Unit] =
    ZIO.die(new IllegalStateException(name))

  private def quietEdges: Perform.Edges[Any] =
    Perform.Edges((_, _, _) => die("watch"), die("reload"), _ => die("reboot"), die("bsp"), ZIO.unit)

  private def edge(hit: Ref[List[String]], name: String): Perform.Edges[Any] =
    Perform.Edges(
      (_, _, _) => die("watch"),
      mark(hit, "reload", name),
      onto => mark(hit, "reboot", name).when(onto == successor).unit,
      mark(hit, "bsp", name),
      ZIO.unit,
    )

  private val successor = Successor(Some("histos 0.3.1"), None)

  private def mark(hit: Ref[List[String]], label: String, name: String): ZIO[Any, ServerError, Unit] =
    if label == name then hit.update(_ :+ name)
    else die(label)

  private def drive(
      loaded: Either[BuildFault, Option[LoadedBuild]],
      call: Call,
      edges: Perform.Edges[Any],
  ): ZIO[
    Resolve & Loader & ContentStore & Compilers & CompileMemo & ResolutionService & histos.format.Formatters & IvyRoot,
    ServerError,
    Chunk[Message],
  ] =
    for
      messages <- Ref.make(Chunk.empty[Message])
      say = new Say(message => messages.update(_ :+ message))
      runs  <- Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]])
      _     <- Perform.run(repo, jar, loaded, LawMachine.idle, runs, false, version, call, say, edges)
      heard <- messages.get
    yield heard

  private def readers: List[Call] =
    needed.collect { case (_, call, true) => call }

  private val publishCalls = List(
    Call.PublishLocal(Nil),
    Call.PublishSnapshots(Nil),
    Call.PublishRelease(Nil, false),
    Call.PreviewPublishLocal(Nil),
    Call.PreviewPublishSnapshots(Nil),
  )

  private def compileFault(error: GraphCompileError): CallResult =
    CallResult.Fault(BuildFault.Compile(error))

  private def resolveFault: CallResult =
    CallResult.Fault(BuildFault.Resolve(ResolveError.NoLock(pin)))

  private def reported(error: GraphCompileError): CallResult =
    CallResult.reported(error.message)

  private def jdkRows: List[(Call, GraphCompileError, CallResult)] =
    val publish =
      for
        call  <- publishCalls
        error <- List(lock, cells, missed, ran, target)
      yield (call, error, compileFault(error))
    val others = List(
      Call.Test(Nil, Nil, TestMode.Run),
      Call.PreviewTest(Nil, Nil, TestMode.Run),
      Call.Compile(Nil),
      Call.Format(FormatTargets.Build, FormatMode.Write),
      Call.PublishDrop(DeploymentId("drop")),
      Call.Version,
    )
    val stopped = List(
      lock   -> resolveFault,
      cells  -> reported(cells),
      ran    -> reported(ran),
      missed -> compileFault(missed),
      target -> compileFault(target),
    )
    val other =
      for
        call            <- others
        (error, wanted) <- stopped
      yield (call, error, wanted)
    publish ++ other

  private def needed: List[(String, Call, Boolean)] = List(
    ("compile", Call.Compile(Nil), true),
    ("test", Call.Test(Nil, Nil, TestMode.Run), true),
    ("publish local", Call.PublishLocal(Nil), true),
    ("publish snapshots", Call.PublishSnapshots(Nil), true),
    ("publish release", Call.PublishRelease(Nil, false), true),
    ("publish assets", Call.PublishAssets(Nil), true),
    ("publish drop", Call.PublishDrop(DeploymentId("drop")), true),
    ("graph", Call.Graph, true),
    ("run", Call.Run("api", None), true),
    ("task", Call.Task("demo/pack", PluginArgs.empty), true),
    ("workflow", Call.Workflow(WorkflowAction.Check), true),
    ("assembly", Call.Assembly(None), true),
    ("format", Call.Format(FormatTargets.Build, FormatMode.Write), true),
    ("preview compile", Call.PreviewCompile(Nil), true),
    ("preview test", Call.PreviewTest(Nil, Nil, TestMode.Run), true),
    ("preview publish local", Call.PreviewPublishLocal(Nil), true),
    ("preview publish snapshots", Call.PreviewPublishSnapshots(Nil), true),
    ("preview run", Call.PreviewRun("api", None), true),
    ("clean named", Call.Clean(List(api), CleanExtent.Products), true),
    ("preview clean named", Call.PreviewClean(List(api), CleanExtent.Products), true),
    ("clean empty", Call.Clean(Nil, CleanExtent.Products), false),
    ("preview clean empty", Call.PreviewClean(Nil, CleanExtent.Products), false),
    ("publish promote", Call.PublishPromote(DeploymentId("id")), false),
    ("cancel", Call.Cancel(runId), false),
    ("resolve", Call.Resolve(ResolveAction.Verify), false),
    ("clean full", Call.Clean(Nil, CleanExtent.Full), false),
    ("reload", Call.Reload, false),
    ("reboot", Call.Reboot(successor), false),
    ("version", Call.Version, false),
    ("cache path", Call.CachePath, false),
    ("cache list", Call.CacheList, false),
    ("cache clean", Call.CacheClean(CacheCleanMode.Remove), false),
    ("cache promote", Call.CachePromote(CacheNamespace.Main, CacheNamespace.Main), false),
    ("watch", Call.Watch(Call.Version, WatchPolicy.After), false),
    ("bsp", Call.Bsp, false),
    ("preview clean full", Call.PreviewClean(Nil, CleanExtent.Full), false),
  )
end PerformLawSpec
