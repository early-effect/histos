package histos.server

import histos.LawDir
import histos.build.BuildEvent
import histos.cache.{CacheProject, ContentStore}
import histos.clean.Products
import histos.load.{LoadedBuild, Loader}
import histos.plugin.{NamedText, ParamName, PluginArgs}
import histos.publish.{LauncherError, LauncherPin}
import histos.report.{BuildFault, CallResult, Rejection}
import histos.resolve.{ResolutionService, Resolve}
import histos.schedule.CacheError
import histos.task.{RunId, TaskError}
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.{CompileMemo, Compilers}
import histos.{Module, Platform, Platforms, ProjectGraph, ProjectName, ScalaVersion, ScalaVersions, SourcePos}
import slayer.{any, stub, stubbed}

import java.nio.file.{Files, Path}
import java.time.Instant
import java.util.UUID

import zio.*
import zio.test.*

object PerformRouteLawSpec extends ZIOSpecDefault:
  private val repo    = Path.of("repo")
  private val jar     = Path.of("api.jar")
  private val version = "fixture"
  private val runId   = UUID.fromString("00000000-0000-0000-0000-000000000001")

  def spec = suite("perform route laws")(
    test("a tracked inspect with arguments names the run and then refuses"):
      val loaded = Right(Some(LoadedBuild(graph, Calls.quiet)))
      val args   = PluginArgs(List(NamedText(ParamName("name"), "ada")), Set.empty)
      val program =
        for
          _             <- TestRandom.feedUUIDs(runId)
          (heard, left) <- drive(loaded, Call.Task("inspect", args), track = true)
        yield assertTrue(
          heard == Chunk(
            Message.Event(BuildEvent.Out(s"run ${runId.toString}")),
            Message.Done(CallResult.Fault(BuildFault.Rejected(Rejection.TaskArguments("inspect")))),
          ),
          left.isEmpty,
        )
      program.provideSomeLayer[TestEnvironment](services)
    ,
    test("empty preview clean prints the product roots and preview clean full adds the store line"):
      val clean = List(
        ".histos/classes",
        ".histos/analysis",
        ".histos/test-classes",
        ".histos/test-analysis",
        ".histos/test-work",
        ".histos/test-quick",
        ".histos/tests",
        ".histos/jars",
        ".histos/link",
        ".histos/run",
        ".histos/provider",
        ".histos/provider-src",
      ).mkString("\n")
      val full = s"$clean\ncontent-store entries for this repo would be forgotten"
      val program =
        for
          (preview, _) <- drive(Right(None), Call.PreviewClean(Nil))
          (entire, _)  <- drive(Right(None), Call.PreviewCleanFull)
        yield assertTrue(
          preview == Chunk(Message.Event(BuildEvent.Out(clean)), Message.Done(CallResult.reported())),
          entire == Chunk(Message.Event(BuildEvent.Out(full)), Message.Done(CallResult.reported())),
        )
      program.provideSomeLayer[TestEnvironment](services)
    ,
    test("inspect prints project names, and arguments or an empty id are refused"):
      val loaded  = Right(Some(LoadedBuild(graph, Calls.quiet)))
      val args    = PluginArgs(List(NamedText(ParamName("name"), "ada")), Set.empty)
      val program =
        for
          (names, _) <- drive(loaded, Call.Task("inspect", PluginArgs.empty))
          (refused, _) <- drive(loaded, Call.Task("inspect", args))
          (empty, _)   <- drive(loaded, Call.Task("", PluginArgs.empty))
        yield assertTrue(
          names == Chunk(Message.Event(BuildEvent.Out("core,api")), Message.Done(CallResult.reported())),
          refused == Chunk(
            Message.Done(CallResult.Fault(BuildFault.Rejected(Rejection.TaskArguments("inspect"))))
          ),
          empty == Chunk(Message.Done(CallResult.Fault(BuildFault.Task(TaskError.EmptyId)))),
        )
      program.provideSomeLayer[TestEnvironment](services)
    ,
    suite("on disk")(
      test("clean says cleaned and a store failure after the wipe does not"):
        val failure = CacheError.Io(IoFailure(Path.of("cas"), IoFault.Read))
        val program =
          for
            root <- ZIO.service[Path]
            gone <- LawDir.child(root, "clean")
            kept <- LawDir.child(root, "clean-fail")
            _    <- plant(gone)
            _    <- plant(kept)
            _    <- stub[CompileMemo](_.forget)(ZIO.unit)
            _    <- stub[Compilers](_.drop)(ZIO.unit)
            _    <- stubForget(gone, ZIO.die(new IllegalStateException("clean forgets the store")))
            (cleaned, _) <- drive(Right(None), Call.Clean(Nil), where = gone)
            removed      <- absent(gone)
            _            <- stubForget(kept, ZIO.fail(failure))
            (failed, _) <- drive(Right(None), Call.CleanFull, where = kept)
            wiped       <- absent(kept)
          yield assertTrue(
            cleaned == Chunk(Message.Event(BuildEvent.Out("cleaned")), Message.Done(CallResult.reported())),
            removed,
            failed == Chunk(Message.Done(CallResult.Fault(BuildFault.Cache(failure)))),
            wiped,
          )
        program.provideSomeLayer[TestEnvironment & Scope & Path](services)
      ,
      test("launcher latest reprints a release pin and a missing pin is absent"):
        val program =
          for
            root    <- ZIO.service[Path]
            present <- LawDir.child(root, "pin")
            missing <- LawDir.child(root, "no-pin")
            _       <- ZIO.attemptBlocking {
              Files.writeString(present.resolve(LauncherPin.file), "histos.version=9.8.7\n")
            }.orDie
            (latest, _) <- drive(Right(None), Call.Launcher(LauncherAction.Latest), where = present)
            text        <- ZIO.attemptBlocking(Files.readString(present.resolve(LauncherPin.file))).orDie
            (absent, _) <- drive(Right(None), Call.Launcher(LauncherAction.Latest), where = missing)
          yield assertTrue(
            latest == Chunk(
              Message.Event(BuildEvent.Out("histos.version=9.8.7")),
              Message.Done(CallResult.reported()),
            ),
            text == "histos.version=9.8.7\n",
            absent == Chunk(
              Message.Done(
                CallResult.Fault(BuildFault.Launcher(LauncherError.Absent(missing.resolve(LauncherPin.file))))
              )
            ),
          )
        program.provideSomeLayer[TestEnvironment & Scope & Path](services),
    ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer),
  )

  private def services =
    stubbed[Resolve] ++ stubbed[Loader] ++ stubbed[ContentStore] ++ stubbed[Compilers] ++ stubbed[CompileMemo] ++
      stubbed[ResolutionService]

  private def stubForget(
      repo: Path,
      result: IO[CacheError, List[histos.cache.CacheEntry]],
  ): URIO[slayer.Stubbed[ContentStore], Unit] =
    stub[ContentStore](_.forget(any[CacheProject], any[Instant])) { (project: CacheProject, now: Instant) =>
      if project.value == CacheProject.of(repo).value && now == Instant.EPOCH then result
      else ZIO.die(new IllegalStateException(s"forget ${project.value} $now"))
    }

  private def plant(repo: Path): UIO[Unit] =
    ZIO
      .attemptBlocking {
        val classes = repo.resolve(".histos").resolve("classes")
        Files.createDirectories(classes)
        Files.writeString(classes.resolve("keep.txt"), "x")
        Files.writeString(Products.observation(repo), "{}")
        ()
      }
      .orDie

  private def absent(repo: Path): UIO[Boolean] =
    ZIO
      .attemptBlocking(
        !Files.exists(repo.resolve(".histos").resolve("classes").resolve("keep.txt")) &&
          !Files.exists(Products.observation(repo))
      )
      .orDie

  private def drive(
      loaded: Either[BuildFault, Option[LoadedBuild]],
      call: Call,
      edges: Perform.Edges[Any] = quiet,
      track: Boolean = false,
      where: Path = repo,
  ): ZIO[
    Resolve & Loader & ContentStore & Compilers & CompileMemo & ResolutionService,
    ServerError,
    (Chunk[Message], Map[RunId, Fiber[ServerError, Unit]]),
  ] =
    for
      messages <- Ref.make(Chunk.empty[Message])
      say = new Say(message => messages.update(_ :+ message))
      runs  <- Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]])
      _     <- Perform.run(where, jar, loaded, Calls.idleMachine, runs, track, version, call, say, edges)
      heard <- messages.get
      left  <- runs.get
    yield (heard, left)

  private def quiet: Perform.Edges[Any] =
    Perform.Edges(
      (_, _, _) => ZIO.die(new IllegalStateException("watch")),
      ZIO.die(new IllegalStateException("reload")),
      ZIO.die(new IllegalStateException("reboot")),
      ZIO.die(new IllegalStateException("bsp")),
    )

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
    ProjectGraph.trusted(List(module(ProjectName("core")), module(ProjectName("api"))))
end PerformRouteLawSpec
