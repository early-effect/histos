package histos.load

import histos.plugin.{
  ApiDigest,
  Contribution,
  DeclaredOutput,
  DeclaredTask,
  HistosPlugin,
  ModuleView,
  NamedText,
  NoteKey,
  NoteText,
  OutputName,
  ParamName,
  PluginArgs,
  PluginId,
  PluginOutput,
  PluginRunError,
  PluginTaskId,
  RepoPath,
  RunRequest,
  Seal,
  TaskDep,
  TaskName,
  WrittenFile,
}
import histos.schedule.{ActionCache, Digest, PluginManifest, RemoteCache}
import histos.task.{TaskId, Tasks}
import histos.*

import java.nio.file.{Files, Path}

import _root_.scala.jdk.CollectionConverters.*

import zio.{Promise, Ref, Scope, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

object PluginRunSpec extends ZIOSpecDefault:
  private val launcher = ApiDigest.trusted("ab" * 32)
  private val echoId = PluginId("test.echo")
  private val otherId = PluginId("test.other")

  private final class Script(
      val id: PluginId,
      value: Contribution,
      calls: Ref[Int],
      seen: Ref[List[ModuleView]],
      argsSeen: Ref[List[(String, PluginArgs)]],
      body: RunRequest => ZIO[Scope, PluginRunError, PluginOutput],
  ) extends HistosPlugin:
    def contribute: Contribution = value

    def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
      calls.update(_ + 1) *>
        seen.set(request.modules) *>
        argsSeen.update(_ :+ (request.id.render -> request.args)) *>
        body(request)

  private def task(name: TaskName, seal: Seal, outputs: List[DeclaredOutput], dependsOn: List[TaskDep]): DeclaredTask =
    DeclaredTask(name, seal, Set.empty, Nil, outputs, dependsOn)

  private def script(
      id: PluginId,
      tasks: List[DeclaredTask],
      calls: Ref[Int],
      seen: Ref[List[ModuleView]],
      argsSeen: Ref[List[(String, PluginArgs)]],
      body: RunRequest => ZIO[Scope, PluginRunError, PluginOutput],
  ): Script =
    Script(id, Contribution(tasks, Nil, Nil), calls, seen, argsSeen, body)

  private def graph(plugin: HistosPlugin, docs: ModuleDef): UIO[ProjectGraph] =
    Projects(use(plugin), docs) match
      case Left(err)    => ZIO.dieMessage(err.message)
      case Right(built) => ZIO.succeed(built)

  private def runtime(repo: Path, built: ProjectGraph, plugin: HistosPlugin): PluginRuntime =
    val parent = Option(getClass.getClassLoader).getOrElse(ClassLoader.getSystemClassLoader)
    PluginRuntime.loaded(repo, built, parent, List(plugin), Nil, launcher)

  private def recording: UIO[(Ref[Map[String, String]], ActionCache)] =
    Ref.make(Map.empty[String, String]).map: values =>
      val cache: ActionCache = new ActionCache:
        def get(digest: Digest): UIO[Option[String]] = values.get.map(_.get(digest.value))
        override def put(digest: Digest, value: String): UIO[Unit] = values.update(_ + (digest.value -> value))
      (values, cache)

  private def directory: ZIO[Scope, Nothing, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-plugin-run")).orDie)(delete)

  private def delete(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.ignore

  private def run(plugins: PluginRuntime, id: PluginTaskId, args: PluginArgs, cache: ActionCache) =
    plugins.run(id, args).provide(ZLayer.succeed(cache), RemoteCache.off)

  private val core = module("core", Jvm)(scala"3.9.0")

  private val noted =
    core
      .note(echoId, NoteKey("site"), NoteText("docs"))
      .note(otherId, NoteKey("site"), NoteText("other"))

  private val plugin = suite("plugin run")(
    test("a run sees only the notes of its own plugin"):
      ZIO.scoped:
        for
          calls    <- Ref.make(0)
          seen     <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          page      = task(TaskName("echo"), Seal.Open, Nil, Nil)
          live      = script(echoId, List(page), calls, seen, argsSeen, _ => ZIO.succeed(PluginOutput("ok", Nil)))
          built    <- graph(live, noted)
          dir      <- directory
          (_, cache) <- recording
          out <- run(runtime(dir, built, live), PluginTaskId(echoId, TaskName("echo")), PluginArgs.empty, cache)
          views <- seen.get
        yield
          val notes = views.flatMap(_.notes)
          assertTrue(
            out.text == "ok",
            notes.map(_.plugin) == List(echoId),
            notes.map(_.value.value) == List("docs"),
            !notes.exists(_.plugin == otherId),
          )
    ,
    test("a hermetic file is stored, a second run does not enter, and deleting the file does"):
      ZIO.scoped:
        for
          calls    <- Ref.make(0)
          seen     <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          page      = task(TaskName("echo"), Seal.Hermetic, List(DeclaredOutput(OutputName("page"), RepoPath("out.txt"))), Nil)
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            request =>
              ZIO.attempt(Files.writeString(request.repo.resolve("out.txt"), "page")).orDie.as {
                PluginOutput("page", List(WrittenFile(OutputName("page"), RepoPath("out.txt"))))
              },
          )
          built <- graph(live, core)
          dir   <- directory
          (values, cache) <- recording
          id     = PluginTaskId(echoId, TaskName("echo"))
          first <- run(runtime(dir, built, live), id, PluginArgs.empty, cache)
          once  <- calls.get
          stored <- values.get
          second <- run(runtime(dir, built, live), id, PluginArgs.empty, cache)
          still  <- calls.get
          _      <- ZIO.attempt(Files.delete(dir.resolve("out.txt"))).orDie
          third  <- run(runtime(dir, built, live), id, PluginArgs.empty, cache)
          again  <- calls.get
        yield
          val blobs = stored.values.toList
          assertTrue(
            first.text == "page" && second.text == "page" && third.text == "page",
            once == 1 && still == 1 && again == 2,
            blobs match
              case only :: Nil => only.startsWith(PluginManifest.Header) && only.contains("out.txt")
              case _           => false,
          )
    ,
    test("a missing declared file is not stored"):
      ZIO.scoped:
        for
          calls    <- Ref.make(0)
          seen     <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          page      = task(TaskName("echo"), Seal.Hermetic, List(DeclaredOutput(OutputName("page"), RepoPath("out.txt"))), Nil)
          live      = script(echoId, List(page), calls, seen, argsSeen, _ => ZIO.succeed(PluginOutput("nope", Nil)))
          built    <- graph(live, core)
          dir      <- directory
          (values, cache) <- recording
          out <- run(runtime(dir, built, live), PluginTaskId(echoId, TaskName("echo")), PluginArgs.empty, cache).either
          stored <- values.get
        yield assertTrue(out == Left(PluginRunError.MissingOutput(OutputName("page"))) && stored.isEmpty)
    ,
    test("an undeclared written path is not stored"):
      ZIO.scoped:
        for
          calls    <- Ref.make(0)
          seen     <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          page      = task(TaskName("echo"), Seal.Hermetic, Nil, Nil)
          extra     = WrittenFile(OutputName("page"), RepoPath("extra.txt"))
          live      = script(echoId, List(page), calls, seen, argsSeen, _ => ZIO.succeed(PluginOutput("x", List(extra))))
          built    <- graph(live, core)
          dir      <- directory
          (values, cache) <- recording
          out <- run(runtime(dir, built, live), PluginTaskId(echoId, TaskName("echo")), PluginArgs.empty, cache).either
          stored <- values.get
        yield assertTrue(out == Left(PluginRunError.ExtraOutput("extra.txt")) && stored.isEmpty)
    ,
    test("a task that is not in the graph is unknown"):
      ZIO.scoped:
        for
          calls    <- Ref.make(0)
          seen     <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          page      = task(TaskName("echo"), Seal.Open, Nil, Nil)
          live      = script(echoId, List(page), calls, seen, argsSeen, _ => ZIO.succeed(PluginOutput("ok", Nil)))
          built    <- graph(live, core)
          dir      <- directory
          (_, cache) <- recording
          missing    = PluginTaskId(echoId, TaskName("missing"))
          out <- run(runtime(dir, built, live), missing, PluginArgs.empty, cache).either
        yield assertTrue(out == Left(PluginRunError.Unknown(missing)))
    ,
    test("only the requested root receives the caller's arguments"):
      ZIO.scoped:
        for
          calls    <- Ref.make(0)
          seen     <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          prep      = task(TaskName("prep"), Seal.Open, Nil, Nil)
          echo      = task(TaskName("echo"), Seal.Open, Nil, List(TaskDep.Same(TaskName("prep"))))
          live = script(
            echoId,
            List(prep, echo),
            calls,
            seen,
            argsSeen,
            request => ZIO.succeed(PluginOutput(request.id.name.value, Nil)),
          )
          built <- graph(live, core)
          dir   <- directory
          (_, cache) <- recording
          passed = PluginArgs(List(NamedText(ParamName("out"), "docs")), Set.empty)
          _     <- run(runtime(dir, built, live), PluginTaskId(echoId, TaskName("echo")), passed, cache)
          seenArgs <- argsSeen.get
        yield assertTrue:
          seenArgs match
            case ("test.echo/prep", prepArgs) :: ("test.echo/echo", echoArgs) :: Nil =>
              prepArgs == PluginArgs.empty && echoArgs == passed
            case _ => false
    ,
    test("interrupting a parked plugin releases its scope and stores nothing"):
      ZIO.scoped:
        for
          calls    <- Ref.make(0)
          seen     <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          started  <- Promise.make[Nothing, Unit]
          released <- Ref.make(false)
          page      = task(TaskName("wait"), Seal.Hermetic, Nil, Nil)
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            _ => ZIO.acquireRelease(started.succeed(()).unit)(_ => released.set(true)) *> ZIO.never,
          )
          built <- graph(live, core)
          dir   <- directory
          (values, cache) <- recording
          id     = PluginTaskId(echoId, TaskName("wait"))
          fiber <- run(runtime(dir, built, live), id, PluginArgs.empty, cache).fork
          _     <- started.await
          _     <- fiber.interrupt
          exit  <- fiber.await
          done  <- released.get
          stored <- values.get
        yield assertTrue(exit.isInterrupted && done && stored.isEmpty)
    ,
    test("cancelling the bound fiber releases the plugin scope"):
      ZIO.scoped:
        for
          calls    <- Ref.make(0)
          seen     <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          started  <- Promise.make[Nothing, Unit]
          released <- Ref.make(false)
          page      = task(TaskName("wait"), Seal.Hermetic, Nil, Nil)
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            _ => ZIO.acquireRelease(started.succeed(()).unit)(_ => released.set(true)) *> ZIO.never,
          )
          built <- graph(live, core)
          dir   <- directory
          (values, cache) <- recording
          running = runtime(dir, built, live)
          bound <- running.bind.provide(ZLayer.succeed(cache), RemoteCache.off)
          id     = PluginTaskId(echoId, TaskName("wait"))
          result <-
            TaskId.from(id.render) match
              case Left(err) => ZIO.dieMessage(err.message)
              case Right(key) =>
                ZIO.serviceWithZIO[Tasks]: tasks =>
                  for
                    startedRun <- tasks.run(key)
                    _          <- started.await
                    _          <- tasks.cancel(startedRun.id)
                    exit       <- startedRun.await.exit
                    done       <- released.get
                    stored     <- values.get
                  yield assertTrue(exit.isInterrupted && done && stored.isEmpty)
                .provideSome[Scope](Tasks.live(built, bound))
        yield result
    ,
  )

  def spec = plugin @@ TestAspect.timeout(30.seconds)
