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
  PluginLinked,
  PluginOutput,
  PluginRunError,
  PluginTaskId,
  RepoPath,
  RunRequest,
  Seal,
  TaskDep,
  TaskName,
  WrittenFile
}
import histos.resolve.{IdleResolve, Resolve}
import histos.schedule.{ActionCache, Digest, PluginManifest, RemoteCache}
import histos.task.{TaskId, Tasks}
import histos.*

import java.nio.file.{Files, Path}

import _root_.scala.jdk.CollectionConverters.*

import zio.{Promise, Ref, Scope, UIO, URIO, ZIO, ZLayer, durationInt}
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
      body: RunRequest => ZIO[Scope, PluginRunError, PluginOutput]
  ) extends HistosPlugin:
    def contribute: Contribution = value

    def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
      calls.update(_ + 1) *>
        seen.set(request.modules) *>
        argsSeen.update(_ :+ (request.id.render -> request.args)) *>
        body(request)

  private def task(
      name: TaskName,
      seal: Seal,
      outputs: List[DeclaredOutput],
      dependsOn: List[TaskDep]
  ): DeclaredTask =
    DeclaredTask(name, seal, Set.empty, Nil, outputs, dependsOn)

  private def script(
      id: PluginId,
      tasks: List[DeclaredTask],
      calls: Ref[Int],
      seen: Ref[List[ModuleView]],
      argsSeen: Ref[List[(String, PluginArgs)]],
      body: RunRequest => ZIO[Scope, PluginRunError, PluginOutput]
  ): Script =
    Script(id, Contribution(tasks, Nil, Nil), calls, seen, argsSeen, body)

  private def graph(
      plugin: HistosPlugin,
      docs: ModuleDef
  ): ZIO[Any, BuildError, ProjectGraph] =
    ZIO.fromEither(Projects(use(plugin), docs))

  private def runtime(
      repo: Path,
      built: ProjectGraph,
      plugin: HistosPlugin,
      links: List[PluginLink] = Nil
  ): URIO[Scope, PluginRuntime] =
    val parent = Option(getClass.getClassLoader).getOrElse(
      ClassLoader.getSystemClassLoader
    )
    PluginThreads
      .make(parent)
      .flatMap(threads =>
        PluginRuntime
          .loaded(repo, built, parent, threads, List(plugin), links, launcher)
      )

  private def jarsAt(jars: List[Path]): List[PluginLink] =
    List(
      PluginLink(
        echoId,
        PluginLinked.FromBinary(launcher),
        jars.map(_.toString)
      )
    )

  private def recording: UIO[(Ref[Map[String, String]], ActionCache)] =
    Ref
      .make(Map.empty[String, String])
      .map: values =>
        val cache: ActionCache = new ActionCache:
          def get(digest: Digest): UIO[Option[String]] =
            values.get.map(_.get(digest.value))
          override def put(digest: Digest, value: String): UIO[Unit] =
            values.update(_ + (digest.value -> value))
        (values, cache)

  private def directory: ZIO[Scope, Nothing, Path] =
    ZIO.acquireRelease(
      ZIO.attempt(Files.createTempDirectory("histos-plugin-run")).orDie
    )(delete)

  private def delete(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths
          .sortBy(_.getNameCount)
          .reverse
          .foreach(path => Files.deleteIfExists(path))
    }.ignore

  private def run(
      plugins: URIO[Scope, PluginRuntime],
      id: PluginTaskId,
      args: PluginArgs,
      cache: ActionCache,
      resolve: zio.ULayer[Resolve] = IdleResolve.layer
  ) =
    plugins.flatMap:
      _.run(id, args)
        .provide(ZLayer.succeed(cache), RemoteCache.off, resolve)

  private val core = module("core", Jvm)(scala"3.9.0")

  private val noted =
    core
      .note(echoId, NoteKey("site"), NoteText("docs"))
      .note(otherId, NoteKey("site"), NoteText("other"))

  private val plugin = suite("plugin run")(
    test(
      "plugin compile and classpath preparation share the in-flight lock generation"
    ):
      ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          name <- ZIO.fromEither(ProjectName.from("docs"))
          page = task(
            TaskName("echo"),
            Seal.Open,
            Nil,
            List(TaskDep.Compile(name, None, Some(Jvm)))
          )
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            _ => ZIO.succeed(PluginOutput("captured generation", Nil))
          )
          dependency = Dep("dev.fixture:gate:1.0.0")
          built <- graph(
            live,
            module("docs", Jvm)(scala"3.9.0").libraryDependencies(dependency)
          )
          dir <- directory
          _ <- ZIO.attempt {
            Files.createDirectories(dir.resolve("docs/src"))
            Files.writeString(
              dir.resolve("docs/src/Hello.scala"),
              "object Hello:\n  val value = 1\n"
            )
          }
          jar = dir.resolve("gate.jar")
          _ <- ZIO.attemptBlocking {
            val stream =
              new java.util.jar.JarOutputStream(Files.newOutputStream(jar))
            stream.close()
          }
          hash <- histos.toolchain.RuntimeFiles.sha256(jar)
          tools <- ZIO.service[histos.resolve.LockedTools]
          artifact = histos.resolve.LockedArtifact(
            "dev.fixture",
            "gate",
            "1.0.0",
            hash,
            histos.resolve.Repository.Central
          )
          before = histos.resolve.DependencyLock(
            List(
              histos.resolve.LockedCell(
                name,
                ScalaVersion("3.9.0"),
                Jvm,
                List(
                  histos.resolve
                    .LockedDirect("dev.fixture", "gate", "1.0.0", DepKind.Java)
                ),
                List(artifact),
                Nil
              )
            ),
            Nil,
            tools.lock.tools.filter(_.id.scalaVersion == ScalaVersion("3.9.0"))
          )
          after = before.copy(
            cells =
              before.cells.map(_.copy(scalaVersion = ScalaVersion("3.10.0"))),
            tools = tools.lock.tools.filter(
              _.id.scalaVersion == ScalaVersion("3.10.0")
            )
          )
          _ <- histos.workspace.Workspace.writeString(
            Resolve.lockPath(dir),
            histos.resolve.DependencyLock.render(before)
          )
          reached <- Promise.make[Nothing, Unit]
          proceed <- Promise.make[Nothing, Unit]
          resolver = new Resolve:
            def resolveTool(request: histos.resolve.ToolRequest) =
              tools.resolve.resolveTool(request)
            def resolve(cell: histos.resolve.ResolveCell) =
              tools.resolve.resolve(cell)
            def fetch(held: histos.resolve.LockedArtifact) =
              if held.organization == "dev.fixture" && held.name == "gate" then
                reached.succeed(()).unit *> proceed.await *>
                  ZIO.succeed(
                    histos.resolve.ResolvedArtifact(
                      held.organization,
                      held.name,
                      held.revision,
                      jar,
                      hash,
                      held.repository
                    )
                  )
              else tools.resolve.fetch(held)
            def resolvePlugin(
                dependency: Dep,
                repositories: histos.resolve.Repositories
            ) = tools.resolve.resolvePlugin(dependency, repositories)
            def sources(
                organization: String,
                name: String,
                revision: String,
                repository: histos.resolve.Repository
            ) = tools.resolve.sources(organization, name, revision, repository)
          (_, cache) <- recording
          running <- run(
            runtime(dir, built, live),
            PluginTaskId(echoId, TaskName("echo")),
            PluginArgs.empty,
            cache,
            ZLayer.succeed(resolver)
          ).forkScoped
          _ <- reached.await
          _ <- histos.workspace.Workspace.writeString(
            Resolve.lockPath(dir),
            histos.resolve.DependencyLock.render(after)
          )
          _ <- proceed.succeed(())
          result <- running.join
          entered <- calls.get
        yield assertTrue(
          result == PluginOutput("captured generation", Nil),
          entered == 1
        )
    ,
    test("a run sees only the notes of its own plugin"):
      ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          page = task(TaskName("echo"), Seal.Open, Nil, Nil)
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            _ => ZIO.succeed(PluginOutput("ok", Nil))
          )
          built <- graph(live, noted)
          dir <- directory
          (_, cache) <- recording
          out <- run(
            runtime(dir, built, live),
            PluginTaskId(echoId, TaskName("echo")),
            PluginArgs.empty,
            cache
          )
          views <- seen.get
        yield
          val notes = views.flatMap(_.notes)
          assertTrue(
            out.text == "ok",
            notes.map(_.plugin) == List(echoId),
            notes.map(_.value.value) == List("docs"),
            !notes.exists(_.plugin == otherId)
          )
    ,
    test(
      "a hermetic file is stored, a second run does not enter, and deleting the file does"
    ):
      ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          page = task(
            TaskName("echo"),
            Seal.Hermetic,
            List(DeclaredOutput(OutputName("page"), RepoPath("out.txt"))),
            Nil
          )
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            request =>
              ZIO
                .attempt(
                  Files.writeString(request.repo.resolve("out.txt"), "page")
                )
                .orDie
                .as {
                  PluginOutput(
                    "page",
                    List(WrittenFile(OutputName("page"), RepoPath("out.txt")))
                  )
                }
          )
          built <- graph(live, core)
          dir <- directory
          (values, cache) <- recording
          id = PluginTaskId(echoId, TaskName("echo"))
          first <- run(runtime(dir, built, live), id, PluginArgs.empty, cache)
          once <- calls.get
          stored <- values.get
          second <- run(runtime(dir, built, live), id, PluginArgs.empty, cache)
          still <- calls.get
          _ <- ZIO.attempt(Files.delete(dir.resolve("out.txt"))).orDie
          third <- run(runtime(dir, built, live), id, PluginArgs.empty, cache)
          again <- calls.get
        yield
          val blobs = stored.values.toList
          assertTrue(
            first.text == "page" && second.text == "page" && third.text == "page",
            once == 1 && still == 1 && again == 2,
            blobs match
              case only :: Nil =>
                only.startsWith(PluginManifest.Header) && only.contains(
                  "out.txt"
                )
              case _ => false
          )
    ,
    test("the same plugin jar at another path keeps the digest"):
      ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          page = task(TaskName("echo"), Seal.Hermetic, Nil, Nil)
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            _ => ZIO.succeed(PluginOutput("ok", Nil))
          )
          built <- graph(live, core)
          dir <- directory
          here <- directory
          there <- directory
          (first, second) <- ZIO.attempt {
            (
              Files.writeString(here.resolve("echo.jar"), "jar"),
              Files.writeString(there.resolve("echo.jar"), "jar")
            )
          }.orDie
          (values, cache) <- recording
          id = PluginTaskId(echoId, TaskName("echo"))
          _ <- run(
            runtime(dir, built, live, jarsAt(List(first))),
            id,
            PluginArgs.empty,
            cache
          )
          _ <- run(
            runtime(dir, built, live, jarsAt(List(second))),
            id,
            PluginArgs.empty,
            cache
          )
          entered <- calls.get
          stored <- values.get
        yield assertTrue(entered == 1, stored.size == 1)
    ,
    test(
      "two jars with one file name stay two inputs, and a jar listed twice is one"
    ):
      ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          page = task(TaskName("echo"), Seal.Hermetic, Nil, Nil)
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            _ => ZIO.succeed(PluginOutput("ok", Nil))
          )
          built <- graph(live, core)
          dir <- directory
          here <- directory
          there <- directory
          (first, second) <- ZIO.attempt {
            (
              Files.writeString(here.resolve("echo.jar"), "one"),
              Files.writeString(there.resolve("echo.jar"), "two")
            )
          }.orDie
          listed = List(first, second, first)
          id = PluginTaskId(echoId, TaskName("echo"))
          hashed <- PluginInputs.hash(
            PluginInputs.HashRequest(
              dir,
              built,
              echoId,
              page,
              id,
              listed.map(_.toString),
              launcher,
              Map.empty
            )
          )
          (_, cache) <- recording
          out <- run(
            runtime(dir, built, live, jarsAt(listed)),
            id,
            PluginArgs.empty,
            cache
          ).either
        yield
          val jars =
            hashed.inputs.filter(_.name.value.startsWith("plugin.jar:"))
          assertTrue(
            jars.size == 2,
            jars.map(_.name).distinct.size == 2,
            out == Right(PluginOutput("ok", Nil))
          )
    ,
    test("a missing declared file is not stored"):
      ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          page = task(
            TaskName("echo"),
            Seal.Hermetic,
            List(DeclaredOutput(OutputName("page"), RepoPath("out.txt"))),
            Nil
          )
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            _ => ZIO.succeed(PluginOutput("nope", Nil))
          )
          built <- graph(live, core)
          dir <- directory
          (values, cache) <- recording
          out <- run(
            runtime(dir, built, live),
            PluginTaskId(echoId, TaskName("echo")),
            PluginArgs.empty,
            cache
          ).either
          stored <- values.get
        yield assertTrue(
          out == Left(
            PluginFault.Task(PluginRunError.MissingOutput(OutputName("page")))
          ) && stored.isEmpty
        )
    ,
    test("an undeclared written path is not stored"):
      ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          page = task(TaskName("echo"), Seal.Hermetic, Nil, Nil)
          extra = WrittenFile(OutputName("page"), RepoPath("extra.txt"))
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            _ => ZIO.succeed(PluginOutput("x", List(extra)))
          )
          built <- graph(live, core)
          dir <- directory
          (values, cache) <- recording
          out <- run(
            runtime(dir, built, live),
            PluginTaskId(echoId, TaskName("echo")),
            PluginArgs.empty,
            cache
          ).either
          stored <- values.get
        yield assertTrue(
          out == Left(
            PluginFault.Task(PluginRunError.ExtraOutput("extra.txt"))
          ) && stored.isEmpty
        )
    ,
    test("a task that is not in the graph is unknown"):
      ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          page = task(TaskName("echo"), Seal.Open, Nil, Nil)
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            _ => ZIO.succeed(PluginOutput("ok", Nil))
          )
          built <- graph(live, core)
          dir <- directory
          (_, cache) <- recording
          missing = PluginTaskId(echoId, TaskName("missing"))
          out <- run(
            runtime(dir, built, live),
            missing,
            PluginArgs.empty,
            cache
          ).either
        yield assertTrue(
          out == Left(PluginFault.Task(PluginRunError.Unknown(missing)))
        )
    ,
    test("only the requested root receives the caller's arguments"):
      ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          prep = task(TaskName("prep"), Seal.Open, Nil, Nil)
          echo = task(
            TaskName("echo"),
            Seal.Open,
            Nil,
            List(TaskDep.Same(TaskName("prep")))
          )
          live = script(
            echoId,
            List(prep, echo),
            calls,
            seen,
            argsSeen,
            request => ZIO.succeed(PluginOutput(request.id.name.value, Nil))
          )
          built <- graph(live, core)
          dir <- directory
          (_, cache) <- recording
          passed = PluginArgs(
            List(NamedText(ParamName("out"), "docs")),
            Set.empty
          )
          _ <- run(
            runtime(dir, built, live),
            PluginTaskId(echoId, TaskName("echo")),
            passed,
            cache
          )
          seenArgs <- argsSeen.get
        yield assertTrue:
          seenArgs match
            case ("test.echo/prep", prepArgs) :: (
                  "test.echo/echo",
                  echoArgs
                ) :: Nil =>
              prepArgs == PluginArgs.empty && echoArgs == passed
            case _ => false
    ,
    test(
      "a compile dependency is compiled and its classpath is on the request"
    ):
      ZIO.withSystem(zio.System.SystemLive)(ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          name <- ZIO.fromEither(ProjectName.from("docs"))
          out = DeclaredOutput(
            OutputName.trusted("out"),
            RepoPath.trusted("out.txt")
          )
          page = task(
            TaskName("echo"),
            Seal.Open,
            List(out),
            List(TaskDep.Compile(name, None, Some(Jvm)))
          )
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            request => {
              val text = request.compiled
                .flatMap(_.compile)
                .map(_.toString)
                .mkString("\n")
              ZIO
                .attempt(
                  Files.writeString(request.repo.resolve("out.txt"), text)
                )
                .orDie *>
                ZIO.succeed(
                  PluginOutput(text, List(WrittenFile(out.name, out.path)))
                )
            }
          )
          docs = module("docs", Jvm)(scala"3.9.0")
          built <- graph(live, docs)
          dir <- directory
          _ <- ZIO.attempt {
            Files.createDirectories(dir.resolve(".histos"))
            Files.createDirectories(dir.resolve("docs/src/main/scala"))
            Files.writeString(
              dir.resolve("docs/src/main/scala/Hello.scala"),
              "object Hello:\n  def greet: String = \"hi\"\n"
            )
            Files.writeString(
              dir.resolve(".histos/dependencies.lock"),
              "cell docs 3.9.0 jvm\n"
            )
          }.orDie
          (_, cache) <- recording
          _ <- histos.ToolFixture.seed(dir, built)
          _ <- run(
            runtime(dir, built, live),
            PluginTaskId(echoId, TaskName("echo")),
            PluginArgs.empty,
            cache,
            Resolve.live(dir.resolve("coursier"))
          )
          text <- ZIO.attempt(Files.readString(dir.resolve("out.txt"))).orDie
          classes = dir
            .resolve(".histos")
            .resolve("classes")
            .resolve("docs")
            .resolve("3.9.0")
            .resolve("jvm")
          listed <- ZIO.attempt {
            val walk = Files.walk(classes)
            try
              walk.iterator.asScala
                .filter(Files.isRegularFile(_))
                .map(_.getFileName.toString)
                .mkString(",")
            finally walk.close()
          }.orDie
        yield assertTrue(
          text.contains(classes.toString),
          listed.contains("Hello.class")
        ))
    ,
    test("a compile error does not run the plugin"):
      ZIO.withSystem(zio.System.SystemLive)(ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          name <- ZIO.fromEither(ProjectName.from("docs"))
          page = task(
            TaskName("echo"),
            Seal.Open,
            Nil,
            List(TaskDep.Compile(name, None, Some(Jvm)))
          )
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            _ => ZIO.succeed(PluginOutput("ran", Nil))
          )
          docs = module("docs", Jvm)(scala"3.9.0")
          built <- graph(live, docs)
          dir <- directory
          _ <- ZIO.attempt {
            Files.createDirectories(dir.resolve(".histos"))
            Files.createDirectories(dir.resolve("docs/src/main/scala"))
            Files.writeString(
              dir.resolve("docs/src/main/scala/Hello.scala"),
              "object Hello:\n  val n: Int = \"no\"\n"
            )
            Files.writeString(
              dir.resolve(".histos/dependencies.lock"),
              "cell docs 3.9.0 jvm\n"
            )
          }.orDie
          (_, cache) <- recording
          _ <- histos.ToolFixture.seed(dir, built)
          ran <- run(
            runtime(dir, built, live),
            PluginTaskId(echoId, TaskName("echo")),
            PluginArgs.empty,
            cache,
            Resolve.live(dir.resolve("coursier"))
          ).either
          count <- calls.get
        yield
          val failed = ran.left.toOption.collect:
            case PluginFault.Compiled(
                  histos.zinc.GraphCompileError
                    .Failed(module, version, platform, _)
                ) =>
              module.value == "docs" && version.value == "3.9.0" && platform == Jvm
          assertTrue(failed.contains(true), count == 0))
    ,
    test("interrupting a parked plugin releases its scope and stores nothing"):
      ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          started <- Promise.make[Nothing, Unit]
          released <- Ref.make(false)
          page = task(TaskName("wait"), Seal.Hermetic, Nil, Nil)
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            _ =>
              ZIO.acquireRelease(started.succeed(()).unit)(_ =>
                released.set(true)
              ) *> ZIO.never
          )
          built <- graph(live, core)
          dir <- directory
          (values, cache) <- recording
          id = PluginTaskId(echoId, TaskName("wait"))
          fiber <- run(
            runtime(dir, built, live),
            id,
            PluginArgs.empty,
            cache
          ).fork
          _ <- started.await
          _ <- fiber.interrupt
          exit <- fiber.await
          done <- released.get
          stored <- values.get
        yield assertTrue(exit.isInterrupted && done && stored.isEmpty)
    ,
    test("cancelling the bound fiber releases the plugin scope"):
      ZIO.scoped:
        for
          calls <- Ref.make(0)
          seen <- Ref.make(List.empty[ModuleView])
          argsSeen <- Ref.make(List.empty[(String, PluginArgs)])
          started <- Promise.make[Nothing, Unit]
          released <- Ref.make(false)
          page = task(TaskName("wait"), Seal.Hermetic, Nil, Nil)
          live = script(
            echoId,
            List(page),
            calls,
            seen,
            argsSeen,
            _ =>
              ZIO.acquireRelease(started.succeed(()).unit)(_ =>
                released.set(true)
              ) *> ZIO.never
          )
          built <- graph(live, core)
          dir <- directory
          (values, cache) <- recording
          running <- runtime(dir, built, live)
          bound <- running.bind
            .provide(ZLayer.succeed(cache), RemoteCache.off, IdleResolve.layer)
          id = PluginTaskId(echoId, TaskName("wait"))
          result <-
            TaskId.from(id.render) match
              case Left(err)  => ZIO.fail(err)
              case Right(key) =>
                ZIO
                  .serviceWithZIO[Tasks]: tasks =>
                    for
                      startedRun <- tasks.run(key)
                      _ <- started.await
                      _ <- tasks.cancel(startedRun.id)
                      exit <- startedRun.await.exit
                      done <- released.get
                      stored <- values.get
                    yield assertTrue(
                      exit.isInterrupted && done && stored.isEmpty
                    )
                  .provideSome[Scope](Tasks.live(built, bound))
        yield result
  )

  def spec =
    plugin.provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      histos.ToolFixture.jvm(histos.ScalaVersion("3.9.0"))
    ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(90.seconds)
