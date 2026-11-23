package histos.load

import histos.*
import histos.cache.ContentStore
import histos.plugin.*
import histos.resolve.{Resolve, ResolutionService}
import histos.schedule.{ActionCache, RemoteCache}
import histos.workspace.Workspace
import histos.zinc.GraphCompile

import java.nio.file.{Files, Path}

import zio.{Ref, Scope, UIO, ZIO, ZLayer, durationInt}
import histos.schedule.{Histos, Outcome, Parallelism}
import zio.test.*

object PluginScopedCompileSpec extends ZIOSpecDefault:
  private val name    = ProjectName("app")
  private val version = ScalaVersion("3.10.1")
  private val mainDep = TaskDep.CompileScoped(name, None, None, SourceSet.Main)
  private val testDep = TaskDep.CompileScoped(name, None, None, SourceSet.Test)
  private val legacy  = TaskDep.Compile(name, None, None)

  private val tests = suite("plugin scoped compile adapters")(
    test("a real Main-only task succeeds with invalid Test sources while test-complete tasks fail") {
      ZIO
        .scoped {
          for
            repo <- Workspace.work("plugin-scoped-compile")
            _    <- hostFacts
            _    <- write(repo.resolve("app/src/demo/Main.scala"), "package demo\nobject Main:\n  val value = 1\n")
            _    <- write(
              repo.resolve("app/test/demo/Broken.scala"),
              "package demo\nobject Broken:\n  val value: Int = \"invalid\"\n",
            )
            observed <- Ref.make(List.empty[CellClasspath])
            plugin = Probe(observed)
            graph <- ZIO.fromEither(Projects(module("app", Jvm)(version)))
            linked = graph.withPlugins(List(RegisteredPlugin(plugin.id, plugin.contribute)(plugin)))
            resolution <- ResolutionService.open(repo, linked)
            store      <- ZIO.attemptBlocking(Files.createDirectories(repo.resolve("store")))
            leases     <- GenerationLeases.make
            threads    <- PluginThreads.make(getClass.getClassLoader)
            runtime    <- PluginRuntime.loaded(
              repo,
              linked,
              getClass.getClassLoader,
              threads,
              Nil,
              ApiDigest.trusted("ab" * 32),
              leases,
              Nil,
            )
            result <- runtime
              .run(PluginTaskId(plugin.id, TaskName("main")), PluginArgs.empty)
              .either
              .provideSome[Resolve](
                ZLayer.succeed(resolution),
                ZLayer.succeed(ContentStore.in(store)),
                RemoteCache.off,
                ZLayer.succeed(ActionCache.miss),
              )
            facts  <- observed.get
            scoped <- runtime
              .run(PluginTaskId(plugin.id, TaskName("test")), PluginArgs.empty)
              .either
              .provideSome[Resolve](
                ZLayer.succeed(resolution),
                ZLayer.succeed(ContentStore.in(store)),
                RemoteCache.off,
                ZLayer.succeed(ActionCache.miss),
              )
            complete <- runtime
              .run(PluginTaskId(plugin.id, TaskName("legacy")), PluginArgs.empty)
              .either
              .provideSome[Resolve](
                ZLayer.succeed(resolution),
                ZLayer.succeed(ContentStore.in(store)),
                RemoteCache.off,
                ZLayer.succeed(ActionCache.miss),
              )
            classes = GraphCompile.classesDir(repo, name, version, Platform.Jvm)
            compiled <- ZIO.attemptBlocking(Files.isRegularFile(classes.resolve("demo/Main.class")))
            _        <- write(
              repo.resolve("app/test/demo/Broken.scala"),
              "package demo\nobject Broken:\n  val value: Int = 2\n",
            )
            valid <- runtime
              .run(PluginTaskId(plugin.id, TaskName("test")), PluginArgs.empty)
              .either
              .provideSome[Resolve](
                ZLayer.succeed(resolution),
                ZLayer.succeed(ContentStore.in(store)),
                RemoteCache.off,
                ZLayer.succeed(ActionCache.miss),
              )
            testFacts <- observed.get
            tests = histos.testrun.TestCompile.testClasses(repo, name, version, Platform.Jvm)
            testCompiled <- ZIO.attemptBlocking(Files.isRegularFile(tests.resolve("demo/Broken.class")))
          yield assertTrue(
            result.map(_.text) == Right("main"),
            compiled,
            facts.size == 1,
            facts.exists(fact =>
              fact.stage == CompilationStage(
                Cell(name, version, Platform.Jvm),
                SourceSet.Main,
              ) && fact.test.isEmpty && fact.compile.contains(classes)
            ),
            scoped.left.exists(isTestFailure),
            complete.left.exists(isTestFailure),
            valid.map(_.text) == Right("test"),
            testCompiled,
            testFacts.size == 1,
            testFacts.exists(fact =>
              fact.scope == SourceSet.Test && fact.compile.contains(classes) && fact.test.contains(classes) && fact.test
                .contains(tests)
            ),
          )
        }
        .provide(Resolve.coursier, PluginProducts.live)
    },
    test("selection merges Main into Test and emission metadata does not invent a product") {
      val selected = for
        graph   <- Projects(module("app", Jvm)(version))
        cells   <- PluginCells.selected(graph, declared(TaskName("merged"), List(mainDep, testDep, legacy)))
        emitted <- PluginCells.selected(
          graph,
          declared(
            TaskName("emitted"),
            List(
              TaskDep.Emit(name)
            ),
          ),
        )
      yield (cells, emitted)
      assertTrue(selected.exists { case (cells, emitted) =>
        cells.size == 1 && cells.forall(_.scope == SourceSet.Test) && emitted.isEmpty
      })
    },
    test("Main dependency identity ignores Test products while Test identity includes them") {
      ZIO.scoped {
        for
          repo  <- Workspace.work("plugin-scoped-identity")
          graph <- ZIO.fromEither(Projects(module("app", Jvm)(version)))
          mainTask = declared(TaskName("main"), List(mainDep))
          testTask = declared(TaskName("test"), List(testDep))
          classes  = GraphCompile.classesDir(repo, name, version, Platform.Jvm)
          tests    = histos.testrun.TestCompile.testClasses(repo, name, version, Platform.Jvm)
          _          <- write(classes.resolve("Main.class"), "main")
          _          <- write(tests.resolve("Test.class"), "before")
          mainBefore <- PluginInputs.compileInputs(repo, graph, mainTask)
          testBefore <- PluginInputs.compileInputs(repo, graph, testTask)
          _          <- write(tests.resolve("Test.class"), "after")
          mainAfter  <- PluginInputs.compileInputs(repo, graph, mainTask)
          testAfter  <- PluginInputs.compileInputs(repo, graph, testTask)
        yield assertTrue(
          mainBefore == mainAfter,
          testBefore != testAfter,
          mainBefore != testBefore,
          mainBefore.exists(_.name.value.endsWith(":main")),
          testBefore.exists(_.name.value.endsWith(":test")),
        )
      }
    },
    test("the actual executor shares Main, retains Test, separates modes and orders the task body") {
      ZIO.scoped {
        for
          repo       <- Workspace.work("plugin-step-modes").catchAll(error => ZIO.dieMessage(error.message))
          graph      <- ZIO.fromEither(Projects(module("app", Js)(version).jsOptimize(JsOptimize.Full)))
          calls      <- Ref.make(List.empty[Callback])
          beforeBody <- Ref.make(Option.empty[List[Callback]])
          id   = PluginTaskId(PluginId("probe"), TaskName("modes"))
          task = declared(
            id.name,
            List(
              TaskDep.CompileScoped(name, None, Some(Platform.Js), SourceSet.Test),
              TaskDep.Link(name, None, Some(JsOptimize.Fast)),
              TaskDep.Link(name, None, None),
              TaskDep.Link(name, None, Some(JsOptimize.Full)),
            ),
          )
          goal <- ZIO.fromEither(
            PluginSteps.goal(
              id,
              asked => Option.when(asked == id)(task),
              PluginCells.selected(graph, _),
              PluginCells.links(graph, _),
              cell => calls.update(_ :+ Callback.Compile(cell.scope)),
              request => calls.update(_ :+ Callback.Link(request.optimize)).as(request.optimize.token),
              (_, _, _) =>
                calls.get.flatMap(found => beforeBody.set(Some(found))) *> calls.update(_ :+ Callback.Body).as("done"),
            )
          )
          environment <- (Parallelism.live >>> Histos.live).build
          result      <- environment
            .get[Histos]
            .start(goal)
            .flatMap(_.outcome)
            .provideSome[Scope](Resolve.coursier, ZLayer.succeed(ContentStore.in(repo.resolve("cas"))))
          all    <- calls.get
          before <- beforeBody.get
        yield assertTrue(
          result == Outcome.Done("done"),
          all.count(_ == Callback.Compile(SourceSet.Main)) == 1,
          all.count(_ == Callback.Compile(SourceSet.Test)) == 1,
          all.count(_ == Callback.Link(JsOptimize.Fast)) == 1,
          all.count(_ == Callback.Link(JsOptimize.Full)) == 1,
          all.count(_ == Callback.Body) == 1,
          before.exists(found =>
            found.size == 4 && found.toSet == Set(
              Callback.Compile(SourceSet.Main),
              Callback.Compile(SourceSet.Test),
              Callback.Link(JsOptimize.Fast),
              Callback.Link(JsOptimize.Full),
            )
          ),
          all.lastOption.contains(Callback.Body),
        )
      }
    },
    test("link selection resolves tool, module, and task mode before scheduling") {
      val selected = for
        graph     <- Projects(module("app", Js)(version).jsOptimize(JsOptimize.Full))
        inherited <- PluginCells.links(graph, declared(TaskName("link"), List(TaskDep.Link(name, None, None))))
        explicit  <- PluginCells.links(
          graph,
          declared(TaskName("link"), List(TaskDep.Link(name, None, Some(JsOptimize.Full)))),
        )
        overrideMode <- PluginCells.links(
          graph,
          declared(TaskName("link"), List(TaskDep.Link(name, None, Some(JsOptimize.Fast)))),
        )
        defaults <- Projects.tools(
          ToolCatalog.default.copy(js = ToolCatalog.default.js.copy(optimize = JsOptimize.Full))
        )(module("app", Js)(version))
        toolMode <- PluginCells.links(
          defaults,
          declared(TaskName("link"), List(TaskDep.Link(name, None, None))),
        )
      yield (inherited, explicit, overrideMode, toolMode)
      assertTrue(selected.exists { case (inherited, explicit, overrideMode, toolMode) =>
        inherited == explicit && selectedMode(inherited, JsOptimize.Full) && selectedMode(explicit, JsOptimize.Full) &&
        selectedMode(overrideMode, JsOptimize.Fast) && selectedMode(toolMode, JsOptimize.Full)
      })
    },
  )

  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)

  private enum Callback:
    case Compile(scope: SourceSet)
    case Link(mode: JsOptimize)
    case Body

  private def selectedMode(found: List[PluginCells.Link], mode: JsOptimize): Boolean =
    found.size == 1 && found.exists(request =>
      request.optimize == mode && request.cell.module.name == name &&
        request.cell.version == version && request.cell.platform == Platform.Js && request.cell.scope == SourceSet.Main
    )

  private def declared(name: TaskName, deps: List[TaskDep]): DeclaredTask =
    DeclaredTask(name, Seal.Hermetic, Set.empty, Nil, Nil, deps)

  private final case class Probe(observed: Ref[List[CellClasspath]]) extends HistosPlugin:
    val id                       = PluginId("probe")
    def contribute: Contribution = Contribution(
      List(
        declared(TaskName("main"), List(mainDep)),
        declared(TaskName("test"), List(testDep)),
        declared(TaskName("legacy"), List(legacy)),
      ),
      Nil,
      Nil,
    )
    def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      observed.set(request.compiled).as(PluginOutput(request.id.name.value, Nil, Nil))

  private def isTestFailure(fault: PluginFault): Boolean = fault match
    case PluginFault.Tested(_) => true
    case PluginFault.Task(_) | PluginFault.Products(_) | PluginFault.MissingProduct(_) | PluginFault.Compiled(_) |
        PluginFault.Resolved(_) | PluginFault.Scheduled(_) =>
      false

  private def write(path: Path, text: String): UIO[Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, text)
      ()
    }.orDie

  private def hostFacts: UIO[Unit] =
    ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")): key =>
      Option(System.getProperty(key)) match
        case Some(value) if value.nonEmpty => TestSystem.putProperty(key, value)
        case _                             => ZIO.dieMessage(s"the test JVM has no $key")
end PluginScopedCompileSpec
