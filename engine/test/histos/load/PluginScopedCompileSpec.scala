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
    test("actual coverage runs a generated JVM source/resource producer without broadening requested suites") {
      for
        calls    <- Ref.make(0)
        observed <- Ref.make(List.empty[CellClasspath])
        plugin      = CoverageProducer(calls, observed)
        measured    = ScalaVersion("2.13.18")
        unsupported = ScalaVersion("2.12.20")
        host        = ProjectName("host")
        graph <- ZIO.fromEither(
          Projects(
            module("host", Jvm)(version),
            module("app", Jvm, Js, Native)(measured, unsupported).clang(ClangMajor(21)).test(TestMain("demo.Check")),
          )
        )
        linked = graph.withPlugins(List(RegisteredPlugin(plugin.id, plugin.contribute)(plugin)))
        result <- histos.testrun.TestExecutionFixtures.withGraph(linked): fixture =>
          val effect =
            for
              _ <- Workspace.writeString(
                fixture.repo.resolve("host/src/demo/Host.scala"),
                "package demo\nobject Host { def value: Int = 1 }\n",
              )
              _ <- Workspace.writeString(fixture.repo.resolve("host/test/demo/Broken.scala"), "this must never compile")
              _ <- Workspace.writeString(
                fixture.repo.resolve("app/src/demo/Main.scala"),
                "package demo\nobject Main { def value: Int = Generated.value }\n",
              )
              _ <- Workspace.writeString(
                fixture.repo.resolve("app/test/jvm/demo/Check.scala"),
                """package demo
                |object Check {
                |  def main(args: Array[String]): Unit = {
                |    val text = Option(getClass.getResourceAsStream("/generated.txt")).fold("missing") { input =>
                |      try new String(input.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
                |      finally input.close()
                |    }
                |    println("generated=" + Main.value + ":" + text)
                |  }
                |}
                |""".stripMargin,
              )
              _ <- Workspace.writeString(fixture.repo.resolve("nonJvm-counter"), "0")
              _ <- ZIO.foreachDiscard(List("js", "native")): platform =>
                Workspace.writeString(
                  fixture.repo.resolve(s"app/test/$platform/demo/Check.scala"),
                  "package demo\nobject Check { def main(args: Array[String]): Unit = java.nio.file.Files.writeString(java.nio.file.Path.of(sys.env.getOrElse(\"HISTOS_REPO\", \".\"), \"nonJvm-counter\"), \"1\") }\n",
                )
              leases  <- GenerationLeases.make
              threads <- PluginThreads.make(getClass.getClassLoader)
              runtime <- PluginRuntime.loaded(
                fixture.repo,
                linked,
                getClass.getClassLoader,
                threads,
                Nil,
                ApiDigest.trusted("ab" * 32),
                leases,
                Nil,
              )
              heard   <- Ref.make(List.empty[String])
              outcome <- histos.coverage.Run.execute(
                histos.coverage.CoverageRequest(
                  fixture.repo,
                  linked,
                  List(name),
                  fixture.session.jdk,
                  fixture.runtimes,
                  CoverageFloors(statements = Some(100)),
                  histos.coverage.Excludes(Nil, Nil),
                  321L,
                ),
                histos.coverage.CoverageLines(line => heard.update(_ :+ line), _ => ZIO.unit),
                Some(runtime),
              )
              inventory <- histos.coverage.Gather.read(
                histos.coverage.Flags.classesDir(fixture.repo, name, measured, Jvm)
              )
              report = histos.coverage.Report.of(inventory._1, inventory._2)
              counter <- Workspace.readString(fixture.repo.resolve("nonJvm-counter"))
              solved  <- ResolutionService.open(fixture.repo, linked).flatMap(_.current)
              count   <- calls.get
              facts   <- observed.get
              lines   <- heard.get
              xml     <- Workspace.readString(fixture.repo.resolve(".histos/coverage/scoverage.xml"))
            yield assertTrue(
              outcome == histos.coverage.CoverageResult.Met(
                histos.coverage.Render.show(
                  report,
                  CoverageFloors(statements = Some(100)),
                  histos.coverage.Cell.skipped(linked.projects.collect {
                    case module: Module if module.name == name => module
                  }),
                )
              ),
              report.statements.total > 0,
              report.statements.hit == report.statements.total,
              inventory._2.nonEmpty,
              count == 1,
              lines.exists(_.contains("generated=42:resource")),
              counter == "0",
              solved.tools.nonEmpty,
              solved.tools.forall(_.id.variant == histos.resolve.ToolVariant.Jvm),
              solved.tools.forall(_.id.scalaVersion != unsupported),
              solved.cells.forall(_.platform == Jvm),
              facts.map(_.stage) == List(CompilationStage(Cell(host, version, Jvm), SourceSet.Main)),
              !solved.cells.exists(cell => cell.module == host && cell.scope == histos.resolve.ClasspathScope.Test),
              xml.contains("timestamp=\"321\""),
            )
          effect.provideSomeLayer[histos.testrun.TestExecutionFixtures.Env & Scope](PluginProducts.live)
      yield result
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
    test("BuildExecution shares Main, retains Test, separates modes and orders the task body") {
      val id   = PluginTaskId(PluginId("probe"), TaskName("modes"))
      val task = declared(
        id.name,
        List(
          TaskDep.CompileScoped(name, None, Some(Platform.Js), SourceSet.Test),
          TaskDep.Link(name, None, Some(JsOptimize.Fast)),
          TaskDep.Link(name, None, None),
          TaskDep.Link(name, None, Some(JsOptimize.Full)),
        ),
      )
      ZIO
        .fromEither(Projects(module("app", Js)(version).jsOptimize(JsOptimize.Full)))
        .flatMap: graph =>
          val plugin = ProbePlugin(task)
          val linked = graph.withPlugins(List(RegisteredPlugin(plugin.id, plugin.contribute)(plugin)))
          histos.testrun.TestExecutionFixtures.withGraph(linked): fixture =>
            for
              calls      <- Ref.make(List.empty[Callback])
              beforeBody <- Ref.make(Option.empty[List[Callback]])
              digest  = histos.jdk.Sha256Digest.of(zio.Chunk.empty)
              compile = compileLeaf(fixture.repo, calls, digest)
              leaves  = histos.build
                .ExecutionLeaves[histos.testrun.TestExecutionFixtures.Env, histos.build.ExecutionError](
                  compile,
                  compile,
                  (_, _, _) =>
                    calls.get.flatMap(found => beforeBody.set(Some(found))) *>
                      calls
                        .update(_ :+ Callback.Body)
                        .as(histos.build.GeneratedTask(PluginOutput("done", Nil, Nil), ZIO.succeed(Nil))),
                  (_, _, mode, _) =>
                    calls
                      .update(_ :+ Callback.Link(mode))
                      .as(histos.js.JsLinked(fixture.repo.resolve(mode.token), digest)),
                )
              result <- new histos.build.BuildExecution(leaves, fixture.scheduler).ensure(
                histos.build.ExecutionRequest(fixture.session, List(histos.build.BuildNode.Generate(id)))
              )
              output <- ZIO.fromEither(result.require(id))
              all    <- calls.get
              before <- beforeBody.get
            yield assertTrue(
              output.output.text == "done",
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

  private final case class CoverageProducer(count: Ref[Int], observed: Ref[List[CellClasspath]]) extends HistosPlugin:
    val id                       = PluginId("coverage-producer")
    private val source           = OutputName("source")
    private val resource         = OutputName("resource")
    def contribute: Contribution = Contribution(
      List(
        DeclaredTask(
          TaskName("generate"),
          Seal.Hermetic,
          Set.empty,
          Nil,
          List(
            DeclaredOutput.File(source, RepoPath("generated/Generated.scala")),
            DeclaredOutput.File(resource, RepoPath("generated/resource.txt")),
          ),
          List(
            TaskDep.CompileScoped(ProjectName("host"), Some(version), Some(Jvm), SourceSet.Main),
            TaskDep.EmitScoped(
              name,
              SourceSet.Main,
              PlatformSelection.only(Jvm),
              EmittedContent.SourceFile(ArtifactPath("demo/Generated.scala")),
              source,
            ),
            TaskDep.EmitScoped(
              name,
              SourceSet.Main,
              PlatformSelection.only(Jvm),
              EmittedContent.ResourceFile(ResourceName("generated.txt")),
              resource,
            ),
          ),
        )
      ),
      Nil,
      Nil,
    )
    def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      for
        _            <- count.update(_ + 1)
        _            <- observed.set(request.compiled)
        sourcePath   <- ZIO.fromEither(request.destinations.file(source))
        resourcePath <- ZIO.fromEither(request.destinations.file(resource))
        _            <- Workspace
          .writeString(sourcePath, "package demo\nobject Generated { def value: Int = 42 }\n")
          .mapError(fault => PluginRunError.Failed(PluginDetail.trusted(fault.message)))
        _ <- Workspace
          .writeString(resourcePath, "resource")
          .mapError(fault => PluginRunError.Failed(PluginDetail.trusted(fault.message)))
      yield PluginOutput(
        "generated",
        List(
          WrittenFile(source, RepoPath("generated/Generated.scala")),
          WrittenFile(resource, RepoPath("generated/resource.txt")),
        ),
        Nil,
      )

  private def compileLeaf(
      repo: Path,
      calls: Ref[List[Callback]],
      digest: histos.jdk.Sha256,
  ): (histos.build.ExecutionRequest, CompilationStage, histos.build.ExecutionProducts) => UIO[
    histos.build.StageProduct
  ] =
    (_, stage, _) =>
      val classes  = repo.resolve(s"${stage.scope}")
      val compiled = histos.zinc.CompiledCell(name, version, Js, classes, 0, None, false, None, None)
      calls
        .update(_ :+ Callback.Compile(stage.scope))
        .as(
          histos.build.StageProduct(stage, compiled, repo.resolve("analysis"), digest, digest, List(classes))
        )

  private final case class ProbePlugin(task: DeclaredTask) extends HistosPlugin:
    val id                       = PluginId("probe")
    def contribute: Contribution = Contribution(List(task), Nil, Nil)
    def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      ZIO.succeed(PluginOutput(request.id.name.value, Nil, Nil))

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
        PluginFault.Resolved(_) | PluginFault.Scheduled(_) | PluginFault.Scoped(_) | PluginFault.Linked(_) =>
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
