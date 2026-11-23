package histos.zinc

import histos.*
import histos.build.{
  BuildDag,
  BuildDagError,
  BuildEnv,
  BuildExecution,
  BuildNode as Node,
  ExecutionError,
  ExecutionLeaves,
  ExecutionProducts,
  ExecutionRequest,
  GeneratedTask,
  StageProduct,
}
import histos.cache.{ContentStore, LinkerStarts}
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256Digest}
import histos.js.{JsLink, JsLinkRequest, JsLinked}
import histos.load.{PluginCompileSupport, PluginFault}
import histos.native.NativeOrigin
import histos.plugin.*
import histos.resolve.{LockedTools, Resolve, ResolutionService}
import histos.schedule.{ActionCache, Histos, Parallelism, RemoteCache}
import histos.toolchain.RuntimeIdentity
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object GeneratedDagSpec extends ZIOSpecDefault:
  private val version = ScalaVersion("3.10.1")
  private type Env = BuildEnv & LinkerStarts

  private def stage(name: ProjectName, platform: Platform, scope: SourceSet): CompilationStage =
    CompilationStage(Cell(name, version, platform), scope)

  private val tests = suite("generated compilation DAG")(
    test("a cold named consumer builds its host File producer after linking the relay") {
      ZIO
        .scoped {
          for
            repo <- Workspace.work("histos-cold-generated-dag")
            _    <- Workspace.writeString(
              repo.resolve("consumer/src/demo/Consumer.scala"),
              "package demo\nobject Consumer:\n  def text: String = Generated.text\n",
            )
            _ <- Workspace.writeString(
              repo.resolve("relay/src/demo/Relay.scala"),
              "package demo\nobject Relay:\n  def main(args: Array[String]): Unit = println(\"relay\")\n",
            )
            order <- Ref.make(List.empty[String])
            feeds <- Ref.make(List.empty[CompileFeed])
            plugin = Relay(order)
            host   = module("host", Jvm)(version)
            graph <- ZIO.fromEither(
              Projects(
                host,
                module("consumer", Jvm)(version).dependsOn(host),
                module("relay", Js)(version).main(MainClass("demo.Relay")),
              )
            )
            linked = graph.withPlugins(List(RegisteredPlugin(plugin.id, plugin.contribute)(plugin)))
            result <- execute(
              repo,
              linked,
              List(Node.Compile(stage(ProjectName("consumer"), Platform.Jvm, SourceSet.Main))),
              feed => feeds.update(_ :+ feed),
            ) { (request, id, products) =>
              for
                script <- ZIO.fromEither(
                  products.require(Cell(ProjectName("relay"), version, Platform.Js), JsOptimize.Fast)
                )
                text <- Workspace.readString(script.script).mapError(io)
                _ <- if text.contains("relay") then ZIO.unit else ZIO.fail(ExecutionError.Dependency(Node.Generate(id)))
                generated <- invokeFixture(plugin, request, id, products)
              yield generated
            }
            observed <- order.get
            reported <- feeds.get
            consumer <- ZIO.fromEither(result.require(stage(ProjectName("consumer"), Platform.Jvm, SourceSet.Main)))
            output   <- PluginCompileSupport.invoke(consumer.classes, consumer.classpath, "demo.Consumer$", "text")
            packaged <- StageRoots.of(linked).read(repo, stage(ProjectName("host"), Platform.Jvm, SourceSet.Main))
          yield assertTrue(
            output == "relay",
            observed == List("relay"),
            result.nodes.contains(Node.Link(Cell(ProjectName("relay"), version, Platform.Js), JsOptimize.Fast)),
            packaged.sources.map(_.entry) == List("demo/Generated.scala"),
            consumer.compiled.sources.exists(_.compiled == 1),
            reported.exists {
              case CompileFeed.Pulse(name, _, _, _) => name == ProjectName("consumer")
              case _                                => false
            },
          )
        }
        .provide(Resolve.coursier)
    },
    test("the docs diamond shares a File producer and Main ignores broken Test sources") {
      ZIO
        .scoped {
          for
            repo <- Workspace.work("histos-docs-generated-diamond")
            _    <- Workspace.writeString(
              repo.resolve("docs/src/jvm/demo/Transcript.scala"),
              "package demo\nobject Transcript:\n  def render: String = \"package demo\\nobject Generated:\\n  val text: String = \\\"transcript\\\"\\n\"\n",
            )
            _ <- Workspace.writeString(
              repo.resolve("docs/src/js/demo/Client.scala"),
              "package demo\nobject Client:\n  val text: String = Generated.text\n",
            )
            broken = repo.resolve("docs/test/demo/Check.scala")
            _     <- Workspace.writeString(broken, "package demo\nobject Check:\n  val text: Int = \"broken\"\n")
            count <- Ref.make(0)
            plugin = Transcripts(count)
            graph <- ZIO.fromEither(Projects(module("docs", Jvm, Js)(version)))
            linked   = graph.withPlugins(List(RegisteredPlugin(plugin.id, plugin.contribute)(plugin)))
            producer = PluginTaskId(plugin.id, TaskName("transcript"))
            mainOnly <- execute(repo, linked, List(Node.Generate(producer))) { (request, id, products) =>
              invokeFixture(plugin, request, id, products)
            }
            untouched <- ZIO
              .attemptBlocking(!Files.exists(repo.resolve(".histos/test-classes/docs")))
              .mapError(_ => IoFailure(broken, IoFault.Stat))
            _ <- Workspace.writeString(broken, "package demo\nobject Check:\n  def text: String = Generated.text\n")
            _ <- Workspace.delete(repo.resolve("target/transcripts/Generated.scala"))
            _ <- count.set(0)
            testStage = stage(ProjectName("docs"), Platform.Jvm, SourceSet.Test)
            jsStage   = stage(ProjectName("docs"), Platform.Js, SourceSet.Main)
            result <- execute(
              repo,
              linked,
              List(Node.Compile(jsStage), Node.Generate(producer), Node.Compile(testStage)),
            ) { (request, id, products) =>
              invokeFixture(plugin, request, id, products)
            }
            compiled  <- ZIO.fromEither(result.require(testStage))
            js        <- ZIO.fromEither(result.require(jsStage))
            text      <- PluginCompileSupport.invoke(compiled.classes, compiled.classpath, "demo.Check$", "text")
            producers <- count.get
            missing = result.require(stage(ProjectName("docs"), Platform.Js, SourceSet.Test))
          yield assertTrue(
            mainOnly.nodes == Set(
              Node.Generate(producer),
              Node.Compile(stage(ProjectName("docs"), Platform.Jvm, SourceSet.Main)),
            ),
            untouched,
            text == "transcript",
            js.compiled.classFiles > 0,
            producers == 1,
            compiled.compiled.capture match
              case CompilationCapture.Recorded(snapshot) =>
                snapshot.sources.forall(_.set == SourceSet.Test) &&
                snapshot.sources.exists(_.provenance == SourceProvenance.Generated)
              case CompilationCapture.Unrecorded => false,
            missing == Left(
              ExecutionError.MissingProduct(Node.Compile(stage(ProjectName("docs"), Platform.Js, SourceSet.Test)))
            ),
          )
        }
        .provide(Resolve.coursier)
    },
    test("Main resources are copied after class restore and stale owned entries disappear on a warm hit") {
      ZIO
        .scoped {
          for
            repo <- Workspace.work("histos-stage-resource-restore")
            _    <- Workspace.writeString(repo.resolve("app/src/demo/Reader.scala"), readerSource)
            stale = repo.resolve("assets/main/stale.txt")
            _    <- Workspace.writeString(stale, "stale")
            text <- Ref.make("first")
            plugin = ResourceAsset(text)
            graph <- ZIO.fromEither(
              Projects(
                module("app", Jvm)(version)
                  .roots(ScopedRoot(RepoPath("assets/main"), SourceSet.Main, PlatformSelection.All, RootKind.Resources))
              )
            )
            linked = graph.withPlugins(List(RegisteredPlugin(plugin.id, plugin.contribute)(plugin)))
            main   = stage(ProjectName("app"), Platform.Jvm, SourceSet.Main)
            first <- execute(repo, linked, List(Node.Compile(main))) { (request, id, products) =>
              invokeFixture(plugin, request, id, products)
            }
            before   <- ZIO.fromEither(first.require(main))
            one      <- PluginCompileSupport.invoke(before.classes, before.classpath, "demo.Reader$", "text")
            _        <- Workspace.delete(before.classes)
            _        <- text.set("second")
            restored <- execute(repo, linked, List(Node.Compile(main))) { (request, id, products) =>
              invokeFixture(plugin, request, id, products)
            }
            second <- ZIO.fromEither(restored.require(main))
            two    <- PluginCompileSupport.invoke(second.classes, second.classpath, "demo.Reader$", "text")
            _      <- Workspace.delete(stale)
            warm   <- execute(repo, linked, List(Node.Compile(main))) { (request, id, products) =>
              invokeFixture(plugin, request, id, products)
            }
            third   <- ZIO.fromEither(warm.require(main))
            removed <- ZIO
              .attemptBlocking(!Files.exists(third.classes.resolve("stale.txt")))
              .mapError(_ => IoFailure(third.classes, IoFault.Stat))
          yield assertTrue(
            one == "first",
            two == "second",
            !second.compiled.changed,
            second.compiled.sources.contains(SourceCount(0, 1)),
            third.compiled.sources.contains(SourceCount(0, 1)),
            before.implementation != second.implementation,
            removed,
          )
        }
        .provide(Resolve.coursier)
    },
    test("legacy Compile is test-complete and a broken Test source prevents its task from running") {
      ZIO
        .scoped {
          for
            repo <- Workspace.work("histos-legacy-test-complete")
            _    <- Workspace
              .writeString(repo.resolve("docs/src/demo/Main.scala"), "package demo\nobject Main:\n  val value = 1\n")
            _ <- Workspace.writeString(
              repo.resolve("docs/test/demo/Broken.scala"),
              "package demo\nobject Broken:\n  val value: Int = \"broken\"\n",
            )
            count <- Ref.make(0)
            plugin = Transcripts(count)
            graph <- ZIO.fromEither(Projects(module("docs", Jvm)(version)))
            contribution = Contribution(
              List(
                DeclaredTask(
                  TaskName("legacy"),
                  Seal.Hermetic,
                  Set.empty,
                  Nil,
                  Nil,
                  List(TaskDep.Compile(ProjectName("docs"), Some(version), Some(Platform.Jvm))),
                )
              ),
              Nil,
              Nil,
            )
            linked = graph.withPlugins(List(RegisteredPlugin(plugin.id, contribution)(plugin)))
            result <- execute(repo, linked, List(Node.Generate(PluginTaskId(plugin.id, TaskName("legacy"))))) {
              (_, id, _) => count.update(_ + 1) *> ZIO.fail(ExecutionError.Dependency(Node.Generate(id)))
            }.either
            calls <- count.get
            classes = GraphCompile.classesDir(repo, ProjectName("docs"), version, Platform.Jvm)
            main <- ZIO
              .attemptBlocking(Files.isRegularFile(classes.resolve("demo/Main.class")))
              .mapError(_ => IoFailure(classes, IoFault.Stat))
          yield assertTrue(
            result.left.exists {
              case ExecutionError.Compile(GraphCompileError.Failed(module, _, Platform.Jvm, CompileError.Failed(_))) =>
                module == ProjectName("docs")
              case _ => false
            },
            main,
            calls == 0,
          )
        }
        .provide(Resolve.coursier)
    },
    test("generated read leases outlive node staging and close with the caller's scope") {
      ZIO
        .scoped {
          for
            repo  <- Workspace.work("histos-generated-read-lease")
            order <- Ref.make(List.empty[String])
            plugin = Relay(order)
            graph <- ZIO.fromEither(Projects(module("host", Jvm)(version)))
            contribution = plugin.contribute.copy(tasks =
              plugin.contribute.tasks.map(task =>
                task.copy(dependsOn = task.dependsOn.filter {
                  case TaskDep.Link(_, _, _) => false
                  case TaskDep.Compile(_, _, _) | TaskDep.CompileScoped(_, _, _, _) | TaskDep.Emit(_) |
                      TaskDep.EmitScoped(_, _, _, _, _) | TaskDep.Same(_) | TaskDep.Generate(_) =>
                    true
                })
              )
            )
            linked = graph.withPlugins(List(RegisteredPlugin(plugin.id, contribution)(plugin)))
            stagedClosed <- zio.Promise.make[Nothing, Unit]
            readOpened   <- zio.Promise.make[Nothing, Unit]
            readClosed   <- zio.Promise.make[Nothing, Unit]
            observed     <- ZIO.scoped {
              execute(repo, linked, List(Node.Compile(stage(ProjectName("host"), Platform.Jvm, SourceSet.Main)))) {
                (request, id, _) =>
                  val physical  = request.session.repo.resolve(".histos/view/Generated.scala")
                  val locations =
                    List(GeneratedLocation(GeneratedRootId(id, OutputName("source")), OutputKind.File, physical))
                  val read = ZIO.acquireRelease(readOpened.succeed(()).as(locations))(_ => readClosed.succeed(()).unit)
                  for
                    _ <- ZIO.addFinalizer(stagedClosed.succeed(()).unit)
                    _ <- Workspace
                      .writeString(physical, "package demo\nobject Generated:\n  def text: String = \"leased\"\n")
                      .mapError(io)
                  yield GeneratedTask(
                    PluginOutput(
                      "leased",
                      List(WrittenFile(OutputName("source"), RepoPath("target/generated/Relay.scala"))),
                      Nil,
                    ),
                    read,
                  )
              }.flatMap { products =>
                for
                  host   <- ZIO.fromEither(products.require(stage(ProjectName("host"), Platform.Jvm, SourceSet.Main)))
                  text   <- PluginCompileSupport.invoke(host.classes, host.classpath, "demo.Generated$", "text")
                  opened <- readOpened.isDone
                  closed <- readClosed.isDone
                  staged <- stagedClosed.isDone
                yield (text, opened, closed, staged)
              }
            }
            closed <- readClosed.isDone
          yield assertTrue(observed == ("leased", true, false, true), closed)
        }
        .provide(Resolve.coursier)
    },
    test("a source-free Native aggregate has an actual empty Main product, not fabricated member cells") {
      ZIO
        .scoped {
          for
            repo <- Workspace.work("histos-source-free-main")
            first  = module("first", Native)(version).clang(ClangMajor(21))
            second = module("second", Native)(version).clang(ClangMajor(21))
            owner  = module("nativeTests", Native)(version).clang(ClangMajor(21)).testAggregate(first, second)
            graph <- ZIO.fromEither(Projects(first, second, owner))
            ownerStage = stage(ProjectName("nativeTests"), Platform.Native, SourceSet.Main)
            result <- execute(repo, graph, List(Node.Compile(ownerStage))) { (_, id, _) =>
              ZIO.fail(ExecutionError.MissingProduct(Node.Generate(id)))
            }
            product <- ZIO.fromEither(result.require(ownerStage))
            present <- ZIO
              .attemptBlocking(Files.isDirectory(product.classes))
              .mapError(_ => IoFailure(product.classes, IoFault.Stat))
          yield assertTrue(
            present,
            product.compiled.classFiles == 0,
            product.compiled.sources.contains(SourceCount(0, 0)),
            product.api == histos.zinc.ApiDigest.empty,
            result.nodes == Set(Node.Compile(ownerStage)),
            result.require(stage(ProjectName("first"), Platform.Native, SourceSet.Main)) ==
              Left(
                ExecutionError.MissingProduct(
                  Node.Compile(stage(ProjectName("first"), Platform.Native, SourceSet.Main))
                )
              ),
          )
        }
        .provide(Resolve.coursier)
    },
    test("interrupting an invocation closes its running producer scope") {
      ZIO
        .scoped {
          for
            repo <- Workspace.work("histos-dag-interruption")
            _    <- Workspace.writeString(
              repo.resolve("docs/src/jvm/demo/Transcript.scala"),
              "package demo\nobject Transcript:\n  def render: String = \"transcript\"\n",
            )
            count <- Ref.make(0)
            plugin = Transcripts(count)
            graph <- ZIO.fromEither(Projects(module("docs", Jvm, Js)(version)))
            linked = graph.withPlugins(List(RegisteredPlugin(plugin.id, plugin.contribute)(plugin)))
            started  <- zio.Promise.make[Nothing, Unit]
            released <- zio.Promise.make[Nothing, Unit]
            fiber    <- execute(repo, linked, List(Node.Generate(PluginTaskId(plugin.id, TaskName("transcript"))))) {
              (_, _, _) => ZIO.addFinalizer(released.succeed(()).unit) *> started.succeed(()).unit *> ZIO.never
            }.forkScoped
            _      <- started.await
            exit   <- fiber.interrupt
            closed <- released.isDone
          yield assertTrue(exit.isInterrupted, closed)
        }
        .provide(Resolve.coursier)
    },
    test("an indirect compile/link/generate cycle is refused before a leaf can run") {
      for
        order <- Ref.make(List.empty[String])
        graph <- ZIO.fromEither(Projects(module("host", Jvm)(version), module("relay", Js)(version)))
        plugin       = Relay(order)
        contribution = plugin.contribute.copy(tasks =
          plugin.contribute.tasks :+ DeclaredTask(
            TaskName("relay-source"),
            Seal.Hermetic,
            Set.empty,
            Nil,
            List(DeclaredOutput.File(OutputName("relay-source"), RepoPath("target/relay/Main.scala"))),
            List(
              TaskDep.CompileScoped(ProjectName("host"), None, Some(Platform.Jvm), SourceSet.Main),
              TaskDep.EmitScoped(
                ProjectName("relay"),
                SourceSet.Main,
                PlatformSelection.only(Platform.Js),
                EmittedContent.SourceFile(ArtifactPath("demo/Relay.scala")),
                OutputName("relay-source"),
              ),
            ),
          )
        )
        linked = graph.withPlugins(List(RegisteredPlugin(plugin.id, contribution)(plugin)))
        plan   = BuildDag.plan(linked, List(Node.Compile(stage(ProjectName("host"), Platform.Jvm, SourceSet.Main))))
        observed <- order.get
      yield assertTrue(
        plan.left.exists {
          case BuildDagError.Declaration(BuildError.Contract(ContractFault.Cycle(path))) =>
            path.exists {
              case histos.BuildNode.Link(_)                               => true
              case histos.BuildNode.Compile(_) | histos.BuildNode.Task(_) => false
            }
          case _ => false
        },
        observed.isEmpty,
      )
    },
  )

  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)

  private def execute(
      repo: Path,
      graph: ProjectGraph,
      roots: List[Node],
      report: CompileFeed => zio.UIO[Unit] = _ => ZIO.unit,
  )(
      producer: (ExecutionRequest, PluginTaskId, ExecutionProducts) => ZIO[Env & Scope, ExecutionError, GeneratedTask[
        ExecutionError
      ]]
  ): ZIO[Resolve & Scope, ExecutionError | histos.resolve.ResolveError | IoFailure, ExecutionProducts] =
    for
      _ <- ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")) { key =>
        ZIO.foreachDiscard(sys.props.get(key))(TestSystem.putProperty(key, _))
      }
      resolution <- ResolutionService.open(repo, graph)
      lock       <- resolution.current
      cas        <- Workspace.createDirectories(repo.resolve("cas"))
      jdk        <- jdkRuntime
      scheduler  <- Histos.live.build.provideSomeLayer[Scope](ZLayer.succeed(Parallelism.default)).map(_.get[Histos])
      leaves = ExecutionLeaves[Env, ExecutionError](GraphCompile.stage, testLeaf, producer, linkLeaf)
      products <- new BuildExecution(leaves, scheduler)
        .ensure(
          ExecutionRequest(
            CompileSession(repo, graph, jdk, NativeOrigin.Central, lock),
            roots,
            report,
          )
        )
        .provideSome[Resolve & Scope](
          ZLayer.succeed(resolution),
          ZLayer.succeed(ContentStore.in(cas)),
          ZLayer.succeed(ActionCache.miss),
          RemoteCache.off,
          CompilerStarts.live,
          Compilers.live,
          CompileMemo.live,
          LinkerStarts.live,
        )
    yield products

  private def linkLeaf(
      request: ExecutionRequest,
      cell: Cell,
      mode: JsOptimize,
      products: ExecutionProducts,
  ): ZIO[Env & Scope, ExecutionError, JsLinked] =
    for
      main   <- ZIO.fromEither(products.require(CompilationStage(cell, SourceSet.Main)))
      module <- ZIO
        .fromOption(request.session.graph.projects.collectFirst {
          case module: Module if module.name == cell.module => module
        })
        .orElseFail(ExecutionError.MissingProduct(Node.Link(cell, mode)))
      mainClass  <- ZIO.fromOption(module.application).orElseFail(ExecutionError.MissingProduct(Node.Link(cell, mode)))
      resolution <- ZIO.service[ResolutionService]
      lock       <- resolution.current
      tools      <- LockedTools.make(lock, request.session.graph, resolution)
      linked     <- JsLink
        .link(
          JsLinkRequest(
            cell.version,
            histos.pack.MainClass.trusted(mainClass.value),
            main.classpath,
            request.session.repo.resolve(s".histos/link/${cell.module.value}/$mode"),
            request.session.jdk,
            kind = module.jsModule,
            revision = module.jsVersion,
            optimize = mode,
          )
        )
        .mapError(ExecutionError.Link(_))
        .provideSomeLayer(ZLayer.succeed(tools))
    yield linked

  private def testLeaf(
      request: ExecutionRequest,
      stage: CompilationStage,
      products: ExecutionProducts,
  ): ZIO[Env & Scope, ExecutionError, StageProduct] =
    val session = request.session
    val cell    = stage.cell
    val failed  = (error: CompileError) =>
      ExecutionError.Compile(GraphCompileError.Failed(cell.module, cell.version, cell.platform, error))
    for
      main  <- ZIO.fromEither(products.require(stage.copy(scope = SourceSet.Main)))
      files <- StageRoots
        .of(session.graph)
        .read(session.repo, stage, products.generatedRoots)
        .mapError(ExecutionError.Roots(_))
      resolution <- ZIO.service[ResolutionService]
      lock       <- resolution.current
      tools      <- LockedTools.make(lock, session.graph, resolution)
      classes = session.repo.resolve(
        s".histos/test-classes/${cell.module.value}/${cell.version.value}/${cell.platform.token}"
      )
      analysis = session.repo.resolve(
        s".histos/test-analysis/${cell.module.value}/${cell.version.value}/${cell.platform.token}"
      )
      compile = CompileRequest(
        CompileToolchain(cell.version, cell.platform, session.jdk, ScalacFlags.empty),
        files.sources.map(_.path),
        main.classpath,
        classes,
        analysis,
        Nil,
        List(main.api, main.implementation),
        checkout = session.repo,
      )
      output <- JvmCompile
        .compilePooled(compile, Nil, _ => (), ZIO.unit, _ => ())
        .mapError(failed)
        .provideSomeLayer(ZLayer.succeed(tools))
      _ <- histos.pack.AssetSync
        .sync(
          classes,
          Some(analysis.resolve(histos.pack.AssetSync.Manifest)),
          files.resources,
          files.assets,
          session.repo,
        )
        .mapError(error => ExecutionError.Roots(StageRootError.Resource(error)))
      api            <- ZIO.fromEither(histos.zinc.ApiDigest.read(analysis)).mapError(failed)
      implementation <- CellCapture.implementation(classes).mapError(failed)
      capture        <- CellCapture
        .record(
          CaptureRequest(
            session.repo,
            stage,
            files,
            CaptureProduct(classes, main.classpath),
            CaptureTools(session.jdk.version, output.runtime, output.compiler, output.bridge),
          )
        )
        .mapError(ExecutionError.Compile(_))
    yield StageProduct(
      stage,
      CompiledCell(
        cell.module,
        cell.version,
        cell.platform,
        classes,
        output.classFiles,
        Some(output.key),
        output.changed,
        output.sources,
        output.classesWritten,
        capture,
      ),
      analysis,
      api,
      implementation,
      classes :: main.classpath,
    )

  private def invokeFixture(
      plugin: HistosPlugin,
      request: ExecutionRequest,
      id: PluginTaskId,
      products: ExecutionProducts,
  ): ZIO[Env & Scope, ExecutionError, GeneratedTask[ExecutionError]] =
    for
      staged      <- Workspace.work("histos-dag-fixture-output").mapError(io)
      declaration <- ZIO
        .fromOption(plugin.contribute.tasks.find(_.name == id.name))
        .orElseFail(ExecutionError.MissingProduct(Node.Generate(id)))
      destinations = OutputDestinations(declaration.outputs.map {
        case DeclaredOutput.File(name, _)      => OutputDestination.File(name, staged.resolve(name.value))
        case DeclaredOutput.Directory(name, _) => OutputDestination.Directory(name, staged.resolve(name.value))
      })
      scope  <- ZIO.scope
      output <- plugin
        .run(
          RunRequest(
            id,
            request.session.repo,
            Nil,
            PluginArgs.empty,
            Nil,
            InvocationContext(
              InvocationId.derive("fixture"),
              ClientId("cli"),
              CommandIdentity.Task(id),
              histos.lock.ExecutionKind.Local,
              Interaction.Unattended,
              OfflineSelection.Online,
            ),
            PluginEvents.closedInvocation,
            products.stages.toList.collect {
              case (stage, product) if stage.scope == SourceSet.Main =>
                CellClasspath.mainOnly(stage.cell.module, stage.cell.version, stage.cell.platform, product.classpath)
            },
            Nil,
            destinations,
          )
        )
        .mapError(error => ExecutionError.Plugin(PluginFault.Task(error)))
        .provideSomeLayer(PluginScopes.live(scope))
      _ <- ZIO.foreachDiscard(declaration.outputs) {
        case DeclaredOutput.File(name, path) =>
          Workspace
            .readString(staged.resolve(name.value))
            .flatMap(text => Workspace.writeString(request.session.repo.resolve(path.value), text))
            .mapError(io)
        case DeclaredOutput.Directory(_, path) =>
          ZIO.fail(
            ExecutionError.Roots(StageRootError.Missing(request.session.repo.resolve(path.value), OutputKind.Directory))
          )
      }
    yield GeneratedTask(
      output,
      ZIO.succeed(declaration.outputs.map { output =>
        GeneratedLocation(
          GeneratedRootId(id, output.name),
          output.kind,
          request.session.repo.resolve(output.path.value),
        )
      }),
    )

  private final case class Relay(order: Ref[List[String]]) extends HistosPlugin:
    val id                       = PluginId("relay-generator")
    def contribute: Contribution = Contribution(
      List(
        DeclaredTask(
          TaskName("emit"),
          Seal.Hermetic,
          Set.empty,
          Nil,
          List(DeclaredOutput.File(OutputName("source"), RepoPath("target/generated/Relay.scala"))),
          List(
            TaskDep.Link(ProjectName("relay"), None, None),
            TaskDep.EmitScoped(
              ProjectName("host"),
              SourceSet.Main,
              PlatformSelection.only(Platform.Jvm),
              EmittedContent.SourceFile(ArtifactPath("demo/Generated.scala")),
              OutputName("source"),
            ),
          ),
        )
      ),
      Nil,
      Nil,
    )
    def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      for
        _    <- order.update(_ :+ "relay")
        path <- ZIO.fromEither(request.file(OutputName("source")))
        _    <- Workspace
          .writeString(path, "package demo\nobject Generated:\n  def text: String = \"relay\"\n")
          .mapError(_ => PluginRunError.MissingOutput(OutputName("source"), RepoPath("target/generated/Relay.scala")))
      yield PluginOutput(
        "generated",
        List(WrittenFile(OutputName("source"), RepoPath("target/generated/Relay.scala"))),
        Nil,
      )

  private final case class Transcripts(count: Ref[Int]) extends HistosPlugin:
    val id                       = PluginId("transcripts")
    def contribute: Contribution = Contribution(
      List(
        DeclaredTask(
          TaskName("transcript"),
          Seal.Hermetic,
          Set.empty,
          Nil,
          List(DeclaredOutput.File(OutputName("source"), RepoPath("target/transcripts/Generated.scala"))),
          List(
            TaskDep.CompileScoped(ProjectName("docs"), Some(version), Some(Platform.Jvm), SourceSet.Main),
            TaskDep.EmitScoped(
              ProjectName("docs"),
              SourceSet.Test,
              PlatformSelection.only(Platform.Jvm),
              EmittedContent.SourceFile(ArtifactPath("demo/Generated.scala")),
              OutputName("source"),
            ),
            TaskDep.EmitScoped(
              ProjectName("docs"),
              SourceSet.Main,
              PlatformSelection.only(Platform.Js),
              EmittedContent.SourceFile(ArtifactPath("demo/Generated.scala")),
              OutputName("source"),
            ),
          ),
        )
      ),
      Nil,
      Nil,
    )
    def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      for
        _        <- count.update(_ + 1)
        compiled <- ZIO
          .fromOption(
            request.compiled.find(cell =>
              cell.module == ProjectName("docs") &&
                cell.platform == Platform.Jvm && cell.scope == SourceSet.Main
            )
          )
          .orElseFail(
            PluginRunError.MissingOutput(OutputName("source"), RepoPath("target/transcripts/Generated.scala"))
          )
        text <- compiled.compile match
          case classes :: runtime =>
            PluginCompileSupport
              .invoke(classes, runtime, "demo.Transcript$", "render")
              .mapError(_ =>
                PluginRunError.MissingOutput(OutputName("source"), RepoPath("target/transcripts/Generated.scala"))
              )
          case Nil =>
            ZIO.fail(PluginRunError.MissingOutput(OutputName("source"), RepoPath("target/transcripts/Generated.scala")))
        path <- ZIO.fromEither(request.file(OutputName("source")))
        _    <- Workspace
          .writeString(path, text)
          .mapError(_ =>
            PluginRunError.MissingOutput(OutputName("source"), RepoPath("target/transcripts/Generated.scala"))
          )
      yield PluginOutput(
        "transcript",
        List(WrittenFile(OutputName("source"), RepoPath("target/transcripts/Generated.scala"))),
        Nil,
      )

  private final case class ResourceAsset(text: Ref[String]) extends HistosPlugin:
    val id                       = PluginId("resource-asset")
    def contribute: Contribution = Contribution(
      List(
        DeclaredTask(
          TaskName("emit"),
          Seal.Hermetic,
          Set.empty,
          Nil,
          List(DeclaredOutput.File(OutputName("asset"), RepoPath("target/assets/arbitrary.js"))),
          List(
            TaskDep.EmitScoped(
              ProjectName("app"),
              SourceSet.Main,
              PlatformSelection.only(Platform.Jvm),
              EmittedContent.ResourceFile(ResourceName("apps-browser/view.js")),
              OutputName("asset"),
            )
          ),
        )
      ),
      Nil,
      Nil,
    )
    def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      for
        path  <- ZIO.fromEither(request.file(OutputName("asset")))
        value <- text.get
        _     <- Workspace
          .writeString(path, value)
          .mapError(_ => PluginRunError.MissingOutput(OutputName("asset"), RepoPath("target/assets/arbitrary.js")))
      yield PluginOutput("asset", List(WrittenFile(OutputName("asset"), RepoPath("target/assets/arbitrary.js"))), Nil)

  private val readerSource =
    """|package demo
       |object Reader:
       |  def text: String =
       |    Option(getClass.getResourceAsStream("/apps-browser/view.js")) match
       |      case None => "missing"
       |      case Some(stream) =>
       |        try new String(stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
       |        finally stream.close()
       |""".stripMargin

  private def io(error: IoFailure): ExecutionError = ExecutionError.Roots(StageRootError.Io(error))

  private def jdkRuntime: ZIO[Any, IoFailure, JdkRuntime] =
    val home    = Path.of(sys.props.getOrElse("java.home", "."))
    val version = JdkVersion.trusted("25", JdkMajor.trusted(25))
    ZIO.succeed(
      JdkRuntime(home, version, RuntimeIdentity.Local(home, Sha256Digest.of(Chunk.fromArray(version.value.getBytes))))
    )
end GeneratedDagSpec
