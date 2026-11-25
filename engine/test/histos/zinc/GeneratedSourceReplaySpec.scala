package histos.zinc

import histos.*
import histos.build.{
  BuildEnv,
  BuildExecution,
  BuildNode as Node,
  ExecutionError,
  ExecutionLeaves,
  ExecutionRequest,
  GeneratedTask,
  StageProduct,
}
import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Sha256, Sha256Digest}
import histos.load.{CommittedOutputs, PluginCompileSupport, PluginProducts, ProductError, ProductRequest, RestoreResult}
import histos.native.NativeOrigin
import histos.plugin.*
import histos.resolve.{Resolve, ResolutionService}
import histos.schedule.{ActionCache, Histos, Parallelism, RemoteCache}
import histos.suite.HistosSpec
import histos.toolchain.RuntimeIdentity
import histos.workspace.{IoFailure, Workspace}

import java.nio.file.Path

import zio.{Chunk, Ref, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object GeneratedSourceReplaySpec extends HistosSpec:
  private val version    = ScalaVersion("3.10.1")
  private val name       = ProjectName("generated")
  private val stage      = CompilationStage(Cell(name, version, Platform.Jvm), SourceSet.Main)
  private val taskId     = PluginTaskId(PluginId("source-replay"), TaskName("emit"))
  private val outputName = OutputName("source")
  private val outputPath = RepoPath("target/generated/Arbitrary.output")
  private val task       = DeclaredTask(
    taskId.name,
    Seal.Hermetic,
    Set.empty,
    Nil,
    List(DeclaredOutput.File(outputName, outputPath)),
    List(
      TaskDep.EmitScoped(
        name,
        SourceSet.Main,
        PlatformSelection.All,
        EmittedContent.SourceFile(ArtifactPath("demo/Generated.scala")),
        outputName,
      )
    ),
  )

  private val declaration = new HistosPlugin:
    val id                       = taskId.plugin
    def contribute: Contribution = Contribution(List(task), Nil, Nil)
    def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      ZIO.succeed(PluginOutput(request.id.render, Nil, Nil))

  private val tests = suite("generated source replay identity")(
    test("equal bytes in a restored generation reuse resident compilation, changed bytes miss") {
      ZIO
        .scoped {
          for
            repo <- Workspace.work("histos-generated-source-replay")
            _    <- ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")) { key =>
              ZIO.foreachDiscard(sys.props.get(key))(TestSystem.putProperty(key, _))
            }
            graph <- ZIO.fromEither(Projects(module("generated", Jvm)(version)))
            linked = graph.withPlugins(List(RegisteredPlugin(declaration.id, declaration.contribute)(declaration)))
            products <- PluginProducts.make()
            request = ProductRequest(repo, task, InvocationId.derive("source replay"))
            cas        <- Workspace.createDirectories(repo.resolve("cas"))
            resolution <- ResolutionService.open(repo, linked)
            lock       <- resolution.current
            starts     <- Ref.make(0)
            services   <- (Compilers.live ++ CompileMemo.live).build
            scheduler  <- Histos.live.build
              .provideSomeLayer[Scope](ZLayer.succeed(Parallelism.default))
              .map(_.get[Histos])
            home  = Path.of(sys.props.getOrElse("java.home", "."))
            floor = JdkVersion.trusted("25", JdkMajor.trusted(25))
            jdk   = JdkRuntime(
              home,
              floor,
              RuntimeIdentity.Local(home, Sha256Digest.of(Chunk.fromArray(floor.value.getBytes))),
            )
            session = CompileSession(repo, linked, jdk, NativeOrigin.Central, lock)
            environment <- (ZLayer.environment[Resolve] ++ ZLayer.succeed(resolution) ++ ZLayer.succeed(
              ContentStore.in(cas)
            ) ++
              ZLayer.succeed(ActionCache.miss) ++ RemoteCache.off ++ ZLayer.succeed(CompilerStarts(starts)) ++
              ZLayer.succeed(services.get[Compilers]) ++ ZLayer.succeed(services.get[CompileMemo])).build
            first       <- produce(products, request, "old").provideSomeLayer(ZLayer.succeed(ContentStore.in(cas)))
            before      <- ZIO.scoped[BuildEnv](observe(session, first, scheduler)).provideEnvironment(environment)
            firstStarts <- starts.get
            _           <- Workspace.delete(repo.resolve(outputPath.value))
            restored    <- ZIO
              .scoped(products.restore(request, first.manifest))
              .provideSomeLayer(ZLayer.succeed(ContentStore.in(cas)))
            replay <- restored match
              case RestoreResult.Hit(outputs) => ZIO.succeed(outputs)
              case RestoreResult.Miss         => ZIO.fail(ProductError.InvalidManifest)
            after          <- ZIO.scoped[BuildEnv](observe(session, replay, scheduler)).provideEnvironment(environment)
            replayStarts   <- starts.get
            _              <- Workspace.delete(after.product.classes)
            _              <- Workspace.delete(after.product.analysis)
            loaded         <- ZIO.scoped[BuildEnv](observe(session, replay, scheduler)).provideEnvironment(environment)
            loadedStarts   <- starts.get
            unchangedFresh <- ZIO.scoped(fresh(session, replay, loaded.product))
            changed        <- produce(products, request, "new").provideSomeLayer(ZLayer.succeed(ContentStore.in(cas)))
            changedFresh   <- ZIO.scoped(fresh(session, changed, loaded.product))
            edited         <- ZIO.scoped[BuildEnv](observe(session, changed, scheduler)).provideEnvironment(environment)
            editedStarts   <- starts.get
            reused         <- ZIO.scoped[BuildEnv](observe(session, changed, scheduler)).provideEnvironment(environment)
            reusedStarts   <- starts.get
          yield assertTrue(
            before.physical != after.physical,
            before.logical == after.logical,
            before.product.compiled.key == after.product.compiled.key,
            before.text == "old",
            after.text == "old",
            firstStarts == 1,
            replayStarts == 1,
            loaded.product.compiled.key == after.product.compiled.key,
            loaded.text == "old",
            loadedStarts == 1,
            unchangedFresh,
            !changedFresh,
            edited.logical != after.logical,
            edited.product.compiled.key != after.product.compiled.key,
            edited.text == "new",
            editedStarts == 2,
            reused.product.compiled.key == edited.product.compiled.key,
            reused.text == "new",
            reusedStarts == 2,
          )
        }
        .provide(Resolve.coursier)
    }
  )

  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)

  private final case class Observed(product: StageProduct, physical: Path, logical: Sha256, text: String)

  private def observe(
      session: CompileSession,
      outputs: CommittedOutputs,
      scheduler: Histos,
  ): ZIO[BuildEnv & Scope, ExecutionError | ProductError | CompileError | histos.load.PluginCompileError, Observed] =
    val leaves = ExecutionLeaves[BuildEnv, ProductError](
      GraphCompile.stage,
      (_, compiling, _) => ZIO.fail(ExecutionError.MissingProduct(Node.Compile(compiling))),
      (_, id, _) =>
        ZIO.succeed(
          GeneratedTask(
            PluginOutput(outputs.manifest.text, List(WrittenFile(outputName, outputPath)), Nil),
            outputs.read.map { locations =>
              locations.values.map(location =>
                GeneratedLocation(GeneratedRootId(id, location.output.name), location.output.kind, location.path)
              )
            },
          )
        ),
      (_, cell, mode, _) => ZIO.fail(ExecutionError.MissingProduct(Node.Link(cell, mode))),
    )
    for
      completed <- new BuildExecution(leaves, scheduler).ensure(ExecutionRequest(session, List(Node.Compile(stage))))
      compiled  <- ZIO.fromEither(completed.require(stage))
      files     <- StageRoots
        .of(session.graph)
        .read(session.repo, stage, completed.generatedRoots)
        .mapError(ExecutionError.Roots(_))
      source  <- ZIO.fromOption(files.sources.headOption).orElseFail(ExecutionError.MissingProduct(Node.Compile(stage)))
      inputs  <- ZIO.fromEither(files.compileInputs(session.repo)).mapError(ExecutionError.Roots(_))
      logical <- CompileSources.digest(inputs)
      text    <- PluginCompileSupport.invoke(compiled.classes, compiled.classpath, "demo.Generated$", "text")
    yield Observed(compiled, source.path, logical, text)

  private def fresh(
      session: CompileSession,
      outputs: CommittedOutputs,
      product: StageProduct,
  ): ZIO[Scope, ProductError | StageRootError | CompileError, Boolean] =
    for
      locations <- outputs.read
      roots = GeneratedRoots.Provided(
        locations.values.map(location =>
          GeneratedLocation(GeneratedRootId(taskId, location.output.name), location.output.kind, location.path)
        )
      )
      files  <- StageRoots.of(session.graph).read(session.repo, stage, roots)
      inputs <- ZIO.fromEither(files.compileInputs(session.repo))
      request = CompileRequest(
        CompileToolchain(version, Platform.Jvm, session.jdk, ScalacFlags.empty),
        inputs.map(_.path),
        product.classpath,
        product.classes,
        product.analysis,
        Nil,
        Nil,
        checkout = session.repo,
        sourceInputs = inputs,
      )
      current <- SourceStale.current(request)
    yield current

  private def produce(
      products: PluginProducts,
      request: ProductRequest,
      value: String,
  ): ZIO[ContentStore, ProductError | IoFailure, CommittedOutputs] =
    ZIO.scoped {
      for
        staged    <- products.stage(request)
        file      <- ZIO.fromEither(staged.destinations.file(outputName)).mapError(ProductError.Report(_))
        _         <- Workspace.writeString(file, s"package demo\nobject Generated:\n  def text: String = \"$value\"\n")
        manifest  <- products.seal(staged, PluginOutput(value, List(WrittenFile(outputName, outputPath)), Nil))
        committed <- products.commit(staged, manifest)
      yield committed
    }
end GeneratedSourceReplaySpec
