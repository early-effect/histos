package histos.load

import histos.*
import histos.cache.ContentStore
import histos.cli.{Cli, CliExit, CompileConsole}
import histos.jdk.Machine
import histos.plugin.*
import histos.publish.{IvyRoot, LocalPublishError, ReleaseError}
import histos.report.{BuildFault, CallResult, WireJson}
import histos.resolve.{ResolutionService, Resolve}
import histos.schedule.{ActionCache, RemoteCache}
import histos.server.*
import histos.task.RunId
import histos.testrun.ScopedTestFailure
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers}
import histos.suite.HistosSpec

import java.nio.file.{Files, Path}

import zio.*
import zio.json.*
import zio.test.*

object CallerProjectionSpec extends HistosSpec:
  import WireJson.given

  private val version     = ScalaVersion("3.10.1")
  private val id          = PluginTaskId(PluginId("caller-products"), TaskName("generate"))
  private val source      = OutputName("source")
  private val tree        = OutputName("tree")
  private val declaration = DeclaredTask(
    id.name,
    Seal.Hermetic,
    Set.empty,
    Nil,
    List(
      DeclaredOutput.File(source, RepoPath("mixed/Generated.scala")),
      DeclaredOutput.Directory(tree, RepoPath("mixed/tree")),
    ),
    List(
      TaskDep.EmitScoped(
        ProjectName("app"),
        SourceSet.Main,
        PlatformSelection.only(Jvm),
        EmittedContent.SourceFile(ArtifactPath("Generated.scala")),
        source,
      ),
      TaskDep.EmitScoped(
        ProjectName("app"),
        SourceSet.Test,
        PlatformSelection.only(Jvm),
        EmittedContent.SourceTree,
        tree,
      ),
    ),
  )
  private type Env = Resolve & ContentStore & ActionCache & RemoteCache & ResolutionService

  private val tests = suite("production caller projections")(
    test(
      "Perform clean adopts ownership, refuses active readers before classes, and retires File/Directory generations"
    ) {
      withRuntime { f =>
        for
          _        <- f.run
          physical <- ZIO.scoped(
            f.build.plugins.generated(id).flatMap(locations => ZIO.fromEither(locations.file(source)))
          )
          _        <- Workspace.writeString(f.repo.resolve("mixed/User.scala"), "object User")
          _        <- Workspace.writeString(f.repo.resolve(".histos/classes/app/kept.class"), "classes")
          rejected <- ZIO.scoped {
            f.build.plugins.generated(id) *> f.perform(Call.Clean(List(ProjectName("app")), CleanExtent.Products))
          }
          fullRefusal <- ZIO.scoped {
            f.build.plugins.generated(id) *> f.perform(Call.Clean(Nil, CleanExtent.Full))
          }
          classes <- f.inspect(Files.exists(f.repo.resolve(".histos/classes/app/kept.class")))
          cleaned <- f.perform(Call.Clean(Nil, CleanExtent.Products))
          sibling <- Workspace.readString(f.repo.resolve("mixed/User.scala"))
          gone    <- f.inspect(
            Files.notExists(f.repo.resolve("mixed/Generated.scala")) &&
              Files.notExists(f.repo.resolve("mixed/tree")) && Files.notExists(f.repo.resolve(".histos/classes/app")) &&
              Files.notExists(physical)
          )
          unbuilt <- f.send(say => Calls.previewCompile(f.repo, f.build.graph, Nil, say, f.build.plugins))
        yield assertTrue(
          Perform.needsGraph(Call.Clean(Nil, CleanExtent.Products)),
          Perform.needsGraph(Call.Clean(Nil, CleanExtent.Full)),
          rejected.contains(CallResult.Fault(BuildFault.Plugin(PluginFault.Products(ProductError.ActiveReaders)))),
          fullRefusal.contains(CallResult.Fault(BuildFault.Plugin(PluginFault.Products(ProductError.ActiveReaders)))),
          classes,
          cleaned.contains(CallResult.reported()),
          sibling == "object User",
          gone,
          unbuilt.contains(CallResult.Fault(BuildFault.Plugin(PluginFault.MissingProduct(id)))),
        )
      }
    },
    test("local named and full clean use the loaded owner and never forget cache on an active-reader refusal") {
      withRuntime { f =>
        for
          _     <- f.run
          _     <- Workspace.writeString(f.repo.resolve(".histos/classes/app/kept.class"), "classes")
          _     <- Workspace.writeString(f.repo.resolve("mixed/User.scala"), "object User")
          store <- ZIO.service[ContentStore]
          marker = histos.schedule.Digest.trusted("ef" * 32)
          _       <- store.replace(histos.cache.CacheProject.of(f.repo), Set(marker))
          refused <- ZIO.scoped {
            f.build.plugins.generated(id) *> Cli.cleanFullLocal(f.repo, Some(f.build))
          }
          classes  <- f.inspect(Files.exists(f.repo.resolve(".histos/classes/app/kept.class")))
          retained <- store.kept
          named    <- Cli.cleanNamed(f.repo, f.build, List(ProjectName("app")))
          gone     <- f.inspect(
            Files.notExists(f.repo.resolve("mixed/Generated.scala")) &&
              Files.notExists(f.repo.resolve("mixed/tree"))
          )
          sibling   <- Workspace.readString(f.repo.resolve("mixed/User.scala"))
          _         <- f.run
          full      <- Cli.cleanFullLocal(f.repo, Some(f.build))
          forgotten <- store.kept
          fullGone  <- f.inspect(
            Files.notExists(f.repo.resolve("mixed/Generated.scala")) &&
              Files.notExists(f.repo.resolve("mixed/tree"))
          )
        yield assertTrue(
          refused.exit == CliExit.Failed,
          refused.err.contains("read"),
          classes,
          named.exit == CliExit.Success,
          gone,
          sibling == "object User",
          retained.contains(marker),
          full.exit == CliExit.Success,
          fullGone,
          !forgotten.contains(marker),
        )
      }
    },
    test("graph-free local recovery removes ordinary classes but deliberately preserves generated ownership") {
      withRuntime { f =>
        for
          _         <- f.run
          _         <- Workspace.writeString(f.repo.resolve(".histos/classes/app/kept.class"), "classes")
          recovered <- Cli
            .cleanLoaded(f.repo, f.repo.resolve("missing-api.jar"))(build => Cli.cleanNamed(f.repo, build, Nil))(
              Cli.cleanLocal(f.repo)
            )
            .provideSomeLayer[Env & Scope](Loader.live)
          kept <- f.inspect(
            Files.exists(f.repo.resolve("mixed/Generated.scala")) &&
              Files.exists(f.repo.resolve("mixed/tree/Suite.scala"))
          )
          gone <- f.inspect(Files.notExists(f.repo.resolve(".histos/classes/app")))
        yield assertTrue(recovered.exit == CliExit.Success, kept, gone)
      }
    },
    test(
      "warmed compile and Test previews lease physical products without invoking producers; embedded links remain refused"
    ) {
      withRuntime { f =>
        for
          cold     <- f.send(say => Calls.previewCompile(f.repo, f.build.graph, Nil, say, f.build.plugins))
          _        <- f.run
          compiled <- f.send(say => Calls.previewCompile(f.repo, f.build.graph, Nil, say, f.build.plugins))
          tested   <- f.send(say =>
            Calls.previewTest(f.repo, f.build.graph, TestSelection.named(Nil), Nil, say, f.build.plugins)
          )
          count  <- f.count.get
          leased <- Calls.previewUsing(
            f.build.graph,
            f.build.plugins,
            List(
              histos.build.BuildNode.Compile(
                CompilationStage(Cell(ProjectName("app"), version, Jvm), SourceSet.Test)
              )
            ),
          ) { generated =>
            for
              refused <- Calls.cleanProducts(f.repo, f.build.graph, f.build.plugins, Nil).either
              _       <- ZIO.fromEither(
                Either.cond(
                  refused == Left(BuildFault.Plugin(PluginFault.Products(ProductError.ActiveReaders))),
                  (),
                  histos.preview.PreviewError.Scoped(ScopedTestFailure.MissingGenerated(id, tree)),
                )
              )
              plan <- histos.preview.Plan.testSelection(f.repo, f.build.graph, TestSelection.named(Nil), generated)
            yield plan.units.nonEmpty
          }
          fixed <- ZIO.scoped(
            f.build.plugins.generated(id).flatMap(locations => ZIO.fromEither(locations.directory(tree)))
          )
          _ <- ZIO
            .attemptBlocking(
              Files.createSymbolicLink(fixed.resolve("Escape.scala"), f.repo.resolveSibling("outside.scala"))
            )
            .mapError(_ => histos.workspace.IoFailure(fixed.resolve("Escape.scala"), histos.workspace.IoFault.Create))
          escaped <- f.send(say =>
            Calls.previewTest(f.repo, f.build.graph, TestSelection.named(Nil), Nil, say, f.build.plugins)
          )
        yield assertTrue(
          cold.contains(CallResult.Fault(BuildFault.Plugin(PluginFault.MissingProduct(id)))),
          compiled.contains(CallResult.reported()),
          tested.contains(CallResult.reported()),
          count == 1,
          leased,
          escaped.contains(
            CallResult.Fault(
              BuildFault.Preview(
                histos.preview.PreviewError.Scoped(
                  ScopedTestFailure.RootLink(fixed.resolve("Escape.scala"))
                )
              )
            )
          ),
        )
      }
    },
    test("release Stage failures retain scoped publication fields through the actual adapter and CallResult JSON") {
      ZIO.scoped {
        for
          repo    <- Workspace.work("release-caller")
          console <- CompileConsole.open(repo)
          feeds   <- Ref.make(Chunk.empty[Feed])
          say     <- Say.record(repo, feeds)
          failure = LocalPublishError.Scoped(
            ScopedTestFailure.MissingUpstream(
              CompilationStage(Cell(ProjectName("app"), version, Jvm), SourceSet.Test),
              ProjectName("fixture"),
            )
          )
          _    <- Calls.released(repo, say, console, ZIO.fail(ReleaseError.Stage(failure)))
          seen <- feeds.get
        yield
          val results  = seen.collect { case Feed.Done(result) => result }.toList
          val expected = CallResult.Fault(BuildFault.Publish(failure))
          assertTrue(results == List(expected), results.map(_.toJson.fromJson[CallResult]) == List(Right(expected)))
      }
    },
  )
  private val checked = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)
  def spec            = checked

  private final case class Fixture(repo: Path, build: LoadedBuild, count: Ref[Int], machine: Machine):
    def inspect[A](body: => A): IO[histos.workspace.IoFailure, A] =
      ZIO.attemptBlocking(body).mapError(_ => histos.workspace.IoFailure(repo, histos.workspace.IoFault.Stat))
    def run: ZIO[Env, PluginFault, PluginOutput] = build.plugins.run(id, PluginArgs(Nil, Set.empty))
    def send[R](use: Say => ZIO[R, ServerError, Unit]): ZIO[R, ServerError, Option[CallResult]] =
      for
        feeds <- Ref.make(Chunk.empty[Feed])
        say   <- Say.record(repo, feeds)
        _     <- use(say)
        seen  <- feeds.get
      yield seen.collectFirst { case Feed.Done(result) => result }

    def perform(call: Call): ZIO[Env & Scope, ServerError, Option[CallResult]] =
      send { say =>
        for
          runs <- Ref.make(Map.empty[RunId, Fiber[ServerError, Unit]])
          desk <- ConsentDesk.open(
            repo,
            ConsentDesk.strategy(histos.lock.ExecutionKind.Local, None),
            ConsentDesk.command,
            ConsentDesk.preparer(repo),
            ConsentDesk.feed(say),
          )
          edges = Perform.Edges[Any]((_, _, _) => ZIO.unit, ZIO.unit, _ => ZIO.unit, ZIO.unit, ZIO.unit, ZIO.unit)
          _ <- Perform
            .run(
              repo,
              repo.resolve("api.jar"),
              Right(Some(build)),
              machine,
              runs,
              false,
              "test",
              call,
              say,
              edges,
              desk,
            )
            .provideSomeLayer[Env & Scope](
              Loader.live ++ CompileMemo.live ++ Compilers.live ++ histos.format.Formatters.live ++ IvyRoot.ivy2Local
            )
        yield ()
      }
  end Fixture

  private def withRuntime[E, A](use: Fixture => ZIO[Env & Scope, E, A]) =
    val effect = ZIO.scoped {
      for
        repo  <- Workspace.work("caller-products")
        count <- Ref.make(0)
        producer = new HistosPlugin:
          val id         = CallerProjectionSpec.id.plugin
          def contribute = Contribution(List(declaration), Nil, Nil)
          def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
            for
              _         <- count.update(_ + 1)
              file      <- ZIO.fromEither(request.file(source))
              directory <- ZIO.fromEither(request.directory(tree))
              _ <- Workspace.writeString(file, "object Generated").mapError(_ => PluginRunError.Defect("write source"))
              _ <- Workspace
                .writeString(directory.resolve("Suite.scala"), "object Suite")
                .mapError(_ => PluginRunError.Defect("write Test source"))
            yield PluginOutput(
              "generated",
              List(WrittenFile(source, RepoPath("mixed/Generated.scala"))),
              List(WrittenDirectory(tree, RepoPath("mixed/tree"))),
            )
        base <- ZIO.fromEither(Projects(module("app", Jvm)(version)))
        graph = base.withPlugins(List(RegisteredPlugin(producer.id, producer.contribute)(producer)))
        leases   <- GenerationLeases.make
        _        <- ZIO.addFinalizer(leases.retire)
        threads  <- PluginThreads.make(getClass.getClassLoader)
        products <- PluginProducts.make()
        runtime  <- PluginRuntime
          .loaded(repo, graph, getClass.getClassLoader, threads, Nil, ApiDigest.trusted("ab" * 32), leases, Nil)
          .provideSomeLayer[Scope](ZLayer.succeed(products))
        build = new HistosBuild:
          def graph = Right(base.withPlugins(List(RegisteredPlugin(producer.id, producer.contribute)(producer))))
        machine    <- Machine.load(repo)
        resolution <- ResolutionService.open(repo, graph)
        store = ContentStore.in(repo.resolve("cas"))
        result <- use(Fixture(repo, LoadedBuild(graph, runtime, build), count, machine)).provideSome[Resolve & Scope](
          ZLayer.succeed(resolution),
          ZLayer.succeed(store),
          ZLayer.succeed(ContentStore.actions(store)),
          RemoteCache.off,
        )
      yield result
    }
    ZIO.withSystem(zio.System.SystemLive)(effect).provide(Resolve.coursier)
end CallerProjectionSpec
