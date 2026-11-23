package histos.load

import histos.*
import histos.cache.{ContentStore, CacheProject}
import histos.plugin.*
import histos.resolve.{DependencyLock, ResolutionService, Resolve}
import histos.schedule.{ActionCache, RemoteCache, RemoteMode, RemoteError, Digest, PluginManifest}
import zio.Chunk
import java.nio.charset.StandardCharsets.UTF_8
import histos.version.Catalog
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import zio.{IO, Promise, Ref, Scope, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

object PluginRuntimeProductsSpec extends ZIOSpecDefault:
  private val site      = OutputName("site")
  private val generated = OutputName("generated")
  private val id        = PluginTaskId(PluginId("site"), TaskName("build"))
  private val task      = DeclaredTask(
    id.name,
    Seal.Hermetic,
    Set.empty,
    Nil,
    List(
      DeclaredOutput.Directory(site, RepoPath("site")),
      DeclaredOutput.File(generated, RepoPath("generated/Generated.scala")),
    ),
    List(
      TaskDep.EmitScoped(
        ProjectName("consumer"),
        SourceSet.Main,
        PlatformSelection.only(Platform.Jvm),
        EmittedContent.SourceFile(ArtifactPath("demo/Generated.scala")),
        generated,
      )
    ),
  )

  private enum Behavior:
    case Publish, Fail, Hold

  private val tests = suite("runtime owned plugin products")(
    test("a real task publishes Directory/File results and failures retain pages, assets, stamp and source") {
      fixture { f =>
        for
          first  <- f.run("old").either
          _      <- f.behavior.set(Behavior.Fail)
          failed <- f.run("changed").either
          page   <- f.read("site/index.html")
          asset  <- f.read("site/assets/client.js")
          stamp  <- f.read("site/stamp")
          source <- f.read("generated/Generated.scala")
        yield assertTrue(
          first.exists(out =>
            out.directories == List(WrittenDirectory(site, RepoPath("site"))) && out.files == List(
              WrittenFile(generated, RepoPath("generated/Generated.scala"))
            )
          ),
          failed == Left(PluginFault.Task(PluginRunError.Failed(PluginDetail("staged failure")))),
          page == "old",
          asset == "old",
          stamp == "old",
          source == "old",
        )
      }
    },
    test("interruption after partial staging preserves the complete previous publication") {
      fixture { f =>
        for
          first  <- f.run("old").either
          _      <- f.behavior.set(Behavior.Hold)
          fiber  <- f.run("changed").forkScoped
          _      <- f.ready.await
          exit   <- fiber.interrupt
          page   <- f.read("site/index.html")
          asset  <- f.read("site/assets/client.js")
          stamp  <- f.read("site/stamp")
          source <- f.read("generated/Generated.scala")
          again  <- f.behavior.set(Behavior.Publish) *> f.run("old").either
          count  <- f.count.get
        yield assertTrue(
          first.isRight,
          exit.isInterrupted,
          page == "old",
          asset == "old",
          stamp == "old",
          source == "old",
          again.isRight,
          count == 2,
        )
      }
    },
    test("a real task interrupted at the prepared promotion boundary retains the previous publication") {
      for
        phase <- Ref.make(Behavior.Publish)
        ready <- Promise.make[Nothing, Unit]
        hold  <- Promise.make[Nothing, Unit]
        publication = ProductPublication(phase.get.flatMap {
          case Behavior.Publish => ZIO.unit
          case Behavior.Fail    => ZIO.fail(ProductError.Changed)
          case Behavior.Hold    => ready.succeed(()) *> hold.await
        })
        result <- fixture(
          f =>
            for
              _      <- f.run("old")
              _      <- phase.set(Behavior.Hold)
              fiber  <- f.run("new").forkScoped
              _      <- ready.await
              exit   <- fiber.interrupt
              page   <- f.read("site/index.html")
              stamp  <- f.read("site/stamp")
              _      <- phase.set(Behavior.Fail)
              failed <- f.run("new").either
              after  <- f.read("site/assets/client.js")
            yield assertTrue(
              exit.isInterrupted,
              page == "old",
              stamp == "old",
              after == "old",
              failed == Left(PluginFault.Products(ProductError.Changed)),
            ),
          publication,
        )
      yield result
    },
    test("deleted output replay restores CAS bytes without invoking the real producer again") {
      fixture { f =>
        for
          first  <- f.run("old").either
          _      <- ZIO.attemptBlocking(Workspace.deleteSync(f.repo.resolve("site"))).orDie
          _      <- ZIO.attemptBlocking(Workspace.deleteSync(f.repo.resolve("generated/Generated.scala"))).orDie
          replay <- f.run("old").either
          page   <- f.read("site/index.html")
          source <- f.read("generated/Generated.scala")
          count  <- f.count.get
        yield assertTrue(
          first.isRight,
          replay.exists(_.text == PluginRuntime.cacheHit("old")),
          page == "old",
          source == "old",
          count == 1,
        )
      }
    },
    test("fixed generation readers retain old bytes while preview uses the logical Directory and excludes clean") {
      fixture { f =>
        for
          _       <- f.run("old")
          output  <- f.runtime.committed(id)
          kept    <- f.store.kept.mapError(error => PluginFault.Products(ProductError.Cache(error)))
          checked <- ZIO.scoped {
            for
              fixed     <- f.runtime.generated(id)
              oldFile   <- ZIO.fromEither(fixed.file(generated)).mapError(PluginFault.Products(_))
              oldSite   <- ZIO.fromEither(fixed.directory(site)).mapError(PluginFault.Products(_))
              logical   <- f.runtime.previewDirectory(id, site)
              _         <- f.run("new")
              old       <- ZIO.attemptBlocking(Files.readString(oldSite.resolve("index.html"))).orDie
              newPage   <- ZIO.attemptBlocking(Files.readString(logical.resolve("index.html"))).orDie
              oldSource <- ZIO.attemptBlocking(Files.readString(oldFile)).orDie
              clean     <- f.runtime.cleanProducts(id).either
            yield (old, newPage, oldSource, clean, logical, oldFile)
          }
          _      <- f.runtime.cleanProducts(id)
          absent <- ZIO.attemptBlocking(!Files.exists(f.repo.resolve("site"))).orDie
        yield assertTrue(
          output.manifest.contentDigests.subsetOf(kept),
          checked._1 == "old",
          checked._2 == "new",
          checked._3 == "old",
          checked._4 == Left(PluginFault.Products(ProductError.ActiveReaders)),
          checked._5 == f.repo.resolve("site"),
          Option(checked._6.getFileName).exists(_.toString == "Generated.scala"),
          task.dependsOn == List(
            TaskDep.EmitScoped(
              ProjectName("consumer"),
              SourceSet.Main,
              PlatformSelection.only(Platform.Jvm),
              EmittedContent.SourceFile(ArtifactPath("demo/Generated.scala")),
              generated,
            )
          ),
          absent,
        )
      }
    },
    test("a reader from an earlier load excludes cleanup in the next load using the same product service") {
      fixture { f =>
        for
          _       <- f.run("old")
          checked <- ZIO.scoped {
            for
              fixed    <- f.runtime.generated(id)
              sitePath <- ZIO.fromEither(fixed.directory(site)).mapError(PluginFault.Products(_))
              next     <- f.reload
              reloaded = f.copy(runtime = next)
              _       <- reloaded.run("new")
              clean   <- next.cleanProducts(id).either
              old     <- ZIO.attemptBlocking(Files.readString(sitePath.resolve("index.html"))).orDie
              current <- f.read("site/index.html")
            yield (clean, old, current, next)
          }
          _      <- checked._4.cleanProducts(id)
          absent <- ZIO.attemptBlocking(!Files.exists(f.repo.resolve("site"))).orDie
        yield assertTrue(
          checked._1 == Left(PluginFault.Products(ProductError.ActiveReaders)),
          checked._2 == "old",
          checked._3 == "new",
          absent,
        )
      }
    },
    test("a remote v2 action with a missing local blob is a real miss, not a false hit") {
      fixture { f =>
        for
          _       <- f.run("old")
          output  <- f.runtime.committed(id)
          entries <- f.store.list.mapError(error => PluginFault.Products(ProductError.Cache(error)))
          values  <- ZIO.foreach(entries)(entry =>
            f.store.read(entry.digest).mapError(error => PluginFault.Products(ProductError.Cache(error)))
          )
          data <- ZIO
            .fromOption(values.collectFirst {
              case Some(bytes) if PluginManifest.products(new String(bytes.toArray, UTF_8)).exists(_.nonEmpty) => bytes
            })
            .orElseFail(PluginFault.Products(ProductError.InvalidManifest))
          _ <- ZIO.foreachDiscard(output.manifest.contentDigests)(digest =>
            f.store.remove(digest).mapError(error => PluginFault.Products(ProductError.Cache(error)))
          )
          remote = new RemoteCache:
            val mode                                                             = RemoteMode.Read
            def find(action: Digest): IO[RemoteError, Option[Chunk[Byte]]]       = ZIO.some(data)
            def store(action: Digest, bytes: Chunk[Byte]): IO[RemoteError, Unit] = ZIO.fail(RemoteError.ReadOnly)
            def purge: IO[RemoteError, Unit]                                     = ZIO.fail(RemoteError.ReadOnly)
          result <- f.run("old", remote)
          count  <- f.count.get
          page   <- f.read("site/index.html")
        yield assertTrue(result.text == "old", count == 2, page == "old")
      }
    },
    test("output ownership and exact emission names participate in task cache identity") {
      fixture { f =>
        val graph = ProjectGraph.trusted(Nil, Splits.aggregate, Catalog.empty, None, Nil)
        def hashed(declared: DeclaredTask): IO[PluginFault, List[histos.schedule.Input]] =
          PluginInputs
            .hash(
              PluginInputs.HashRequest(
                f.repo,
                graph,
                id.plugin,
                declared,
                id,
                Nil,
                ApiDigest.trusted("ab" * 32),
                Map.empty,
                PluginArgs.empty,
                None,
                Nil,
                Nil,
              )
            )
            .map(_.inputs)
            .mapError(PluginFault.Task(_))
        val renamed = task.copy(outputs =
          List(
            DeclaredOutput.Directory(OutputName("renamed"), RepoPath("site")),
            DeclaredOutput.File(generated, RepoPath("generated/Generated.scala")),
          )
        )
        val resource = task.copy(dependsOn =
          List(
            TaskDep.EmitScoped(
              ProjectName("consumer"),
              SourceSet.Test,
              PlatformSelection.only(Platform.Jvm),
              EmittedContent.ResourceFile(ResourceName("apps-browser/counter-view.js")),
              generated,
            )
          )
        )
        val otherName = resource.copy(dependsOn =
          List(
            TaskDep.EmitScoped(
              ProjectName("consumer"),
              SourceSet.Test,
              PlatformSelection.only(Platform.Jvm),
              EmittedContent.ResourceFile(ResourceName("apps-browser/reference-host.js")),
              generated,
            )
          )
        )
        for
          before  <- hashed(task)
          changed <- hashed(renamed)
          emitted <- hashed(resource)
          named   <- hashed(otherName)
        yield assertTrue(before != changed, before != emitted, emitted != named)
      }
    },
    test("product refusal remains typed through the actual runtime") {
      fixture { f =>
        for
          _      <- ZIO.attemptBlocking(Files.createDirectories(f.repo.resolve("site"))).orDie
          _      <- write(f.repo.resolve("site/index.html"), "unmanaged").mapError(PluginFault.Task(_))
          result <- f.run("new").either
          page   <- f.read("site/index.html")
        yield assertTrue(
          result == Left(PluginFault.Products(ProductError.Unmanaged(f.repo.resolve("site")))),
          page == "unmanaged",
        )
      }
    },
  )

  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)

  private final case class Fixture(
      repo: Path,
      runtime: PluginRuntime,
      reload: ZIO[Scope, Nothing, PluginRuntime],
      behavior: Ref[Behavior],
      count: Ref[Int],
      ready: Promise[Nothing, Unit],
      store: ContentStore,
      cache: ActionCache,
      resolve: Resolve,
      resolution: ResolutionService,
  ):
    def run(label: String): IO[PluginFault, PluginOutput]                      = run(label, RemoteCache.unconfigured)
    def run(label: String, remote: RemoteCache): IO[PluginFault, PluginOutput] =
      runtime
        .run(id, PluginArgs(List(NamedText(ParamName("label"), label)), Set.empty))
        .provide(
          ZLayer.succeed(store),
          ZLayer.succeed(cache),
          ZLayer.succeed(resolve),
          ZLayer.succeed(resolution),
          ZLayer.succeed(remote),
        )
    def read(path: String): UIO[String] = ZIO.attemptBlocking(Files.readString(repo.resolve(path))).orDie

  private def fixture[A](
      use: Fixture => ZIO[Scope, PluginFault, A],
      publication: ProductPublication = ProductPublication.live,
  ): ZIO[Any, PluginFault, A] =
    ZIO
      .scoped {
        for
          repo      <- Workspace.work("runtime-products").catchAll(error => ZIO.dieMessage(error.message))
          _         <- hostFacts
          count     <- Ref.make(0)
          behavior  <- Ref.make(Behavior.Publish)
          ready     <- Promise.make[Nothing, Unit]
          hold      <- Promise.make[Nothing, Unit]
          storePath <- ZIO.attemptBlocking(Files.createDirectories(repo.resolve("store"))).orDie
          store  = ContentStore.in(storePath)
          plugin = Producer(behavior, count, ready, hold)
          graph  = ProjectGraph.trusted(
            List(
              Module(
                ProjectName("consumer"),
                Platforms(Platform.Jvm, Nil),
                ScalaVersions(ScalaVersion("3.10.1"), Nil),
                Nil,
                Nil,
                SourcePos("histos.scala", 1, 1),
              )
            ),
            Splits.aggregate,
            Catalog.empty,
            None,
            Nil,
            plugins = List(RegisteredPlugin(id.plugin, plugin.contribute)(plugin)),
          )
          products <- ZIO.service[PluginProducts]
          reload = (for
            leases  <- GenerationLeases.make
            _       <- ZIO.addFinalizer(leases.retire)
            threads <- PluginThreads.make(getClass.getClassLoader)
            runtime <- PluginRuntime.loaded(
              repo,
              graph,
              getClass.getClassLoader,
              threads,
              Nil,
              ApiDigest.trusted("ab" * 32),
              leases,
              Nil,
            )
          yield runtime).provideSomeLayer[Scope](ZLayer.succeed(products))
          runtime    <- reload
          resolve    <- ZIO.service[Resolve]
          resolution <- ResolutionService.open(repo, graph).catchAll(error => ZIO.dieMessage(error.message))
          result     <- use(
            Fixture(
              repo,
              runtime,
              reload,
              behavior,
              count,
              ready,
              store,
              ContentStore.actions(store),
              resolve,
              resolution,
            )
          )
        yield result
      }
      .provide(Resolve.coursier, ZLayer.fromZIO(PluginProducts.make(publication)))

  private final case class Producer(
      behavior: Ref[Behavior],
      count: Ref[Int],
      ready: Promise[Nothing, Unit],
      hold: Promise[Nothing, Unit],
  ) extends HistosPlugin:
    val id: PluginId             = PluginRuntimeProductsSpec.id.plugin
    def contribute: Contribution = Contribution(List(task), Nil, Nil)
    def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      for
        label <- ZIO.fromEither(request.args.require(ParamName("label")))
        _     <- count.update(_ + 1)
        dir   <- ZIO.fromEither(destination(request.directory(site), request.repo.resolve("site")))
        file  <- ZIO.fromEither(destination(request.file(generated), request.repo.resolve("generated/Generated.scala")))
        _     <- write(dir.resolve("index.html"), label)
        _     <- write(dir.resolve("assets/client.js"), label)
        _     <- write(dir.resolve("stamp"), label)
        _     <- write(file, label)
        selected <- behavior.get
        _        <- selected match
          case Behavior.Publish => ZIO.unit
          case Behavior.Fail    => ZIO.fail(PluginRunError.Failed(PluginDetail("staged failure")))
          case Behavior.Hold    => ready.succeed(()) *> hold.await
      yield PluginOutput(
        label,
        List(WrittenFile(generated, RepoPath("generated/Generated.scala"))),
        List(WrittenDirectory(site, RepoPath("site"))),
      )

  private def destination(found: Either[PluginRunError, Path], legacy: Path): Either[PluginRunError, Path] = found match
    case Left(PluginRunError.MissingDestination(_)) => Right(legacy)
    case other                                      => other

  private def write(path: Path, text: String): IO[PluginRunError, Unit] =
    ZIO
      .attemptBlocking {
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.writeString(path, text)
        ()
      }
      .orElseFail(PluginRunError.Defect("write fixture"))

  private def hostFacts: UIO[Unit] =
    ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")): key =>
      Option(System.getProperty(key)) match
        case Some(value) if value.nonEmpty => TestSystem.putProperty(key, value)
        case _                             => ZIO.dieMessage(s"the test JVM has no $key")
end PluginRuntimeProductsSpec
