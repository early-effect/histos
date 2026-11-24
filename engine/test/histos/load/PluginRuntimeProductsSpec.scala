package histos.load

import histos.*
import histos.cache.{ContentStore, CacheProject}
import histos.plugin.*
import histos.resolve.{ResolutionService, Resolve}
import histos.schedule.{ActionCache, RemoteCache, RemoteMode, RemoteError, Digest, PluginManifest}
import zio.Chunk
import java.nio.charset.StandardCharsets.UTF_8
import histos.version.Catalog
import histos.workspace.Workspace

import java.nio.file.{Files, Path}
import _root_.scala.jdk.OptionConverters.*

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

  private val previewId   = PluginTaskId(id.plugin, TaskName("preview"))
  private val previewTask = DeclaredTask(
    previewId.name,
    Seal.Open,
    Set(Need.Network),
    List(DeclaredInput.Directory(RepoPath("site"))),
    Nil,
    List(TaskDep.Same(id.name)),
  )

  private enum Behavior:
    case Publish, Fail, Hold, AssetsOnly

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
          case Behavior.Publish | Behavior.AssetsOnly => ZIO.unit
          case Behavior.Fail                          => ZIO.fail(ProductError.Changed)
          case Behavior.Hold                          => ready.succeed(()) *> hold.await
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
    test("a runtime hit retains the scheduler generation with one restore and no producer invocation") {
      fixture { f =>
        for
          _                <- f.run("old")
          fresh            <- f.runtime.committed(id)
          freshPaths       <- fresh.read.mapError(PluginFault.Products(_))
          freshSite        <- ZIO.fromEither(freshPaths.directory(site)).mapError(PluginFault.Products(_))
          _                <- f.reads.set(0)
          replay           <- f.run("old")
          hitReads         <- f.reads.get
          producerAfterHit <- f.count.get
          hit              <- f.runtime.committed(id)
          hitPaths         <- hit.read.mapError(PluginFault.Products(_))
          hitSite          <- ZIO.fromEither(hitPaths.directory(site)).mapError(PluginFault.Products(_))
          _                <- f.run("new")
          laterPaths       <- hit.read.mapError(PluginFault.Products(_))
          laterSite        <- ZIO.fromEither(laterPaths.directory(site)).mapError(PluginFault.Products(_))
          oldPages         <- ZIO.foreach(List(freshSite, hitSite, laterSite))(path =>
            ZIO.attemptBlocking(Files.readString(path.resolve("index.html"))).orDie
          )
          oldAsset <- ZIO.attemptBlocking(Files.readString(hitSite.resolve("assets/client.js"))).orDie
          oldStamp <- ZIO.attemptBlocking(Files.readString(hitSite.resolve("stamp"))).orDie
          current  <- f.read("site/index.html")
          count    <- f.count.get
        yield assertTrue(
          replay.text == PluginRuntime.cacheHit("old"),
          hit.manifest == fresh.manifest,
          freshSite == hitSite,
          laterSite == hitSite,
          hitReads == hit.manifest.contentDigests.size + 1,
          producerAfterHit == 1,
          count == 2,
          oldPages == List("old", "old", "old"),
          oldAsset == "old",
          oldStamp == "old",
          current == "new",
        )
      }
    },
    test("malformed cached entries preserve their product error through the runtime classifier") {
      fixture { f =>
        val invalid = new ActionCache:
          def get(digest: Digest): IO[histos.schedule.CacheError, Option[String]] =
            f.cache.get(digest).map(_.map(_.replace("assets/client.js", "../escape")))
          def put(digest: Digest, value: String): IO[histos.schedule.CacheError, Unit] =
            f.cache.put(digest, value)
        for
          _      <- f.run("old")
          failed <- f.copy(cache = invalid).run("old").either
          count  <- f.count.get
          page   <- f.read("site/index.html")
        yield assertTrue(
          failed == Left(PluginFault.Products(ProductError.InvalidEntry("../escape"))),
          count == 1,
          page == "old",
        )
      }
    },
    test("Open tasks rerun effects but persist and restore the entire committed manifest") {
      fixture(
        f =>
          for
            _       <- write(f.repo.resolve("unowned/keep.txt"), "untouched").mapError(PluginFault.Task(_))
            _       <- f.run("old")
            first   <- f.runtime.committed(id)
            before  <- snapshot(f.repo.resolve("site"))
            _       <- f.run("old")
            second  <- f.runtime.committed(id)
            reruns  <- f.count.get
            _       <- f.behavior.set(Behavior.AssetsOnly)
            _       <- f.run("new")
            third   <- f.runtime.committed(id)
            changed <- snapshot(f.repo.resolve("site"))
            asset   <- f.read("site/assets/client.js")
            stamp   <- f.read("site/stamp")
            blobs   <- ZIO.foreach(third.manifest.contentDigests)(digest =>
              f.store.read(digest).mapError(error => PluginFault.Products(ProductError.Cache(error)))
            )
            _        <- f.runtime.cleanProducts(id)
            restored <- ZIO.scoped {
              f.products
                .restore(
                  ProductRequest(f.repo, task.copy(seal = Seal.Open), InvocationId.derive("restore-open")),
                  third.manifest,
                )
                .provideSomeLayer[Scope](ZLayer.succeed(f.store))
                .mapError(PluginFault.Products(_))
            }
            after   <- snapshot(f.repo.resolve("site"))
            source  <- f.read("generated/Generated.scala")
            sibling <- f.read("unowned/keep.txt")
            count   <- f.count.get
          yield assertTrue(
            reruns == 2,
            first.manifest == second.manifest,
            first.manifest != third.manifest,
            before.filterNot((path, _) => Set("assets/client.js", "stamp").contains(path)) ==
              changed.filterNot((path, _) => Set("assets/client.js", "stamp").contains(path)),
            before != changed,
            asset == "new",
            stamp == "new",
            blobs.forall(_.nonEmpty),
            restored match
              case RestoreResult.Hit(outputs) => outputs.manifest == third.manifest
              case RestoreResult.Miss         => false,
            after == changed,
            source == "old",
            sibling == "untouched",
            count == 3,
          ),
        seal = Seal.Open,
      )
    },
    test("a real preview child holds its input lease across invocation close and reload until generation retirement") {
      fixture { f =>
        ZIO.scoped {
          for
            generation <- Scope.make
            _          <- ZIO.addFinalizer(generation.close(zio.Exit.unit))
            runtime    <- generation.extend(f.reload)
            owner = f.copy(runtime = runtime)
            output <- owner.runTask(previewId, "old")
            parts = output.text.split(":").toList
            address <- ZIO
              .fromOption(parts match
                case port :: pid :: Nil => port.toIntOption.zip(pid.toLongOption)
                case _                  => None)
              .orElseFail(PluginFault.Task(PluginRunError.Defect("preview address")))
            (port, pid) = address
            child <- ZIO
              .fromOption(ProcessHandle.of(pid).toScala)
              .orElseFail(PluginFault.Task(PluginRunError.Defect("preview child")))
            old         <- page(port)
            clean       <- runtime.cleanProducts(id).either
            next        <- f.reload
            _           <- f.copy(runtime = next).run("new")
            newer       <- page(port)
            afterReload <- next.cleanProducts(id).either
            _           <- generation.close(zio.Exit.unit)
            _           <- ZIO
              .fromCompletionStage(child.onExit())
              .orElseFail(PluginFault.Task(PluginRunError.Defect("preview exit")))
            stopped <- ZIO.succeed(!child.isAlive)
            rebound <- ZIO.acquireReleaseWith(
              ZIO
                .attemptBlocking(new java.net.ServerSocket(port))
                .orElseFail(PluginFault.Task(PluginRunError.Defect("preview socket release")))
            )(socket => ZIO.attemptBlocking(socket.close()).ignore)(_ => ZIO.succeed(true))
            _      <- next.cleanProducts(id)
            absent <- PluginProductFiles
              .io(f.repo.resolve("site"), histos.workspace.IoFault.Stat)(!Files.exists(f.repo.resolve("site")))
              .mapError(PluginFault.Products(_))
          yield assertTrue(
            old == "old",
            clean == Left(PluginFault.Products(ProductError.ActiveReaders)),
            newer == "new",
            afterReload == Left(PluginFault.Products(ProductError.ActiveReaders)),
            stopped,
            rebound,
            absent,
          )
        }
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
      reads: Ref[Int],
      ready: Promise[Nothing, Unit],
      store: ContentStore,
      cache: ActionCache,
      resolve: Resolve,
      resolution: ResolutionService,
      products: PluginProducts,
  ):
    def run(label: String): IO[PluginFault, PluginOutput]                      = run(label, RemoteCache.unconfigured)
    def run(label: String, remote: RemoteCache): IO[PluginFault, PluginOutput] =
      runTask(id, label, remote)
    def runTask(
        taskId: PluginTaskId,
        label: String,
        remote: RemoteCache = RemoteCache.unconfigured,
    ): IO[PluginFault, PluginOutput] =
      runtime
        .run(taskId, PluginArgs(List(NamedText(ParamName("label"), label)), Set.empty))
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
      seal: Seal = Seal.Hermetic,
  ): ZIO[Any, PluginFault, A] =
    ZIO
      .scoped {
        for
          repo      <- Workspace.work("runtime-products").catchAll(error => ZIO.dieMessage(error.message))
          _         <- hostFacts
          count     <- Ref.make(0)
          reads     <- Ref.make(0)
          behavior  <- Ref.make(Behavior.Publish)
          ready     <- Promise.make[Nothing, Unit]
          hold      <- Promise.make[Nothing, Unit]
          storePath <- ZIO.attemptBlocking(Files.createDirectories(repo.resolve("store"))).orDie
          store  = counting(ContentStore.in(storePath), reads)
          plugin = Producer(behavior, count, ready, hold, task.copy(seal = seal))
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
            threads <- leases.use(PluginThreads.make(getClass.getClassLoader))
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
              reads,
              ready,
              store,
              ContentStore.actions(store),
              resolve,
              resolution,
              products,
            )
          )
        yield result
      }
      .provide(Resolve.coursier, ZLayer.fromZIO(PluginProducts.make(publication)))

  private def counting(store: ContentStore, reads: Ref[Int]): ContentStore = new ContentStore:
    export store.{root, put, remove, list, replace, retain, kept, clean, forget}
    def read(digest: Digest): IO[histos.schedule.CacheError, Option[Chunk[Byte]]] =
      reads.update(_ + 1) *> store.read(digest)

  private final case class Producer(
      behavior: Ref[Behavior],
      count: Ref[Int],
      ready: Promise[Nothing, Unit],
      hold: Promise[Nothing, Unit],
      declared: DeclaredTask,
  ) extends HistosPlugin:
    val id: PluginId             = PluginRuntimeProductsSpec.id.plugin
    def contribute: Contribution = Contribution(List(declared, previewTask), Nil, Nil)
    def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      if request.id == previewId then preview(request)
      else publish(request)

    private def preview(request: RunRequest): ZIO[PluginScopes, PluginRunError, PluginOutput] =
      for
        scopes <- ZIO.service[PluginScopes]
        ready  <- Promise.make[Nothing, String]
        argv = List(
          Path.of(sys.props.getOrElse("java.home", ".")).resolve("bin/java").toString,
          "-cp",
          sys.props.getOrElse("java.class.path", ""),
          "histos.load.ProductPreviewProbe",
          request.repo.resolve("site").toString,
        )
        watch = (event: ProgramEvent) =>
          event match
            case ProgramEvent.Out(line) if line.startsWith("ready:") => ready.succeed(line.stripPrefix("ready:")).unit
            case ProgramEvent.Out(_) | ProgramEvent.Err(_)           => ZIO.unit
        fiber   <- scopes(ResourceLife.Generation).extend(Program.start(argv, request.repo, request.events, watch))
        address <- ready.await.raceFirst(fiber.join *> ZIO.fail(PluginRunError.Defect("preview ended before ready")))
      yield PluginOutput(address, Nil)

    private def publish(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
      for
        label <- ZIO.fromEither(request.args.require(ParamName("label"))).catchSome {
          case PluginRunError.MissingArg(_) => ZIO.succeed("old")
        }
        _    <- count.update(_ + 1)
        dir  <- ZIO.fromEither(destination(request.directory(site), request.repo.resolve("site")))
        file <- ZIO.fromEither(destination(request.file(generated), request.repo.resolve("generated/Generated.scala")))
        selected <- behavior.get
        page = if selected == Behavior.AssetsOnly then "old" else label
        _ <- write(dir.resolve("index.html"), page)
        _ <- write(dir.resolve("metadata.json"), "metadata")
        _ <- write(dir.resolve("guides/intro.html"), "guide")
        _ <- write(dir.resolve("assets/theme.css"), "theme")
        _ <- write(dir.resolve("assets/client.js"), label)
        _ <- write(dir.resolve("stamp"), label)
        _ <- write(file, page)
        _ <- selected match
          case Behavior.Publish | Behavior.AssetsOnly => ZIO.unit
          case Behavior.Fail                          => ZIO.fail(PluginRunError.Failed(PluginDetail("staged failure")))
          case Behavior.Hold                          => ready.succeed(()) *> hold.await
      yield PluginOutput(
        label,
        List(WrittenFile(generated, RepoPath("generated/Generated.scala"))),
        List(WrittenDirectory(site, RepoPath("site"))),
      )
  end Producer

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

  private def snapshot(root: Path): IO[PluginFault, List[(String, Option[Chunk[Byte]])]] =
    (for
      real    <- PluginProductFiles.io(root, histos.workspace.IoFault.Read)(root.toRealPath())
      paths   <- PluginProductFiles.paths(real)
      entries <- ZIO.foreach(paths.sortBy(real.relativize(_).toString)) { path =>
        PluginProductFiles.io(path, histos.workspace.IoFault.Read) {
          val bytes = if Files.isDirectory(path) then None else Some(Chunk.fromArray(Files.readAllBytes(path)))
          real.relativize(path).toString -> bytes
        }
      }
    yield entries).mapError(PluginFault.Products(_))

  private def page(port: Int): IO[PluginFault, String] =
    ZIO.acquireReleaseWith(
      ZIO
        .attemptBlocking(java.net.http.HttpClient.newHttpClient())
        .orElseFail(PluginFault.Task(PluginRunError.Defect("preview HTTP client")))
    )(client => ZIO.attemptBlocking(client.close()).ignore) { client =>
      ZIO
        .attemptBlocking {
          val request = java.net.http.HttpRequest.newBuilder(java.net.URI.create(s"http://localhost:$port/")).build()
          client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString()).body()
        }
        .orElseFail(PluginFault.Task(PluginRunError.Defect("preview HTTP")))
    }

  private def hostFacts: UIO[Unit] =
    ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")): key =>
      Option(System.getProperty(key)) match
        case Some(value) if value.nonEmpty => TestSystem.putProperty(key, value)
        case _                             => ZIO.dieMessage(s"the test JVM has no $key")
end PluginRuntimeProductsSpec

object ProductPreviewProbe extends zio.ZIOAppDefault:
  private def io[A](body: => A): IO[PluginRunError, A] =
    ZIO.attemptBlockingInterrupt(body).orElseFail(PluginRunError.Defect("preview probe IO"))

  def run: ZIO[zio.ZIOAppArgs & Scope, PluginRunError, Unit] =
    ZIO.scoped {
      for
        args   <- getArgs
        root   <- ZIO.fromOption(args.headOption).orElseFail(PluginRunError.Defect("preview root"))
        server <- ZIO.acquireRelease(io(new java.net.ServerSocket(0)))(socket => io(socket.close()).ignore)
        _      <- io {
          java.lang.System.out.println(s"ready:${server.getLocalPort}:${ProcessHandle.current().pid()}")
          java.lang.System.out.flush()
        }
        _ <- ZIO.scoped {
          ZIO.acquireRelease(io(server.accept()))(socket => io(socket.close()).ignore).flatMap { socket =>
            io {
              val input = new java.io.BufferedReader(new java.io.InputStreamReader(socket.getInputStream, UTF_8))
              input.readLine()
              val bytes  = Files.readAllBytes(Path.of(root).resolve("index.html"))
              val output = socket.getOutputStream
              output.write(
                s"HTTP/1.1 200 OK\r\nContent-Length: ${bytes.length}\r\nConnection: close\r\n\r\n".getBytes(UTF_8)
              )
              output.write(bytes)
              output.flush()
            }
          }
        }.forever
      yield ()
    }
end ProductPreviewProbe
