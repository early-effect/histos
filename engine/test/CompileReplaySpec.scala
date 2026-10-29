package histos.zinc

import histos.cache.{CacheCleanMode, CacheEntry, CacheProject, ContentStore}
import histos.clean.Products
import histos.native.NativeOrigin
import histos.schedule.Digest
import histos.workspace.Workspace
import histos.{Jvm, ProjectName, Projects, ScalaVersion, module}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object CompileReplaySpec extends histos.SuiteClock:
  private val scala3 = ScalaVersion("3.9.0")

  private final case class Saved(
      zip: Chunk[Byte],
      core: Chunk[Byte],
      module: Chunk[Byte],
      api: String,
      product: String,
  )

  private val replay = suite("replay after clean")(
    test("a wiped compile restores the analysis and does not start the compiler"):
      ZIO.scoped:
        for
          jdk   <- GraphCompile.jdkHere
          root  <- temp("histos-replay")
          cas   <- temp("histos-replay-cas")
          count <- Ref.make(0)
          store = ContentStore.in(cas)
          built <- ZIO.fromEither(Projects(module("core", Jvm)(scala3)))
          _     <- ZIO.attempt {
            write(root, "histos.scala", buildFile)
            write(root, "core/src/main/scala/Core.scala", "object Core:\n  def n: Int = 1\n")
            write(root, ".histos/dependencies.lock", "cell core 3.9.0 jvm\n")
          }
          env = held(store, count)
          first   <- cells(root, built, jdk, env)
          entries <- store.list
          stored = archive(entries, first)
          before  <- saved(root)
          now     <- zio.Clock.ClockLive.instant
          dry     <- store.clean(now, ContentStore.Retention, CacheCleanMode.DryRun)
          removed <- store.clean(now, ContentStore.Retention, CacheCleanMode.Remove)
          _       <- Products.wipe(root)
          wiped   <- ZIO.attempt(missing(root))
          kept    <- readEntry(store, stored)
          second  <- cells(root, built, jdk, env)
          after   <- saved(root)
          opened  <- count.get
          _       <- Products.wipe(root)
          dropped <- store.forget(CacheProject.of(root), now)
          gone    <- readEntry(store, stored)
          _       <- cells(root, built, jdk, env)
          again   <- count.get
          done = finished(second)
        yield assertTrue(
          stored.isDefined,
          digestOf(first) == stored.map(_.digest),
          kept.exists(bytes => bytes.take(4) == magic),
          dry.isEmpty,
          removed.isEmpty,
          wiped,
          before == after,
          opened == 1,
          done.exists(cell =>
            !cell.changed && cell.classesWritten.contains(0) && cell.sources.contains(SourceCount(0, 1))
          ),
          done.exists(_.classFiles == 2),
          stored.exists(entry => dropped.exists(_.digest == entry.digest)),
          gone.isEmpty,
          again == 2,
        )
    ,
    test("a corrupt blob is a stored failure and does not start the compiler"):
      ZIO.scoped:
        for
          jdk   <- GraphCompile.jdkHere
          root  <- temp("histos-replay-bad")
          cas   <- temp("histos-replay-bad-cas")
          count <- Ref.make(0)
          store = ContentStore.in(cas)
          built <- ZIO.fromEither(Projects(module("core", Jvm)(scala3)))
          _     <- ZIO.attempt {
            write(root, "histos.scala", buildFile)
            write(root, "core/src/main/scala/Core.scala", "object Core:\n  def n: Int = 1\n")
            write(root, ".histos/dependencies.lock", "cell core 3.9.0 jvm\n")
          }
          env = held(store, count)
          first   <- cells(root, built, jdk, env)
          entries <- store.list
          _       <- ZIO.foreach(archive(entries, first))(entry =>
            ZIO.attempt(Files.write(entry.path, "nope".getBytes(StandardCharsets.UTF_8)))
          )
          _      <- Products.wipe(root)
          feeds  <- cells(root, built, jdk, env)
          opened <- count.get
        yield assertTrue(
          feeds.exists:
            case CompileFeed.Failed(GraphCompileError.Failed(_, _, _, CompileError.Stored(_))) => true
            case _                                                                             => false
          ,
          !feeds.exists:
            case CompileFeed.Done(_) => true
            case _                   => false
          ,
          opened == 1,
        ),
  )

  private def held(store: ContentStore, count: Ref[Int]) =
    ZLayer.fromZIO(ZIO.serviceWith[histos.resolve.LockedTools](_.resolve)) ++
      ZLayer.succeed(store) ++
      Compiler.counted(count)

  private def cells(
      root: Path,
      built: histos.ProjectGraph,
      jdk: histos.jdk.JdkRuntime,
      env: ZLayer[histos.resolve.LockedTools, Nothing, histos.resolve.Resolve & ContentStore & Compiler],
  ) =
    histos.ToolFixture
      .streamAt(root, built)(
        GraphCompile.consoleCounted(root, built, Nil, jdk, NativeOrigin.Central, None)
      )
      .runCollect
      .provideSomeLayer[histos.resolve.LockedTools](env)

  private def archive(entries: List[CacheEntry], feeds: Chunk[CompileFeed]): Option[CacheEntry] =
    digestOf(feeds).flatMap(id => entries.find(_.digest == id))

  private def digestOf(feeds: Chunk[CompileFeed]): Option[Digest] =
    feeds.collect { case CompileFeed.Done(cell) => cell.key }.toList match
      case Some(key) :: Nil => Digest.from(key.value)
      case _                => None

  private def finished(feeds: Chunk[CompileFeed]): Chunk[CompiledCell] =
    feeds.collect { case CompileFeed.Done(cell) => cell }

  private def readEntry(store: ContentStore, entry: Option[CacheEntry]) =
    entry match
      case Some(found) => store.read(found.digest)
      case None        => ZIO.succeed(None)

  private def saved(root: Path): ZIO[Any, Throwable, Saved] =
    val classes = GraphCompile.classesDir(root, ProjectName("core"), scala3)
    ZIO
      .fromOption(GraphCompile.analysisDirOf(classes))
      .orElseFail(new java.io.IOException(classes.toString))
      .flatMap: dir =>
        ZIO.attempt:
          def chunk(path: Path) = Chunk.fromArray(Files.readAllBytes(path))
          Saved(
            chunk(dir.resolve("inc.zip")),
            chunk(classes.resolve("Core.class")),
            chunk(classes.resolve("Core$.class")),
            Files.readString(dir.resolve(ApiDigest.fileName)),
            Files.readString(dir.resolve(ProductDigest.fileName)),
          )

  private def missing(root: Path): Boolean =
    val classes = GraphCompile.classesDir(root, ProjectName("core"), scala3)
    val zip     = GraphCompile.analysisDirOf(classes).map(_.resolve("inc.zip"))
    !Files.exists(classes.resolve("Core.class")) && !zip.exists(Files.exists(_))

  private val magic = Chunk.fromArray("HC1\n".getBytes(StandardCharsets.US_ASCII))

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp(prefix: String): ZIO[Scope, Nothing, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory(prefix)).orDie)(dir => Workspace.delete(dir).ignore)

  private val buildFile =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph = Projects(module("core", Jvm)(scala"3.9.0"))
      |""".stripMargin

  def spec = replay.provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
    histos.ToolFixture.jvm(scala3)
  ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes)
end CompileReplaySpec
