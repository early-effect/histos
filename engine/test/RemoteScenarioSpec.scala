package histos.schedule

import histos.cache.{CacheForget, CacheVerify}
import histos.jdk.{JdkRuntime, Machine, Sha256}
import histos.native.NativeOrigin
import histos.zinc.{CompileFeed, CompiledCell, GraphCompile}
import histos.{Checkout, CompilerClosure, Jvm, ProjectName, Projects, Promote, Scenario, module}

import java.nio.file.Files

import zio.{Chunk, Scope, ZIO, durationInt}
import zio.test.*

/** Compile reads and writes the remote a plugin task uses. These laws use the loopback store. */
object RemoteScenarioSpec extends ZIOSpecDefault:
  private val core =
    """object Core:
      |  def n: Int = 1
      |""".stripMargin

  private val edited =
    """object Core:
      |  def n: Int = 2
      |""".stripMargin

  private val source = "core/src/main/scala/Core.scala"

  def spec = suite("remote cache scenarios")(
    test("a second checkout compiles from the remote and does not start the compiler"):
      ZIO.scoped:
        for
          port     <- GrpcLoopback.serve(GrpcLoopback.Answer.Store)
          writer   <- CacheConfig.open(env(port, "main", ci = true))
          scala3   <- ZIO.serviceWith[CompilerClosure](_.scalaVersion)
          built    <- ZIO.fromEither(Projects.locked(module("core", Jvm)(scala3)))
          scenario <- Scenario.open
          _        <- scenario.use(writer)
          left     <- scenario.checkout(built)
          right    <- scenario.checkout(built)
          _        <- left.write(source, core)
          _        <- right.write(source, core)
          first    <- left.compile
          _        <- scenario.emptyStore
          second   <- right.compile
          starts   <- scenario.starts
        yield assertTrue(
          writer.mode == RemoteMode.Write,
          compiled(first),
          restored(second),
          keyOf(first).isDefined,
          keyOf(first) == keyOf(second),
          starts == 1,
        )
    ,
    test("a laptop write opens as read and does not store"):
      ZIO.scoped:
        for
          port     <- GrpcLoopback.serve(GrpcLoopback.Answer.Store)
          laptop   <- CacheConfig.open(env(port, "main", ci = false))
          witness  <- CacheConfig.open(env(port, "main", ci = true))
          scala3   <- ZIO.serviceWith[CompilerClosure](_.scalaVersion)
          built    <- ZIO.fromEither(Projects.locked(module("core", Jvm)(scala3)))
          scenario <- Scenario.open
          _        <- scenario.use(laptop)
          checkout <- scenario.checkout(built)
          _        <- checkout.write(source, core)
          first    <- checkout.compile
          stored   <- lookup(witness, first)
          starts   <- scenario.starts
        yield assertTrue(laptop.mode == RemoteMode.Read, compiled(first), stored == None, starts == 1)
    ,
    test("a pull request never writes main, and copy-index does what rebuild does not"):
      ZIO.scoped:
        for
          port     <- GrpcLoopback.serve(GrpcLoopback.Answer.Store)
          pull     <- CacheConfig.open(env(port, "pr/7", ci = true))
          main     <- CacheConfig.open(env(port, "main", ci = true))
          reader   <- CacheConfig.open(env(port, "main", ci = false))
          scala3   <- ZIO.serviceWith[CompilerClosure](_.scalaVersion)
          built    <- ZIO.fromEither(Projects.locked(module("core", Jvm)(scala3)))
          scenario <- Scenario.open
          _        <- scenario.use(pull)
          left     <- scenario.checkout(built)
          right    <- scenario.checkout(built)
          _        <- left.write(source, core)
          _        <- right.write(source, core)
          first    <- left.compile
          onMain   <- lookup(main, first)
          onPull   <- lookup(pull, first)
          skipped  <- copy(Promote.Rebuild, main, first, onPull)
          copied   <- copy(Promote.CopyIndex, main, first, onPull)
          _        <- scenario.emptyStore
          _        <- scenario.use(reader)
          second   <- right.compile
          starts   <- scenario.starts
        yield assertTrue(
          pull.mode == RemoteMode.Write,
          reader.mode == RemoteMode.Read,
          compiled(first),
          onMain == None,
          onPull.isDefined,
          skipped == None,
          copied == onPull,
          restored(second),
          starts == 1,
        )
    ,
    test("a compile whose sources change while it runs is not stored"):
      ZIO.scoped:
        for
          scala3   <- ZIO.serviceWith[CompilerClosure](_.scalaVersion)
          built    <- ZIO.fromEither(Projects.locked(module("core", Jvm)(scala3)))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          path = checkout.root.resolve(source)
          _      <- checkout.write(source, core)
          first  <- checkout.compileDuring(_ => ZIO.attemptBlocking(Files.writeString(path, edited)).orDie.unit)
          kept   <- held(scenario, first)
          second <- checkout.compile
          starts <- scenario.starts
        yield assertTrue(
          compiled(first),
          kept == Some(false),
          keyOf(first).isDefined,
          keyOf(second).isDefined,
          keyOf(first) != keyOf(second),
          ran(second),
          starts == 2,
        )
    ,
    test("forgetting a module drops its hit, and the next compile starts"):
      ZIO.scoped:
        for
          scala3   <- ZIO.serviceWith[CompilerClosure](_.scalaVersion)
          built    <- ZIO.fromEither(Projects.locked(module("core", Jvm)(scala3)))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          _        <- checkout.write(source, core)
          first    <- checkout.compile
          before   <- held(scenario, first)
          _        <- CacheForget.module(checkout.root, scenario.store, ProjectName("core"))
          after    <- held(scenario, first)
          second   <- checkout.compile
          starts   <- scenario.starts
        yield assertTrue(
          before == Some(true),
          after == Some(false),
          keyOf(first).isDefined,
          keyOf(first) == keyOf(second),
          ran(second),
          starts == 2,
        )
    ,
    test("verify names a tampered archive and not an honest one"):
      ZIO.scoped:
        for
          scala3   <- ZIO.serviceWith[CompilerClosure](_.scalaVersion)
          built    <- ZIO.fromEither(Projects.locked(module("core", Jvm)(scala3)))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          _        <- checkout.write(source, core)
          first    <- checkout.compile
          machine  <- Machine.load(checkout.root)
          jdk      <- GraphCompile.jdk(machine)
          honest   <- differ(checkout, jdk, machine.native, scenario)
          _        <- tamper(scenario, first)
          named    <- differ(checkout, jdk, machine.native, scenario)
        yield assertTrue(honest == Nil, named == List(ProjectName("core")))
    ,
    test("a module with no marker is not a differ"):
      ZIO.scoped:
        for
          scala3   <- ZIO.serviceWith[CompilerClosure](_.scalaVersion)
          built    <- ZIO.fromEither(Projects.locked(module("core", Jvm)(scala3)))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          machine  <- Machine.load(checkout.root)
          jdk      <- GraphCompile.jdk(machine)
          named    <- differ(checkout, jdk, machine.native, scenario)
          starts   <- scenario.starts
        yield assertTrue(named == Nil, starts == 0)
    ,
    test("purge is reachable on the loopback and does not drop a stored blob"):
      ZIO.scoped:
        for
          port   <- GrpcLoopback.serve(GrpcLoopback.Answer.Store)
          writer <- CacheConfig.openWriter(env(port, "main", ci = false), CacheNamespace.Main)
          digest = Digest.trusted("ab" * 32)
          bytes  = Chunk[Byte](1, 2, 3)
          _      <- writer.store(digest, bytes)
          purged <- writer.purge.either
          found  <- writer.find(digest)
        yield assertTrue(
          writer.mode == RemoteMode.Write,
          purged == Left(RemoteError.HttpOnly("purge")),
          found == Some(bytes),
        ),
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem @@
    TestAspect.timeout(8.minutes)

  private def env(port: Int, namespace: String, ci: Boolean): Map[String, String] =
    val base = Map(
      CacheConfig.UriKey  -> s"grpc://127.0.0.1:$port",
      CacheConfig.ModeKey -> "write",
      CacheNamespace.Key  -> namespace,
    )
    if ci then base.updated("GITHUB_ACTIONS", "true") else base

  /** Run `strategy` against the bytes `found` names. Rebuild does not store. CopyIndex does. */
  private def copy(
      strategy: Promote,
      main: RemoteCache,
      feeds: Chunk[CompileFeed],
      found: Option[Chunk[Byte]],
  ): ZIO[Any, RemoteError, Option[Chunk[Byte]]] =
    (digest(feeds), found) match
      case (Some(action), Some(bytes)) =>
        IndexPromote.run(strategy, main.store(action, bytes)) *> main.find(action)
      case _ => ZIO.none

  private def lookup(cache: RemoteCache, feeds: Chunk[CompileFeed]): ZIO[Any, RemoteError, Option[Chunk[Byte]]] =
    digest(feeds) match
      case Some(action) => cache.find(action)
      case None         => ZIO.none

  private def digest(feeds: Chunk[CompileFeed]): Option[Digest] =
    keyOf(feeds).flatMap(sha => Digest.from(sha.value))

  private def held(scenario: Scenario, feeds: Chunk[CompileFeed]): ZIO[Any, CacheError, Option[Boolean]] =
    keyOf(feeds) match
      case Some(sha) => scenario.holds(sha).map(Some(_))
      case None      => ZIO.succeed(None)

  private def differ(
      checkout: Checkout,
      jdk: JdkRuntime,
      native: NativeOrigin,
      scenario: Scenario,
  ): ZIO[histos.resolve.Resolve, CacheError | histos.workspace.IoFailure, List[ProjectName]] =
    CacheVerify.differ(checkout.root, checkout.graph, Nil, jdk, native, scenario.store)

  private def tamper(scenario: Scenario, feeds: Chunk[CompileFeed]): ZIO[Any, CacheError, Unit] =
    val wanted = keyOf(feeds).map(_.value)
    scenario.store.list.flatMap: entries =>
      entries.collectFirst { case entry if wanted.contains(entry.digest.value) => entry.path } match
        case Some(path) => ZIO.attemptBlocking(Files.writeString(path, "not an archive")).orDie.unit
        case None       => ZIO.unit

  private def compiled(feeds: Chunk[CompileFeed]): Boolean =
    done(feeds).exists(cell => cell.changed && cell.classesWritten.exists(_ >= 1))

  /** The compiler ran this round. A restore emits no miss. Zinc may write no class when the analysis already matches.
    */
  private def ran(feeds: Chunk[CompileFeed]): Boolean =
    done(feeds).nonEmpty && feeds.exists:
      case CompileFeed.Miss(_, _, _, _) => true
      case _                            => false

  private def restored(feeds: Chunk[CompileFeed]): Boolean =
    done(feeds).exists(cell => !cell.changed && cell.classesWritten.contains(0) && cell.classFiles >= 1)

  private def done(feeds: Chunk[CompileFeed]): Chunk[CompiledCell] =
    feeds.collect { case CompileFeed.Done(cell) => cell }

  private def keyOf(feeds: Chunk[CompileFeed]): Option[Sha256] =
    done(feeds).flatMap(cell => Chunk.fromIterable(cell.key)).toList match
      case key :: Nil => Some(key)
      case _          => None
end RemoteScenarioSpec
