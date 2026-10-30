package histos

import histos.clean.Products
import histos.jdk.Sha256
import histos.resolve.{
  DependencyLock,
  LockError,
  LockedArtifact,
  LockedCell,
  LockedTool,
  LockedTools,
  RepoUrl,
  Repository,
  Resolve,
  ResolveError,
  ToolId,
  ToolInput,
  ToolRole,
  ToolVariant,
}
import histos.testrun.{ModuleTestError, TestFeed}
import histos.toolchain.BuildCompiler
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{
  ApiDigest,
  CompileError,
  CompileFeed,
  CompileInput,
  CompileInputs,
  CompileRow,
  CompileKey,
  CompileSeverity,
  CompiledCell,
  GraphCompile,
  GraphCompileError,
  KeyMiss,
  ProductDigest,
}
import histos.{Js, Jvm, Platform, ProjectName, Projects, ScalaVersion, ScalacFlag, TestMain, module}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.zip.{ZipEntry, ZipOutputStream}
import java.io.ByteArrayOutputStream

import zio.{Chunk, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

/** Live `Compiler` and live `ContentStore`. The store is what a hit would restore. The compiler is what a miss runs.
  * The toolchain is shared. Each test opens its own store.
  */
object ScenarioSpec extends histos.SuiteClock:
  private val scala3   = ScalaVersion("3.10.0")
  private val scala213 = ScalaVersion("2.13.18")
  private val scala33  = ScalaVersion("3.3.6")
  private val scala212 = ScalaVersion("2.12.20")
  private val scala39  = ScalaVersion("3.9.0")

  def spec = suite("scenarios")(
    restores,
    quick,
    typed,
    diagnostic,
    semantic,
    refused,
    tuple,
    script,
    signature,
    product,
    downstream,
    identity,
    incremental,
    corrupt,
    cached,
    library,
    hidden,
    pool,
  ).provideSomeLayerShared[zio.test.TestEnvironment & Scope](toolchain) @@ TestAspect.withLiveSystem @@
    TestAspect.timeout(3.minutes)

  // The store restores the bytes. The compiler is live, so a second start would be counted.
  private def restores =
    test("a second checkout compiles again, and the wiped checkout restores"):
      ZIO.scoped:
        Scenario.open.flatMap: store =>
          (for
            left   <- store.checkout
            right  <- store.checkout
            built  <- ZIO.fromEither(Projects(module("core", Jvm)(scala3)))
            _      <- left.write("core/src/main/scala/Core.scala", core)
            _      <- left.write(".histos/dependencies.lock", lock)
            _      <- right.write("core/src/main/scala/Core.scala", core)
            _      <- right.write(".histos/dependencies.lock", lock)
            first  <- left.compile(built)
            second <- right.compile(built)
            _      <- Products.wipe(left.root)
            again  <- left.compile(built)
            opened <- store.opened
          yield assertTrue(
            changed(first),
            changed(second),
            restored(again),
            opened == 2,
            differs(keyOf(first), keyOf(second)),
            matches(keyOf(first), keyOf(again)),
          )).provideSomeLayer[LockedTools & Scope](supplied(store))

  // The store holds the compile. The compiler is live, so a hit is not a stubbed skip.
  private def quick =
    test("a quick hit stays in its checkout"):
      ZIO.scoped:
        Scenario.open.flatMap: store =>
          val app = module("app", Jvm)(scala3).test(TestMain("demo.Foo"))
          (for
            left  <- store.checkout
            right <- store.checkout
            built <- ZIO.fromEither(Projects(app))
            _     <- writeApp(left)
            _     <- writeApp(right)
            first <- left.test(built)
            hit   <- left.test(built)
            other <- right.test(built)
          yield assertTrue(
            out(first).contains("alpha") && hits(first).isEmpty,
            hits(hit) == List("demo.Foo") && !out(hit).contains("alpha"),
            out(other).contains("alpha") && hits(other).isEmpty,
          )).provideSomeLayer[LockedTools & Scope](supplied(store))

  // A stubbed store could return a hit and hide the error. A stubbed compiler could invent the failure.
  private def typed =
    test("a type error is a typed failure"):
      scenario: (store, version) =>
        val app = module("app", Jvm)(version)
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(app))
          _        <- checkout.write("app/src/main/scala/Bad.scala", bad)
          _        <- checkout.write(".histos/dependencies.lock", cell("app", version))
          feeds    <- checkout.compile(built)
          opened   <- store.opened
        yield failed(feeds) match
          case GraphCompileError.Failed(_, _, _, CompileError.Failed(diagnostics)) :: Nil =>
            assertTrue(diagnostics.nonEmpty, cells(feeds).isEmpty, opened == 1)
          case other =>
            assertTrue(other.map(_.message) == List("typed failure"))

  // The compiler calls hear before it returns the failure. The store is the miss that let the compiler run.
  private def diagnostic =
    test("a compile diagnostic is on the console before the cell fails"):
      scenario: (store, version) =>
        val app = module("app", Jvm)(version)
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(app))
          _        <- checkout.write("app/src/main/scala/Bad.scala", bad)
          _        <- checkout.write(".histos/dependencies.lock", cell("app", version))
          feeds    <- checkout.compile(built)
          opened   <- store.opened
        yield
          val indexed = feeds.zipWithIndex.toList
          val errors  = indexed.collect:
            case (CompileFeed.Diagnostic(_, _, _, problem), index) if problem.severity == CompileSeverity.Error =>
              index
          val failedAt = indexed.collect { case (CompileFeed.Failed(_), index) => index }
          (errors, failedAt) match
            case (at :: _, end :: Nil) => assertTrue(at < end, opened == 1)
            case _                     => assertTrue(errors.size == 1, failedAt == List(-1))

  // Scala 2.13 reaches the compiler. The semanticdb file is what that compiler wrote.
  private def semantic =
    test("a 2.13 cell writes semanticdb and compiles its sources"):
      scenario: (store, _) =>
        val app = module("old", Jvm)(scala213)
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(app))
          _        <- checkout.write("old/src/main/scala/Old.scala", old)
          _        <- checkout.write(".histos/dependencies.lock", cell("old", scala213))
          feeds    <- checkout.compile(built)
          names    <- filesUnder(GraphCompile.classesDir(checkout.root, ProjectName("old"), scala213, Platform.Jvm))
          opened   <- store.opened
        yield assertTrue(
          changed(feeds),
          opened == 1,
          names.exists(_.endsWith(".class")),
          names.exists(_.endsWith(".semanticdb")),
        )

  // Refused in the runtime lookup, before the pool opens.
  private def refused =
    test("Scala 2.12 does not reach the compiler"):
      scenario: (store, _) =>
        val app  = module("app", Jvm)(scala212)
        val text = DependencyLock.render(
          DependencyLock(
            List(LockedCell(ProjectName("app"), scala212, Platform.Jvm, Nil, Nil, Nil)),
            Nil,
            List(LockedTool(ToolId(ToolRole.Compiler, scala212, ToolVariant.Jvm), Nil, Nil, Nil, Nil)),
          )
        )
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(app))
          _        <- checkout.write("app/src/main/scala/App.scala", box)
          _        <- checkout.write(".histos/dependencies.lock", text)
          feeds    <- checkout.compile(built)
          opened   <- store.opened
          openings <- store.openings
        yield assertTrue(label(feeds) == "unsupported", opened == 0, openings == 0)

  // Scala 3.3 rejecting the tuple is the project compiler. The engine compiler accepts it.
  private def tuple =
    test("Scala 3.10 compiles a named tuple that Scala 3.3 rejects"):
      scenario: (store, version) =>
        val app = module("app", Jvm)(version, scala33)
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(app))
          _        <- checkout.write("app/src/main/scala/Named.scala", named)
          _        <- checkout.write(".histos/dependencies.lock", cell("app", version) + cell("app", scala33))
          feeds    <- checkout.compile(built)
          opened   <- store.opened
          openings <- store.openings
        yield assertTrue(
          cells(feeds).exists(item => item.version == version && item.changed),
          labelOf(feeds, scala33) == "failed",
          opened == 2,
          openings == 2,
        )

  // One Scala compiler writes the sjsir. Scala.js is not a second opening. The test source is not on that compile.
  private def script =
    test("a js module compiles to sjsir, and its tests are not on that classpath"):
      scenario: (store, _) =>
        val app = module("web", Js)(scala39)
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(app))
          _        <- checkout.write("web/src/main/scala/App.scala", box)
          _        <- checkout.write("web/src/test/scala/Hidden.scala", hiddenSource)
          _        <- checkout.write(".histos/dependencies.lock", cell("web", scala39, Platform.Js))
          first    <- checkout.compile(built)
          names    <- filesUnder(GraphCompile.classesDir(checkout.root, ProjectName("web"), scala39, Platform.Js))
          _        <- checkout.write("web/src/main/scala/App.scala", seesHidden)
          second   <- checkout.compile(built)
          opened   <- store.opened
          openings <- store.openings
        yield assertTrue(
          names.exists(_.endsWith(".sjsir")),
          !names.exists(_.contains("Hidden")),
          label(second) == "failed",
          textOf(second).contains("Hidden"),
          changed(first),
          opened == 2,
          openings == 1,
        )

  private def signature =
    test("a comment and a method body keep the public signature, and a new parameter moves it"):
      scenario: (store, version) =>
        val app = module("app", Jvm)(version)
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(app))
          _        <- checkout.write("app/src/main/scala/Box.scala", box)
          _        <- checkout.write(".histos/dependencies.lock", cell("app", version))
          _        <- checkout.compile(built)
          first    <- apiOf(checkout.root, "app", version)
          _        <- checkout.write("app/src/main/scala/Box.scala", commented)
          _        <- checkout.compile(built)
          noted    <- apiOf(checkout.root, "app", version)
          _        <- checkout.write("app/src/main/scala/Box.scala", body)
          _        <- checkout.compile(built)
          edited   <- apiOf(checkout.root, "app", version)
          _        <- checkout.write("app/src/main/scala/Box.scala", parameter)
          _        <- checkout.compile(built)
          signed   <- apiOf(checkout.root, "app", version)
        yield assertTrue(first == noted, first == edited, first != signed)

  private def product =
    test("a comment keeps the product digest, and a method body moves it"):
      scenario: (store, version) =>
        val app = module("app", Jvm)(version)
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(app))
          _        <- checkout.write("app/src/main/scala/Box.scala", box)
          _        <- checkout.write(".histos/dependencies.lock", cell("app", version))
          _        <- checkout.compile(built)
          first    <- productOf(checkout.root, "app", version)
          _        <- checkout.write("app/src/main/scala/Box.scala", commented)
          _        <- checkout.compile(built)
          noted    <- productOf(checkout.root, "app", version)
          _        <- checkout.write("app/src/main/scala/Box.scala", body)
          _        <- checkout.compile(built)
          edited   <- productOf(checkout.root, "app", version)
        yield assertTrue(first == noted, first != edited)

  private def downstream =
    test("a public descriptor change invalidates the downstream class"):
      scenario: (store, version) =>
        val core = module("core", Jvm)(version)
        val app  = module("app", Jvm)(version).dependsOn(core)
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(core, app))
          _        <- checkout.write("core/src/main/scala/Core.scala", coreSource)
          _        <- checkout.write("app/src/main/scala/App.scala", appSource)
          _        <- checkout.write(".histos/dependencies.lock", cell("core", version) + cell("app", version))
          _        <- checkout.compile(built)
          before   <- stamp(classFile(checkout.root, "app", version, "App$.class"))
          api      <- apiOf(checkout.root, "core", version)
          _        <- checkout.write("core/src/main/scala/Core.scala", coreSigned)
          feeds    <- checkout.compile(built)
          after    <- stamp(classFile(checkout.root, "app", version, "App$.class"))
          signed   <- apiOf(checkout.root, "core", version)
        yield assertTrue(
          cells(feeds).size == 2,
          api != signed,
          before != after,
          before.nonEmpty,
          after.nonEmpty,
        )

  // The stored rows are the key. An unused tool has no row.
  private def identity =
    test("flags, the Scala version, the platform, the JDK, and locked tool bytes are part of the key"):
      scenario: (store, version) =>
        val plain   = module("app", Jvm)(version)
        val flagged = module("app", Jvm)(version).scalac(ScalacFlag.EncodingUtf8)
        for
          checkout <- store.checkout
          first    <- ZIO.fromEither(Projects(plain))
          second   <- ZIO.fromEither(Projects(flagged))
          _        <- checkout.write("app/src/main/scala/Box.scala", box)
          _        <- checkout.write(".histos/dependencies.lock", cell("app", version))
          feeds    <- checkout.compile(first)
          rows     <- rowsOf(checkout.root, "app", version, store)
          locked   <- readLock(checkout.root)
          _        <- checkout.write("app/src/main/scala/Box.scala", box)
          again    <- checkout.compile(second)
          opened   <- store.opened
          openings <- store.openings
        yield
          val inputs = rows.map(_.map(_.input))
          val key    = keyed(rows)
          assertTrue(
            inputs == Some(keyInputs),
            key == keyOf(feeds),
            row(rows, CompileInput.Scala) == Some(version.value),
            row(rows, CompileInput.Platform) == Some(Platform.Jvm.token),
            row(rows, CompileInput.Jdk).exists(_.nonEmpty),
            row(rows, CompileInput.Runtime).exists(_.nonEmpty),
            row(rows, CompileInput.Flags) == Some(""),
            row(rows, CompileInput.Tools) == Some(""),
            row(rows, CompileInput.Compiler) == fingerprint(locked, version),
            locked.tools.map(_.id.role) == List(ToolRole.Compiler),
            misses(again) == List(KeyMiss.Changed(List(CompileInput.Flags))),
            differs(keyOf(feeds), keyOf(again)),
            opened == 2,
            openings == 1,
          )

  private def incremental =
    test("an unchanged class stays, a neighbor is rebuilt, and a removed source deletes its class"):
      scenario: (store, version) =>
        val app = module("app", Jvm)(version)
        for
          checkout  <- store.checkout
          built     <- ZIO.fromEither(Projects(app))
          _         <- checkout.write("app/src/main/scala/Keep.scala", keep)
          _         <- checkout.write("app/src/main/scala/Touch.scala", touch)
          _         <- checkout.write("app/src/main/scala/Neighbor.scala", neighbor)
          _         <- checkout.write("app/src/main/scala/Gone.scala", gone)
          _         <- checkout.write("app/src/main/scala/Idle.scala", idle)
          _         <- checkout.write("app/src/main/scala/Idle2.scala", idle2)
          _         <- checkout.write("app/src/main/scala/Idle3.scala", idle3)
          _         <- checkout.write(".histos/dependencies.lock", cell("app", version))
          _         <- checkout.compile(built)
          kept      <- stamp(classFile(checkout.root, "app", version, "Keep$.class"))
          keptBytes <- fileBytes(classFile(checkout.root, "app", version, "Keep$.class"))
          near      <- stamp(classFile(checkout.root, "app", version, "Neighbor$.class"))
          _         <- checkout.write("app/src/main/scala/Touch.scala", touchMoved)
          _         <- Workspace.delete(checkout.root.resolve("app/src/main/scala/Gone.scala"))
          feeds     <- checkout.compile(built)
          keptNow   <- stamp(classFile(checkout.root, "app", version, "Keep$.class"))
          keptNew   <- fileBytes(classFile(checkout.root, "app", version, "Keep$.class"))
          nearNow   <- stamp(classFile(checkout.root, "app", version, "Neighbor$.class"))
          removed   <- stamp(classFile(checkout.root, "app", version, "Gone$.class"))
        yield assertTrue(
          cells(feeds).size == 1,
          kept.nonEmpty,
          kept == keptNow,
          keptBytes == keptNew,
          nearNow.nonEmpty,
          near != nearNow,
          removed.isEmpty,
        )

  // The hit is in the store. The checksum fails in the tool fetch, before that hit is loaded.
  private def corrupt =
    test("a corrupt tool is refused before a hit"):
      scenario: (store, version) =>
        val app = module("app", Jvm)(version)
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(app))
          _        <- checkout.write("app/src/main/scala/Box.scala", box)
          _        <- checkout.write(".histos/dependencies.lock", cell("app", version))
          first    <- checkout.compile(built)
          _        <- Products.wipe(checkout.root)
          locked   <- readLock(checkout.root)
          wrote    <- spoil(checkout, locked, version)
          second   <- checkout.compile(built)
          gone     <- stamp(classFile(checkout.root, "app", version, "Box$.class"))
          opened   <- store.opened
        yield assertTrue(changed(first), wrote, label(second) == "checksum", gone.isEmpty, opened == 1)

  // The stored success is a real run. The next test must not return it once the compiler checksum fails.
  private def cached =
    test("a cached test success cannot bypass a corrupt selected compiler"):
      scenario: (store, version) =>
        val app = module("app", Jvm)(version).test(TestMain("demo.Foo"))
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(app))
          _        <- checkout.write("app/src/main/scala/demo/Lib.scala", lib)
          _        <- checkout.write("app/src/test/scala/demo/Foo.scala", foo)
          _        <- checkout.write(".histos/dependencies.lock", cell("app", version))
          first    <- checkout.test(built)
          record   <- filesUnder(checkout.root.resolve(".histos").resolve("test-quick"))
          locked   <- readLock(checkout.root)
          _        <- spoil(checkout, locked, version)
          second   <- checkout.test(built).either
          opened   <- store.opened
        yield second match
          case Left(error) =>
            assertTrue(checksum(error) == "checksum", record.nonEmpty, hits(first).isEmpty, opened == 1)
          case Right(feeds) =>
            assertTrue(hits(feeds) == List("checksum"), out(feeds) == "checksum")

  // A compiler jar is fetched like any library. A jar that contains the histos build is banned before a start.
  private def library =
    test("a declared compiler is a library, and a histos api jar is not"):
      scenario: (store, version) =>
        val app = module("app", Jvm)(version)
        for
          allowed <- store.checkout
          banned  <- store.checkout
          built   <- ZIO.fromEither(Projects(app))
          _       <- allowed.write("app/src/main/scala/Box.scala", box)
          _       <- allowed.write(".histos/dependencies.lock", cell("app", version))
          _       <- allowed.compile(built)
          locked  <- readLock(allowed.root)
          added   <- declareCompiler(allowed, locked, version)
          again   <- allowed.compile(built)
          _       <- banned.write("app/src/main/scala/Box.scala", box)
          _       <- publishHistos(banned.root)
          feeds   <- banned.compile(built)
          opened  <- store.opened
        yield assertTrue(
          added,
          cells(again).size == 1,
          misses(again) == List(KeyMiss.Changed(List(CompileInput.Artifacts))),
          label(feeds) == "banned",
          opened == 2,
        )

  private def hidden =
    test("compiled code cannot see the compiler, reflect, jline, or diffutils"):
      scenario: (store, version) =>
        val app = module("app", Jvm)(version)
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(app))
          _        <- checkout.write("app/src/main/scala/SeeCompiler.scala", seeCompiler)
          _        <- checkout.write("app/src/main/scala/SeeReflect.scala", seeReflect)
          _        <- checkout.write("app/src/main/scala/SeeLine.scala", seeLine)
          _        <- checkout.write("app/src/main/scala/SeeDiff.scala", seeDiff)
          _        <- checkout.write(".histos/dependencies.lock", cell("app", version))
          feeds    <- checkout.compile(built)
          opened   <- store.opened
          text = textOf(feeds)
        yield assertTrue(
          label(feeds) == "failed",
          text.contains("value tools is not a member of scala"),
          text.contains("value runtime is not a member of reflect"),
          text.contains("value jline is not a member of org"),
          text.contains("value github is not a member of com"),
          opened == 1,
        )

  private def pool =
    test("two compiles on one pool open the compiler once"):
      scenario: (store, version) =>
        val left  = module("left", Jvm)(version)
        val right = module("right", Jvm)(version)
        for
          checkout <- store.checkout
          built    <- ZIO.fromEither(Projects(left, right))
          _        <- checkout.write("left/src/main/scala/Left.scala", box)
          _        <- checkout.write("right/src/main/scala/Right.scala", box)
          _        <- checkout.write(".histos/dependencies.lock", cell("left", version) + cell("right", version))
          feeds    <- checkout.compile(built)
          opened   <- store.opened
          openings <- store.openings
        yield assertTrue(changed(feeds), cells(feeds).size == 2, opened == 2, openings == 1)

  private def supplied(store: Store) =
    ZLayer.fromZIO(ZIO.serviceWith[LockedTools](_.resolve)) ++
      ZLayer.succeed(store.content) ++
      store.compiler

  private def writeApp(checkout: Checkout): ZIO[Any, IoFailure, Unit] =
    checkout.write("app/src/main/scala/demo/Lib.scala", lib) *>
      checkout.write("app/src/test/scala/demo/Foo.scala", foo) *>
      checkout.write(".histos/dependencies.lock", "cell app 3.10.0 jvm\n")

  private def changed(feeds: Chunk[CompileFeed]): Boolean =
    val written = done(feeds)
    written.nonEmpty && written.forall(cell => cell.changed && cell.classesWritten.exists(_ > 0))

  private def restored(feeds: Chunk[CompileFeed]): Boolean =
    done(feeds).exists(cell => !cell.changed && cell.classesWritten.contains(0))

  private def done(feeds: Chunk[CompileFeed]): Chunk[CompiledCell] =
    feeds.collect { case CompileFeed.Done(cell) => cell }

  private def keyOf(feeds: Chunk[CompileFeed]): Option[Sha256] =
    done(feeds).flatMap(cell => Chunk.fromIterable(cell.key.toList)).toList match
      case key :: Nil => Some(key)
      case _          => None

  private def differs(left: Option[Sha256], right: Option[Sha256]): Boolean =
    (left, right) match
      case (Some(one), Some(other)) => one != other
      case _                        => false

  private def matches(left: Option[Sha256], right: Option[Sha256]): Boolean =
    (left, right) match
      case (Some(one), Some(other)) => one == other
      case _                        => false

  private def out(feeds: Chunk[TestFeed]): String =
    feeds.collect { case TestFeed.Out(text) => text }.mkString("\n")

  private def hits(feeds: Chunk[TestFeed]): List[String] =
    feeds.collect { case TestFeed.Cached(_, _, _, suite) => suite }.toList

  private val lock =
    "cell core 3.10.0 jvm\n"

  private val core =
    """object Core:
      |  def n: Int = 1
      |""".stripMargin

  private val lib =
    """package demo
      |
      |object Lib:
      |  def word: String = "alpha"
      |""".stripMargin

  private val foo =
    """package demo
      |
      |object Foo:
      |  def main(args: Array[String]): Unit =
      |    println(Lib.word)
      |""".stripMargin

  private def toolchain =
    ZLayer.scoped:
      BuildCompiler.version.flatMap: version =>
        ZIO
          .service[LockedTools]
          .provideSomeLayer[Scope](
            ToolFixture.layer(
              Projects(
                module("jvm", Jvm)(version, scala213, scala33),
                module("web", Js)(scala39),
              )
            )
          )

  private def scenario[E, A](
      body: (
          Store,
          ScalaVersion,
      ) => ZIO[
        LockedTools & Scope & Resolve & histos.cache.ContentStore & histos.zinc.Compiler,
        E,
        A,
      ]
  ): ZIO[LockedTools, E | IoFailure | ResolveError, A] =
    BuildCompiler.version.flatMap: version =>
      ZIO.scoped:
        Scenario.open.flatMap: store =>
          body(store, version).provideSomeLayer[LockedTools & Scope](supplied(store))

  private def cell(name: String, version: ScalaVersion, platform: Platform = Platform.Jvm): String =
    s"cell $name ${version.value} ${platform.token}\n"

  private def failed(feeds: Chunk[CompileFeed]): List[GraphCompileError] =
    feeds.collect { case CompileFeed.Failed(error) => error }.toList

  private def cells(feeds: Chunk[CompileFeed]): List[CompiledCell] =
    feeds.collect { case CompileFeed.Done(cell) => cell }.toList

  private def misses(feeds: Chunk[CompileFeed]): List[KeyMiss] =
    feeds.collect { case CompileFeed.Miss(_, _, _, change) => change }.toList

  private def label(feeds: Chunk[CompileFeed]): String =
    feeds.collect { case CompileFeed.Failed(error) => word(error) }.mkString(",")

  private def labelOf(feeds: Chunk[CompileFeed], version: ScalaVersion): String =
    feeds
      .collect {
        case CompileFeed.Failed(error @ GraphCompileError.Failed(_, found, _, _)) if found == version =>
          word(error)
      }
      .mkString(",")

  private def textOf(feeds: Chunk[CompileFeed]): String =
    val problems   = feeds.collect { case CompileFeed.Diagnostic(_, _, _, problem) => problem.text }
    val failedText = feeds.collect:
      case CompileFeed.Failed(GraphCompileError.Failed(_, _, _, CompileError.Failed(diagnostics))) =>
        diagnostics.map(_.text).mkString("\n")
      case CompileFeed.Failed(error) => error.message
    (problems ++ failedText).mkString("\n")

  private def word(error: GraphCompileError): String =
    error match
      case GraphCompileError.Failed(_, _, _, inner) => compileWord(inner)
      case GraphCompileError.Cells(errors)          => errors.map(word).mkString(",")
      case GraphCompileError.Lock(inner)            => resolveWord(inner)
      case other                                    => other.message

  private def compileWord(error: CompileError): String =
    error match
      case CompileError.UnsupportedScala(_)                         => "unsupported"
      case CompileError.Resolved(ResolveError.Checksum(_, _, _))    => "checksum"
      case CompileError.Banned(_, _)                                => "banned"
      case CompileError.Failed(diagnostics) if diagnostics.nonEmpty => "failed"
      case CompileError.Resolved(inner)                             => resolveWord(inner)
      case other                                                    => other.message

  private def resolveWord(error: ResolveError): String =
    error match
      case ResolveError.Checksum(_, _, _) => "checksum"
      case other                          => other.message

  private def checksum(
      error: ModuleTestError | ResolveError | IoFailure | GraphCompileError
  ): String =
    error match
      case ModuleTestError.Compile(found) => word(found)
      case ModuleTestError.Lock(found)    => resolveWord(found)
      case found: GraphCompileError       => word(found)
      case found: ResolveError            => resolveWord(found)
      case found: IoFailure               => found.message
      case found: ModuleTestError         => found.message

  private def apiOf(root: Path, module: String, version: ScalaVersion): ZIO[Any, CompileError, Sha256] =
    digestOf(root, module, version, ApiDigest.read)

  private def productOf(root: Path, module: String, version: ScalaVersion): ZIO[Any, CompileError, Sha256] =
    digestOf(root, module, version, ProductDigest.read)

  private def digestOf(
      root: Path,
      module: String,
      version: ScalaVersion,
      read: Path => Either[CompileError, Sha256],
  ): ZIO[Any, CompileError, Sha256] =
    val classes = GraphCompile.classesDir(root, ProjectName.trusted(module), version, Platform.Jvm)
    GraphCompile.analysisDirOf(classes) match
      case Some(dir) => ZIO.fromEither(read(dir))
      case None      => ZIO.fail(CompileError.Io(IoFailure(classes, IoFault.Stat)))

  private def rowsOf(
      root: Path,
      module: String,
      version: ScalaVersion,
      store: Store,
  ): ZIO[Any, CompileError, Option[List[CompileRow]]] =
    val classes = GraphCompile.classesDir(root, ProjectName.trusted(module), version, Platform.Jvm)
    GraphCompile.analysisDirOf(classes) match
      case None      => ZIO.succeed(None)
      case Some(dir) =>
        store.content
          .read(CompileInputs.slot(dir))
          .mapError(CompileError.Stored(_))
          .map(_.flatMap(CompileInputs.decode))

  private def row(rows: Option[List[CompileRow]], input: CompileInput): Option[String] =
    rows.flatMap(_.collectFirst { case CompileRow(found, value) if found == input => value })

  private def keyed(rows: Option[List[CompileRow]]): Option[Sha256] =
    rows.flatMap: found =>
      found
        .collectFirst { case CompileRow(CompileInput.Compiler, value) => value }
        .flatMap: raw =>
          val hashes = raw.split("\n").toList.filter(_.nonEmpty).map(Sha256.from)
          hashes
            .foldLeft[Option[List[Sha256]]](Some(Nil)):
              case (None, _)               => None
              case (Some(_), Left(_))      => None
              case (Some(acc), Right(sha)) => Some(acc :+ sha)
            .map: parsed =>
              val declared = found.filter(_.input != CompileInput.Compiler)
              CompileKey.withTools(Sha256.of(CompileInputs.encode(declared)), parsed)

  private val keyInputs = List(
    CompileInput.Scala,
    CompileInput.Platform,
    CompileInput.Jdk,
    CompileInput.Runtime,
    CompileInput.Flags,
    CompileInput.Sources,
    CompileInput.Artifacts,
    CompileInput.Apis,
    CompileInput.Tools,
    CompileInput.Compiler,
  )

  private def fingerprint(lock: DependencyLock, version: ScalaVersion): Option[String] =
    lock.tools.collectFirst:
      case tool if tool.id.role == ToolRole.Compiler && tool.id.scalaVersion == version =>
        val rows = tool.artifacts.map: artifact =>
          ToolInput.Row(
            artifact.organization,
            artifact.name,
            artifact.revision,
            artifact.classifier,
            artifact.sha256,
          )
        ToolInput.fingerprint(tool.id, rows).value

  private def readLock(root: Path): ZIO[Any, IoFailure | LockError, DependencyLock] =
    Workspace.readString(Resolve.lockPath(root)).flatMap(text => ZIO.fromEither(DependencyLock.parse(text)))

  private def spoil(checkout: Checkout, lock: DependencyLock, version: ScalaVersion): ZIO[Any, IoFailure, Boolean] =
    val bad            = Sha256.of(Chunk.fromArray("corrupt-tool".getBytes(StandardCharsets.UTF_8)))
    val (tools, wrote) = lock.tools.foldLeft((List.empty[LockedTool], false)):
      case ((acc, false), tool) if tool.id.role == ToolRole.Compiler && tool.id.scalaVersion == version =>
        val (artifacts, changed) = tool.artifacts.foldLeft((List.empty[LockedArtifact], false)):
          case ((found, false), artifact) if artifact.name.contains("scala3-compiler") =>
            (found :+ artifact.copy(sha256 = bad), true)
          case ((found, seen), artifact) =>
            (found :+ artifact, seen)
        (acc :+ tool.copy(artifacts = artifacts), changed)
      case ((acc, seen), tool) =>
        (acc :+ tool, seen)
    Workspace.writeString(Resolve.lockPath(checkout.root), DependencyLock.render(lock.copy(tools = tools))).as(wrote)

  private def declareCompiler(
      checkout: Checkout,
      lock: DependencyLock,
      version: ScalaVersion,
  ): ZIO[Any, IoFailure, Boolean] =
    val artifact = lock.tools
      .collectFirst:
        case tool if tool.id.role == ToolRole.Compiler && tool.id.scalaVersion == version =>
          tool.artifacts.find(_.name.contains("scala3-compiler"))
      .flatten
    artifact match
      case None        => ZIO.succeed(false)
      case Some(found) =>
        val cells = lock.cells.map: cell =>
          if cell.module.value == "app" && cell.scalaVersion == version && cell.platform == Platform.Jvm then
            cell.copy(artifacts = cell.artifacts :+ found)
          else cell
        Workspace
          .writeString(Resolve.lockPath(checkout.root), DependencyLock.render(lock.copy(cells = cells)))
          .as(cells != lock.cells)

  private def publishHistos(root: Path): ZIO[Any, IoFailure | ResolveError, Unit] =
    BuildCompiler.version.flatMap: version =>
      ZIO
        .attemptBlocking(publishFiles(root))
        .mapError(_ => IoFailure(root.resolve("repo"), IoFault.Write))
        .flatMap: (repo, jar) =>
          val artifact = LockedArtifact(
            "dev.histos",
            "api",
            "1.0.0",
            Sha256.of(Chunk.fromArray(jar)),
            Repository.Maven(RepoUrl.file(repo)),
          )
          val lock = DependencyLock(
            List(LockedCell(ProjectName("app"), version, Platform.Jvm, Nil, List(artifact), Nil))
          )
          Workspace.writeString(Resolve.lockPath(root), DependencyLock.render(lock))

  private def publishFiles(root: Path): (Path, Array[Byte]) =
    val repo = root.resolve("repo")
    val jar  = histosJar
    val dir  = List("dev", "histos", "api", "1.0.0").foldLeft(repo)(_.resolve(_))
    Files.createDirectories(dir)
    val jarName  = "api-1.0.0.jar"
    val pomName  = "api-1.0.0.pom"
    val pomBytes = pom.getBytes(StandardCharsets.UTF_8)
    Files.write(dir.resolve(jarName), jar)
    Files.write(dir.resolve(pomName), pomBytes)
    Files.writeString(dir.resolve(s"$jarName.sha1"), sha1(jar) + "\n")
    Files.writeString(dir.resolve(s"$pomName.sha1"), sha1(pomBytes) + "\n")
    (repo, jar)

  private def histosJar: Array[Byte] =
    val buffer = new ByteArrayOutputStream
    val zip    = new ZipOutputStream(buffer)
    try
      zip.putNextEntry(new ZipEntry("histos/HistosBuild.class"))
      zip.write(Array[Byte](0xca.toByte, 0xfe.toByte, 0xba.toByte, 0xbe.toByte))
      zip.closeEntry()
    finally zip.close()
    buffer.toByteArray

  private def sha1(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-1").digest(bytes).map(byte => f"${byte & 0xff}%02x").mkString

  private val pom =
    """<?xml version="1.0" encoding="UTF-8"?>
      |<project>
      |  <modelVersion>4.0.0</modelVersion>
      |  <groupId>dev.histos</groupId>
      |  <artifactId>api</artifactId>
      |  <version>1.0.0</version>
      |</project>
      |""".stripMargin

  private def filesUnder(root: Path): ZIO[Any, IoFailure, List[String]] =
    ZIO
      .attemptBlocking:
        if !Files.exists(root) then Left(IoFailure(root, IoFault.Walk))
        else
          val walk = Files.walk(root)
          try
            import _root_.scala.jdk.CollectionConverters.*
            Right(
              walk.iterator.asScala
                .filter(path => Files.isRegularFile(path))
                .map(path => root.relativize(path).iterator.asScala.map(_.toString).mkString("/"))
                .toList
                .sorted
            )
          finally walk.close()
      .mapError(_ => IoFailure(root, IoFault.Walk))
      .flatMap(ZIO.fromEither)

  private def fileBytes(path: Path): ZIO[Any, IoFailure, Option[Sha256]] =
    ZIO
      .attemptBlocking:
        if Files.isRegularFile(path) then Some(Sha256.of(Chunk.fromArray(Files.readAllBytes(path))))
        else None
      .mapError(_ => IoFailure(path, IoFault.Read))

  private def stamp(path: Path): ZIO[Any, IoFailure, Option[(Long, Long)]] =
    ZIO
      .attemptBlocking:
        if Files.exists(path) then Some((Files.getLastModifiedTime(path).toMillis, Files.size(path)))
        else None
      .mapError(_ => IoFailure(path, IoFault.Stat))

  private def classFile(root: Path, module: String, version: ScalaVersion, name: String): Path =
    GraphCompile.classesDir(root, ProjectName.trusted(module), version, Platform.Jvm).resolve(name)

  private val bad =
    """object Bad:
      |  def n: Int = "no"
      |""".stripMargin

  private val old =
    """object Old {
      |  def n: Int = 1
      |}
      |""".stripMargin

  private val box =
    """object Box:
      |  def n: Int = 1
      |""".stripMargin

  private val commented =
    """object Box:
      |  def n: Int = 1
      |  // noted
      |""".stripMargin

  private val body =
    """object Box:
      |  def n: Int = 2
      |""".stripMargin

  private val parameter =
    """object Box:
      |  def n(x: Int): Int = x
      |""".stripMargin

  private val named =
    """object Named:
      |  val pair: (x: Int, y: Int) = (x = 1, y = 2)
      |""".stripMargin

  private val hiddenSource =
    """object Hidden:
      |  def n: Int = 2
      |""".stripMargin

  private val seesHidden =
    """object App:
      |  def n: Int = Hidden.n
      |""".stripMargin

  private val coreSource =
    """object Core:
      |  def n: Int = 1
      |""".stripMargin

  private val coreSigned =
    """object Core:
      |  def n: String = "a"
      |""".stripMargin

  private val appSource =
    """object App:
      |  def m: Any = Core.n
      |""".stripMargin

  private val keep =
    """object Keep:
      |  def n: Int = 1
      |""".stripMargin

  private val touch =
    """object Touch:
      |  def n: Int = 1
      |""".stripMargin

  private val touchMoved =
    """object Touch:
      |  def n: String = "a"
      |""".stripMargin

  private val neighbor =
    """object Neighbor:
      |  def n: Any = Touch.n
      |""".stripMargin

  private val gone =
    """object Gone:
      |  def n: Int = 1
      |""".stripMargin

  // Zinc recompiles every source once invalidated sources exceed half of them.
  private val idle =
    """object Idle:
      |  def n: Int = 1
      |""".stripMargin

  private val idle2 =
    """object Idle2:
      |  def n: Int = 1
      |""".stripMargin

  private val idle3 =
    """object Idle3:
      |  def n: Int = 1
      |""".stripMargin

  private val seeCompiler =
    """object SeeCompiler:
      |  def n: scala.tools.nsc.Global = ???
      |""".stripMargin

  private val seeReflect =
    """object SeeReflect:
      |  def n: scala.reflect.runtime.JavaUniverse = ???
      |""".stripMargin

  private val seeLine =
    """object SeeLine:
      |  def n: org.jline.terminal.Terminal = ???
      |""".stripMargin

  private val seeDiff =
    """object SeeDiff:
      |  def n: com.github.difflib.DiffUtils = ???
      |""".stripMargin
end ScenarioSpec
