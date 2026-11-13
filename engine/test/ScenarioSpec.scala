package histos

import histos.doc.DocClasspath
import histos.jdk.{Sha256, Sha256Digest}
import histos.preview.QuickAnswer
import histos.program.ProgramFeed
import histos.resolve.{ClasspathScope, DependencyLock, LockedArtifact, LockedCell, Resolve, ResolveError, ToolRole}
import histos.cli.Lines
import histos.testrun.{SessionFeed, TestFeed, TestMode, TestStatus}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{
  ApiDigest,
  CompileFeed,
  CompileInput,
  CompileSeverity,
  CompiledCell,
  GraphCompile,
  GraphCompileError,
  KeyMiss,
}

import java.net.{InetAddress, InetSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.ServerSocketChannel
import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime

import zio.{Chunk, IO, Scope, UIO, URIO, ZIO, durationInt}
import zio.test.*

object ScenarioSpec extends ZIOSpecDefault:
  /** The newest Scala 3 compiler this checkout's lock pins. */
  private val locked: URIO[CompilerClosure, ScalaVersion] = ZIO.serviceWith[CompilerClosure](_.scalaVersion)

  def spec = suite("scenarios")(
    test("a second checkout of the same bytes is a hit, and the wiped checkout restores without a start"):
      ZIO.scoped:
        for
          scala3   <- locked
          built    <- ZIO.fromEither(Projects.locked(module("core", Jvm)(scala3)))
          scenario <- Scenario.open
          left     <- scenario.checkout(built)
          right    <- scenario.checkout(built)
          _        <- left.write("core/src/main/scala/Core.scala", core)
          _        <- right.write("core/src/main/scala/Core.scala", core)
          first    <- left.compile
          second   <- right.compile
          landed   <- exists(coreClass(right, scala3))
          _        <- left.clean
          wiped    <- exists(coreClass(left, scala3))
          again    <- left.compile
          back     <- exists(coreClass(left, scala3))
          starts   <- scenario.starts
        yield assertTrue(
          failed(first ++ second ++ again).isEmpty,
          compiled(first),
          restored(second),
          restored(again),
          missed(second).isEmpty,
          missed(again).isEmpty,
          landed,
          !wiped,
          back,
          starts == 1,
          matches(keyOf(first), keyOf(second)),
          matches(keyOf(first), keyOf(again)),
        )
    ,
    test("a same-size edit with the modified time put back is a sources miss, and the compiler starts again"):
      ZIO.scoped:
        for
          scala3   <- locked
          built    <- ZIO.fromEither(Projects.locked(module("core", Jvm)(scala3)))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          source = checkout.root.resolve("core/src/main/scala/Core.scala")
          _      <- checkout.write("core/src/main/scala/Core.scala", core)
          first  <- checkout.compile
          stamp  <- modified(source)
          _      <- checkout.write("core/src/main/scala/Core.scala", edited)
          _      <- restore(source, stamp)
          kept   <- modified(source)
          second <- checkout.compile
          starts <- scenario.starts
        yield assertTrue(
          core.length == edited.length,
          kept == stamp,
          failed(first ++ second).isEmpty,
          compiled(first),
          compiled(second),
          differs(keyOf(first), keyOf(second)),
          changes(second) == List(KeyMiss.Changed(List(CompileInput.Sources))),
          starts == 2,
        )
    ,
    test("a source added with the directory's modified time put back is a sources miss, and it compiles"):
      ZIO.scoped:
        for
          scala3   <- locked
          built    <- ZIO.fromEither(Projects.locked(module("core", Jvm)(scala3)))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          dir = checkout.root.resolve("core/src/main/scala")
          _      <- checkout.write("core/src/main/scala/Core.scala", core)
          first  <- checkout.compile
          stamp  <- modified(dir)
          _      <- checkout.write("core/src/main/scala/Extra.scala", extra)
          _      <- restore(dir, stamp)
          kept   <- modified(dir)
          second <- checkout.compile
          added  <- exists(GraphCompile.classesDir(checkout.root, ProjectName("core"), scala3).resolve("Extra.class"))
          starts <- scenario.starts
        yield assertTrue(
          kept == stamp,
          failed(first ++ second).isEmpty,
          compiled(second),
          differs(keyOf(first), keyOf(second)),
          changes(second) == List(KeyMiss.Changed(List(CompileInput.Sources))),
          added,
          starts == 2,
        )
    ,
    test("a source that moved to another module leaves no tasty behind, and the next compile reads the new member"):
      ZIO.scoped:
        for
          scala3 <- locked
          upstream = module("lib", Jvm)(scala3)
          built    <- ZIO.fromEither(Projects.locked(upstream, module("app", Jvm)(scala3).dependsOn(upstream)))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          app = GraphCompile.classesDir(checkout.root, ProjectName("app"), scala3)
          lib = GraphCompile.classesDir(checkout.root, ProjectName("lib"), scala3)
          _           <- checkout.write("lib/src/main/scala/histos/cli/Place.scala", stdioPlace)
          _           <- checkout.write("app/src/main/scala/histos/cli/Stdio.scala", stdioOld)
          _           <- checkout.write("app/src/main/scala/histos/cli/Use.scala", useOld)
          first       <- checkout.compile
          appClass    <- exists(app.resolve("histos/cli/Stdio.class"))
          appModule   <- exists(app.resolve("histos/cli/Stdio$.class"))
          appTasty    <- exists(app.resolve("histos/cli/Stdio.tasty"))
          _           <- Workspace.delete(checkout.root.resolve("app/src/main/scala/histos/cli/Stdio.scala"))
          _           <- checkout.write("lib/src/main/scala/histos/cli/Stdio.scala", stdioMoved)
          _           <- checkout.write("app/src/main/scala/histos/cli/Use.scala", useMoved)
          second      <- checkout.compile
          staleClass  <- exists(app.resolve("histos/cli/Stdio.class"))
          staleModule <- exists(app.resolve("histos/cli/Stdio$.class"))
          staleTasty  <- exists(app.resolve("histos/cli/Stdio.tasty"))
          useClass    <- exists(app.resolve("histos/cli/Use.class"))
          useTasty    <- exists(app.resolve("histos/cli/Use.tasty"))
          libClass    <- exists(lib.resolve("histos/cli/Stdio.class"))
          libTasty    <- exists(lib.resolve("histos/cli/Stdio.tasty"))
          placeTasty  <- exists(lib.resolve("histos/cli/Place.tasty"))
        yield assertTrue(
          failed(first).isEmpty,
          appClass,
          appModule,
          appTasty,
          failed(second).map(_.message).toList == Nil,
          !staleClass,
          !staleModule,
          !staleTasty,
          useClass,
          useTasty,
          libClass,
          libTasty,
          placeTasty,
        )
    ,
    test("an unchanged native cell is a hit that needs neither the store nor the compiler"):
      ZIO.scoped:
        for
          native   <- ZIO.serviceWithZIO[CompilerClosure](_.nativeScala)
          built    <- ZIO.fromEither(Projects.locked(module("core", Native)(native).clang(ClangMajor(21))))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          _        <- checkout.write("core/src/main/scala/Core.scala", core)
          first    <- checkout.compile
          _        <- scenario.emptyStore
          second   <- checkout.compile
          starts   <- scenario.starts
        yield assertTrue(
          failed(first ++ second).isEmpty,
          compiled(first),
          done(second).map(_.changed) == Chunk(false),
          changes(second).isEmpty,
          matches(keyOf(first), keyOf(second)),
          starts == 1,
        )
    ,
    // The timer is clang and the Scala Native linker: three misses each compile the bridge and link.
    test(
      "a stored native link is restored: a comment keeps it, a body or a path constant relinks, and a wipe restores"
    ):
      ZIO.scoped:
        for
          _      <- ZIO.serviceWithZIO[CompilerClosure](_.held(ToolRole.NativeLinker))
          native <- ZIO.serviceWithZIO[CompilerClosure](_.nativeScala)
          built  <- ZIO.fromEither(
            Projects.locked(module("core", Native)(native).clang(ClangMajor(21)).main(MainClass("demo.Run")))
          )
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          source  = "core/src/main/scala/demo/Run.scala"
          classes = GraphCompile
            .classesDir(checkout.root, ProjectName("core"), native, Platform.Native)
            .resolve("demo/Run.class")
          _         <- checkout.write(source, linked(1, "/api/v1/users"))
          first     <- checkout.compile
          binary    <- checkout.link("core")
          once      <- scenario.linkerStarts
          _         <- checkout.write(source, linked(1, "/api/v1/users") + "\n// trailing note\n")
          comment   <- checkout.compile
          kept      <- checkout.link("core")
          commented <- scenario.linkerStarts
          _         <- checkout.write(source, linked(2, "/api/v1/users"))
          body      <- checkout.compile
          edited    <- checkout.link("core")
          relinked  <- scenario.linkerStarts
          _         <- checkout.write(source, linked(2, "/api/v2/users"))
          path      <- checkout.compile
          moved     <- checkout.link("core")
          changed   <- scenario.linkerStarts
          _         <- checkout.clean
          gone      <- exists(classes)
          again     <- checkout.compile
          back      <- exists(classes)
          brought   <- checkout.link("core")
          after     <- scenario.linkerStarts
        yield assertTrue(
          failed(first ++ comment ++ body ++ path ++ again).isEmpty,
          compiled(first),
          restored(again),
          binary.nonEmpty,
          sha(kept) == sha(binary),
          sha(edited) != sha(binary),
          sha(moved) != sha(edited),
          sha(brought) == sha(moved),
          !gone,
          back,
          once == 1,
          commented == 1,
          relinked == 2,
          changed == 3,
          after == 3,
        )
    @@ TestAspect.timeout(15.minutes) @@ TestAspect.withLiveClock,
    test("a module over an older upstream compiles, hits, and documents against that upstream's own lock cell"):
      ZIO.scoped:
        for
          newest  <- locked
          older   <- ZIO.serviceWithZIO[CompilerClosure](_.previous)
          closure <- ZIO.service[CompilerClosure]
          base = module("lib", Jvm)(older)
          built <- ZIO.fromEither(Projects.locked(base, module("app", Jvm)(newest).dependsOn(base)))
          tasty = tastyCore(closure.lock, older)
          jars     <- ZIO.serviceWithZIO[Resolve](resolve => ZIO.foreach(tasty)(resolve.fetch))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          cells = List(cell(ProjectName("lib"), older, tasty), cell(ProjectName("app"), newest, Nil))
          _ <- checkout.write(
            "dependencies.lock",
            DependencyLock.render(DependencyLock(cells, tools = closure.lock.tools)),
          )
          _      <- checkout.write("lib/src/main/scala/Core.scala", core)
          _      <- checkout.write("app/src/main/scala/Extra.scala", extra)
          first  <- checkout.compile
          _      <- scenario.emptyStore
          second <- checkout.compile
          starts <- scenario.starts
          app      = done(first).find(_.module == ProjectName("app"))
          declared = built.projects.collectFirst { case found: Module if found.name == ProjectName("app") => found }
          docs <- ZIO.foreach(app.zip(declared))((compiled, module) =>
            DocClasspath.of(checkout.root, built, module, compiled).either
          )
        yield
          val libClasses = GraphCompile.classesDir(checkout.root, ProjectName("lib"), older, Platform.Jvm)
          assertTrue(
            tasty.nonEmpty,
            failed(first ++ second).isEmpty,
            done(first).map(_.module.value) == Chunk("lib", "app"),
            done(second).map(_.changed) == Chunk(false, false),
            changes(second).isEmpty,
            starts == 2,
            docs == app.map(compiled => Right(compiled.classes :: jars.map(_.path) ++ List(libClasses))),
          )
    ,
    test("a program over an older upstream runs on the classes and lock jars of that upstream's own cell"):
      ZIO.scoped:
        for
          newest  <- locked
          older   <- ZIO.serviceWithZIO[CompilerClosure](_.previous)
          closure <- ZIO.service[CompilerClosure]
          base = module("lib", Jvm)(older)
          built <- ZIO.fromEither(
            Projects.locked(base, module("app", Jvm)(newest).dependsOn(base).main(MainClass("demo.Run")))
          )
          tasty = tastyCore(closure.lock, older)
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          cells = List(cell(ProjectName("lib"), older, tasty), cell(ProjectName("app"), newest, Nil))
          _ <- checkout.write(
            "dependencies.lock",
            DependencyLock.render(DependencyLock(cells, tools = closure.lock.tools)),
          )
          _    <- checkout.write("lib/src/main/scala/demo/Reader.scala", reader)
          _    <- checkout.write("app/src/main/scala/demo/Run.scala", runs)
          seen <- checkout.run("app").runCollect.timeout(60.seconds).either
        yield assertTrue(
          tasty.nonEmpty,
          seen.map(_.map(_.collect { case ProgramFeed.Out(text) => text })) ==
            Right(Some(Chunk("dotty.tools.tasty.TastyReader"))),
          seen.exists(_.exists(_.exists { case ProgramFeed.Exited(_, _, _, code) => code == 0; case _ => false })),
        )
    @@ TestAspect.withLiveClock,
    test("a quick record over an older upstream moves when that upstream gains a public method the suite never calls"):
      ZIO.scoped:
        for
          newest   <- locked
          older    <- ZIO.serviceWithZIO[CompilerClosure](_.previous)
          built    <- ZIO.fromEither(upstreamAt(older, newest))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          _        <- writeUpstream(checkout)
          first    <- checkout.test(TestMode.Quick)
          hit      <- checkout.test(TestMode.Quick)
          _        <- checkout.write("core/src/main/scala/demo/Other.scala", widened)
          signed   <- checkout.test(TestMode.Quick)
        yield assertTrue(
          statuses(first) == List(TestStatus.Passed),
          out(first).contains("alpha 1 /api/v1/users"),
          cached(hit) == List("demo.Foo"),
          statuses(signed) == List(TestStatus.Passed),
          cached(signed).isEmpty,
        )
    ,
    test("a preview of a quick hit over an older upstream names the suite a skip"):
      ZIO.scoped:
        for
          newest   <- locked
          older    <- ZIO.serviceWithZIO[CompilerClosure](_.previous)
          built    <- ZIO.fromEither(upstreamAt(older, newest))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          _        <- writeUpstream(checkout)
          first    <- checkout.test(TestMode.Quick)
          modules = built.projects.collect { case found: Module => found }
          answer <- ZIO.foreach(modules.find(_.name == ProjectName("app")))(app =>
            QuickAnswer.read(checkout.root, app, modules, newest, Platform.Jvm, built)
          )
        yield assertTrue(
          statuses(first) == List(TestStatus.Passed),
          answer == Some(QuickAnswer.Named(List("demo.Foo"), Nil, Nil)),
        )
    ,
    test("an upstream comment keeps the quick stamp, and a new public method the suite never calls moves it"):
      ZIO.scoped:
        for
          scala3   <- locked
          built    <- ZIO.fromEither(upstream(scala3))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          otherClass = upstreamClass(checkout, scala3, "Other")
          _         <- writeUpstream(checkout)
          first     <- checkout.test(TestMode.Quick)
          stamp     <- record(checkout, scala3)
          signature <- api(checkout, scala3)
          bytes     <- digestOf(otherClass)
          _         <- checkout.write("core/src/main/scala/demo/Other.scala", other + "  // trailing note\n")
          commented <- checkout.test(TestMode.Quick)
          kept      <- record(checkout, scala3)
          unmoved   <- api(checkout, scala3)
          rebuilt   <- digestOf(otherClass)
          _         <- checkout.write("core/src/main/scala/demo/Other.scala", widened)
          signed    <- checkout.test(TestMode.Quick)
          moved     <- record(checkout, scala3)
          wider     <- api(checkout, scala3)
        yield assertTrue(
          statuses(first) == List(TestStatus.Passed),
          out(first).contains("alpha"),
          cached(first).isEmpty,
          stamp.isDefined,
          rebuilt != bytes,
          matches(signature, unmoved),
          cached(commented) == List("demo.Foo"),
          !out(commented).contains("alpha"),
          statuses(commented).isEmpty,
          kept == stamp,
          differs(signature, wider),
          statuses(signed) == List(TestStatus.Passed),
          out(signed).contains("alpha"),
          cached(signed).isEmpty,
          differs(stamp, moved),
        )
    ,
    test("a trailing comment in a class the suite calls keeps the quick stamp, and the suite stays cached"):
      ZIO.scoped:
        for
          scala3   <- locked
          built    <- ZIO.fromEither(upstream(scala3))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          libClass = upstreamClass(checkout, scala3, "Lib")
          _         <- writeUpstream(checkout)
          first     <- checkout.test(TestMode.Quick)
          stamp     <- record(checkout, scala3)
          bytes     <- digestOf(libClass)
          _         <- checkout.write(libSource, libOf(1, "/api/v1/users") + "  // trailing note\n")
          commented <- checkout.test(TestMode.Quick)
          kept      <- record(checkout, scala3)
          rebuilt   <- digestOf(libClass)
        yield assertTrue(
          statuses(first) == List(TestStatus.Passed),
          out(first).contains("alpha 1 /api/v1/users"),
          stamp.isDefined,
          rebuilt != bytes,
          cached(commented) == List("demo.Foo"),
          !out(commented).contains("alpha"),
          statuses(commented).isEmpty,
          kept == stamp,
        )
    ,
    test("a body change in a class the suite calls moves the quick stamp, a path constant's directory included"):
      ZIO.scoped:
        for
          scala3   <- locked
          built    <- ZIO.fromEither(upstream(scala3))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          _        <- writeUpstream(checkout)
          first    <- checkout.test(TestMode.Quick)
          stamp    <- record(checkout, scala3)
          _        <- checkout.write(libSource, libOf(2, "/api/v1/users"))
          body     <- checkout.test(TestMode.Quick)
          bumped   <- record(checkout, scala3)
          _        <- checkout.write(libSource, libOf(2, "/api/v2/users"))
          path     <- checkout.test(TestMode.Quick)
          moved    <- record(checkout, scala3)
        yield assertTrue(
          statuses(first) == List(TestStatus.Passed),
          out(first).contains("alpha 1 /api/v1/users"),
          statuses(body) == List(TestStatus.Passed),
          out(body).contains("alpha 2 /api/v1/users"),
          cached(body).isEmpty,
          differs(stamp, bumped),
          statuses(path) == List(TestStatus.Passed),
          out(path).contains("alpha 2 /api/v2/users"),
          cached(path).isEmpty,
          differs(bumped, moved),
        )
    ,
    test("a second checkout of the same test is not cached by the first checkout's record, a file and not a blob"):
      ZIO.scoped:
        for
          scala3   <- locked
          built    <- ZIO.fromEither(upstream(scala3))
          scenario <- Scenario.open
          left     <- scenario.checkout(built)
          right    <- scenario.checkout(built)
          _        <- writeUpstream(left)
          _        <- writeUpstream(right)
          first    <- left.test(TestMode.Quick)
          kept     <- record(left, scala3)
          hit      <- left.test(TestMode.Quick)
          before   <- record(right, scala3)
          second   <- right.test(TestMode.Quick)
          after    <- record(right, scala3)
          shared   <- ZIO.exists(kept.toList)(scenario.holds)
        yield assertTrue(
          statuses(first) == List(TestStatus.Passed),
          out(first).contains("alpha"),
          cached(first).isEmpty,
          kept.isDefined,
          cached(hit) == List("demo.Foo"),
          !out(hit).contains("alpha"),
          statuses(hit).isEmpty,
          before.isEmpty,
          statuses(second) == List(TestStatus.Passed),
          out(second).contains("alpha"),
          cached(second).isEmpty,
          after.isDefined,
          after == kept,
          !shared,
        )
    ,
    test("a compile error names its file and line, in main sources and in tests, and a warning is not the failure"):
      ZIO.scoped:
        for
          scala3   <- locked
          built    <- ZIO.fromEither(Projects.locked(module("app", Jvm)(scala3).test(TestMain("demo.Foo"))))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          lib = checkout.root.resolve("app/src/main/scala/demo/Lib.scala")
          foo = checkout.root.resolve("app/src/test/scala/demo/Foo.scala")
          _ <- checkout.write(
            "app/src/main/scala/demo/Lib.scala",
            "package demo\n\nobject Lib:\n  def word: String = 1\n",
          )
          main <- checkout.compile
          _    <- writeApp(checkout)
          _    <- checkout.write(
            "app/src/test/scala/demo/Foo.scala",
            "package demo\n\nobject Foo:\n  def n: Int = Lib.nope\n",
          )
          tested <- checkout.test(TestMode.Run).either
        yield
          val mainText = failed(main).map(_.message).mkString("\n")
          val testText = tested match
            case Left(error: histos.testrun.ModuleTestError) => error.message
            case Left(error: GraphCompileError)              => error.message
            case Left(error: ResolveError)                   => error.message
            case Right(_)                                    => ""
          assertTrue(
            mainText.contains(s"$lib:4:"),
            mainText.contains(": error: "),
            testText.contains(s"$foo:4:"),
            testText.contains(": error: "),
            testText.contains("nope"),
            !testText.contains(": warning: "),
          )
    ,
    test("a warning in a test source is on the feed, and the suite still passes"):
      ZIO.scoped:
        for
          scala3 <- locked
          built  <- ZIO.fromEither(
            Projects.locked(
              module("app", Jvm)(scala3).scalac(ScalacFlag.Deprecation).test(TestMain("demo.Foo"))
            )
          )
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          foo = checkout.root.resolve("app/src/test/scala/demo/Foo.scala")
          _     <- checkout.write("app/src/main/scala/demo/Lib.scala", deprecatedLib)
          _     <- checkout.write("app/src/test/scala/demo/Foo.scala", callsDeprecated)
          feeds <- checkout.session(TestMode.Run)
          tests = feeds.collect { case SessionFeed.Test(feed) => feed }
          shown = feeds.collect {
            case SessionFeed.Compile(CompileFeed.Diagnostic(_, _, _, problem))
                if problem.severity == CompileSeverity.Warn =>
              Lines.located(problem)
          }
        yield assertTrue(
          shown.exists(line => line.contains(foo.toString) && line.contains(":5:") && line.contains(": warn: ")),
          shown.exists(_.contains("deprecated")),
          statuses(tests) == List(TestStatus.Passed),
          failed(feeds.collect { case SessionFeed.Compile(feed) => feed }).isEmpty,
        )
    ,
    test("a restored upstream enum re-expands an unchanged test that inlines its mirror size, and clean keeps it"):
      ZIO.scoped:
        for
          scala3   <- locked
          built    <- ZIO.fromEither(Projects.locked(module("core", Jvm)(scala3).test(TestMain("demo.Check"))))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          _        <- writeMirror(checkout, threeCases, samples("A", "B", "C"))
          first    <- checkout.test(TestMode.Run)
          _        <- writeMirror(checkout, twoCases, samples("A", "B"))
          removed  <- checkout.test(TestMode.Run)
          _        <- writeMirror(checkout, threeCases, samples("A", "B", "C") + "// again\n")
          back     <- checkout.test(TestMode.Run)
          _        <- checkout.clean
          stored   <- checkout.test(TestMode.Run)
        yield assertTrue(
          out(first) == "cases 3 covered true",
          out(removed) == "cases 2 covered true",
          out(back) == "cases 3 covered true",
          out(stored) == "cases 3 covered true",
        )
    ,
    test("a running program's output arrives while it runs"):
      ZIO.scoped:
        for
          scala3   <- locked
          built    <- ZIO.fromEither(Projects.locked(module("app", Jvm)(scala3).main(MainClass("demo.Wait"))))
          scenario <- Scenario.open
          checkout <- scenario.checkout(built)
          gate     <- listen
          _        <- checkout.write("app/src/main/scala/demo/Wait.scala", waiting(gate.socket.getLocalPort))
          seen     <- checkout
            .run("app")
            .tap:
              case ProgramFeed.Out("ready") => answer(gate)
              case _                        => ZIO.unit
            .runCollect
            .timeout(60.seconds)
        yield assertTrue(
          seen.map(_.collect { case ProgramFeed.Out(text) => text }) == Some(Chunk("ready", "done")),
          seen.exists(_.exists { case ProgramFeed.Exited(_, _, _, code) => code == 0; case _ => false }),
        )
    @@ TestAspect.withLiveClock,
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer) @@ TestAspect.withLiveSystem @@
    TestAspect.timeout(3.minutes)

  private def writeApp(checkout: Checkout): IO[IoFailure, Unit] =
    checkout.write("app/src/main/scala/demo/Lib.scala", lib) *>
      checkout.write("app/src/test/scala/demo/Foo.scala", foo)

  /** `app`'s suite `demo.Foo` over `core`. */
  private def upstream(version: ScalaVersion): Either[BuildError, ProjectGraph] =
    val core = module("core", Jvm)(version)
    Projects.locked(core, module("app", Jvm)(version).dependsOn(core).test(TestMain("demo.Foo")))

  /** [[upstream]] with `core` at `older` and `app` at `newest`. */
  private def upstreamAt(older: ScalaVersion, newest: ScalaVersion): Either[BuildError, ProjectGraph] =
    val core = module("core", Jvm)(older)
    Projects.locked(core, module("app", Jvm)(newest).dependsOn(core).test(TestMain("demo.Foo")))

  /** `Lib` and `Other` in `core`. `Foo` calls `Lib` and never names `Other`. */
  private def writeUpstream(checkout: Checkout): IO[IoFailure, Unit] =
    checkout.write(libSource, libOf(1, "/api/v1/users")) *>
      checkout.write("core/src/main/scala/demo/Other.scala", other) *>
      checkout.write("app/src/test/scala/demo/Foo.scala", calls)

  private val libSource = "core/src/main/scala/demo/Lib.scala"

  private def upstreamClass(checkout: Checkout, version: ScalaVersion, name: String): Path =
    GraphCompile.classesDir(checkout.root, ProjectName("core"), version).resolve(s"demo/$name.class")

  /** `E` in main sources, its samples in tests, and `Check`, which inlines the mirror's size through two files. */
  private def writeMirror(checkout: Checkout, cases: String, values: String): IO[IoFailure, Unit] =
    checkout.write("core/src/main/scala/demo/E.scala", cases) *>
      checkout.write("core/src/test/scala/demo/Count.scala", count) *>
      checkout.write("core/src/test/scala/demo/Law.scala", law) *>
      checkout.write("core/src/test/scala/demo/Samples.scala", values) *>
      checkout.write("core/src/test/scala/demo/Check.scala", check)

  /** The `tasty-core_3` jar of the locked `version` compiler. Every Scala 3 compiler row has one. */
  private def tastyCore(lock: DependencyLock, version: ScalaVersion): List[LockedArtifact] =
    lock.tools
      .filter(row => row.id.role == ToolRole.Compiler && row.id.scalaVersion == version)
      .flatMap(_.artifacts.filter(_.name == "tasty-core_3"))

  /** A JVM compile cell of a module with no dependencies, holding `artifacts`. */
  private def cell(name: ProjectName, version: ScalaVersion, artifacts: List[LockedArtifact]): LockedCell =
    LockedCell(name, version, Platform.Jvm, Nil, artifacts, Nil, ClasspathScope.Compile)

  private def coreClass(checkout: Checkout, version: ScalaVersion): Path =
    GraphCompile.classesDir(checkout.root, ProjectName("core"), version).resolve("Core.class")

  /** The fingerprint a passing quick run stored under this checkout. */
  private def record(checkout: Checkout, version: ScalaVersion): IO[IoFailure, Option[Sha256]] =
    val path = checkout.root.resolve(s".histos/test-quick/app/${version.value}/jvm/demo/Foo")
    Workspace
      .readIfPresent(path)
      .map(_.flatMap(text => Sha256.from(text.trim.stripPrefix("success ")).toOption))

  /** The API digest the last compile of `core` wrote. */
  private def api(checkout: Checkout, version: ScalaVersion): IO[IoFailure, Option[Sha256]] =
    GraphCompile.analysisDirOf(GraphCompile.classesDir(checkout.root, ProjectName("core"), version)) match
      case Some(dir) =>
        Workspace
          .readIfPresent(dir.resolve(ApiDigest.fileName))
          .map(_.flatMap(text => Sha256.from(text.trim).toOption))
      case None => ZIO.none

  private def digestOf(path: Path): IO[IoFailure, Sha256] =
    ZIO
      .attemptBlocking(Files.readAllBytes(path))
      .mapBoth(_ => IoFailure(path, IoFault.Read), bytes => Sha256Digest.of(Chunk.fromArray(bytes)))

  private def exists(path: Path): IO[IoFailure, Boolean] =
    ZIO.attemptBlocking(Files.isRegularFile(path)).mapError(_ => IoFailure(path, IoFault.Stat))

  private def modified(path: Path): IO[IoFailure, FileTime] =
    ZIO.attemptBlocking(Files.getLastModifiedTime(path)).mapError(_ => IoFailure(path, IoFault.Stat))

  private def restore(path: Path, time: FileTime): IO[IoFailure, Unit] =
    ZIO.attemptBlocking(Files.setLastModifiedTime(path, time)).mapError(_ => IoFailure(path, IoFault.Write)).unit

  private def changes(feeds: Chunk[CompileFeed]): List[KeyMiss] =
    feeds.collect { case CompileFeed.Miss(_, _, _, change) => change }.toList

  private def compiled(feeds: Chunk[CompileFeed]): Boolean =
    done(feeds).exists(cell => cell.changed && cell.classesWritten.exists(_ >= 1))

  private def restored(feeds: Chunk[CompileFeed]): Boolean =
    done(feeds).exists(cell => !cell.changed && cell.classesWritten.contains(0) && cell.classFiles >= 1)

  private def done(feeds: Chunk[CompileFeed]): Chunk[CompiledCell] =
    feeds.collect { case CompileFeed.Done(cell) => cell }

  private def missed(feeds: Chunk[CompileFeed]): Chunk[CompileFeed] =
    feeds.collect { case miss: CompileFeed.Miss => miss }

  private def failed(feeds: Chunk[CompileFeed]): Chunk[GraphCompileError] =
    feeds.collect { case CompileFeed.Failed(error) => error }

  private def keyOf(feeds: Chunk[CompileFeed]): Option[Sha256] =
    done(feeds).flatMap(cell => Chunk.fromIterable(cell.key)).toList match
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

  /** What `app`'s suites did. `core` has none. */
  private def statuses(feeds: Chunk[TestFeed]): List[TestStatus] =
    feeds.collect { case TestFeed.Done(cell) if cell.module == ProjectName("app") => cell.status }.toList

  private def out(feeds: Chunk[TestFeed]): String =
    feeds.collect { case TestFeed.Out(_, _, _, text) => text }.mkString("\n")

  private def cached(feeds: Chunk[TestFeed]): List[String] =
    feeds.collect { case TestFeed.Cached(_, _, _, suite) => suite }.toList

  /** Prints `ready`, then blocks on a read from the gate at `port` and prints `done` once the law answers. A run that
    * holds output until exit never shows `ready`, so the law never answers.
    */
  private def waiting(port: Int): String =
    s"""package demo
       |
       |import java.net.{InetAddress, Socket}
       |
       |object Wait:
       |  def main(args: Array[String]): Unit =
       |    println("ready")
       |    System.out.flush()
       |    Socket(InetAddress.getLoopbackAddress, $port).getInputStream.read()
       |    println("done")
       |""".stripMargin

  /** A loopback listener the program connects to. The connection waits in the backlog until the law answers it. */
  private def listen: ZIO[Scope, Nothing, ServerSocketChannel] =
    ZIO
      .fromAutoCloseable(
        ZIO.attemptBlocking(ServerSocketChannel.open().bind(InetSocketAddress(InetAddress.getLoopbackAddress, 0)))
      )
      .orDie

  private def answer(gate: ServerSocketChannel): UIO[Unit] =
    ZIO
      .scoped:
        ZIO
          .fromAutoCloseable(ZIO.attemptBlockingInterrupt(gate.accept()))
          .flatMap(peer => ZIO.attemptBlockingInterrupt(peer.write(ByteBuffer.wrap(Array[Byte](1)))))
      .orDie
      .unit

  /** `Run` prints both, so the native link keeps the int and the path. */
  private def linked(f: Int, home: String): String =
    s"""package demo
       |
       |object Api:
       |  def f: Int = $f
       |  def home: String = "$home"
       |
       |object Run:
       |  def main(args: Array[String]): Unit =
       |    println(s"$${Api.f} $${Api.home}")
       |""".stripMargin

  private def sha(bytes: Chunk[Byte]): Sha256 = Sha256Digest.of(bytes)

  /** `Stdio` starts in `app` and moves to `lib`, gaining `redirected`. `Use` calls whichever member is current. */
  private val stdioOld =
    """package histos.cli
      |
      |object Stdio:
      |  def old: Int = 1
      |""".stripMargin

  private val stdioMoved =
    """package histos.cli
      |
      |object Stdio:
      |  def redirected: Int = 2
      |""".stripMargin

  private val useOld =
    """package histos.cli
      |
      |object Use:
      |  def n: Int = Stdio.old
      |""".stripMargin

  private val useMoved =
    """package histos.cli
      |
      |object Use:
      |  def n: Int = Stdio.redirected
      |""".stripMargin

  /** Keeps `lib` a module with sources before `Stdio` arrives, so the second compile is incremental. */
  private val stdioPlace =
    """package histos.cli
      |
      |object Place:
      |  def n: Int = 0
      |""".stripMargin

  private val core =
    """object Core:
      |  def n: Int = 1
      |""".stripMargin

  private val edited =
    """object Core:
      |  def n: Int = 2
      |""".stripMargin

  private val extra =
    """object Extra:
      |  def m: Int = Core.n
      |""".stripMargin

  private val threeCases =
    """package demo
      |
      |enum E:
      |  case A, B, C
      |""".stripMargin

  private val twoCases =
    """package demo
      |
      |enum E:
      |  case A, B
      |""".stripMargin

  private val count =
    """package demo
      |
      |import scala.compiletime.constValue
      |import scala.deriving.Mirror
      |
      |object Count:
      |  inline def cases[A](using m: Mirror.SumOf[A]): Int =
      |    constValue[Tuple.Size[m.MirroredElemTypes]]
      |
      |  inline def covered[A <: scala.reflect.Enum](values: List[A])(using m: Mirror.SumOf[A]): Boolean =
      |    values.map(_.ordinal) == (0 until constValue[Tuple.Size[m.MirroredElemTypes]]).toList
      |""".stripMargin

  private val law =
    """package demo
      |
      |import scala.deriving.Mirror
      |
      |object Law:
      |  inline def line[A <: scala.reflect.Enum](values: List[A])(using Mirror.SumOf[A]): String =
      |    s"cases ${Count.cases[A]} covered ${Count.covered(values)}"
      |""".stripMargin

  private def samples(names: String*): String =
    s"""package demo
       |
       |object Samples:
       |  val es: List[E] = List(${names.map(name => s"E.$name").mkString(", ")})
       |""".stripMargin

  private val check =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    println(Law.line(Samples.es))
      |""".stripMargin

  private val lib =
    """package demo
      |
      |object Lib:
      |  def word: String = "alpha"
      |""".stripMargin

  /** The call in the test source is the warning. Defining `word` does not warn. */
  private val deprecatedLib =
    """package demo
      |
      |object Lib:
      |  @deprecated("gone", "0")
      |  def word: String = "alpha"
      |""".stripMargin

  private val callsDeprecated =
    """package demo
      |
      |object Foo:
      |  def main(args: Array[String]): Unit =
      |    println(Lib.word)
      |""".stripMargin

  /** `core`'s `Lib`. `home` looks like a path, so a scrub that cuts a path to its leaf reads `v1` and `v2` as one. */
  private def libOf(f: Int, home: String): String =
    s"""package demo
       |
       |object Lib:
       |  def word: String = "alpha"
       |  def f: Int = $f
       |  def home: String = "$home"
       |""".stripMargin

  private val calls =
    """package demo
      |
      |object Foo:
      |  def main(args: Array[String]): Unit =
      |    println(s"${Lib.word} ${Lib.f} ${Lib.home}")
      |""".stripMargin

  private val other =
    """package demo
      |
      |object Other:
      |  def word: String = "other"
      |""".stripMargin

  private val widened =
    """package demo
      |
      |object Other:
      |  def word: String = "other"
      |  def extra: Int = 1
      |""".stripMargin

  private val foo =
    """package demo
      |
      |object Foo:
      |  def main(args: Array[String]): Unit =
      |    println(Lib.word)
      |""".stripMargin

  /** Names a class from `lib`'s locked `tasty-core_3` jar, so the run fails without that jar. */
  private val reader =
    """package demo
      |
      |object Reader:
      |  def name: String = classOf[dotty.tools.tasty.TastyReader].getName
      |""".stripMargin

  private val runs =
    """package demo
      |
      |object Run:
      |  def main(args: Array[String]): Unit =
      |    println(Reader.name)
      |""".stripMargin
end ScenarioSpec
