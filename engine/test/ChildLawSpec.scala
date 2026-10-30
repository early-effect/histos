package histos.testrun

import histos.jdk.{JdkRuntime, Toolchain}
import histos.native.{NativeOrigin, NativeSource, NativeToolchain}
import histos.pack.{Assembly, MainClass, MainClassError}
import histos.process.{ChildClasspath, ChildError, ChildEvent, ChildRequest, Process, ProcessExit}
import histos.program.{Program, ProgramError, ProgramFeed}
import histos.resolve.{BuildResolvers, LockedTools, Resolve, ResolveError}
import histos.toolchain.{BuildCompiler, Runtimes}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.TestMachine
import histos.zinc.GraphCompile
import histos.{
  ClangMajor,
  Dep,
  Js,
  JsEnv,
  Jvm,
  Native,
  NativeVersion,
  Platform,
  ProjectGraph,
  ProjectName,
  Projects,
  Requires,
  ScalaVersion,
  TestMain,
  Tool,
  ToolCatalog,
  module,
}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarOutputStream}
import javax.tools.ToolProvider

import _root_.scala.jdk.CollectionConverters.*
import _root_.scala.jdk.OptionConverters.*

import zio.{Chunk, Scope, System, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

/** Live children. Process laws provide `Process` and the assembly jar is their one `java -jar`. Runner laws provide the
  * toolchain: one main, one Node run, one Native link, one JVM provider, a js provider on each environment, and a
  * native provider.
  */
object ChildLawSpec extends histos.SuiteClock:
  private val exitMain  = MainClass.trusted("demo.Exit")
  private val sleepMain = MainClass.trusted("demo.Sleeper")
  private val hiMain    = MainClass.trusted("demo.Hi")
  private val app       = ProjectName("app")

  def spec =
    suite("counted children")(
      binaryName,
      retained,
      suite("process")(isolated, cancelled, refused, missing, assembled)
        .provide(Process.live) @@ TestAspect.timeout(60.seconds),
      suite("runner")(
        kept,
        idle,
        choose,
        unbuilt,
        chosen @@ Requires(Tool.Node),
        unhosted,
        linked @@ Requires(Tool.Clang(ClangMajor(21))),
        discovered,
        missed,
        matched,
        bridged @@ Requires(Tool.Node),
        dom @@ Requires(Tool.Node, Tool.Npm),
        browser @@ Requires(Tool.Firefox, Tool.Node),
        nativeSuite @@ Requires(Tool.Clang(ClangMajor(21))),
      ).provideSomeLayerShared[TestEnvironment & Scope](running) @@ TestAspect.timeout(4.minutes),
    ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem

  private def isolated =
    test("System.exit stays in the child and the resolved jar is on the classpath"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-exit")
          libDir  <- Workspace.createDirectories(root.resolve("lib-classes"))
          classes <- Workspace.createDirectories(root.resolve("classes"))
          lib     <- javac(root, libDir, "Lib.java", libSource, Nil)
          jar = root.resolve("lib.jar")
          _     <- ZIO.when(lib.code == 0)(classJar(libDir.resolve("demo").resolve("Lib.class"), "demo/Lib.class", jar))
          exit  <- javac(root, classes, "Exit.java", exitSource, List(jar))
          work  <- realDir(root.resolve("work"))
          java  <- runningJava
          found <- java match
            case Some(bin) if lib.code == 0 && exit.code == 0 =>
              val req = ChildRequest(
                Platform.Jvm,
                bin,
                ChildClasspath(classes, List(jar)),
                exitMain,
                work,
              )
              ZIO.serviceWithZIO[Process](_.run(req)).either
            case _ => ZIO.succeed(Left(ChildError.Classes(classes)))
          expected = lines("hi", "home=null", s"dir=$work")
        yield assertTrue(
          lib.code == 0 && lib.text.isEmpty,
          exit.code == 0 && exit.text.isEmpty && java.nonEmpty,
          found.fold(_.message, _.out.text) == expected,
          found.fold(_.message, _.err.text).isEmpty,
          found.exists(_.exit == ProcessExit(7)),
        )

  private def cancelled =
    test("cancel kills the sleeping test and the child in its process group"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-cancel")
          classes <- Workspace.createDirectories(root.resolve("classes"))
          built   <- javac(root, classes, "Sleeper.java", sleeperSource, Nil)
          work    <- realDir(root.resolve("work"))
          java    <- runningJava
          sleep   <- ZIO.attemptBlocking(Files.isExecutable(Path.of("/bin/sleep"))).orElseSucceed(false)
          req = java.map(bin => ChildRequest(Platform.Jvm, bin, ChildClasspath(classes, Nil), sleepMain, work))
          fiber <- req match
            case Some(asked) => ZIO.serviceWithZIO[Process](_.run(asked)).forkScoped
            case None        => ZIO.never.forkScoped
          child <- awaitPid(work.resolve("child.pid"))
          self  <- awaitPid(work.resolve("self.pid"))
          childBefore = child.exists(alive)
          selfBefore  = self.exists(alive)
          exit      <- fiber.interrupt
          childDead <- ZIO.foreach(child.toList)(awaitDead)
          selfDead  <- ZIO.foreach(self.toList)(awaitDead)
        yield assertTrue(
          built.code == 0 && built.text.isEmpty && sleep && java.nonEmpty,
          child.nonEmpty && self.nonEmpty && childBefore && selfBefore,
          exit.isInterrupted,
          childDead == List(true) && selfDead == List(true),
        )

  private def refused =
    test("js is rejected before a process starts"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-child-js")
          work <- realDir(root.resolve("work"))
          java <- runningJava
          req = ChildRequest(
            Platform.Js,
            java.getOrElse(work.resolve("java")),
            ChildClasspath(work, Nil),
            exitMain,
            work,
          )
          out <- ZIO.serviceWithZIO[Process](_.run(req)).either
        yield assertTrue(java.nonEmpty, out == Left(ChildError.UnsupportedPlatform(Platform.Js)))

  private def missing =
    test("a missing java executable is its own error"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-child-java")
          work <- realDir(root.resolve("work"))
          java = work.resolve("missing-java")
          req  = ChildRequest(Platform.Jvm, java, ChildClasspath(work, Nil), exitMain, work)
          out <- ZIO.serviceWithZIO[Process](_.run(req)).either
        yield assertTrue:
          out match
            case Left(ChildError.Java(path)) => path == java
            case _                           => false

  private def assembled =
    test("java -jar runs the assembly main"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-jar")
          classes <- Workspace.createDirectories(root.resolve("classes"))
          built   <- javac(root, classes, "Hi.java", hiSource, Nil)
          jar = root.resolve("hi.jar")
          wrote <-
            if built.code == 0 then Assembly.write(List(classes), Nil, hiMain, jar).as(true)
            else ZIO.succeed(false)
          work <- realDir(root.resolve("work"))
          home <- System.property("java.home")
          java = home.map(dir => Path.of(dir, "bin", "java"))
          events <- java match
            case Some(bin) if wrote =>
              ZIO.serviceWithZIO[Process](
                _.command(Toolchain.command(bin, jar, Nil), work, Map.empty).runCollect
              )
            case _ => ZIO.succeed(Chunk.empty)
        yield
          val said = events.collect { case ChildEvent.Out(text) => text }
          val code = events.collect { case ChildEvent.Exited(status) => status }
          val err  = events.collect { case ChildEvent.Err(text) => text }
          assertTrue(
            built.code == 0 && built.text.isEmpty && wrote && java.nonEmpty,
            said == Chunk("ran"),
            code == Chunk(ProcessExit(0)),
            err.isEmpty,
          )

  private def binaryName =
    test("a main class is a binary name"):
      assertTrue(
        MainClass.from("") == Left(MainClassError.Empty),
        MainClass.from("1bad") == Left(MainClassError.Bad("1bad")),
        MainClass.from("demo.Exit") == Right(exitMain),
        MainClass.from("demo.Exit$") == Right(MainClass.trusted("demo.Exit$")),
      )

  private def retained =
    test("a suite keeps the repo, PATH, HOME, and a non-empty cache home"):
      System.envs
        .orElseSucceed(Map.empty)
        .map: machine =>
          val env  = TestCompile.suiteEnv(Path.of("/repo"), machine + ("USER" -> "someone"))
          val kept = List("PATH", "HOME", "XDG_CACHE_HOME").flatMap: key =>
            machine.get(key).filter(_.nonEmpty).map(key -> _)
          assertTrue(env == Map("HISTOS_REPO" -> Path.of("/repo").toAbsolutePath.normalize.toString) ++ kept)

  private val running =
    ZLayer.scoped:
      BuildCompiler.version.flatMap: version =>
        val bridge = ToolCatalog.default.native.bridgeScala
        ZIO
          .service[LockedTools]
          .provideSomeLayer[Scope](
            histos.ToolFixture.layer(
              Projects(
                module("jvm", Jvm)(version),
                module("js", Js)(version),
                module("native", Native)(bridge).clang(ClangMajor(21)),
              )
            )
          )

  private def kept =
    test("a named main runs from main classes and a non-zero exit keeps its output"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-kept")
          version <- BuildCompiler.version
          graph   <- ZIO.fromEither(Projects(module("app", Jvm)(version).main(histos.MainClass("demo.Check"))))
          _       <- Workspace.writeString(root.resolve("app/src/main/scala/demo/Check.scala"), keptSource)
          jdk     <- GraphCompile.jdkHere
          here    <- TestMachine.runtimes(root)
          feeds   <- runProgram(root, graph, jdk, here)
        yield
          val said  = feeds.collect { case ProgramFeed.Out(text) => text }
          val code  = feeds.collect { case ProgramFeed.Exited(_, _, platform, status) => (platform, status) }
          val files = listed(root)
          assertTrue(
            said.contains("kept"),
            code == Chunk((Platform.Jvm, 4)),
            files.exists(path => path.contains("classes/") && path.endsWith("Check.class")),
            !files.exists(_.contains("test-classes")),
          )

  private def idle =
    test("a module with nothing to run is not a pass"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-idle")
          version <- BuildCompiler.version
          graph   <- ZIO.fromEither(Projects(module("app", Jvm)(version)))
          jdk     <- GraphCompile.jdkHere
          here    <- TestMachine.runtimes(root)
          ran     <- runProgram(root, graph, jdk, here).either
        yield assertTrue(
          ran == Left(ProgramError.NoMain(app)),
          !Files.exists(GraphCompile.classesDir(root, app, version, Platform.Jvm)),
        )

  private def choose =
    test("a multi-platform module does not start until a platform is named"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-choose")
          version <- BuildCompiler.version
          graph   <- ZIO.fromEither(
            Projects(module("app", Jvm, Js)(version).main(histos.MainClass("demo.Check")))
          )
          jdk  <- GraphCompile.jdkHere
          here <- TestMachine.runtimes(root)
          ran  <- runProgram(root, graph, jdk, here).either
        yield
          val refused = ran match
            case Left(ProgramError.Choose(name, platforms)) =>
              name == app && platforms.map(_.token).sorted == List("js", "jvm")
            case _ => false
          assertTrue(
            refused,
            !Files.exists(root.resolve(".histos").resolve("classes")),
          )

  private def unbuilt =
    test("a platform the module does not build is refused"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-unbuilt")
          version <- BuildCompiler.version
          graph   <- ZIO.fromEither(Projects(module("app", Jvm)(version).main(histos.MainClass("demo.Check"))))
          jdk     <- GraphCompile.jdkHere
          here    <- TestMachine.runtimes(root)
          ran     <- runProgram(root, graph, jdk, here, Some(Native)).either
        yield
          val refused = ran match
            case Left(ProgramError.Unbuilt(name, platform, builtOn)) =>
              name == app && platform == Native && builtOn == List(Jvm)
            case _ => false
          assertTrue(refused, !Files.exists(root.resolve(".histos").resolve("classes")))

  private def chosen =
    test("a named js main runs on the chosen node"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-node")
          version <- BuildCompiler.version
          graph   <- ZIO.fromEither(Projects(module("app", Js)(version).main(histos.MainClass("demo.Check"))))
          _       <- Workspace.writeString(root.resolve("app/src/main/scala/demo/Check.scala"), hiScala)
          binary  <- installNode(root)
          jdk     <- GraphCompile.jdkHere
          here    <- TestMachine.runtimes(root)
          feeds   <- runProgram(root, graph, jdk, here)
          args    <- launches(root.resolve("chosen-node.log"))
          logged  <- Workspace.readIfPresent(root.resolve("chosen-node.log"))
        yield
          val said = feeds.collect { case ProgramFeed.Out(text) => text }
          val code = feeds.collect { case ProgramFeed.Exited(_, _, platform, status) => (platform, status) }
          assertTrue(
            binary.nonEmpty,
            said.contains("hi"),
            code == Chunk((Platform.Js, 0)),
            args.nonEmpty,
            logged.getOrElse("missing").nonEmpty,
            !Files.exists(root.resolve(".histos").resolve("test-classes")),
          )

  private def unhosted =
    test("a js main with no node to choose fails before the link"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-nonode")
          version <- BuildCompiler.version
          graph   <- ZIO.fromEither(Projects(module("app", Js)(version).main(histos.MainClass("demo.Check"))))
          _       <- Workspace.writeString(root.resolve("app/src/main/scala/demo/Check.scala"), hiScala)
          jdk     <- GraphCompile.jdkHere
          here    <- Runtimes.read(TestMachine.at(root, Map.empty))
          ran     <- runProgram(root, graph, jdk, here).either
        yield
          val refused = ran match
            case Left(ProgramError.NoNode(name, scala, _)) => name == app && scala == version
            case _                                         => false
          val files = listed(root)
          assertTrue(
            refused,
            files.exists(_.contains("classes/")),
            !files.exists(_.contains("/run")),
          )

  private def linked =
    test("a named native main links and runs the binary"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-child-native")
          version = ToolCatalog.default.native.bridgeScala
          graph <- ZIO.fromEither(
            Projects(module("app", Native)(version).clang(ClangMajor(21)).main(histos.MainClass("demo.Check")))
          )
          _     <- Workspace.writeString(root.resolve("app/src/main/scala/demo/Check.scala"), hiScala)
          jdk   <- GraphCompile.jdkHere
          here  <- TestMachine.runtimes(root)
          feeds <- runProgram(root, graph, jdk, here)
        yield
          val said = feeds.collect { case ProgramFeed.Out(text) => text }
          val code = feeds.collect { case ProgramFeed.Exited(_, _, platform, status) => (platform, status) }
          assertTrue(
            said.contains("hi"),
            code == Chunk((Platform.Native, 0)),
            !Files.exists(root.resolve(".histos").resolve("test-classes")),
          )

  private def discovered =
    test("ZIO is the default provider, a failing suite is not a pass, and the pattern runs only that suite"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-provider")
          version <- BuildCompiler.version
          graph   <- ZIO.fromEither(
            Projects(
              module("app", Jvm)(version).testDependencies(
                Dep("dev.zio::zio-test:2.1.27"),
                Dep("dev.zio::zio-test-sbt:2.1.27"),
              )
            )
          )
          _    <- Workspace.writeString(root.resolve("app/src/test/scala/keep/Boom.scala"), boom)
          _    <- Workspace.writeString(root.resolve("app/src/test/scala/drop/Fine.scala"), fine)
          jdk  <- GraphCompile.jdkHere
          here <- TestMachine.runtimes(root)
          ran  <- SuitePattern.from("keep.*") match
            case Left(_)        => ZIO.succeed(Left(ModuleTestError.Pattern(SuitePatternError.Empty)))
            case Right(pattern) => runTests(root, graph, jdk, here, List(pattern)).either
        yield
          val failed = ran match
            case Right(feeds) =>
              val said   = feeds.collect { case TestFeed.Out(text) => text }
              val errs   = feeds.collect { case TestFeed.Err(text) => text }
              val text   = (said ++ errs).mkString("\n")
              val status = feeds.collect { case TestFeed.Done(cell) => cell.status }
              status.exists {
                case TestStatus.Failed(_) => true
                case _                    => false
              } && text.contains("keep-ran") && !text.contains("drop-ran")
            case Left(_) => false
          val detail = explain(ran)
          assertTrue(failed, detail.nonEmpty || failed)

  private def missed =
    test("a pattern that matches nothing fails the run"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-miss")
          version <- BuildCompiler.version
          graph   <- ZIO.fromEither(
            Projects(module("app", Jvm)(version).test(TestMain("demo.Check")))
          )
          _    <- Workspace.writeString(root.resolve("app/src/test/scala/demo/Check.scala"), missSource)
          jdk  <- GraphCompile.jdkHere
          here <- TestMachine.runtimes(root)
          ran  <- SuitePattern.from("other.*") match
            case Left(_)        => ZIO.succeed(Left(ModuleTestError.Pattern(SuitePatternError.Empty)))
            case Right(pattern) => runTests(root, graph, jdk, here, List(pattern)).either
        yield
          val unmatched = ran match
            case Left(ModuleTestError.NoneMatched(patterns)) => patterns.map(_.value) == List("other.*")
            case _                                           => false
          val detail = explain(ran)
          assertTrue(
            unmatched,
            detail.nonEmpty || unmatched,
            !Files.exists(TestCompile.testWork(root, app, version, Platform.Jvm)),
          )

  private def matched =
    test("a suite pattern runs the matching main"):
      ZIO.scoped:
        for
          root    <- Workspace.work("histos-child-match")
          version <- BuildCompiler.version
          graph   <- ZIO.fromEither(Projects(module("app", Jvm)(version).test(TestMain("demo.Check"))))
          _       <- Workspace.writeString(root.resolve("app/src/test/scala/demo/Check.scala"), matchSource)
          jdk     <- GraphCompile.jdkHere
          here    <- TestMachine.runtimes(root)
          ran     <- SuitePattern.from("demo.*") match
            case Left(_)        => ZIO.succeed(Left(ModuleTestError.Pattern(SuitePatternError.Empty)))
            case Right(pattern) => runTests(root, graph, jdk, here, List(pattern)).either
        yield
          val feeds  = ran.fold(_ => Chunk.empty, identity)
          val text   = feeds.collect { case TestFeed.Out(line) => line }.mkString("\n")
          val status = feeds.collect { case TestFeed.Done(cell) => cell.status }
          assertTrue(
            ran.isRight,
            status == Chunk(TestStatus.Passed),
            text.contains("ran-match"),
          )

  private def bridged =
    test("a js provider links the bridge and runs on node"):
      runJs(JsEnv.Node, "node-ran", "org.scalajs.jsenv.nodejs.NodeJSEnv")

  private def dom =
    test("a js provider links the bridge and runs under jsdom"):
      runJs(JsEnv.Jsdom, "dom-ran", "org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv")

  private def browser =
    test("a js provider links the bridge and runs under firefox"):
      runJs(JsEnv.Firefox, "firefox-ran", "chekhov.jsenv.ChekhovJSEnv")

  /** ivy-local nativelib that can unwind a PAuthLR compact frame. The catalog nativelib aborts a multithreaded
    * `TestMain` on macOS when a backtrace steps `thread_start`.
    */
  private val unwindFix = NativeVersion("0.5.13-SNAPSHOT")

  private def unwindNativelib: UIO[Boolean] =
    System
      .property("user.home")
      .orElseSucceed(None)
      .flatMap:
        case None      => ZIO.succeed(false)
        case Some(dir) =>
          val published =
            Path.of(dir, ".ivy2", "local", "org.scala-native", "nativelib_native0.5_3", unwindFix.value)
          ZIO.attemptBlocking(Files.exists(published)).orElseSucceed(false)

  private def nativeSuite =
    test("a native provider links TestMain and runs the binary"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-child-native-provider")
          version = ToolCatalog.default.native.bridgeScala
          fixed <- unwindNativelib
          declared =
            val started = module("app", Native)(version).clang(ClangMajor(21))
            if fixed then started.nativeVersion(unwindFix) else started
          graph <- ZIO.fromEither(
            Projects(
              declared.testDependencies(
                Dep("dev.zio::zio-test:2.1.27"),
                Dep("dev.zio::zio-test-sbt:2.1.27"),
              )
            )
          )
          _ <- Workspace.writeString(root.resolve("app/src/test/scala/demo/Check.scala"), providerSource("native-ran"))
          jdk  <- GraphCompile.jdkHere
          env  <- System.envs.orElseSucceed(Map.empty)
          here <- Runtimes.read(
            TestMachine.at(root, if fixed then env.updated(NativeToolchain.Key, "ivy-local") else env)
          )
          origin = if fixed then NativeOrigin.Chosen(NativeSource.IvyLocal) else NativeOrigin.Central
          ran    <- runTests(root, graph, jdk, here, Nil, origin).either
          bridge <- readRelative(root, "subprocess/NativeBridge.scala")
          binary <- binaryPresent(root, version)
        yield
          val feeds = ran.fold(_ => Chunk.empty, identity)
          val text  = (feeds.collect { case TestFeed.Out(line) => line } ++ feeds.collect { case TestFeed.Err(line) =>
            line
          }).mkString("\n")
          val status = feeds.collect { case TestFeed.Done(cell) => cell.status }
          assertTrue(
            ran.isRight,
            status == Chunk(TestStatus.Passed),
            text.contains("native-ran"),
            bridge.contains(""".withMainClass(Some("scala.scalanative.testinterface.TestMain"))"""),
            bridge.contains(".withMultithreading(true)"),
            binary,
          )

  private def runJs(env: JsEnv, label: String, marker: String) =
    ZIO.scoped:
      for
        root    <- Workspace.work(s"histos-child-js-${env.token}")
        version <- BuildCompiler.version
        graph   <- ZIO.fromEither(
          Projects(
            module("app", Js)(version)
              .jsEnv(env)
              .testDependencies(
                Dep("dev.zio::zio-test:2.1.27"),
                Dep("dev.zio::zio-test-sbt:2.1.27"),
              )
          )
        )
        _         <- Workspace.writeString(root.resolve("app/src/test/scala/demo/Check.scala"), providerSource(label))
        installed <-
          if env == JsEnv.Jsdom then installJsdom(root).map(Some(_))
          else ZIO.succeed(None)
        jdk    <- GraphCompile.jdkHere
        here   <- TestMachine.runtimes(root)
        ran    <- runTests(root, graph, jdk, here, Nil).either
        bridge <- readRelative(root, "bridge/src/subprocess/JsSuite.scala")
        files = listed(root)
      yield
        val feeds = ran.fold(_ => Chunk.empty, identity)
        val text  = (feeds.collect { case TestFeed.Out(line) => line } ++ feeds.collect { case TestFeed.Err(line) =>
          line
        }).mkString("\n")
        val status    = feeds.collect { case TestFeed.Done(cell) => cell.status }
        val script    = files.exists(path => path.contains("/out/") && path.endsWith(".js"))
        val npmExit   = installed.toList.flatMap(_.collect { case ChildEvent.Exited(code) => code }.toList)
        val npmErr    = installed.toList.flatMap(_.collect { case ChildEvent.Err(line) => line }.toList).mkString
        val npmExpect = if env == JsEnv.Jsdom then List(ProcessExit(0)) else Nil
        val npmNote   = if npmExit == npmExpect then "" else npmErr
        assertTrue(
          npmNote.isEmpty,
          npmExit == npmExpect,
          ran.isRight,
          status == Chunk(TestStatus.Passed),
          text.contains(label),
          bridge.contains("org.scalajs.testing.bridge.Bridge"),
          bridge.contains(marker),
          script,
        )

  /** The jsdom environment loads `jsdom` by walking up from the child directory, which sits under this checkout. */
  private def installJsdom(root: Path) =
    for
      env    <- System.envs.orElseSucceed(Map.empty[String, String])
      path   <- ZIO.fromOption(env.get("PATH").filter(_.nonEmpty)).orElseFail(IoFailure(root, IoFault.Stat))
      events <- ZIO
        .serviceWithZIO[Process](
          _.command(
            List(
              "npm",
              "install",
              "--prefix",
              root.toAbsolutePath.toString,
              "--no-fund",
              "--no-audit",
              "--no-progress",
              "jsdom@27",
            ),
            root,
            Map("PATH" -> path),
          ).runCollect
        )
        .provide(Process.live)
    yield events

  private def readRelative(root: Path, suffix: String): ZIO[Any, IoFailure, String] =
    listed(root).find(_.endsWith(suffix)) match
      case None           => ZIO.succeed("")
      case Some(relative) =>
        Workspace.readIfPresent(root.resolve(".histos").resolve(relative)).map(_.getOrElse(""))

  private def binaryPresent(root: Path, version: ScalaVersion): ZIO[Any, IoFailure, Boolean] =
    Workspace
      .readIfPresent(
        TestCompile.linkWork(root, app, version, Platform.Native).resolve("child").resolve("binary.path")
      )
      .flatMap:
        case None      => ZIO.succeed(false)
        case Some(raw) =>
          val text = raw.trim
          if text.isEmpty then ZIO.succeed(false)
          else ZIO.attemptBlocking(Files.isRegularFile(Path.of(text))).orElseSucceed(false)

  private def runProgram(
      root: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      here: Runtimes,
      choice: Option[Platform] = None,
  ): ZIO[LockedTools, ProgramError | ResolveError | IoFailure, Chunk[ProgramFeed]] =
    (
      for
        _     <- Resolve.update(root, graph, BuildResolvers(graph.resolvers))
        feeds <- Program
          .console(root, graph, app.value, jdk, here, choice, _ => ZIO.unit, ZIO.unit)
          .runCollect
      yield feeds
    ).provideSomeLayer[LockedTools](histos.ToolFixture.resolver)

  private def runTests(
      root: Path,
      graph: ProjectGraph,
      jdk: JdkRuntime,
      here: Runtimes,
      patterns: List[SuitePattern],
      native: NativeOrigin = NativeOrigin.Central,
  ): ZIO[LockedTools, ModuleTestError | ResolveError | IoFailure, Chunk[TestFeed]] =
    val asked = TestRun(root, graph, List(app), jdk, here, TestMode.Run, patterns)
    (
      for
        _     <- Resolve.update(root, graph, BuildResolvers(graph.resolvers), native)
        feeds <- ModuleTest.console(asked).runCollect
      yield feeds
    ).provideSomeLayer[LockedTools](histos.ToolFixture.resolver)

  private def installNode(root: Path): ZIO[Any, IoFailure, Option[Path]] =
    for
      env  <- System.envs.orElseSucceed(Map.empty[String, String])
      real <- ZIO
        .attemptBlocking:
          env
            .get("PATH")
            .toList
            .flatMap(_.split(java.io.File.pathSeparatorChar).toList)
            .filter(_.nonEmpty)
            .map(dir => Path.of(dir, "node"))
            .find(path => Files.isExecutable(path))
        .mapError(_ => IoFailure(root, IoFault.Stat))
      _ <- real match
        case None         => ZIO.unit
        case Some(binary) =>
          val home   = root.resolve("chosen-node")
          val script = home.resolve("bin").resolve("node")
          val log    = root.resolve("chosen-node.log")
          val body   =
            s"""#!/bin/sh
               |printf 'launch:%s\\n' "$$*" >> '${log.toAbsolutePath}'
               |exec '${binary.toAbsolutePath}' "$$@"
               |""".stripMargin
          for
            _ <- Workspace.writeString(script, body)
            _ <- ZIO
              .attemptBlocking(
                Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwxr-xr-x"))
              )
              .mapError(_ => IoFailure(script, IoFault.Write))
            _ <- Workspace.writeString(
              root.resolve(".histos").resolve("node.local"),
              home.toAbsolutePath.toString + "\n",
            )
          yield ()
    yield real

  private def launches(log: Path): ZIO[Any, IoFailure, List[String]] =
    Workspace
      .readIfPresent(log)
      .map:
        case None       => Nil
        case Some(text) =>
          text.linesIterator.filter(_.startsWith("launch:")).filterNot(_ == "launch:--version").toList

  private def listed(root: Path): List[String] =
    val base = root.resolve(".histos")
    if !Files.exists(base) then Nil
    else
      val walk = Files.walk(base)
      try
        walk.iterator.asScala.map(path => base.relativize(path).toString.replace('\\', '/')).toList
      finally walk.close()

  private def explain[A](ran: Either[ModuleTestError | ResolveError | IoFailure, Chunk[A]]): String =
    ran match
      case Left(error) =>
        error match
          case found: ModuleTestError => found.message
          case found: ResolveError    => found.message
          case found: IoFailure       => found.message
      case Right(feeds) => feeds.mkString("\n")

  private def lines(text: String*): String =
    text.mkString("", "\n", "\n")

  /** The pid file appears only after the child reaches main. */
  private def awaitPid(path: Path): UIO[Option[Long]] =
    readPid(path)
      .flatMap:
        case found @ Some(_) => ZIO.succeed(found)
        case None            => ZIO.sleep(20.millis) *> awaitPid(path)
      .timeout(10.seconds)
      .map(_.flatten)

  private def readPid(path: Path): UIO[Option[Long]] =
    ZIO
      .attemptBlocking:
        if !Files.isRegularFile(path) then None
        else Files.readString(path).trim.toLongOption.filter(_ > 0)
      .orElseSucceed(None)

  private def awaitDead(pid: Long): UIO[Boolean] =
    ZIO
      .suspendSucceed:
        if alive(pid) then ZIO.sleep(20.millis) *> awaitDead(pid)
        else ZIO.succeed(true)
      .timeout(2.seconds)
      .map(_.contains(true))

  private def alive(pid: Long): Boolean =
    ProcessHandle.of(pid).toScala.exists(_.isAlive)

  private def realDir(path: Path): ZIO[Any, IoFailure, Path] =
    Workspace
      .createDirectories(path)
      .flatMap: dir =>
        ZIO
          .attemptBlocking(dir.toRealPath())
          .mapError(_ => IoFailure(dir, IoFault.Stat))

  private def runningJava: ZIO[Any, Nothing, Option[Path]] =
    System
      .property("java.home")
      .orElseSucceed(None)
      .map(_.filter(_.nonEmpty).map(dir => Path.of(dir, "bin", "java")))

  private final case class Compiled(code: Int, text: String)

  private def javac(
      root: Path,
      classes: Path,
      name: String,
      source: String,
      classpath: List[Path],
  ): ZIO[Any, IoFailure, Compiled] =
    val file = root.resolve("src").resolve(name)
    for
      _        <- Workspace.writeString(file, source)
      compiled <- ZIO
        .attemptBlocking:
          val sink = new ByteArrayOutputStream
          val args =
            (if classpath.isEmpty then Nil
             else List("-cp", classpath.mkString(java.io.File.pathSeparator))) ++
              List("-d", classes.toString, file.toString)
          Option(ToolProvider.getSystemJavaCompiler) match
            case None       => Compiled(-1, "no java compiler")
            case Some(tool) =>
              val code = tool.run(new ByteArrayInputStream(Array.emptyByteArray), sink, sink, args*)
              Compiled(code, sink.toString(java.nio.charset.StandardCharsets.UTF_8))
        .mapError(_ => IoFailure(file, IoFault.Write))
    yield compiled

  private def classJar(file: Path, entry: String, dest: Path): ZIO[Any, IoFailure, Unit] =
    ZIO
      .attemptBlocking:
        val buffer = new ByteArrayOutputStream
        val output = new JarOutputStream(buffer)
        output.putNextEntry(new JarEntry(entry))
        output.write(Files.readAllBytes(file))
        output.closeEntry()
        output.close()
        Files.write(dest, buffer.toByteArray)
      .mapError(_ => IoFailure(dest, IoFault.Write))
      .unit

  private val exitSource =
    """package demo;
      |public final class Exit {
      |  public static void main(String[] args) {
      |    System.out.println(Lib.hi());
      |    System.out.println("home=" + System.getenv("HOME"));
      |    System.out.println("dir=" + System.getProperty("user.dir"));
      |    System.out.flush();
      |    System.exit(7);
      |  }
      |}
      |""".stripMargin

  private val libSource =
    """package demo;
      |public final class Lib {
      |  public static String hi() { return "hi"; }
      |}
      |""".stripMargin

  private val sleeperSource =
    """package demo;
      |import java.nio.file.*;
      |public final class Sleeper {
      |  public static void main(String[] args) throws Exception {
      |    Files.writeString(Path.of("self.pid"), Long.toString(ProcessHandle.current().pid()));
      |    Process child = Runtime.getRuntime().exec(new String[] {"/bin/sleep", "300"});
      |    Files.writeString(Path.of("child.pid"), Long.toString(child.pid()));
      |    Thread.sleep(300_000L);
      |  }
      |}
      |""".stripMargin

  private val hiSource =
    """package demo;
      |public final class Hi {
      |  public static void main(String[] args) {
      |    System.out.println("ran");
      |  }
      |}
      |""".stripMargin

  private val keptSource =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    println("kept")
      |    System.exit(4)
      |""".stripMargin

  private val hiScala =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    println("hi")
      |""".stripMargin

  private val boom =
    """package keep
      |
      |import zio.test.*
      |
      |object Boom extends ZIOSpecDefault:
      |  def spec = test("keep-ran")(assertTrue(false))
      |""".stripMargin

  private val fine =
    """package drop
      |
      |import zio.test.*
      |
      |object Fine extends ZIOSpecDefault:
      |  def spec = test("drop-ran")(assertTrue(true))
      |""".stripMargin

  private val matchSource =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    println("ran-match")
      |""".stripMargin

  private def providerSource(label: String): String =
    s"""package demo
       |
       |import zio.test.*
       |
       |object Check extends ZIOSpecDefault:
       |  def spec = test("$label")(assertTrue(true))
       |""".stripMargin

  private val missSource =
    """package demo
      |
      |object Check:
      |  def main(args: Array[String]): Unit =
      |    println("ran-miss")
      |""".stripMargin
end ChildLawSpec
