package histos.js

import histos.cache.{ContentStore, LinkerStarts}
import histos.jdk.{JdkRuntime, Machine, Toolchain, ToolchainError}
import histos.pack.MainClass
import histos.process.{ChildError, ChildProcess, ChildRequest}
import histos.resolve.{DependencyLock, LockedTools, Repositories, ResolutionService, Resolve, ResolveError}
import histos.schedule.{CacheError, RemoteCache}
import histos.suite.HistosSpec
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CellPlatform, CompileError, CompileRequest, CompileToolchain, CompilerStarts}
import histos.{JsOptimize, Platform, Resolution, ScalaVersion, ToolCatalog}

import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import zio.{IO, Promise, Ref, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object JsProductsSpec extends HistosSpec:
  private val version = ScalaVersion("3.10.1")
  private val main    = MainClass.trusted("demo.Main")
  private type Env = LockedTools & ContentStore & LinkerStarts & RemoteCache

  enum Failure:
    case Link(error: JsLinkError)
    case Resolve(error: ResolveError)
    case Jdk(error: ToolchainError)
    case Child(error: ChildError)
    case Io(error: IoFailure)
    case Compiler(out: String, err: String)
    case Compile(error: CompileError)
    case NoHost
    case NoChild
    case Environment
    case Cache(error: CacheError)

  def spec =
    val tests = suite("JS link products")(
      test("unchanged own classes relink when dependency IR jar bytes change at the same path") {
        fixture: (root, jdk, starts) =>
          val own        = root.resolve("own")
          val dependency = root.resolve("dependency.jar")
          val request    = JsLinkRequest(version, main, List(own, dependency), root.resolve("work"), jdk)
          for
            _ <- compile(
              root,
              jdk,
              "dependency-one",
              "package demo\nobject Dependency:\n  def text: String = \"one\"",
            )
            _ <- irJar(root.resolve("dependency-one/classes"), dependency)
            _ <- compile(
              root,
              jdk,
              "own-build",
              "package demo\nobject Main:\n  def main(args: Array[String]): Unit = println(Dependency.text)",
              Some(own),
              List(root.resolve("dependency-one/classes")),
            )
            before <- bytes(own.resolve("demo/Main$.sjsir"))
            first  <- JsLink.link(request).mapError(Failure.Link(_))
            one    <- execute(root, first.script)
            _      <- compile(
              root,
              jdk,
              "dependency-two",
              "package demo\nobject Dependency:\n  def text: String = \"two\"",
            )
            _      <- irJar(root.resolve("dependency-two/classes"), dependency)
            second <- JsLink.link(request).mapError(Failure.Link(_))
            two    <- execute(root, second.script)
            again  <- JsLink.link(request).mapError(Failure.Link(_))
            after  <- bytes(own.resolve("demo/Main$.sjsir"))
            count  <- starts.get
          yield assertTrue(
            one.trim == "one",
            two.trim == "two",
            first.key != second.key,
            again.key == second.key,
            before == after,
            count == 2,
          )
      },
      test("alternating Fast and Full preserves both products, restores both, and shares resolved Full keys") {
        fixture: (root, jdk, starts) =>
          val own     = root.resolve("own")
          val request =
            JsLinkRequest(version, main, List(own), root.resolve("work"), jdk, arguments = List("hello", "", "a,b"))
          val implicitFull = JsOptimize.effective(JsOptimize.Full, None, None)
          val explicitFull = JsOptimize.effective(JsOptimize.Fast, None, Some(JsOptimize.Full))
          for
            _                 <- compile(root, jdk, "own-build", argumentMain, Some(own))
            fast              <- JsLink.link(request).mapError(Failure.Link(_))
            fastBytes         <- bytes(fast.script)
            full              <- JsLink.link(request.copy(optimize = implicitFull)).mapError(Failure.Link(_))
            fullBytes         <- bytes(full.script)
            unchangedFast     <- bytes(fast.script)
            fastText          <- execute(root, fast.script)
            fullText          <- execute(root, full.script)
            againFast         <- JsLink.link(request).mapError(Failure.Link(_))
            againFull         <- JsLink.link(request.copy(optimize = explicitFull)).mapError(Failure.Link(_))
            _                 <- io(root, IoFault.Delete) { Files.delete(fast.script); Files.delete(full.script); () }
            restoredFull      <- JsLink.link(request.copy(optimize = explicitFull)).mapError(Failure.Link(_))
            restoredFast      <- JsLink.link(request).mapError(Failure.Link(_))
            restoredFullBytes <- bytes(restoredFull.script)
            restoredFastBytes <- bytes(restoredFast.script)
            count             <- starts.get
          yield assertTrue(
            fast.key != full.key,
            fast.script != full.script,
            unchangedFast == fastBytes,
            fullBytes.size < fastBytes.size,
            fastText == "hello||a,b",
            fullText == fastText,
            againFast == fast,
            againFull == full,
            restoredFull == full,
            restoredFast == fast,
            restoredFullBytes == fullBytes,
            restoredFastBytes == fastBytes,
            count == 2,
          )
      },
      test("changing tool classpath jar bytes at the same path starts a new link") {
        fixture: (root, jdk, starts) =>
          val own     = root.resolve("own")
          val tool    = root.resolve("tool.jar")
          val marker  = root.resolve("tool-content/marker.txt")
          val request = JsLinkRequest(version, main, List(own), root.resolve("work"), jdk, arguments = List("tool"))
          for
            _ <- compile(root, jdk, "own-build", argumentMain, Some(own))
            _ <- io(marker, IoFault.Write) {
              Files.createDirectories(marker.getParent); Files.writeString(marker, "one"); ()
            }
            _   <- jar(marker.getParent, tool)
            old <- io(tool, IoFault.Stat)(Option(java.lang.System.getProperty("java.class.path")))
              .someOrFail(Failure.NoHost)
            result <- ZIO
              .acquireRelease(
                io(tool, IoFault.Write)(
                  java.lang.System.setProperty("java.class.path", old + java.io.File.pathSeparator + tool)
                )
              )(_ => ZIO.attempt(java.lang.System.setProperty("java.class.path", old)).ignore)
              .flatMap: _ =>
                for
                  first  <- JsLink.link(request).mapError(Failure.Link(_))
                  _      <- io(marker, IoFault.Write)(Files.writeString(marker, "two"))
                  _      <- jar(marker.getParent, tool)
                  second <- JsLink.link(request).mapError(Failure.Link(_))
                  text   <- execute(root, second.script)
                  again  <- JsLink.link(request).mapError(Failure.Link(_))
                  count  <- starts.get
                yield assertTrue(first.key != second.key, again == second, text == "tool", count == 2)
          yield result
      },
      test("a failed linker never publishes a partial product or replaces the prior script") {
        fixture: (root, jdk, starts) =>
          val own     = root.resolve("own")
          val request = JsLinkRequest(version, main, List(own), root.resolve("work"), jdk)
          for
            _        <- compile(root, jdk, "own-build", argumentMain, Some(own))
            previous <- JsLink.link(request).mapError(Failure.Link(_))
            original <- bytes(previous.script)
            store    <- ZIO.service[ContentStore]
            before   <- store.list.mapError(Failure.Cache(_))
            failed   <- JsLink.link(request.copy(main = MainClass.trusted("demo.Missing"))).either
            after    <- store.list.mapError(Failure.Cache(_))
            retained <- bytes(previous.script)
            again    <- JsLink.link(request).mapError(Failure.Link(_))
            count    <- starts.get
          yield assertTrue(
            failed match
              case Left(JsLinkError.Linker(text)) => text.contains("demo.Missing")
              case Left(_) | Right(_)             => false,
            original == retained,
            before.map(_.digest) == after.map(_.digest),
            again == previous,
            count == 2,
          )
      },
      test("an actual A to B to A overwrite during IR consumption cannot poison A's key") {
        fixture: (root, jdk, starts) =>
          for
            input  <- dependencyFixture(root, jdk)
            linked <- JsProducts
              .observed(input.request)(
                Some(event =>
                  event.phase match
                    case JsPhase.Captured =>
                      jsIo(input.dependency)(
                        Files.copy(input.other, input.dependency, StandardCopyOption.REPLACE_EXISTING)
                      ).unit
                    case JsPhase.Linked =>
                      jsIo(input.dependency)(Files.write(input.dependency, input.original.toArray)).unit
                    case JsPhase.BeforeIr | JsPhase.IrLoaded | JsPhase.MinifierStarted => ZIO.unit
                )
              )
              .mapError(Failure.Link(_))
            text     <- execute(root, linked.script)
            again    <- JsLink.link(input.request).mapError(Failure.Link(_))
            repeated <- execute(root, again.script)
            count    <- starts.get
          yield assertTrue(text == "one", repeated == "one", linked == again, count == 1)
      },
      test("a persistent in-flight overwrite fails InputsChanged without a new record") {
        fixture: (root, jdk, _) =>
          for
            input  <- dependencyFixture(root, jdk)
            failed <- JsProducts
              .observed(input.request)(
                Some(event =>
                  if event.phase == JsPhase.Captured then
                    jsIo(input.dependency)(
                      Files.copy(input.other, input.dependency, StandardCopyOption.REPLACE_EXISTING)
                    ).unit
                  else ZIO.unit
                )
              )
              .either
            store   <- ZIO.service[ContentStore]
            entries <- store.list.mapError(Failure.Cache(_))
          yield assertTrue(failed == Left(JsLinkError.InputsChanged), entries.isEmpty)
      },
      test("a real child minifier input failure retains the Minifier branch") {
        fixture: (root, jdk, _) =>
          val own     = root.resolve("own")
          val request = JsLinkRequest(version, main, List(own), root.resolve("work"), jdk, optimize = JsOptimize.Full)
          for
            _      <- compile(root, jdk, "own-build", argumentMain, Some(own))
            failed <- JsProducts
              .observed(request)(
                Some(event =>
                  if event.phase == JsPhase.Linked then
                    ZIO
                      .fromOption(event.script)
                      .orElseFail(JsLinkError.Bridge)
                      .flatMap(path => jsIo(Path.of(path))(Files.delete(Path.of(path))))
                  else ZIO.unit
                )
              )
              .either
            store   <- ZIO.service[ContentStore]
            entries <- store.list.mapError(Failure.Cache(_))
            expected = failed match
              case Left(JsLinkError.Minifier(_)) => true
              case Left(_) | Right(_)            => false
          yield assertTrue(expected, entries.isEmpty)
      },
      test("a malformed actual child request reports Bridge rather than a Linker diagnostic") {
        fixture: (root, jdk, _) =>
          val process = root.resolve("malformed-process")
          val result  = root.resolve("malformed-result")
          for
            host   <- hostClasspath
            _      <- io(process, IoFault.Create)(Files.createDirectories(process))
            failed <- JsProducts
              .child(
                ChildRequest(
                  Platform.Jvm,
                  jdk.home.resolve("bin/java"),
                  host,
                  MainClass.trusted("histos.js.child.LinkMain"),
                  process,
                  arguments = List(result.toString),
                ),
                result,
              )
              .either
            store   <- ZIO.service[ContentStore]
            entries <- store.list.mapError(Failure.Cache(_))
          yield assertTrue(failed == Left(JsLinkError.Bridge), entries.isEmpty)
      },
      test("a scoped immutable tool generation reuses its verified identity on warm work") {
        fixture: (root, jdk, starts) =>
          val own     = root.resolve("own")
          val request = JsLinkRequest(version, main, List(own), root.resolve("work"), jdk)
          for
            _        <- compile(root, jdk, "own-build", argumentMain, Some(own))
            captured <- JsProducts.session.mapError(Failure.Link(_)).timed
            (captureTime, generation) = captured
            cold   <- generation.link(request).mapError(Failure.Link(_)).timed
            warm   <- generation.link(request).mapError(Failure.Link(_)).timed
            oneOff <- JsLink.link(request).mapError(Failure.Link(_)).timed
            count  <- starts.get
            _      <- zio.Console
              .printLine(
                s"JS warm work: toolBytes=${generation.toolBytes} captureNs=${captureTime.toNanos} coldNs=${cold._1.toNanos} warmNs=${warm._1.toNanos} oneOffWarmNs=${oneOff._1.toNanos} starts=$count"
              )
              .mapError(_ => Failure.Environment)
          yield assertTrue(cold._2 == warm._2, oneOff._2 == warm._2, generation.toolBytes > 0L, count == 1)
      },
      test("the unavailable host diagnostic identifies the running child classpath") {
        assertTrue(JsLinkError.Unavailable.message == "the running Scala.js child tool classpath is unavailable")
      },
      test("a real child output failure retains the Io branch") {
        fixture: (root, jdk, _) =>
          val own     = root.resolve("own")
          val request = JsLinkRequest(version, main, List(own), root.resolve("work"), jdk)
          for
            _      <- compile(root, jdk, "own-build", argumentMain, Some(own))
            failed <- JsProducts
              .observed(request)(
                Some(event =>
                  if event.phase == JsPhase.BeforeIr then
                    jsIo(Path.of(event.output))(Files.writeString(Path.of(event.output), "not a directory")).unit
                  else ZIO.unit
                )
              )
              .either
            store   <- ZIO.service[ContentStore]
            entries <- store.list.mapError(Failure.Cache(_))
            expected = failed match
              case Left(JsLinkError.Io(IoFailure(_, IoFault.Write))) => true
              case Left(_) | Right(_)                                => false
          yield assertTrue(expected, entries.isEmpty)
      },
      test("the child temporary root is configured at JVM startup even with spaces and both quote characters") {
        fixture: (root, jdk, _) =>
          val scratch   = root.resolve("quoted \"root' with spaces")
          val process   = scratch.resolve("process")
          val temporary = scratch.resolve("temporary")
          val result    = scratch.resolve("result")
          val output    = scratch.resolve("out")
          for
            host   <- hostClasspath
            _      <- io(process, IoFault.Create)(Files.createDirectories(process))
            _      <- io(temporary, IoFault.Create)(Files.createDirectories(temporary))
            seen   <- Ref.make[Option[String]](None)
            failed <- JsProducts
              .child(
                ChildRequest(
                  Platform.Jvm,
                  jdk.home.resolve("bin/java"),
                  host,
                  MainClass.trusted("histos.js.child.LinkMain"),
                  process,
                  environment = JsProducts.temporaryEnvironment(temporary),
                  arguments =
                    List(result.toString, output.toString, "nomodule", "fast", "exports", main.binary, "observed", "0"),
                ),
                result,
                Some(event => seen.set(Some(event.temporary))),
              )
              .either
            configured <- seen.get
            expected = failed match
              case Left(JsLinkError.Linker(_)) => true
              case Left(_) | Right(_)          => false
          yield assertTrue(configured.contains(temporary.toAbsolutePath.normalize.toString), expected)
      },
      test("interrupting real late Full work removes the native process and every execution temporary") {
        fixture: (root, jdk, starts) =>
          val own     = root.resolve("own")
          val request = JsLinkRequest(version, main, List(own), root.resolve("work"), jdk, optimize = JsOptimize.Full)
          for
            _     <- compile(root, jdk, "own-build", argumentMain, Some(own))
            ready <- Promise.make[Nothing, JsReady]
            fiber <- JsProducts
              .observed(request)(
                Some(event =>
                  if event.phase == JsPhase.MinifierStarted then ready.succeed(event) *> ZIO.never else ZIO.unit
                )
              )
              .mapError(Failure.Link(_))
              .forkScoped
            observed <- ready.await.raceFirst(fiber.join.flatMap(_ => ZIO.fail(Failure.NoChild)))
            temporary = Path.of(observed.temporary)
            scratch   = Path.of(observed.scratch)
            _           <- ZIO.addFinalizer(Workspace.delete(temporary).ignore)
            nativePid   <- ZIO.fromOption(observed.descendant).orElseFail(Failure.NoChild)
            process     <- io(root, IoFault.Stat)(ProcessHandle.of(observed.pid).toScala).someOrFail(Failure.NoChild)
            inputExists <- io(temporary, IoFault.Stat)(Files.isRegularFile(temporary.resolve("in.js")))
            nested      <- io(temporary, IoFault.Stat)(
              temporary.toAbsolutePath.normalize.startsWith(scratch.toAbsolutePath.normalize)
            )
            interrupted <- fiber.interrupt
            leaderDead  <- io(root, IoFault.Stat)(!process.isAlive)
            nativeDead  <- io(root, IoFault.Stat)(ProcessHandle.of(nativePid).toScala.forall(handle => !handle.isAlive))
            temporaryGone <- io(temporary, IoFault.Stat)(Files.notExists(temporary))
            scratchGone   <- io(scratch, IoFault.Stat)(Files.notExists(scratch))
            store         <- ZIO.service[ContentStore]
            entries       <- store.list.mapError(Failure.Cache(_))
            count         <- starts.get
          yield assertTrue(
            inputExists,
            nativePid > 0L,
            nested,
            interrupted.isInterrupted,
            leaderDead,
            nativeDead,
            temporaryGone,
            scratchGone,
            entries.isEmpty,
            count == 1,
          )
      },
      test("interrupting an observed real linker child closes its process and scratch without publishing") {
        fixture: (root, jdk, starts) =>
          val own     = root.resolve("own")
          val request = JsLinkRequest(version, main, List(own), root.resolve("work"), jdk, optimize = JsOptimize.Full)
          for
            _     <- compile(root, jdk, "own-build", argumentMain, Some(own))
            ready <- Promise.make[Nothing, JsReady]
            fiber <- JsProducts
              .observed(request)(
                Some(event => if event.phase == JsPhase.IrLoaded then ready.succeed(event) *> ZIO.never else ZIO.unit)
              )
              .mapError(Failure.Link(_))
              .forkScoped
            observed    <- ready.await.raceFirst(fiber.join.flatMap(_ => ZIO.fail(Failure.NoChild)))
            process     <- io(root, IoFault.Stat)(ProcessHandle.of(observed.pid).toScala).someOrFail(Failure.NoChild)
            alive       <- io(root, IoFault.Stat)(process.isAlive)
            interrupted <- fiber.interrupt
            dead        <- io(root, IoFault.Stat)(!process.isAlive)
            removed     <- io(root, IoFault.Stat)(Files.notExists(Path.of(observed.scratch)))
            store       <- ZIO.service[ContentStore]
            entries     <- store.list.mapError(Failure.Cache(_))
            count       <- starts.get
          yield assertTrue(alive, interrupted.isInterrupted, dead, removed, entries.isEmpty, count == 1)
      },
    )
    (tests @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock @@ TestAspect.timeout(
      5.minutes
    ) @@ TestAspect.sequential).provideShared(histos.zinc.Compilers.owned)

  private val argumentMain =
    "package demo\nobject Main:\n  def main(args: Array[String]): Unit = println(args.mkString(\"|\"))"

  private final case class DependencyFixture(
      request: JsLinkRequest,
      dependency: Path,
      other: Path,
      original: List[Byte],
  )

  private def dependencyFixture(
      root: Path,
      jdk: JdkRuntime,
  ): ZIO[LockedTools & histos.zinc.Compilers & RemoteCache, Failure, DependencyFixture] =
    val own        = root.resolve("own")
    val dependency = root.resolve("dependency.jar")
    val other      = root.resolve("other.jar")
    for
      _ <- compile(
        root,
        jdk,
        "dependency-one",
        "package demo\nobject Dependency:\n  def text: String = \"one\"",
      )
      _ <- compile(
        root,
        jdk,
        "dependency-two",
        "package demo\nobject Dependency:\n  def text: String = \"two\"",
      )
      _ <- irJar(root.resolve("dependency-one/classes"), dependency)
      _ <- irJar(root.resolve("dependency-two/classes"), other)
      _ <- compile(
        root,
        jdk,
        "own-build",
        "package demo\nobject Main:\n  def main(args: Array[String]): Unit = println(Dependency.text)",
        Some(own),
        List(root.resolve("dependency-one/classes")),
      )
      original <- bytes(dependency)
    yield DependencyFixture(
      JsLinkRequest(version, main, List(own, dependency), root.resolve("work"), jdk),
      dependency,
      other,
      original,
    )

  private def jsIo[A](path: Path)(body: => A): IO[JsLinkError, A] =
    ZIO.attemptBlocking(body).mapError(_ => JsLinkError.Io(IoFailure(path, IoFault.Write)))

  private def fixture[A](
      use: (Path, JdkRuntime, Ref[Int]) => ZIO[Env & histos.zinc.Compilers & Scope, Failure, A]
  ): ZIO[histos.zinc.Compilers, Failure, A] =
    ZIO
      .scoped:
        for
          root    <- Workspace.work("histos-js-products").mapError(Failure.Io(_))
          jdk     <- Machine.load(root).flatMap(Toolchain.resolve).mapError(Failure.Jdk(_))
          resolve <- ZIO.service[Resolve]
          opened  <- ResolutionService.open(root, Resolution.Solve).mapError(Failure.Resolve(_))
          starts  <- Ref.make(0)
          tools = LockedTools(DependencyLock(Nil), ToolCatalog.default, Repositories.central, resolve, Some(opened))
          result <- use(root, jdk, starts).provideSomeLayer[histos.zinc.Compilers & Scope](
            ZLayer.succeed(tools) ++ ZLayer.succeed(ContentStore.in(root.resolve("cas"))) ++
              ZLayer.succeed(LinkerStarts(starts)) ++ RemoteCache.off
          )
        yield result
      .provideSomeLayer[histos.zinc.Compilers](Resolve.coursier)

  private def compile(
      root: Path,
      jdk: JdkRuntime,
      name: String,
      source: String,
      destination: Option[Path] = None,
      classpath: List[Path] = Nil,
  ): ZIO[LockedTools & histos.zinc.Compilers & RemoteCache, Failure, Unit] =
    val work    = root.resolve(name)
    val classes = destination.getOrElse(work.resolve("classes"))
    val file    = work.resolve("Source.scala")
    val request = CompileRequest(
      CompileToolchain(version, Platform.Js, jdk, histos.ScalacFlags.empty),
      List(file),
      classpath,
      classes,
      work.resolve("analysis"),
      Nil,
      Nil,
      checkout = work,
    )
    for
      _ <- io(file, IoFault.Write) {
        Files.createDirectories(work)
        Files.createDirectories(classes)
        Files.createDirectories(work.resolve("analysis"))
        Files.createDirectories(work.resolve("cas"))
        Files.writeString(file, source)
      }
      revision <- ZIO.serviceWith[LockedTools](_.catalog.js.revision)
      _        <- ScalaJs
        .compile(request, CellPlatform.Js(revision))
        .provideSomeLayer[LockedTools & histos.zinc.Compilers & RemoteCache](
          ZLayer.succeed(ContentStore.in(work.resolve("cas"))) ++ CompilerStarts.live
        )
        .mapError(Failure.Compile(_))
    yield ()

  private def hostClasspath: IO[Failure, histos.process.ChildClasspath] =
    ZIO
      .fromOption(HostClasspath.child)
      .orElseFail(Failure.NoHost)
      .flatMap: host =>
        io(host.classes, IoFault.Stat)(host.entries.filter(Files.exists(_))).flatMap:
          case first :: rest => ZIO.succeed(histos.process.ChildClasspath(first, rest))
          case Nil           => ZIO.fail(Failure.NoHost)

  private def execute(root: Path, script: Path): IO[Failure, String] =
    for
      env    <- ZIO.systemWith(_.envs).mapError(_ => Failure.Environment)
      events <- ChildProcess
        .command(List("/usr/bin/env", "node", script.toString), root, env)
        .runCollect
        .mapError(Failure.Child(_))
      out    = events.collect { case histos.process.ChildEvent.Out(text) => text }.mkString("\n")
      err    = events.collect { case histos.process.ChildEvent.Err(text) => text }.mkString("\n")
      exited = events.collect { case histos.process.ChildEvent.Exited(code) => code.value }
      _ <- ZIO.fail(Failure.Compiler(out, err)).unless(exited.toList == List(0))
    yield out

  private def bytes(path: Path): IO[Failure, List[Byte]] =
    io(path, IoFault.Read)(Files.readAllBytes(path).toList)

  private def jar(classes: Path, destination: Path): IO[Failure, Unit] =
    for
      files <- io(classes, IoFault.Read)(listed(classes, _ => true))
      _     <- writeEntries(classes, destination, files)
    yield ()

  private def irJar(classes: Path, destination: Path): IO[Failure, Unit] =
    for
      files <- io(classes, IoFault.Read)(listed(classes, irName))
      _     <- files match
        case Nil   => ZIO.fail(Failure.Compiler("", s"no .sjsir under $classes"))
        case found => writeEntries(classes, destination, found)
    yield ()

  private def listed(root: Path, keep: Path => Boolean): List[Path] =
    val walk = Files.walk(root)
    try
      walk.iterator.asScala.filter(path => Files.isRegularFile(path) && keep(path)).toList.sortBy(_.toString)
    finally walk.close()

  private def writeEntries(root: Path, destination: Path, files: List[Path]): IO[Failure, Unit] =
    io(destination, IoFault.Write) {
      val stream = new ZipOutputStream(Files.newOutputStream(destination))
      try
        files.foreach: file =>
          val entry = new ZipEntry(root.relativize(file).toString)
          entry.setTime(0L)
          stream.putNextEntry(entry)
          stream.write(Files.readAllBytes(file))
          stream.closeEntry()
      finally stream.close()
    }

  private def irName(path: Path): Boolean =
    path.getNameCount > 0 && path.getFileName.toString.endsWith(".sjsir")

  private def io[A](path: Path, fault: IoFault)(body: => A): IO[Failure, A] =
    ZIO.attemptBlocking(body).mapError(_ => Failure.Io(IoFailure(path, fault)))
end JsProductsSpec
