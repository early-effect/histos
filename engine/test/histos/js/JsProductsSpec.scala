package histos.js

import histos.cache.{ContentStore, LinkerStarts}
import histos.jdk.{JdkRuntime, Machine, Toolchain, ToolchainError}
import histos.pack.MainClass
import histos.process.{ChildError, ChildProcess, ChildRequest}
import histos.resolve.{DependencyLock, LockedTools, Repositories, ResolutionService, Resolve, ResolveError}
import histos.schedule.{CacheError, RemoteCache}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.{JsOptimize, Platform, Resolution, ScalaVersion, ToolCatalog}

import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import zio.{IO, Ref, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object JsProductsSpec extends ZIOSpecDefault:
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
            libraries <- ScalaJs.libraries(version).mapError(error => Failure.Link(JsLinkError.Library(error)))
            _         <- compile(
              root,
              jdk,
              libraries,
              "dependency-one",
              "package demo\nobject Dependency:\n  def text: String = \"one\"",
            )
            _ <- jar(root.resolve("dependency-one/classes"), dependency)
            _ <- compile(
              root,
              jdk,
              libraries :+ dependency,
              "own-build",
              "package demo\nobject Main:\n  def main(args: Array[String]): Unit = println(Dependency.text)",
              Some(own),
            )
            before <- bytes(own.resolve("demo/Main$.sjsir"))
            first  <- JsLink.link(request).mapError(Failure.Link(_))
            one    <- execute(root, first.script)
            _      <- compile(
              root,
              jdk,
              libraries,
              "dependency-two",
              "package demo\nobject Dependency:\n  def text: String = \"two\"",
            )
            _      <- jar(root.resolve("dependency-two/classes"), dependency)
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
            libraries         <- ScalaJs.libraries(version).mapError(error => Failure.Link(JsLinkError.Library(error)))
            _                 <- compile(root, jdk, libraries, "own-build", argumentMain, Some(own))
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
            libraries <- ScalaJs.libraries(version).mapError(error => Failure.Link(JsLinkError.Library(error)))
            _         <- compile(root, jdk, libraries, "own-build", argumentMain, Some(own))
            _         <- io(marker, IoFault.Write) {
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
            libraries <- ScalaJs.libraries(version).mapError(error => Failure.Link(JsLinkError.Library(error)))
            _         <- compile(root, jdk, libraries, "own-build", argumentMain, Some(own))
            previous  <- JsLink.link(request).mapError(Failure.Link(_))
            original  <- bytes(previous.script)
            store     <- ZIO.service[ContentStore]
            before    <- store.list.mapError(Failure.Cache(_))
            failed    <- JsLink.link(request.copy(main = MainClass.trusted("demo.Missing"))).either
            after     <- store.list.mapError(Failure.Cache(_))
            retained  <- bytes(previous.script)
            again     <- JsLink.link(request).mapError(Failure.Link(_))
            count     <- starts.get
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
      test("interrupting an observed real linker child closes its process and scratch without publishing") {
        fixture: (root, jdk, starts) =>
          val own     = root.resolve("own")
          val request = JsLinkRequest(version, main, List(own), root.resolve("work"), jdk, optimize = JsOptimize.Full)
          for
            libraries   <- ScalaJs.libraries(version).mapError(error => Failure.Link(JsLinkError.Library(error)))
            _           <- compile(root, jdk, libraries, "own-build", argumentMain, Some(own))
            fiber       <- JsLink.link(request).mapError(Failure.Link(_)).forkScoped
            observed    <- observe(root).raceFirst(fiber.join.flatMap(_ => ZIO.fail(Failure.NoChild)))
            alive       <- io(root, IoFault.Stat)(observed.process.isAlive)
            interrupted <- fiber.interrupt
            dead        <- io(root, IoFault.Stat)(!observed.process.isAlive)
            removed     <- io(observed.scratch, IoFault.Stat)(Files.notExists(observed.scratch))
            store       <- ZIO.service[ContentStore]
            entries     <- store.list.mapError(Failure.Cache(_))
            count       <- starts.get
          yield assertTrue(alive, interrupted.isInterrupted, dead, removed, entries.isEmpty, count == 1)
      },
    )
    tests @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock @@ TestAspect.timeout(
      5.minutes
    ) @@ TestAspect.sequential

  private val argumentMain =
    "package demo\nobject Main:\n  def main(args: Array[String]): Unit = println(args.mkString(\"|\"))"

  private final case class Observed(process: ProcessHandle, scratch: Path)

  private def observe(root: Path): IO[Failure, Observed] =
    io(root, IoFault.Stat) {
      val processes = ProcessHandle.allProcesses()
      try
        processes.iterator.asScala
          .flatMap: process =>
            process.info
              .arguments()
              .toScala
              .flatMap: raw =>
                val args = raw.toList
                if !args.contains(root.resolve("own").toString) then None
                else
                  args.dropWhile(_ != "histos.js.child.LinkMain").drop(1) match
                    case result :: _ => Option(Path.of(result).getParent).map(Observed(process, _))
                    case Nil         => None
          .toList
          .headOption
      finally processes.close()
    }.flatMap:
      case Some(process) => ZIO.succeed(process)
      case None          => ZIO.yieldNow *> observe(root)

  private def fixture[A](use: (Path, JdkRuntime, Ref[Int]) => ZIO[Env & Scope, Failure, A]): IO[Failure, A] =
    ZIO
      .scoped:
        for
          root    <- Workspace.work("histos-js-products").mapError(Failure.Io(_))
          jdk     <- Machine.load(root).flatMap(Toolchain.resolve).mapError(Failure.Jdk(_))
          resolve <- ZIO.service[Resolve]
          opened  <- ResolutionService.open(root, Resolution.Solve).mapError(Failure.Resolve(_))
          starts  <- Ref.make(0)
          tools = LockedTools(DependencyLock(Nil), ToolCatalog.default, Repositories.central, resolve, Some(opened))
          result <- use(root, jdk, starts).provideSomeLayer[Scope](
            ZLayer.succeed(tools) ++ ZLayer.succeed(ContentStore.in(root.resolve("cas"))) ++
              ZLayer.succeed(LinkerStarts(starts)) ++ RemoteCache.off
          )
        yield result
      .provide(Resolve.coursier)

  private def compile(
      root: Path,
      jdk: JdkRuntime,
      libraries: List[Path],
      name: String,
      source: String,
      destination: Option[Path] = None,
  ): IO[Failure, Unit] =
    val work    = root.resolve(name)
    val classes = destination.getOrElse(work.resolve("classes"))
    for
      host <- hostClasspath
      file <- io(work.resolve("Source.scala"), IoFault.Write) {
        Files.createDirectories(work)
        Files.createDirectories(classes)
        Files.createDirectories(work.resolve("process"))
        Files.writeString(work.resolve("Source.scala"), source)
      }
      result <- ChildProcess
        .run(
          ChildRequest(
            Platform.Jvm,
            jdk.home.resolve("bin/java"),
            host,
            MainClass.trusted("dotty.tools.dotc.Main"),
            work.resolve("process"),
            arguments = List(
              "-scalajs",
              "-classpath",
              libraries.mkString(java.io.File.pathSeparator),
              "-d",
              classes.toString,
              file.toString,
            ),
          )
        )
        .mapError(Failure.Child(_))
      _ <- ZIO.fail(Failure.Compiler(result.out.text, result.err.text)).unless(result.exit.value == 0)
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
    io(destination, IoFault.Write) {
      val walk  = Files.walk(classes)
      val files = try walk.iterator.asScala.filter(Files.isRegularFile(_)).toList.sortBy(_.toString)
      finally walk.close()
      val stream = new ZipOutputStream(Files.newOutputStream(destination))
      try
        files.foreach: file =>
          val entry = new ZipEntry(classes.relativize(file).toString)
          entry.setTime(0L)
          stream.putNextEntry(entry)
          stream.write(Files.readAllBytes(file))
          stream.closeEntry()
      finally stream.close()
    }

  private def io[A](path: Path, fault: IoFault)(body: => A): IO[Failure, A] =
    ZIO.attemptBlocking(body).mapError(_ => Failure.Io(IoFailure(path, fault)))
end JsProductsSpec
