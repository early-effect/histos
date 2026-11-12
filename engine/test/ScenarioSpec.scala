package histos

import histos.jdk.Sha256
import histos.program.ProgramFeed
import histos.resolve.ToolRole
import histos.testrun.{TestFeed, TestMode, TestStatus}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompileFeed, CompileInput, CompiledCell, GraphCompile, GraphCompileError, KeyMiss}

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
    test("a second checkout of the same bytes compiles again, and the wiped checkout restores without a start"):
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
          _        <- left.clean
          wiped    <- exists(coreClass(left, scala3))
          again    <- left.compile
          back     <- exists(coreClass(left, scala3))
          starts   <- scenario.starts
        yield assertTrue(
          failed(first ++ second ++ again).isEmpty,
          compiled(first),
          compiled(second),
          restored(again),
          missed(again).isEmpty,
          !wiped,
          back,
          starts == 2,
          differs(keyOf(first), keyOf(second)),
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
    test("an unchanged native cell is a hit that needs neither the store nor the compiler"):
      ZIO.scoped:
        for
          scala3   <- locked
          _        <- ZIO.serviceWithZIO[CompilerClosure](_.held(ToolRole.NativeLibraries))
          built    <- ZIO.fromEither(Projects.locked(module("core", Native)(scala3).clang(ClangMajor(21))))
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
    test("a quick hit stays in its checkout, and its record is a file there and not a blob"):
      ZIO.scoped:
        for
          scala3   <- locked
          built    <- ZIO.fromEither(Projects.locked(module("app", Jvm)(scala3).test(TestMain("demo.Foo"))))
          scenario <- Scenario.open
          left     <- scenario.checkout(built)
          right    <- scenario.checkout(built)
          _        <- writeApp(left)
          _        <- writeApp(right)
          first    <- left.test(TestMode.Quick)
          kept     <- record(left, scala3)
          hit      <- left.test(TestMode.Quick)
          before   <- record(right, scala3)
          other    <- right.test(TestMode.Quick)
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
          statuses(other) == List(TestStatus.Passed),
          out(other).contains("alpha"),
          cached(other).isEmpty,
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

  private def coreClass(checkout: Checkout, version: ScalaVersion): Path =
    GraphCompile.classesDir(checkout.root, ProjectName("core"), version).resolve("Core.class")

  /** The fingerprint a passing quick run stored under this checkout. */
  private def record(checkout: Checkout, version: ScalaVersion): IO[IoFailure, Option[Sha256]] =
    val path = checkout.root.resolve(s".histos/test-quick/app/${version.value}/jvm/demo/Foo")
    Workspace
      .readIfPresent(path)
      .map(_.flatMap(text => Sha256.from(text.trim.stripPrefix("success ")).toOption))

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

  private def statuses(feeds: Chunk[TestFeed]): List[TestStatus] =
    feeds.collect { case TestFeed.Done(cell) => cell.status }.toList

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
end ScenarioSpec
