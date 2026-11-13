package histos

import histos.jdk.{Sha256, Sha256Digest}
import histos.program.ProgramFeed
import histos.resolve.ResolveError
import histos.testrun.{TestFeed, TestMode, TestStatus}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{ApiDigest, CompileFeed, CompileInput, CompiledCell, GraphCompile, GraphCompileError, KeyMiss}

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
end ScenarioSpec
