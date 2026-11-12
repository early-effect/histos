package histos

import histos.jdk.Sha256
import histos.testrun.{TestFeed, TestMode, TestStatus}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompileFeed, CompiledCell, GraphCompile, GraphCompileError}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, Scope, ZIO, durationInt}
import zio.test.*

object ScenarioSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")

  def spec = suite("scenarios")(
    test("a second checkout of the same bytes compiles again, and the wiped checkout restores without a start"):
      ZIO.scoped:
        for
          built    <- ZIO.fromEither(Projects.locked(module("core", Jvm)(scala3)))
          scenario <- Scenario.open
          left     <- scenario.checkout(built)
          right    <- scenario.checkout(built)
          _        <- left.write("core/src/main/scala/Core.scala", core)
          _        <- right.write("core/src/main/scala/Core.scala", core)
          first    <- left.compile
          second   <- right.compile
          _        <- left.clean
          wiped    <- exists(coreClass(left))
          again    <- left.compile
          back     <- exists(coreClass(left))
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
    test("a quick hit stays in its checkout, and its record is a file there and not a blob"):
      ZIO.scoped:
        for
          built    <- ZIO.fromEither(Projects.locked(module("app", Jvm)(scala3).test(TestMain("demo.Foo"))))
          scenario <- Scenario.open
          left     <- scenario.checkout(built)
          right    <- scenario.checkout(built)
          _        <- writeApp(left)
          _        <- writeApp(right)
          first    <- left.test(TestMode.Quick)
          kept     <- record(left)
          hit      <- left.test(TestMode.Quick)
          before   <- record(right)
          other    <- right.test(TestMode.Quick)
          after    <- record(right)
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
        ),
  ).provideSomeLayerShared[TestEnvironment & Scope](Scenario.tools) @@ TestAspect.withLiveSystem @@
    TestAspect.timeout(3.minutes)

  private def writeApp(checkout: Checkout): IO[IoFailure, Unit] =
    checkout.write("app/src/main/scala/demo/Lib.scala", lib) *>
      checkout.write("app/src/test/scala/demo/Foo.scala", foo)

  private def coreClass(checkout: Checkout): Path =
    GraphCompile.classesDir(checkout.root, ProjectName("core"), scala3).resolve("Core.class")

  /** The fingerprint a passing quick run stored under this checkout. */
  private def record(checkout: Checkout): IO[IoFailure, Option[Sha256]] =
    val path = checkout.root.resolve(".histos/test-quick/app/3.10.0/jvm/demo/Foo")
    Workspace
      .readIfPresent(path)
      .map(_.flatMap(text => Sha256.from(text.trim.stripPrefix("success ")).toOption))

  private def exists(path: Path): IO[IoFailure, Boolean] =
    ZIO.attemptBlocking(Files.isRegularFile(path)).mapError(_ => IoFailure(path, IoFault.Stat))

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
end ScenarioSpec
