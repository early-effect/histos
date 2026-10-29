package histos

import histos.clean.Products
import histos.jdk.Sha256
import histos.resolve.LockedTools
import histos.testrun.TestFeed
import histos.workspace.IoFailure
import histos.zinc.{CompileFeed, CompiledCell}
import histos.{Jvm, Projects, ScalaVersion, TestMain, module}

import zio.{Chunk, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object ScenarioSpec extends histos.SuiteClock:
  private val scala3 = ScalaVersion("3.10.0")

  def spec = suite("scenarios")(
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
    ,
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
            out(first).contains("alpha") && cached(first).isEmpty,
            cached(hit) == List("demo.Foo") && !out(hit).contains("alpha"),
            out(other).contains("alpha") && cached(other).isEmpty,
          )).provideSomeLayer[LockedTools & Scope](supplied(store)),
  ).provideSomeLayerShared[zio.test.TestEnvironment & Scope](
    ToolFixture.jvm(scala3)
  ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes)

  private def supplied(store: Store) =
    ZLayer.fromZIO(ZIO.serviceWith[LockedTools](_.resolve)) ++
      ZLayer.succeed(store.content) ++
      store.compiler

  private def writeApp(checkout: Checkout): ZIO[Any, IoFailure, Unit] =
    checkout.write("app/src/main/scala/demo/Lib.scala", lib) *>
      checkout.write("app/src/test/scala/demo/Foo.scala", foo) *>
      checkout.write(".histos/dependencies.lock", "cell app 3.10.0 jvm\n")

  private def changed(feeds: Chunk[CompileFeed]): Boolean =
    done(feeds).exists(_.changed)

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

  private def cached(feeds: Chunk[TestFeed]): List[String] =
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
end ScenarioSpec
