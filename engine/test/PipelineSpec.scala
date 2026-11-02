package histos.testrun

import histos.workspace.Workspace
import histos.zinc.GraphCompile
import histos.{Jvm, Projects, ScalaVersion, TestMachine, TestMain, module}

import java.nio.file.Path

import zio.{Promise, Ref, ZIO, durationInt}
import zio.test.*

object PipelineSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")

  def spec = suite("a test starts when its compile finishes")(
    test("an unrelated module tests while another compile is held"):
      ZIO.scoped:
        for
          jdk     <- GraphCompile.jdkHere
          root    <- temp
          release <- Promise.make[Nothing, Unit]
          begun   <- Promise.make[Nothing, Unit]
          early   <- Ref.make(false)
          built   <- ZIO
            .attempt {
              val slow  = module("slow", Jvm)(scala3)
              val fast  = module("fast", Jvm)(scala3).test(TestMain("fast.Check"))
              val after = module("after", Jvm)(scala3).dependsOn(slow).test(TestMain("after.Check"))
              write(root, "slow/src/main/scala/Slow.scala", "object Slow:\n  def n: Int = 1\n")
              write(root, "fast/src/main/scala/Fast.scala", "object Fast:\n  def n: Int = 2\n")
              write(
                root,
                "fast/src/test/scala/Check.scala",
                "package fast\n\nobject Check:\n  def main(args: Array[String]): Unit = ()\n",
              )
              write(root, "after/src/main/scala/After.scala", "object After:\n  def n: Int = Slow.n\n")
              write(
                root,
                "after/src/test/scala/Check.scala",
                "package after\n\nobject Check:\n  def main(args: Array[String]): Unit = ()\n",
              )
              write(
                root,
                "dependencies.lock",
                "cell slow 3.10.0 jvm\ncell fast 3.10.0 jvm\ncell after 3.10.0 jvm\n",
              )
              Projects(slow, fast, after)
            }
            .flatMap(ZIO.fromEither(_))
          here  <- TestMachine.runtimes(root)
          fiber <- histos.ToolFixture
            .streamAt(root, built)(
              ModuleTest.session(
                TestRun(root, built, Nil, jdk, here, TestMode.Run),
                name => if name.value == "slow" then release.await else ZIO.unit,
              )
            )
            .tap:
              case SessionFeed.Test(TestFeed.Begin(module, _, _)) if module.value == "fast" =>
                begun.succeed(()).unit
              case SessionFeed.Test(TestFeed.Begin(module, _, _)) if module.value == "after" =>
                early.set(true)
              case _ => ZIO.unit
            .runDrain
            .provideSomeLayer[histos.resolve.LockedTools](histos.ToolFixture.resolver)
            .forkScoped
          started <- Live.live(begun.await.timeout(120.seconds))
          sneaked <- early.get
          _       <- release.succeed(())
          _       <- fiber.join
        yield assertTrue(started.isDefined, !sneaked)
  ).provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
    histos.ToolFixture.layer(
      histos.Projects(module("jvm", Jvm)(scala3))
    )
  ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes)

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    java.nio.file.Files.createDirectories(path.getParent)
    java.nio.file.Files.writeString(path, text)

  private def temp =
    Workspace
      .work("histos-pipeline")
      .mapError(error => new java.io.IOException(error.message))
end PipelineSpec
