package histos.program

import histos.*
import histos.server.RunLaunch
import histos.testrun.TestExecutionFixtures
import histos.workspace.Workspace
import zio.{ZIO, durationInt}
import zio.test.*

object ProgramScopedRunSpec extends ZIOSpecDefault:
  private val tests = suite("scoped program commands")(
    test("defaults and explicit platforms launch the explicit main with VM properties before opaque arguments") {
      val cases = List(
        (Jvm, None),
        (Js, Some(PlatformSelection.only(Jvm))),
      )
      ZIO
        .foreach(cases) { (default, explicit) =>
          val target = module("app", Jvm, Js)(TestExecutionFixtures.version)
            .runPlatform(default)
            .jvmOptions(JvmOptions(List(JvmFlag("-ea")), List(SystemProperty("program.marker=present"))))
          ZIO.fromEither(Projects(target)).flatMap { graph =>
            TestExecutionFixtures.withGraph(graph) { fixture =>
              for
                _ <- Workspace.writeString(
                  fixture.repo.resolve("app/src/demo/Chosen.scala"),
                  """package demo
                  |object Chosen:
                  |  def main(args: Array[String]): Unit =
                  |    println("property=" + sys.props.getOrElse("program.marker", "missing"))
                  |    println("args=" + args.mkString("|"))
                  |    println("assertions=" + getClass.desiredAssertionStatus())
                  |""".stripMargin,
                )
                events <- Program
                  .console(
                    fixture.repo,
                    graph,
                    "app",
                    fixture.session.jdk,
                    fixture.runtimes,
                    explicit,
                    RunLaunch.Tool(Map.empty, None),
                    List("hello", "--opaque"),
                    histos.process.RunRelay.Closed,
                    _ => ZIO.unit,
                    ZIO.unit,
                    mainOverride = Some(MainClass("demo.Chosen")),
                  )
                  .runCollect
              yield (
                events.collect { case ProgramFeed.Out(line) => line }.mkString("\n"),
                events.collect { case ProgramFeed.Exited(_, _, _, code) => code }.toList,
                events.collect { case ProgramFeed.Begin(_, _, platform) => platform }.toList,
              )
            }
          }
        }
        .map(results =>
          assertTrue(
            results.size == 2,
            results.forall { (out, exits, platforms) =>
              out.contains("property=present") && out.contains("args=hello|--opaque") &&
              out.contains("assertions=true") && exits == List(0) && platforms == List(Jvm)
            },
          )
        )
    }
  )
  private val checked = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)
  def spec            = checked
