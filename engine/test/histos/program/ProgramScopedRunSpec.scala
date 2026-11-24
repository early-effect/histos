package histos.program

import histos.*
import histos.server.RunLaunch
import histos.testrun.TestExecutionFixtures
import histos.workspace.Workspace
import zio.{ZIO, durationInt}
import zio.test.*

object ProgramScopedRunSpec extends ZIOSpecDefault:
  private val tests = suite("scoped program commands")(
    test("module-run resolves inherited and explicit Full to the same mode-separated JS products") {
      val tools     = ToolCatalog.default.copy(js = ToolCatalog.default.js.copy(optimize = JsOptimize.Full))
      val inherited = module("app", Js)(TestExecutionFixtures.version).main(MainClass("demo.Chosen"))
      val explicit  = inherited.jsOptimize(JsOptimize.Full)
      for
        inheritedGraph <- ZIO.fromEither(Projects.tools(tools)(inherited))
        explicitGraph  <- ZIO.fromEither(Projects(explicit))
        result         <- TestExecutionFixtures.withGraph(inheritedGraph) { fixture =>
          val work = Program.workDir(fixture.repo, ProjectName("app"), TestExecutionFixtures.version, Js)
          def run(graph: ProjectGraph) = Program
            .console(
              fixture.repo,
              graph,
              "app",
              fixture.session.jdk,
              fixture.runtimes,
              None,
              RunLaunch.Tool(Map.empty, None),
              Nil,
              histos.process.RunRelay.Closed,
              _ => ZIO.unit,
              ZIO.unit,
            )
            .runCollect
          def products = ZIO
            .attemptBlocking {
              val walk = java.nio.file.Files.walk(work)
              try
                import _root_.scala.jdk.CollectionConverters.*
                walk.iterator.asScala
                  .filter(path =>
                    java.nio.file.Files.isRegularFile(path) &&
                      (path.endsWith("main.js") || path.endsWith("full.js"))
                  )
                  .map(work.relativize(_))
                  .toSet
              finally walk.close()
            }
            .mapError(_ => histos.workspace.IoFailure(work, histos.workspace.IoFault.Walk))
          for
            _ <- Workspace.writeString(
              fixture.repo.resolve("app/src/demo/Chosen.scala"),
              """package demo
                |object Chosen:
                |  def main(args: Array[String]): Unit = println("full-module-run")
                |""".stripMargin,
            )
            first             <- run(inheritedGraph)
            inheritedProducts <- products
            second            <- run(explicitGraph)
            explicitProducts  <- products
          yield assertTrue(
            first.collect { case ProgramFeed.Out(line) => line }.mkString.contains("full-module-run"),
            second.collect { case ProgramFeed.Out(line) => line }.mkString.contains("full-module-run"),
            first.collect { case ProgramFeed.Exited(_, _, _, code) => code }.toList == List(0),
            second.collect { case ProgramFeed.Exited(_, _, _, code) => code }.toList == List(0),
            inheritedProducts.nonEmpty,
            inheritedProducts.forall(_.toString.startsWith("full/full-")),
            inheritedProducts == explicitProducts,
          )
        }
      yield result
    },
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
    },
  )
  private val checked = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)
  def spec            = checked
