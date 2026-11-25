package histos.testrun

import histos.*
import histos.build.BuildNode
import histos.pack.MainClass
import histos.plugin.SourceSet
import histos.process.ChildProcess
import histos.suite.HistosSpec
import histos.workspace.Workspace

import zio.{ZIO, durationInt}
import zio.test.*

object ProviderOptionsSpec extends HistosSpec:
  private val tests = suite("provider JVM options")(
    test("a real child sees a provider system property, not a program argument") {
      val version = TestExecutionFixtures.version
      ZIO
        .fromEither(Projects(module("app", Jvm)(version)))
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph): fixture =>
            for
              _ <- Workspace.writeString(
                fixture.repo.resolve("app/src/demo/Options.scala"),
                """package demo
                |object Options:
                |  def main(args: Array[String]): Unit =
                |    println("property=" + sys.props.getOrElse("provider.marker", "missing"))
                |    println("args=" + args.mkString("|"))
                |    println("module=" + sys.props.getOrElse("module.marker", "missing"))
                |    println("assertions=" + getClass.desiredAssertionStatus())
                |""".stripMargin,
              )
              stage = CompilationStage(Cell(ProjectName("app"), version, Jvm), SourceSet.Main)
              products <- fixture.ensure(List(BuildNode.Compile(stage)))
              app      <- ZIO.fromEither(products.require(stage))
              work     <- Workspace.createDirectories(fixture.repo.resolve("child"))
              request = SuiteLaunch.request(
                fixture.repo,
                fixture.session.jdk,
                fixture.runtimes,
                app.classes,
                app.classpath.filterNot(_ == app.classes),
                MainClass.trusted("demo.Options"),
                work,
                properties = List(SystemProperty("provider.marker=present")),
                jvmOptions = JvmOptions(
                  List(JvmFlag("-ea")),
                  List(SystemProperty("provider.marker=overridden"), SystemProperty("module.marker=kept")),
                ),
              )
              outcome <- ChildProcess.run(request.copy(arguments = request.arguments :+ "hello"))
            yield assertTrue(
              outcome.exit.value == 0,
              outcome.out.text.contains("property=present"),
              outcome.out.text.contains("args=hello"),
              !outcome.out.text.contains("args=-D"),
              outcome.out.text.contains("module=kept"),
              outcome.out.text.contains("assertions=true"),
              request.jvmOptions.properties.map(_.value) == List("module.marker=kept", "provider.marker=present"),
            )
    },
    test("ModuleTest passes the module's VM options to a real Test main") {
      val version = TestExecutionFixtures.version
      val target  = module("app", Jvm)(version)
        .test(TestMain("demo.Check"))
        .jvmOptions(JvmOptions(List(JvmFlag("-ea")), List(SystemProperty("module.marker=kept"))))
      ZIO
        .fromEither(Projects(target))
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph): fixture =>
            for
              _ <- Workspace.writeString(
                fixture.repo.resolve("app/test/demo/Check.scala"),
                """package demo
                |object Check:
                |  def main(args: Array[String]): Unit =
                |    println("module=" + sys.props.getOrElse("module.marker", "missing"))
                |    println("assertions=" + getClass.desiredAssertionStatus())
                |""".stripMargin,
              )
              reports <- ModuleTest.run(
                TestRun(
                  fixture.repo,
                  graph,
                  List(ProjectName("app")),
                  fixture.session.jdk,
                  fixture.runtimes,
                  TestMode.Run,
                )
              )
            yield assertTrue(
              reports.exists(_.cell.status == TestStatus.Passed),
              reports.exists(_.transcript.outText.text.contains("module=kept")),
              reports.exists(_.transcript.outText.text.contains("assertions=true")),
            )
    },
  )
  def spec =
    (tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(3.minutes)).provideShared(histos.zinc.Compilers.owned)
