package histos.testrun

import histos.*
import histos.build.BuildNode
import histos.plugin.SourceSet
import histos.workspace.Workspace

import zio.{ZIO, durationInt}
import zio.test.*

object JsScopedTestSpec extends ZIOSpecDefault:
  private val tests = suite("actual JS scoped Test execution")(
    test("a JS Test main imports an upstream Test fixture and runs only the requested target") {
      val version  = TestExecutionFixtures.version
      val fixtures = module("fixtures", Js)(version)
      val consumer = module("consumer", Js)(version)
        .testDependsOn(fixtures)
        .testDependencies(dep"dev.zio::zio-test:2.1.27")
        .test(TestMain("demo.Check"))
      ZIO
        .fromEither(Projects(fixtures, consumer))
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph): fixture =>
            val asked = TestRun(
              fixture.repo,
              graph,
              List(ProjectName("consumer")),
              fixture.session.jdk,
              fixture.runtimes,
              TestMode.Run,
              platforms = PlatformSelection.only(Js),
            )
            for
              _ <- Workspace.writeString(
                fixture.repo.resolve("fixtures/test/lib/Fixture.scala"),
                "package lib\nobject Fixture:\n  def text: String = \"js-fixture\"\n",
              )
              _ <- Workspace.writeString(
                fixture.repo.resolve("consumer/test/demo/Check.scala"),
                """package demo
                  |object Check:
                  |  def main(args: Array[String]): Unit =
                  |    println(lib.Fixture.text + ":" + 'a'.isLetterOrDigit + ":" + (0 until 2).size)
                  |""".stripMargin,
              )
              plan     <- ZIO.fromEither(TestCells.plan(graph, asked.selection))
              products <- fixture.ensure(plan.compileStageRoots.map(BuildNode.Compile(_)))
              main     <- ZIO.fromEither(
                products.require(CompilationStage(Cell(ProjectName("consumer"), version, Js), SourceSet.Main))
              )
              upstream <- ZIO.fromEither(
                products.require(CompilationStage(Cell(ProjectName("fixtures"), version, Js), SourceSet.Test))
              )
              test <- ZIO.fromEither(
                products.require(CompilationStage(Cell(ProjectName("consumer"), version, Js), SourceSet.Test))
              )
              feeds <- hostClasspath(ModuleTest.execute(asked, plan, products).runCollect)
              tools <- ZIO.service[histos.resolve.LockedTools]
              roots = histos.js.JsRequests.libraryRoots(version, tools.catalog.js.revision)
            yield assertTrue(
              test.classpath.forall(path =>
                roots.forall(root => !Option(path.getFileName).exists(_.toString.startsWith(root.name + "-")))
              ),
              !main.classpath.contains(upstream.classes),
              test.classpath.contains(upstream.classes),
              feeds.collect { case TestFeed.Done(cell) => cell.module }.toList == List(ProjectName("consumer")),
              feeds.exists { case TestFeed.Out(_, _, _, text) => text == "js-fixture:true:2"; case _ => false },
              feeds.exists { case TestFeed.Done(cell) => cell.status == TestStatus.Passed; case _ => false },
            )
    }
  )
  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)

  private def hostClasspath[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E | histos.workspace.IoFailure, A] =
    val property = "java.class.path"
    ZIO.acquireReleaseWith(
      ZIO
        .attemptBlocking:
          val previous = Option(java.lang.System.getProperty(property))
          val existing = previous.toList
            .flatMap(_.split(java.io.File.pathSeparator).toList)
            .filter(path => java.nio.file.Files.exists(java.nio.file.Path.of(path)))
          java.lang.System.setProperty(property, existing.mkString(java.io.File.pathSeparator))
          previous
        .mapError(_ => histos.workspace.IoFailure(java.nio.file.Path.of(property), histos.workspace.IoFault.Read))
    )(previous =>
      ZIO
        .attempt(previous match
          case Some(value) => java.lang.System.setProperty(property, value)
          case None        => java.lang.System.clearProperty(property))
        .ignore
    )(_ => effect)
end JsScopedTestSpec
