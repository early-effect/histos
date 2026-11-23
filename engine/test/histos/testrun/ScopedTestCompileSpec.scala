package histos.testrun

import histos.*
import histos.workspace.Workspace

import java.nio.file.Files

import zio.{ZIO, durationInt}
import zio.test.*

object ScopedTestCompileSpec extends ZIOSpecDefault:
  private val version = TestExecutionFixtures.version
  private val tests   = suite("scoped Test compilation")(
    test("a downstream Test reads upstream fixtures and logical resources without executing upstream suites") {
      val upstream = module("fixtures", Jvm)(version)
        .libraryDependencies(dep"dev.zio::zio:2.1.27")
        .testDependencies(dep"dev.zio::zio-test:2.1.27", dep"dev.zio::zio-test-sbt:2.1.27")
      val consumer = module("consumer", Jvm)(version).testDependsOn(upstream).test(TestMain("demo.Check"))
      ZIO
        .fromEither(Projects(upstream, consumer))
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph): fixture =>
            for
              _ <- Workspace.writeString(
                fixture.repo.resolve("fixtures/src/lib/Base.scala"),
                "package lib\nobject Base:\n  def text: String = \"main\"\n",
              )
              _ <- Workspace.writeString(
                fixture.repo.resolve("consumer/src/demo/Main.scala"),
                "package demo\nobject Main:\n  def value: Int = 1\n",
              )
              _ <- Workspace.writeString(
                fixture.repo.resolve("fixtures/test/lib/Fixture.scala"),
                "package lib\nobject Fixture:\n  def text: String = \"fixture\"\n",
              )
              _ <- Workspace.writeString(
                fixture.repo.resolve("fixtures/test/lib/UpstreamSpec.scala"),
                """package lib
                |import zio.*
                |import zio.test.*
                |object UpstreamSpec extends ZIOSpecDefault:
                |  def spec = test("must not execute") {
                |    ZIO.attempt(java.nio.file.Files.writeString(java.nio.file.Path.of(sys.env.getOrElse("HISTOS_REPO", "."), "upstream-counter"), "ran"))
                |      .as(assertTrue(false))
                |  }
                |""".stripMargin,
              )
              _ <- Workspace.writeString(fixture.repo.resolve("fixtures/src/test/resources/fixture.txt"), "resource")
              _ <- Workspace.writeString(
                fixture.repo.resolve("consumer/test/demo/Check.scala"),
                """package demo
                |object Check:
                |  def main(args: Array[String]): Unit =
                |    val text = Option(getClass.getResourceAsStream("/fixture.txt")).fold("missing"): input =>
                |      try new String(input.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
                |      finally input.close()
                |    println(lib.Fixture.text + ":" + text)
                |""".stripMargin,
              )
              reports <- ModuleTest.run(
                TestRun(
                  fixture.repo,
                  graph,
                  List(ProjectName("consumer")),
                  fixture.session.jdk,
                  fixture.runtimes,
                  TestMode.Run,
                )
              )
              ran <- ZIO.attemptBlocking(Files.exists(fixture.repo.resolve("upstream-counter")))
            yield assertTrue(
              reports.map(_.cell.module) == List(ProjectName("consumer")),
              reports.forall(_.cell.status == TestStatus.Passed),
              reports.exists(_.transcript.outText.text.contains("fixture:resource")),
              !ran,
            )
    }
  )
  private val macroTests = suite("Test compile-time identities")(
    test(
      "an upstream Test macro helper body changes unchanged downstream typeCheckErrors and then reuses the warm product"
    ) {
      val upstream = module("validator", Jvm)(version)
      val consumer = module("consumer", Jvm)(version).testDependsOn(upstream)
      ZIO
        .fromEither(Projects(upstream, consumer))
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph): fixture =>
            val upstreamStage =
              CompilationStage(Cell(ProjectName("validator"), version, Jvm), histos.plugin.SourceSet.Test)
            val stage = CompilationStage(Cell(ProjectName("consumer"), version, Jvm), histos.plugin.SourceSet.Test)
            for
              _ <- Workspace.writeString(fixture.repo.resolve("validator/test/lib/Literal.scala"), validator("old"))
              _ <- Workspace.writeString(fixture.repo.resolve("consumer/test/demo/Use.scala"), macroConsumer)
              initial        <- fixture.ensure(List(histos.build.BuildNode.Compile(stage)))
              before         <- ZIO.fromEither(initial.require(stage))
              upstreamBefore <- ZIO.fromEither(initial.require(upstreamStage))
              one <- histos.load.PluginCompileSupport.invoke(before.classes, before.classpath, "demo.Use$", "text")
              _   <- Workspace.writeString(fixture.repo.resolve("validator/test/lib/Literal.scala"), validator("new"))
              updated       <- fixture.ensure(List(histos.build.BuildNode.Compile(stage)))
              after         <- ZIO.fromEither(updated.require(stage))
              upstreamAfter <- ZIO.fromEither(updated.require(upstreamStage))
              two    <- histos.load.PluginCompileSupport.invoke(after.classes, after.classpath, "demo.Use$", "text")
              warm   <- fixture.ensure(List(histos.build.BuildNode.Compile(stage)))
              reused <- ZIO.fromEither(warm.require(stage))
            yield assertTrue(
              one == "false",
              two == "true",
              upstreamBefore.api == upstreamAfter.api,
              upstreamBefore.implementation != upstreamAfter.implementation,
              before.compiled.key != after.compiled.key,
              after.compiled.sources.exists(_.compiled == 1),
              reused.compiled.key == after.compiled.key,
              !reused.compiled.changed,
            )
    }
  )

  def spec = (tests + macroTests) @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)

  private def validator(accepted: String): String =
    s"""|package lib
        |import scala.quoted.*
        |object Validator:
        |  def accepts(raw: String): Boolean = raw == "$accepted"
        |object Literal:
        |  inline def apply(inline raw: String): String = $${literal('raw)}
        |  def literal(raw: Expr[String])(using Quotes): Expr[String] =
        |    raw.value match
        |      case Some(value) if Validator.accepts(value) => Expr(value)
        |      case _ => quotes.reflect.report.errorAndAbort("refused")
        |""".stripMargin

  private val macroConsumer =
    """|package demo
       |object Use:
       |  private val accepted = scala.compiletime.testing.typeCheckErrors("lib.Literal(\"new\")").isEmpty
       |  def text: String = accepted.toString
       |""".stripMargin
end ScopedTestCompileSpec
