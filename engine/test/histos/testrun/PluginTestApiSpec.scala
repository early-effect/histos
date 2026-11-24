package histos.testrun

import histos.*
import histos.build.{BuildNode, ExecutionError}
import histos.jdk.Sha256Digest
import histos.plugin.SourceSet
import histos.resolve.{
  Classpath,
  ClasspathScope,
  Repositories,
  Repository,
  Resolve,
  ResolveCell,
  ResolveError,
  ResolvedArtifact,
  ToolRequest,
}
import histos.workspace.Workspace
import histos.zinc.{ClasspathBan, CompileError, ScalacClasspath}

import java.nio.file.{Files, Path}

import zio.{Chunk, IO, ZIO, ZLayer, durationInt}
import zio.test.*

object PluginTestApiSpec extends ZIOSpecDefault:
  private val version    = TestExecutionFixtures.version
  private val api        = dep"rocks.earlyeffect::histos-api:0.5.0-SNAPSHOT".compileOnly
  private val zioTest    = dep"dev.zio::zio-test:2.1.27"
  private val zioTestSbt = dep"dev.zio::zio-test-sbt:2.1.27"

  private val tests = suite("scoped plugin Test host API")(
    test("a plugin library without a coordinate compiles cold Main and Test against the host") {
      plugin(module("plug", Jvm)(version).testDependencies(zioTest, zioTestSbt).pluginLibrary)
    },
    test("a compile-only coordinate and an old resolved Test API cannot shadow the host") {
      val upstream = module("old", Jvm)(version).libraryDependencies(api)
      val defined  = module("plug", Jvm)(version)
        .libraryDependencies(api)
        .testDependencies(zioTest, zioTestSbt)
        .testDependsOn(upstream)
        .pluginLibrary
      plugin(defined, List(upstream))
    },
    test("an ordinary Test cannot use an untrusted Histos jar") {
      val defined = module("plug", Jvm)(version).testDependencies(api.provided)
      ZIO
        .fromEither(Projects(defined))
        .flatMap: graph =>
          TestExecutionFixtures.withGraph(graph, resolver): fixture =>
            for
              _      <- sources(fixture.repo)
              result <- fixture.ensure(List(BuildNode.Compile(testStage))).either
            yield assertTrue(
              result.left.exists {
                case ExecutionError
                      .Test(ModuleTestError.Sources(_, _, CompileError.Banned(path, ClasspathBan.Histos))) =>
                  Option(path.getFileName).exists(_.toString == "histos-api-old.jar")
                case _ => false
              }
            )
    },
  )

  def spec = tests @@ TestAspect.withLiveClock @@ TestAspect.timeout(5.minutes)

  private val mainStage = CompilationStage(Cell(ProjectName("plug"), version, Jvm), SourceSet.Main)
  private val testStage = mainStage.copy(scope = SourceSet.Test)

  private def plugin(defined: ProjectDef, upstream: List[ProjectDef] = Nil) =
    ZIO
      .fromEither(Projects((upstream :+ defined)*))
      .flatMap: graph =>
        TestExecutionFixtures.withGraph(graph, resolver): fixture =>
          for
            _        <- sources(fixture.repo)
            products <- fixture.ensure(List(BuildNode.Compile(testStage)))
            main     <- ZIO.fromEither(products.require(mainStage))
            tests    <- ZIO.fromEither(products.require(testStage))
            host     <- ScalacClasspath.hostApi(fixture.repo)
            reports  <- ModuleTest.run(
              TestRun(
                fixture.repo,
                graph,
                List(ProjectName("plug")),
                fixture.session.jdk,
                fixture.runtimes,
                TestMode.Run,
              )
            )
          yield assertTrue(
            main.compiled.sources.exists(_.compiled == 1),
            tests.compiled.sources.exists(_.compiled == 1),
            main.classpath.contains(host),
            tests.classpath.contains(host),
            !main.classpath.exists(oldApi),
            !tests.classpath.exists(oldApi),
            reports.map(_.cell.module) == List(ProjectName("plug")),
            reports.forall(_.cell.status == TestStatus.Passed),
            reports.exists(_.transcript.outText.text.contains("host API is visible")),
          )

  private def sources(repo: Path) =
    Workspace.writeString(
      repo.resolve("plug/src/demo/Main.scala"),
      "package demo\nobject Main:\n  def value: Int = 1\n",
    ) *>
      Workspace.writeString(
        repo.resolve("plug/test/demo/Check.scala"),
        """package demo
          |import zio.test.*
          |object Check extends ZIOSpecDefault:
          |  def spec = test("host API is visible") {
          |    assertTrue(classOf[histos.plugin.Guard].getName == "histos.plugin.Guard")
          |  }
          |""".stripMargin,
      )

  private def oldApi(path: Path): Boolean =
    Option(path.getFileName).exists(_.toString == "histos-api-old.jar")

  private val resolver: ZLayer[Any, Nothing, Resolve] = Resolve.coursier >>> ZLayer.scoped:
    for
      repo <- Workspace.work("old-test-api").catchAll(error => ZIO.dieMessage(error.message))
      host <- ScalacClasspath.hostApi(repo).catchAll(error => ZIO.dieMessage(error.message))
      old = repo.resolve("histos-api-old.jar")
      bytes <- ZIO.attemptBlocking {
        Files.copy(host, old)
        Files.readAllBytes(old)
      }.orDie
      inner <- ZIO.service[Resolve]
      artifact = ResolvedArtifact(
        api.organization,
        "histos-api_3",
        api.revision.value,
        old,
        Sha256Digest.of(Chunk.fromArray(bytes)),
        Repository.Central,
      )
    yield new OldApi(inner, artifact)

  private final class OldApi(inner: Resolve, artifact: ResolvedArtifact) extends Resolve:
    def resolveTool(request: ToolRequest): IO[ResolveError, Classpath] = inner.resolveTool(request)

    def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
      val wanted   = cell.dependencies.exists(_.name == api.name)
      val selected = cell.copy(dependencies = cell.dependencies.filterNot(_.name == api.name))
      inner
        .resolve(selected)
        .flatMap: classpath =>
          if wanted || cell.scope == ClasspathScope.Test then
            ZIO.succeed(classpath.copy(artifacts = artifact :: classpath.artifacts))
          else ZIO.succeed(classpath)

    def fetch(locked: histos.resolve.LockedArtifact): IO[ResolveError, ResolvedArtifact] =
      if locked.sha256 == artifact.sha256 && locked.name == artifact.name then ZIO.succeed(artifact)
      else inner.fetch(locked)
    def resolvePlugin(dependency: Dep, repositories: Repositories): IO[ResolveError, Classpath] =
      inner.resolvePlugin(dependency, repositories)
    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository,
    ): IO[ResolveError, ResolvedArtifact] =
      inner.sources(organization, name, revision, repository)
  end OldApi
end PluginTestApiSpec
