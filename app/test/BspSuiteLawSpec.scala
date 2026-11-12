package histos.bsp

import histos.cache.ContentStore
import histos.jdk.Sha256
import histos.load.BuildNotes
import histos.publish.LauncherPin
import histos.resolve.{Classpath, Repositories, Repository, ResolutionService, Resolve, ResolveCell, ResolveError}
import histos.resolve.{LockedArtifact, ResolvedArtifact, ToolRequest}
import histos.testrun.TestCompile
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers, GraphCompile}
import histos.{
  CompilerClosure,
  Dep,
  LawMachine,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
  TestMain,
}

import java.nio.file.{Files, Path}

import zio.json.*
import zio.test.{TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}
import zio.{Chunk, IO, Ref, Scope, ZIO, ZLayer}

/** A test target names its main, its classpath, and its work directory. A main target is not a test. A dependency
  * source is the jar `Resolve` answered for the module's Scala library, so the law does not depend on what the Coursier
  * cache happens to hold.
  */
object BspSuiteLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("suitelaw")

  def spec = suite("bsp suite laws")(
    test("a test main is the suite, a main target is refused, and the library's sources jar is named"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-bsp-suite")
          .flatMap: dir =>
            val main = dir.resolve(lawName.value).resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val probe =
              dir.resolve(lawName.value).resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
            val mainUri   = Bsp.mainUri(dir, lawName.value, scala3, Platform.Jvm)
            val testUri   = Bsp.testUri(dir, lawName.value, scala3, Platform.Jvm)
            val buildUri  = Bsp.buildUri(dir)
            val classes   = TestCompile.testClasses(dir, lawName, scala3, Platform.Jvm)
            val classFile = classes.resolve("Probe.class")
            val mainClass =
              GraphCompile.classesDir(dir, lawName, scala3, Platform.Jvm).resolve("Hello.class")
            val work        = TestCompile.testWork(dir, lawName, scala3, Platform.Jvm).toAbsolutePath.normalize.toString
            val classesMark = s"/test-classes/${lawName.value}/"
            val sourcesJar  = dir.resolve("sources").resolve(s"scala-library-${scala3.value}-sources.jar")
            val program     =
              for
                machine  <- LawMachine.at(dir)
                jdk      <- LawMachine.jdk(machine)
                _        <- Workspace.writeString(main, "object Hello:\n  def ready: Int = 1\n")
                _        <- Workspace.writeString(probe, "object Probe:\n  def main(args: Array[String]): Unit = ()\n")
                _        <- Workspace.writeString(dir.resolve(LauncherPin.file), "histos.version=9.8.7\n")
                notes    <- Ref.make(Chunk.empty[String])
                runtimes <- Runtimes.read(machine, None)
                session  <- Bsp.open(
                  dir,
                  graph(scala3),
                  jdk,
                  runtimes,
                  Nil,
                  line => notes.update(_ :+ line),
                  ZIO.succeed(BuildNotes(false, Nil)),
                  compile => compile,
                  None,
                )
                _            <- ask(session, request(1, "build/initialize"))
                _            <- ask(session, note("build/initialized"))
                listed       <- ask(session, request(2, "workspace/buildTargets"))
                sourced      <- ask(session, asked(3, "buildTarget/sources", List(testUri)))
                opted        <- ask(session, asked(4, "buildTarget/scalacOptions", List(testUri)))
                jars         <- ask(session, asked(5, "buildTarget/dependencySources", List(mainUri)))
                mainCompiled <- ask(session, asked(6, "buildTarget/compile", List(mainUri)))
                compiled     <- ask(session, asked(11, "buildTarget/compile", List(testUri)))
                wrote        <- ZIO.attempt(Files.isRegularFile(classFile)).orDie
                mainWrote    <- ZIO.attempt(Files.isRegularFile(mainClass)).orDie
                suites       <- ask(session, asked(7, "buildTarget/scalaTestClasses", List(testUri, mainUri)))
                room         <- ask(session, asked(8, "buildTarget/jvmTestEnvironment", List(testUri)))
                refused      <- ask(session, asked(9, "buildTarget/jvmTestEnvironment", List(mainUri)))
                buildRefused <- ask(session, asked(10, "buildTarget/jvmTestEnvironment", List(buildUri)))
                heard        <- notes.get
              yield
                val targets = rpcOk[Rpc.BuildTargetsResult](listed).map(_.result.targets)
                val files   = rpcOk[Rpc.SourcesResult](sourced).map(_.result.items.flatMap(_.sources))
                val options =
                  rpcOk[Rpc.ScalacOptionsResult](opted).toOption.flatMap(_.result.items.find(_.target.uri == testUri))
                val sources = rpcOk[Rpc.DependencySourcesResult](jars).toOption
                  .flatMap(_.result.items.find(_.target.uri == mainUri))
                val mainStatus   = rpcOk[Rpc.CompileResult](mainCompiled).map(_.result.statusCode)
                val status       = rpcOk[Rpc.CompileResult](compiled).map(_.result.statusCode)
                val classesNamed = rpcOk[Rpc.ScalaTestClassesResult](suites).map(_.result.items)
                val suite        = classesNamed.toOption.flatMap(_.find(_.target.uri == testUri))
                val mainClasses  = classesNamed.toOption.flatMap(_.find(_.target.uri == mainUri))
                val ran          =
                  rpcOk[Rpc.JvmEnvironmentResult](room).toOption.flatMap(_.result.items.find(_.target.uri == testUri))
                val transcript =
                  if mainStatus == Right(1) && status == Right(1) then "" else heard.mkString("\n")
                val testTarget = targets.toOption.flatMap(_.find(_.displayName == s"${lawName.value}-test"))
                assertTrue(
                  testTarget.exists(item =>
                    item.id.uri == testUri && item.tags == List("test") && item.capabilities.canTest &&
                      !item.capabilities.canRun
                  ),
                  files.exists(_.exists(item => item.uri.endsWith("Probe.scala") && item.kind == 1 && !item.generated)),
                  options.exists(item => item.classDirectory.contains(classesMark) && item.classpath.nonEmpty),
                  sources.map(_.sources) == Some(List(sourcesJar.toAbsolutePath.normalize.toUri.toString)),
                  (mainStatus, status, transcript) == (Right(1), Right(1), ""),
                  mainWrote,
                  wrote,
                  suite.exists(item => item.classes == List("Probe") && item.framework.isEmpty),
                  mainClasses.exists(item => item.classes.isEmpty && item.framework.isEmpty),
                  ran.exists(item =>
                    item.classpath.headOption.exists(_.contains(classesMark)) &&
                      item.workingDirectory == work &&
                      item.environmentVariables.get("HISTOS_REPO").contains(dir.toAbsolutePath.normalize.toString)
                  ),
                  rpcError(refused).exists(_.error.message == "jvmTestEnvironment needs a test target"),
                  rpcError(buildRefused).exists(_.error.message == "jvmTestEnvironment needs a test target"),
                )
            ZIO.serviceWithZIO[Resolve]: offline =>
              program.provideSomeLayer[Scope](session(dir, closure, Sources(offline, sourcesJar, scala3)))
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer)

  /** The offline cache for every jar, and one sources jar for `org.scala-lang:scala-library` at `version`, the library
    * a Scala 3 module runs on. Any other sources request goes to the offline cache.
    */
  private final class Sources(offline: Resolve, jar: Path, version: ScalaVersion) extends Resolve:
    def resolveTool(request: ToolRequest): IO[ResolveError, Classpath]      = offline.resolveTool(request)
    def resolve(cell: ResolveCell): IO[ResolveError, Classpath]             = offline.resolve(cell)
    def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] = offline.fetch(artifact)
    def resolvePlugin(dependency: Dep, repositories: Repositories): IO[ResolveError, Classpath] =
      offline.resolvePlugin(dependency, repositories)
    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository,
    ): IO[ResolveError, ResolvedArtifact] =
      if organization == "org.scala-lang" && name == "scala-library" && revision == version.value then
        ZIO.succeed(ResolvedArtifact(organization, name, revision, jar, Sha256.trusted("ab"), repository))
      else offline.sources(organization, name, revision, repository)

  private def graph(scala3: ScalaVersion): ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      test = Some(TestMain("Probe")),
    )
    ProjectGraph.trusted(List(law), Splits.aggregate, Catalog.empty, None, Nil, resolution = Resolution.Lock)

  private def session(dir: Path, closure: CompilerClosure, resolve: Resolve) =
    ZLayer.succeed(resolve) >+>
      (ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
        Compilers.live(1) ++
        CompileMemo.live ++
        ResolutionService.pinned(closure.pinning(lawName)))

  private def ask(session: Bsp.Session, payload: String) =
    session.answer(payload)

  private def request(id: Int, method: String): String =
    s"""{"id":$id,"method":"$method"}"""

  private def note(method: String): String =
    s"""{"method":"$method"}"""

  private def asked(id: Int, method: String, uris: List[String]): String =
    val body = Asked(uris.map(Rpc.TargetId(_))).toJson
    s"""{"id":$id,"method":"$method","params":$body}"""

  private def rpcError(answer: Bsp.Answer): Option[Rpc.RpcErr] =
    answer match
      case Bsp.Answer.Reply(line) => line.fromJson[Rpc.RpcErr].toOption
      case _                      => None

  private def rpcOk[A: JsonCodec](answer: Bsp.Answer): Either[String, Rpc.RpcOk[A]] =
    answer match
      case Bsp.Answer.Reply(line) => line.fromJson[Rpc.RpcOk[A]]
      case other                  => Left(other.toString)

  private final case class Asked(targets: List[Rpc.TargetId]) derives JsonEncoder
end BspSuiteLawSpec
