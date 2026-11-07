package histos.bsp

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.publish.LauncherPin
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.testrun.TestCompile
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers, GraphCompile}
import histos.{
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
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}
import zio.{Chunk, Ref, Scope, ZIO, ZLayer}

/** A test target names its main, its classpath, and its work directory. A main target is not a test. */
object BspSuiteLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("suitelaw")

  def spec = suite("bsp suite laws")(
    test("a test main is the suite, and a main target is refused"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace.work("histos-bsp-suite").flatMap: dir =>
          val main = dir.resolve(lawName.value).resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
          val probe = dir.resolve(lawName.value).resolve("src").resolve("test").resolve("scala").resolve("Probe.scala")
          val pin = dir.resolve(LauncherPin.file)
          val mainUri = Bsp.mainUri(dir, lawName.value, scala3, Platform.Jvm)
          val testUri = Bsp.testUri(dir, lawName.value, scala3, Platform.Jvm)
          val buildUri = Bsp.buildUri(dir)
          val classes = TestCompile.testClasses(dir, lawName, scala3, Platform.Jvm)
          val classFile = classes.resolve("Probe.class")
          val mainClass =
            GraphCompile.classesDir(dir, lawName, scala3, Platform.Jvm).resolve("Hello.class")
          val work = TestCompile.testWork(dir, lawName, scala3, Platform.Jvm).toAbsolutePath.normalize.toString
          val classesMark = s"/test-classes/${lawName.value}/"
          val program =
            for
              loaded <- Machine.load(dir).either
              machine <- loaded match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              found <- GraphCompile.jdk(machine).either
              jdk <- found match
                case Left(error)  => ZIO.dieMessage(error.message)
                case Right(value) => ZIO.succeed(value)
              _ <- Workspace.createDirectories(parentOf(main))
              _ <- Workspace.createDirectories(parentOf(probe))
              _ <- Workspace.writeString(main, "object Hello:\n  def ready: Int = 1\n")
              _ <- Workspace.writeString(probe, "object Probe:\n  def main(args: Array[String]): Unit = ()\n")
              _ <- Workspace.writeString(pin, "histos.version=9.8.7\n")
              notes <- Ref.make(Chunk.empty[String])
              runtimes <- Runtimes.read(machine, None)
              session <- Bsp.open(dir, graph, jdk, runtimes, notify = line => notes.update(_ :+ line))
              _ <- ask(session, request(1, "build/initialize"))
              _ <- ask(session, note("build/initialized"))
              listed <- ask(session, request(2, "workspace/buildTargets"))
              sourced <- ask(session, asked(3, "buildTarget/sources", List(testUri)))
              opted <- ask(session, asked(4, "buildTarget/scalacOptions", List(testUri)))
              jars <- ask(session, asked(5, "buildTarget/dependencySources", List(mainUri)))
              mainCompiled <- ask(session, asked(6, "buildTarget/compile", List(mainUri)))
              compiled <- ask(session, asked(11, "buildTarget/compile", List(testUri)))
              wrote <- ZIO.attempt(Files.isRegularFile(classFile)).orDie
              mainWrote <- ZIO.attempt(Files.isRegularFile(mainClass)).orDie
              suites <- ask(session, asked(7, "buildTarget/scalaTestClasses", List(testUri, mainUri)))
              room <- ask(session, asked(8, "buildTarget/jvmTestEnvironment", List(testUri)))
              refused <- ask(session, asked(9, "buildTarget/jvmTestEnvironment", List(mainUri)))
              buildRefused <- ask(session, asked(10, "buildTarget/jvmTestEnvironment", List(buildUri)))
              heard <- notes.get
            yield
              val targets = rpcOk[Rpc.BuildTargetsResult](listed).map(_.result.targets)
              val files = rpcOk[Rpc.SourcesResult](sourced).map(_.result.items.flatMap(_.sources))
              val options = rpcOk[Rpc.ScalacOptionsResult](opted).toOption.flatMap(_.result.items.find(_.target.uri == testUri))
              val sources = rpcOk[Rpc.DependencySourcesResult](jars).toOption.flatMap(_.result.items.find(_.target.uri == mainUri))
              val mainStatus = rpcOk[Rpc.CompileResult](mainCompiled).map(_.result.statusCode)
              val status = rpcOk[Rpc.CompileResult](compiled).map(_.result.statusCode)
              val classesNamed = rpcOk[Rpc.ScalaTestClassesResult](suites).map(_.result.items)
              val suite = classesNamed.toOption.flatMap(_.find(_.target.uri == testUri))
              val mainClasses = classesNamed.toOption.flatMap(_.find(_.target.uri == mainUri))
              val ran = rpcOk[Rpc.JvmEnvironmentResult](room).toOption.flatMap(_.result.items.find(_.target.uri == testUri))
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
                sources.exists(item => item.sources.forall(_.startsWith("file:"))),
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
          program.provideSomeLayer[Resolve & Scope](sessionLayer(dir, lock))
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def graph: ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      test = Some(TestMain("Probe")),
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )

  private def sessionLayer(dir: Path, lock: DependencyLock) =
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      Compilers.live(1) ++
      CompileMemo.live ++
      ResolutionService.pinned(pinned(lock))

  private def pinned(lock: DependencyLock): DependencyLock =
    lock.copy(cells =
      LockedCell(lawName, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile) :: lock.cells
    )

  private def opened =
    Resolve.offline(_root_.coursier.cache.CacheDefaults.location.toPath) ++
      ZLayer.fromZIO(Resolve.readLock(repository))

  private def repository: Path =
    def walk(path: Path, left: Int): Option[Path] =
      if left == 0 then None
      else if Files.isRegularFile(path.resolve("dependencies.lock")) && Files.isRegularFile(
          path.resolve("histos.scala")
        )
      then Some(path)
      else
        Option(path.getParent) match
          case Some(parent) => walk(parent, left - 1)
          case None         => None
    walk(Path.of("").toAbsolutePath.normalize, 8).getOrElse(Path.of("").toAbsolutePath.normalize)

  private def parentOf(path: Path): Path =
    Option(path.getParent).getOrElse(path)

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
      case _                       => None

  private def rpcOk[A: JsonCodec](answer: Bsp.Answer): Either[String, Rpc.RpcOk[A]] =
    answer match
      case Bsp.Answer.Reply(line) => line.fromJson[Rpc.RpcOk[A]]
      case other                  => Left(other.toString)

  private final case class Asked(targets: List[Rpc.TargetId]) derives JsonEncoder
end BspSuiteLawSpec
