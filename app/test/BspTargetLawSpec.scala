package histos.bsp

import histos.cache.ContentStore
import histos.jdk.{JdkMajor, JdkRuntime, JdkVersion, Machine, Sha256}
import histos.publish.LauncherPin
import histos.resolve.{ClasspathScope, DependencyLock, LockedCell, ResolutionService, Resolve}
import histos.program.Program
import histos.server.Calls
import histos.toolchain.{RuntimeIdentity, Runtimes}
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
}
import java.nio.file.{Files, Path}

import zio.json.*
import zio.test.{TestAspect, TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}
import zio.{Scope, ZIO, ZLayer}

/** A build target names the module and the build file, and the sources are the files on disk. */
object BspTargetLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("bsplaw")
  private val jdk     = JdkRuntime(
    Path.of("jdk"),
    JdkVersion.trusted("21", JdkMajor.trusted(21)),
    RuntimeIdentity.Local(Path.of("jdk"), Sha256.trusted("ab")),
  )

  def spec = suite("bsp target laws")(
    test("a module and the build file are targets, and each source file is named"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace
          .work("histos-bsp-target")
          .flatMap: dir =>
            val source =
              dir.resolve(lawName.value).resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val build    = dir.resolve("histos.scala")
            val pin      = dir.resolve(LauncherPin.file)
            val mainUri  = Bsp.mainUri(dir, lawName.value, scala3, Platform.Jvm)
            val buildUri = Bsp.buildUri(dir)
            val classes  =
              GraphCompile.classesDir(dir, lawName, scala3, Platform.Jvm).toAbsolutePath.normalize.toUri.toString
            val program =
              for
                _        <- Workspace.createDirectories(parentOf(source))
                _        <- Workspace.writeString(source, "object Hello\n")
                _        <- Workspace.writeString(build, "// law\n")
                _        <- Workspace.writeString(pin, "histos.version=9.8.7\n")
                runtimes <- Runtimes.read(Calls.idleMachine, None)
                session  <- Bsp.open(dir, graph, jdk, runtimes)
                _        <- ask(session, request(1, "build/initialize"))
                _        <- ask(session, note("build/initialized"))
                listed   <- ask(session, request(2, "workspace/buildTargets"))
                sourced  <- ask(session, asked(3, "buildTarget/sources", List(mainUri, buildUri)))
                opted    <- ask(session, asked(4, "buildTarget/scalacOptions", List(mainUri, buildUri)))
                unknown  <- ask(session, asked(5, "buildTarget/sources", List("file:///nope")))
                missing  <- ask(session, request(6, "buildTarget/sources"))
              yield
                val targets  = rpcOk[Rpc.BuildTargetsResult](listed).map(_.result.targets)
                val files    = rpcOk[Rpc.SourcesResult](sourced).map(_.result.items.flatMap(_.sources))
                val options  = rpcOk[Rpc.ScalacOptionsResult](opted).map(_.result.items)
                val module   = targets.toOption.flatMap(_.find(_.displayName == lawName.value))
                val file     = targets.toOption.flatMap(_.find(_.displayName == "histos.scala"))
                val mainOpt  = options.toOption.flatMap(_.find(_.target.uri == mainUri))
                val buildOpt = options.toOption.flatMap(_.find(_.target.uri == buildUri))
                assertTrue(
                  targets.map(_.map(_.displayName).sorted) == Right(List(lawName.value, "histos.scala")),
                  module.exists(lawTarget(mainUri)),
                  file.exists(buildTarget(buildUri)),
                  files.exists(rows =>
                    rows.exists(item => item.uri.endsWith("Hello.scala") && item.kind == 1 && !item.generated) &&
                      rows.exists(item => item.uri.endsWith("histos.scala") && item.kind == 1 && !item.generated)
                  ),
                  mainOpt.exists(item =>
                    item.classDirectory == classes &&
                      item.options.contains("-Xsemanticdb") &&
                      item.classpath.nonEmpty
                  ),
                  buildOpt
                    .exists(item => item.options.contains("-Xsemanticdb") && item.options.contains("-sourceroot")),
                  rpcError(unknown).exists(_.error.message == "unknown build target"),
                  rpcError(missing).exists(_.error.message == "bsp params are required"),
                )
            program.provideSomeLayer[Resolve & Scope](sessionLayer(dir, lock))
    ,
    test("compiling the module writes its class, and a type error is not success"):
      ZIO.serviceWithZIO[DependencyLock]: lock =>
        Workspace
          .work("histos-bsp-compile")
          .flatMap: dir =>
            val source =
              dir.resolve(lawName.value).resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val pin       = dir.resolve(LauncherPin.file)
            val uri       = Bsp.mainUri(dir, lawName.value, scala3, Platform.Jvm)
            val buildUri  = Bsp.buildUri(dir)
            val classFile =
              GraphCompile.classesDir(dir, lawName, scala3, Platform.Jvm).resolve("Hello.class")
            val work =
              Program.workDir(dir, lawName, scala3, Platform.Jvm).toAbsolutePath.normalize.toString
            val program =
              for
                loaded  <- Machine.load(dir).either
                machine <- loaded match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
                found <- GraphCompile.jdk(machine).either
                jdk   <- found match
                  case Left(error)  => ZIO.dieMessage(error.message)
                  case Right(value) => ZIO.succeed(value)
                _ <- Workspace.createDirectories(parentOf(source))
                _ <- Workspace.writeString(
                  source,
                  "object Hello:\n  def main(args: Array[String]): Unit = ()\n",
                )
                _        <- Workspace.writeString(pin, "histos.version=9.8.7\n")
                runtimes <- Runtimes.read(machine, None)
                session  <- Bsp.open(dir, graph, jdk, runtimes)
                _        <- ask(session, request(1, "build/initialize"))
                _        <- ask(session, note("build/initialized"))
                compiled <- ask(session, asked(2, "buildTarget/compile", List(uri)))
                wrote    <- ZIO.attempt(Files.isRegularFile(classFile)).orDie
                mains    <- ask(session, asked(4, "buildTarget/scalaMainClasses", List(uri)))
                room     <- ask(session, asked(5, "buildTarget/jvmRunEnvironment", List(uri)))
                refused  <- ask(session, asked(6, "buildTarget/jvmRunEnvironment", List(buildUri)))
                _        <- Workspace.writeString(source, "object Hello:\n  def ready: Int = \"no\"\n")
                broken   <- ask(session, asked(3, "buildTarget/compile", List(uri)))
              yield
                val ok      = rpcOk[Rpc.CompileResult](compiled).map(_.result.statusCode)
                val bad     = rpcOk[Rpc.CompileResult](broken).map(_.result.statusCode)
                val classes =
                  rpcOk[Rpc.ScalaMainClassesResult](mains).map(_.result.items.flatMap(_.classes.map(_.className)))
                val ran =
                  rpcOk[Rpc.JvmEnvironmentResult](room).toOption.flatMap(_.result.items.find(_.target.uri == uri))
                assertTrue(
                  ok == Right(1),
                  wrote,
                  bad == Right(2),
                  classes == Right(List("Hello")),
                  ran.exists(item =>
                    item.classpath.nonEmpty && item.workingDirectory == work &&
                      item.environmentVariables.get("HISTOS_REPO").contains(dir.toAbsolutePath.normalize.toString)
                  ),
                  rpcError(refused).exists(_.error.message == "jvmRunEnvironment needs a main target"),
                )
            program.provideSomeLayer[Resolve & Scope](sessionLayer(dir, lock)),
  ).provideSomeLayerShared[TestEnvironment & Scope](opened) @@ TestAspect.withLiveSystem

  private def lawTarget(uri: String)(item: Rpc.BuildTarget): Boolean =
    item.id.uri == uri &&
      item.tags == List("library") &&
      item.languageIds == List("scala") &&
      item.dependencies.isEmpty &&
      item.capabilities.canCompile &&
      !item.capabilities.canTest &&
      item.capabilities.canRun &&
      item.data.scalaOrganization == "org.scala-lang" &&
      item.data.scalaVersion == scala3.value &&
      item.data.scalaBinaryVersion == "3" &&
      item.data.platform == 1 &&
      item.data.jars.nonEmpty &&
      item.data.jars.forall(_.startsWith("file:"))

  private def buildTarget(uri: String)(item: Rpc.BuildTarget): Boolean =
    item.id.uri == uri &&
      item.tags == List("library") &&
      item.dependencies.isEmpty &&
      item.capabilities.canCompile &&
      !item.capabilities.canTest &&
      !item.capabilities.canRun &&
      item.data.platform == 1 &&
      item.data.jars.nonEmpty

  private def graph: ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
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
    lock.copy(cells = LockedCell(lawName, scala3, Platform.Jvm, Nil, Nil, Nil, ClasspathScope.Compile) :: lock.cells)

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
      case _                      => None

  private def rpcOk[A: JsonCodec](answer: Bsp.Answer): Either[String, Rpc.RpcOk[A]] =
    answer match
      case Bsp.Answer.Reply(line) => line.fromJson[Rpc.RpcOk[A]]
      case other                  => Left(other.toString)

  private final case class Asked(targets: List[Rpc.TargetId]) derives JsonEncoder
end BspTargetLawSpec
