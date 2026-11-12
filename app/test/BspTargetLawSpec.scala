package histos.bsp

import histos.cache.ContentStore
import histos.load.BuildNotes
import histos.publish.LauncherPin
import histos.resolve.{ResolutionService, Resolve}
import histos.program.Program
import histos.toolchain.Runtimes
import histos.version.Catalog
import histos.workspace.Workspace
import histos.zinc.{CompileMemo, Compilers, GraphCompile}
import histos.{
  CompilerClosure,
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
}
import java.nio.file.{Files, Path}

import zio.json.*
import zio.test.{TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}
import zio.{Scope, ZIO, ZLayer}

/** A build target names the module and the build file, and the sources are the files on disk. */
object BspTargetLawSpec extends ZIOSpecDefault:
  private val lawName = ProjectName.trusted("bsplaw")
  private val quiet   = ZIO.succeed(BuildNotes(false, Nil))

  def spec = suite("bsp target laws")(
    test("a module and the build file are targets, and each source file is named"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-bsp-target")
          .flatMap: dir =>
            val source =
              dir.resolve(lawName.value).resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val mainUri  = Bsp.mainUri(dir, lawName.value, scala3, Platform.Jvm)
            val buildUri = Bsp.buildUri(dir)
            val classes  =
              GraphCompile.classesDir(dir, lawName, scala3, Platform.Jvm).toAbsolutePath.normalize.toUri.toString
            val program =
              for
                _        <- Workspace.writeString(source, "object Hello\n")
                _        <- Workspace.writeString(dir.resolve("histos.scala"), "// law\n")
                _        <- Workspace.writeString(dir.resolve(LauncherPin.file), "histos.version=9.8.7\n")
                runtimes <- Runtimes.read(LawMachine.idle, None)
                session  <- Bsp.open(
                  dir,
                  graph(scala3),
                  LawMachine.fixtureJdk,
                  runtimes,
                  Nil,
                  _ => ZIO.unit,
                  quiet,
                  compile => compile,
                  None,
                )
                _       <- ask(session, request(1, "build/initialize"))
                _       <- ask(session, note("build/initialized"))
                listed  <- ask(session, request(2, "workspace/buildTargets"))
                sourced <- ask(session, asked(3, "buildTarget/sources", List(mainUri, buildUri)))
                opted   <- ask(session, asked(4, "buildTarget/scalacOptions", List(mainUri, buildUri)))
                unknown <- ask(session, asked(5, "buildTarget/sources", List("file:///nope")))
                missing <- ask(session, request(6, "buildTarget/sources"))
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
                  module.exists(lawTarget(mainUri, scala3)),
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
            program.provideSomeLayer[Resolve & Scope](sessionLayer(dir, closure))
    ,
    test("compiling the module writes its class, and a type error is not success"):
      ZIO.serviceWithZIO[CompilerClosure]: closure =>
        val scala3 = closure.scalaVersion
        Workspace
          .work("histos-bsp-compile")
          .flatMap: dir =>
            val source =
              dir.resolve(lawName.value).resolve("src").resolve("main").resolve("scala").resolve("Hello.scala")
            val uri       = Bsp.mainUri(dir, lawName.value, scala3, Platform.Jvm)
            val buildUri  = Bsp.buildUri(dir)
            val classFile =
              GraphCompile.classesDir(dir, lawName, scala3, Platform.Jvm).resolve("Hello.class")
            val work =
              Program.workDir(dir, lawName, scala3, Platform.Jvm).toAbsolutePath.normalize.toString
            val program =
              for
                machine <- LawMachine.at(dir)
                jdk     <- LawMachine.jdk(machine)
                _       <- Workspace.writeString(
                  source,
                  "object Hello:\n  def main(args: Array[String]): Unit = ()\n",
                )
                _        <- Workspace.writeString(dir.resolve(LauncherPin.file), "histos.version=9.8.7\n")
                runtimes <- Runtimes.read(machine, None)
                session  <- Bsp
                  .open(dir, graph(scala3), jdk, runtimes, Nil, _ => ZIO.unit, quiet, compile => compile, None)
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
            program.provideSomeLayer[Resolve & Scope](sessionLayer(dir, closure)),
  ).provideSomeLayerShared[TestEnvironment & Scope](CompilerClosure.layer)

  private def lawTarget(uri: String, scala3: ScalaVersion)(item: Rpc.BuildTarget): Boolean =
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

  private def graph(scala3: ScalaVersion): ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
    )
    ProjectGraph.trusted(List(law), Splits.aggregate, Catalog.empty, None, Nil, resolution = Resolution.Lock)

  private def sessionLayer(dir: Path, closure: CompilerClosure) =
    ZLayer.succeed(ContentStore.in(dir.resolve("store"))) ++
      Compilers.live(1) ++
      CompileMemo.live ++
      ResolutionService.pinned(closure.pinning(lawName))

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
