package histos.js

import histos.pack.MainClass
import histos.process.{ChildClasspath, ChildProcess, ChildRequest}
import histos.zinc.{CompileError, CompileRequest, CompileToolchain, JvmCompile, ResolvedJdk, ScalacFlags}
import histos.ScalaVersion

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Link `.sjsir` in a child JVM. The engine does not load `org.scalajs.linker`. */
object JsLink:
  private val BridgeScala: ScalaVersion = ScalaVersion.trusted("2.13.18")
  private val Linker: String = "1.22.0"
  private val BridgeMain: String = "subprocess.JsBridge"

  def link(
      version: ScalaVersion,
      main: MainClass,
      classpath: List[Path],
      work: Path,
      jdk: ResolvedJdk,
      kind: histos.JsModuleKind = histos.JsModuleKind.NoModule,
  ): IO[JsLinkError, Path] =
    val output = work.resolve("out")
    val bridgeRoot = work.resolve("bridge")
    val classes = bridgeRoot.resolve("classes")
    val source = bridgeRoot.resolve("src").resolve("subprocess").resolve("JsBridge.scala")
    val listed = work.resolve("classpath.txt")
    val child = work.resolve("child")
    for
      libraries <- ScalaJs.libraries(version).mapError(JsLinkError.Library(_))
      linker <- ZIO.attemptBlockingInterrupt(fetchLinker()).mapError(_ => JsLinkError.Unavailable).flatMap(ZIO.fromEither)
      _ <- write(listed, (classpath ++ libraries).map(_.toAbsolutePath.toString).mkString("\n"))
      _ <- write(source, bridge)
      _ <- JvmCompile.compile(CompileRequest(
             CompileToolchain(BridgeScala, histos.Platform.Jvm, jdk, ScalacFlags.empty),
             List(source),
             linker,
             classes,
             bridgeRoot.resolve("analysis"),
             Nil,
           )).mapError(JsLinkError.Library(_))
      _ <- empty(output)
      _ <- empty(child)
      ran <- ChildProcess.run(ChildRequest(
               histos.Platform.Jvm,
               jdk.home.resolve("bin").resolve("java"),
               ChildClasspath(classes, linker),
               MainClass.trusted(BridgeMain),
               child,
               Map(
                 "JS_OUT" -> output.toAbsolutePath.toString,
                 "JS_MAIN" -> main.binary,
                 "JS_CLASSPATH" -> listed.toAbsolutePath.toString,
                 "JS_MODULE" -> kind.token,
               ),
             )).mapError(error => JsLinkError.Linker(error.message))
      linked <- ran.exit.value match
        case 0 => readJs(child.resolve("js.path"))
        case _ => ZIO.fail(JsLinkError.Linker(shown(ran.out.text, ran.err.text)))
    yield linked

  private def bridge: String =
    """package subprocess
      |
      |import java.nio.file.{Files, Path}
      |
      |import scala.jdk.CollectionConverters._
      |
      |import scala.concurrent.Await
      |import scala.concurrent.duration.Duration
      |import scala.concurrent.ExecutionContext.Implicits.global
      |
      |import org.scalajs.linker.PathIRContainer
      |import org.scalajs.linker.PathOutputDirectory
      |import org.scalajs.linker.StandardImpl
      |import org.scalajs.linker.interface.ModuleInitializer
      |import org.scalajs.linker.interface.ModuleKind
      |import org.scalajs.linker.interface.StandardConfig
      |import org.scalajs.logging.Level
      |import org.scalajs.logging.Logger
      |
      |object JsBridge {
      |  def main(args: Array[String]): Unit = {
      |    val output = Path.of(sys.env("JS_OUT"))
      |    val mainClass = sys.env("JS_MAIN")
      |    val classpath = Files.readAllLines(Path.of(sys.env("JS_CLASSPATH"))).asScala.map(Path.of(_)).toSeq
      |    Files.createDirectories(output)
      |    val base = StandardConfig().withOptimizer(false).withSourceMap(false).withBatchMode(true)
      |    val kind = sys.env.getOrElse("JS_MODULE", "nomodule")
      |    val config = kind match {
      |      case "esmodule" => base.withModuleKind(ModuleKind.ESModule)
      |      case "commonjs" => base.withModuleKind(ModuleKind.CommonJSModule)
      |      case _          => base
      |    }
      |    val linker = StandardImpl.linker(config)
      |    val containers = Await.result(PathIRContainer.fromClasspath(classpath), Duration.Inf)._1
      |    val ir = Await.result(StandardImpl.irFileCache().newCache.cached(containers), Duration.Inf)
      |    val init = Seq(ModuleInitializer.mainMethodWithArgs(mainClass, "main"))
      |    val report = Await.result(linker.link(ir, init, PathOutputDirectory(output), logger), Duration.Inf)
      |    report.publicModules.headOption.map(_.jsFileName) match {
      |      case Some(name) =>
      |        Files.writeString(Path.of("js.path"), output.resolve(name).toAbsolutePath.toString)
      |      case None =>
      |        System.err.println("the linker wrote no module")
      |        System.exit(1)
      |    }
      |  }
      |
      |  private val logger = new Logger {
      |    def log(level: Level, message: => String): Unit =
      |      if (level == Level.Error) System.err.println(message)
      |    def trace(thrown: => Throwable): Unit = thrown.printStackTrace(System.err)
      |  }
      |}
      |""".stripMargin

  private def fetchLinker(): Either[JsLinkError, List[Path]] =
    val dependency = coursier.Dependency(
      coursier.Module(coursier.Organization("org.scala-js"), coursier.ModuleName("scalajs-linker_2.13")),
      coursier.version.VersionConstraint(Linker),
    )
    val got = coursier.Fetch(coursier.cache.FileCache())(using coursier.util.Task.sync)
      .noMirrors
      .withGradleModuleSupport(false)
      .withRepositories(Seq(coursier.Repositories.central))
      .addDependencies(dependency)
      .eitherResult()
    got match
      case Left(_) => Left(JsLinkError.Unavailable)
      case Right(result) =>
        val jars = result.detailedArtifacts0.toList.flatMap: (_, publication, _, file) =>
          val classified = publication match
            case Right(published) => published.classifier.nonEmpty
            case Left(variant)    => variant.classifier.nonEmpty
          if file.getName.endsWith(".jar") && !classified then List(file.toPath) else Nil
        if jars.isEmpty then Left(JsLinkError.Unavailable) else Right(jars)

  private def readJs(path: Path): IO[JsLinkError, Path] =
    ZIO.attemptBlocking {
      if Files.isRegularFile(path) then Right(Path.of(Files.readString(path).trim))
      else Left(JsLinkError.Linker("the js bridge did not write a script"))
    }.mapError(_ => JsLinkError.Linker(s"could not read $path")).flatMap(ZIO.fromEither)

  private def write(path: Path, text: String): IO[JsLinkError, Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, text, StandardCharsets.UTF_8)
      ()
    }.mapError(_ => JsLinkError.Linker(s"could not write $path"))

  private def empty(path: Path): IO[JsLinkError, Unit] =
    ZIO.attemptBlocking {
      if Files.exists(path) then
        val walk = Files.walk(path)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(file => Files.deleteIfExists(file))
      Files.createDirectories(path)
      ()
    }.mapError(_ => JsLinkError.Linker(s"could not prepare $path"))

  private def shown(out: String, err: String): String =
    val text = List(out, err).filter(_.nonEmpty).mkString("\n")
    if text.isEmpty then "the js linker failed" else text

enum JsLinkError:
  case Unavailable
  case Library(error: CompileError)
  case Linker(text: String)

  def message: String = this match
    case Unavailable  => "the Scala.js linker could not be fetched"
    case Library(error) => error.message
    case Linker(text) => text
