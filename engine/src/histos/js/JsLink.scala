package histos.js

import histos.jdk.{JdkRuntime, Sha256}
import histos.pack.MainClass
import histos.process.{ChildClasspath, ChildProcess, ChildRequest}
import histos.workspace.Workspace
import histos.zinc.{
  CompileError,
  CompileRequest,
  CompileToolchain,
  GraphCompile,
  JvmCompile,
  ProductDigest,

  ScalacFlags,
}
import histos.ScalaVersion

import histos.resolve.{LockedTools, ResolveError}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** One link of `.sjsir` into a script. The engine does not load `org.scalajs.linker`.
  */
final case class JsLinkRequest(
    version: ScalaVersion,
    main: MainClass,
    classpath: List[Path],
    work: Path,
    jdk: JdkRuntime,
    kind: histos.JsModuleKind = histos.JsModuleKind.NoModule,
    revision: Option[histos.JsVersion] = None,
)

/** Link `.sjsir` in a child JVM. The engine does not load `org.scalajs.linker`.
  */
final case class JsLinked(script: Path, key: Sha256)

object JsLink:
  private val BridgeMain: String = "subprocess.JsBridge"

  def link(request: JsLinkRequest): ZIO[LockedTools, JsLinkError, JsLinked] =
    val JsLinkRequest(version, main, classpath, work, jdk, kind, revision) =
      request
    val output     = work.resolve("out")
    val bridgeRoot = work.resolve("bridge")
    val classes    = bridgeRoot.resolve("classes")
    val source     =
      bridgeRoot.resolve("src").resolve("subprocess").resolve("JsBridge.scala")
    val listed = work.resolve("classpath.txt")
    val child  = work.resolve("child")
    for
      view <- ZIO.service[LockedTools]
      selected = revision.getOrElse(view.catalog.js.revision)
      digests   <- productsOf(classpath)
      libraries <- ScalaJs
        .libraries(version, Some(selected))
        .mapError(JsLinkError.Library(_))
      linker <- view
        .materialize(
          histos.js.JsRequests
            .linker(selected, view.catalog.js.bridgeScala, view.repositories)
        )
        .mapError(JsLinkError.Resolved(_))
        .map(_.artifacts.map(_.path))
      _ <- write(
        listed,
        (classpath ++ libraries).map(_.toAbsolutePath.toString).mkString("\n"),
      )
      _ <- write(source, bridge)
      _ <- JvmCompile
        .compile(
          CompileRequest(
            CompileToolchain(
              view.catalog.js.bridgeScala,
              histos.Platform.Jvm,
              jdk,
              ScalacFlags.empty,
            ),
            List(source),
            linker,
            classes,
            bridgeRoot.resolve("analysis"),
            Nil,
            Nil,
          )
        )
        .mapError(JsLinkError.Library(_))
      _   <- empty(output)
      _   <- empty(child)
      ran <- ChildProcess
        .run(
          ChildRequest(
            histos.Platform.Jvm,
            jdk.home.resolve("bin").resolve("java"),
            ChildClasspath(classes, linker),
            MainClass.trusted(BridgeMain),
            child,
            Map(
              "JS_OUT"       -> output.toAbsolutePath.toString,
              "JS_MAIN"      -> main.binary,
              "JS_CLASSPATH" -> listed.toAbsolutePath.toString,
              "JS_MODULE"    -> kind.token,
            ),
          )
        )
        .mapError(error => JsLinkError.Linker(error.message))
      script <- ran.exit.value match
        case 0 => readJs(child.resolve("js.path"))
        case _ =>
          ZIO.fail(JsLinkError.Linker(shown(ran.out.text, ran.err.text)))
    yield JsLinked(script, JsKey.of(version, selected, kind, main, digests))

  /** The key [[link]] stamps: product digests, the selected linker, the module kind, and the main. */
  private[histos] def keyOf(request: JsLinkRequest): ZIO[LockedTools, JsLinkError, Sha256] =
    val JsLinkRequest(version, main, classpath, _, _, kind, revision) = request
    for
      view    <- ZIO.service[LockedTools]
      digests <- productsOf(classpath)
    yield JsKey.of(version, revision.getOrElse(view.catalog.js.revision), kind, main, digests)

  private def productsOf(classpath: List[Path]): IO[JsLinkError, List[Sha256]] =
    ZIO.foreach(classpath.flatMap(path => GraphCompile.analysisDirOf(path).toList)): dir =>
      ZIO.fromEither(ProductDigest.read(dir)).mapError(JsLinkError.Product(_))

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

  private def readJs(path: Path): IO[JsLinkError, Path] =
    ZIO
      .attemptBlocking {
        if Files.isRegularFile(path) then Right(Path.of(Files.readString(path).trim))
        else Left(JsLinkError.Linker("the js bridge did not write a script"))
      }
      .mapError(_ => JsLinkError.Linker(s"could not read $path"))
      .flatMap(ZIO.fromEither)

  private def write(path: Path, text: String): IO[JsLinkError, Unit] =
    Workspace
      .writeString(path, text)
      .mapError(_ => JsLinkError.Linker(s"could not write $path"))

  private def empty(path: Path): IO[JsLinkError, Unit] =
    Workspace
      .recreate(path)
      .mapError(_ => JsLinkError.Linker(s"could not prepare $path"))
      .unit

  private def shown(out: String, err: String): String =
    val text = List(out, err).filter(_.nonEmpty).mkString("\n")
    if text.isEmpty then "the js linker failed" else text
end JsLink

enum JsLinkError:
  case Resolved(error: ResolveError)
  case Unavailable
  case Library(error: CompileError)
  case Product(error: CompileError)
  case Linker(text: String)

  def message: String = this match
    case Resolved(error) => error.message
    case Unavailable     => "the Scala.js linker could not be fetched"
    case Library(error)  => error.message
    case Product(error)  => error.message
    case Linker(text)    => text
end JsLinkError
