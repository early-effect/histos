package histos.js

import histos.node.NodeBinary
import histos.pack.MainClass
import histos.process.{ChildClasspath, ChildRequest}
import histos.zinc.{CompileError, CompileRequest, CompileToolchain, JvmCompile, ResolvedJdk, ScalacFlags}
import histos.{JsEnv, JsModuleKind, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Start a linked script in the module's JavaScript environment. The engine does not load a `JSEnv`. */
object JsRun:
  private val BridgeScala: ScalaVersion = ScalaVersion.trusted("2.13.18")
  private val FirefoxScala: ScalaVersion = ScalaVersion.trusted("3.9.0")
  private val SuiteMain: String = "subprocess.JsRun"

  def start(
      env: JsEnv,
      kind: JsModuleKind,
      script: Path,
      work: Path,
      jdk: ResolvedJdk,
      processEnv: Map[String, String],
      node: NodeBinary,
  ): IO[JsRunError, ChildRequest] =
    val bridgeRoot = work.resolve("js-run")
    val classes = bridgeRoot.resolve("classes")
    val source = bridgeRoot.resolve("src").resolve("subprocess").resolve("JsRun.scala")
    val child = work.resolve("js-run-child")
    for
      tools <- ZIO.attemptBlockingInterrupt(fetch(env)).mapError(_ => JsRunError.Unavailable).flatMap(ZIO.fromEither)
      _ <- write(source, runner(env))
      _ <- JvmCompile.compile(CompileRequest(
             CompileToolchain(compiler(env), histos.Platform.Jvm, jdk, ScalacFlags.empty),
             List(source),
             tools,
             classes,
             bridgeRoot.resolve("analysis"),
             Nil,
           )).mapError(JsRunError.Compile(_))
      _ <- empty(child)
    yield ChildRequest(
      histos.Platform.Jvm,
      jdk.home.resolve("bin").resolve("java"),
      ChildClasspath(classes, tools),
      MainClass.trusted(SuiteMain),
      child,
      processEnv ++ nodeEnv(env, node) ++ Map(
        "JS_SCRIPT" -> script.toAbsolutePath.toString,
        "JS_MODULE" -> kind.token,
      ),
    )

  /** The child hands the chosen `node` to its `JSEnv`. Chekhov's Playwright driver reads `PLAYWRIGHT_NODEJS_PATH`. */
  private[js] def nodeEnv(env: JsEnv, node: NodeBinary): Map[String, String] = env match
    case JsEnv.Node | JsEnv.Jsdom => Map("JS_NODE" -> node.path.toString)
    case JsEnv.Firefox            => Map("PLAYWRIGHT_NODEJS_PATH" -> node.path.toString)

  /** The expression that builds the `JSEnv` on the `node` in `JS_NODE`. */
  private[js] def envExpr(env: JsEnv): String = env match
    case JsEnv.Node =>
      """new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withExecutable(sys.env("JS_NODE")))"""
    case JsEnv.Jsdom =>
      """new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv.Config().withExecutable(sys.env("JS_NODE")))"""
    case JsEnv.Firefox =>
      "new chekhov.jsenv.ChekhovJSEnv(chekhov.ChekhovBrowser.Firefox, true, false, scala.collection.immutable.Map.empty[String, String])"

  private def compiler(env: JsEnv): ScalaVersion = env match
    case JsEnv.Firefox        => FirefoxScala
    case JsEnv.Node | JsEnv.Jsdom => BridgeScala

  private def fetch(env: JsEnv): Either[JsRunError, List[Path]] =
    val wanted = env match
      case JsEnv.Node => List(Artifact("org.scala-js", "scalajs-env-nodejs_2.13", "1.6.0"))
      case JsEnv.Jsdom => List(Artifact("org.scala-js", "scalajs-env-jsdom-nodejs_2.13", "1.1.1"))
      case JsEnv.Firefox => List(Artifact("rocks.earlyeffect", "chekhov-jsenv_3", "0.1.3"))
    val dependencies = wanted.map: item =>
      coursier.Dependency(
        coursier.Module(coursier.Organization(item.organization), coursier.ModuleName(item.name)),
        coursier.version.VersionConstraint(item.revision),
      )
    val got = coursier.Fetch(coursier.cache.FileCache())(using coursier.util.Task.sync)
      .noMirrors
      .withGradleModuleSupport(false)
      .withRepositories(Seq(coursier.Repositories.central))
      .addDependencies(dependencies*)
      .eitherResult()
    got match
      case Left(_) => Left(JsRunError.Unavailable)
      case Right(result) =>
        val jars = result.detailedArtifacts0.toList.flatMap: (_, publication, _, file) =>
          val classified = publication match
            case Right(published) => published.classifier.nonEmpty
            case Left(variant)    => variant.classifier.nonEmpty
          if file.getName.endsWith(".jar") && !classified then List(file.toPath) else Nil
        if jars.isEmpty then Left(JsRunError.Unavailable) else Right(jars)

  private def runner(env: JsEnv): String =
    """package subprocess
      |
      |import java.nio.file.Path
      |
      |import scala.concurrent.Await
      |import scala.concurrent.duration.Duration
      |import scala.util.control.NonFatal
      |
      |import org.scalajs.jsenv.JSEnv
      |import org.scalajs.jsenv.RunConfig
      |
      |object JsRun {
      |  def main(args: Array[String]): Unit = {
      |    val code = try { body() } catch { case NonFatal(thrown) =>
      |      thrown.printStackTrace(System.err)
      |      1
      |    }
      |    if (code != 0) System.exit(code)
      |  }
      |
      |  private def body(): Int = {
      |    val script = Path.of(sys.env("JS_SCRIPT"))
      |    val kind = sys.env.getOrElse("JS_MODULE", "nomodule")
      |    val jsEnv: JSEnv = __ENV__
      |    val input = kind match {
      |      case "esmodule" => org.scalajs.jsenv.Input.ESModule(script)
      |      case "commonjs" => org.scalajs.jsenv.Input.CommonJSModule(script)
      |      case _          => org.scalajs.jsenv.Input.Script(script)
      |    }
      |    val config = RunConfig().withInheritOut(true).withInheritErr(true)
      |    val started = jsEnv.start(Seq(input), config)
      |    try {
      |      Await.result(started.future, Duration.Inf)
      |      0
      |    } finally started.close()
      |  }
      |}
      |""".stripMargin.replace("__ENV__", envExpr(env))

  private final case class Artifact(organization: String, name: String, revision: String)

  private def write(path: Path, text: String): IO[JsRunError, Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, text, StandardCharsets.UTF_8)
      ()
    }.mapError(_ => JsRunError.Write(path))

  private def empty(path: Path): IO[JsRunError, Unit] =
    ZIO.attemptBlocking {
      if Files.exists(path) then
        val walk = Files.walk(path)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(file => Files.deleteIfExists(file))
      Files.createDirectories(path)
      ()
    }.mapError(_ => JsRunError.Write(path))

enum JsRunError:
  case Unavailable
  case Compile(error: CompileError)
  case Write(path: Path)

  def message: String = this match
    case Unavailable    => "the JavaScript environment could not be fetched"
    case Compile(error) => error.message
    case Write(path)    => s"could not write $path"
