package histos.js

import histos.jdk.JdkRuntime
import histos.node.NodeBinary
import histos.pack.MainClass
import histos.process.{ChildClasspath, ChildRequest}
import histos.workspace.Workspace
import histos.zinc.{
  CompileError,
  CompileRequest,
  CompileToolchain,
  JvmCompile,

  ScalacFlags
}
import histos.{JsEnv, JsModuleKind, ScalaVersion}

import histos.resolve.{LockedTools, ResolveError}

import java.nio.file.Path

import zio.{IO, ZIO}

/** One start of a linked script. The engine does not load a `JSEnv`. */
final case class JsRunRequest(
    env: JsEnv,
    kind: JsModuleKind,
    script: Path,
    work: Path,
    jdk: JdkRuntime,
    processEnv: Map[String, String],
    node: NodeBinary
)

/** Start a linked script in the module's JavaScript environment. The engine
  * does not load a `JSEnv`.
  */
object JsRun:
  private val SuiteMain: String = "subprocess.JsRun"

  def start(request: JsRunRequest): ZIO[LockedTools, JsRunError, ChildRequest] =
    val JsRunRequest(env, kind, script, work, jdk, processEnv, node) = request
    val bridgeRoot = work.resolve("js-run")
    val classes = bridgeRoot.resolve("classes")
    val source =
      bridgeRoot.resolve("src").resolve("subprocess").resolve("JsRun.scala")
    val child = work.resolve("js-run-child")
    for
      view <- ZIO.service[LockedTools]
      tools <- view
        .materialize(
          histos.js.JsRequests.environment(env, view.catalog, view.repositories)
        )
        .mapError(JsRunError.Resolved(_))
        .map(_.artifacts.map(_.path))
      _ <- write(source, runner(env))
      _ <- JvmCompile
        .compile(
          CompileRequest(
            CompileToolchain(
              histos.js.JsRequests.compiler(env, view.catalog),
              histos.Platform.Jvm,
              jdk,
              ScalacFlags.empty
            ),
            List(source),
            tools,
            classes,
            bridgeRoot.resolve("analysis"),
            Nil
          )
        )
        .mapError(JsRunError.Compile(_))
      _ <- empty(child)
    yield ChildRequest(
      histos.Platform.Jvm,
      jdk.home.resolve("bin").resolve("java"),
      ChildClasspath(classes, tools),
      MainClass.trusted(SuiteMain),
      child,
      processEnv ++ nodeEnv(env, node) ++ Map(
        "JS_SCRIPT" -> script.toAbsolutePath.toString,
        "JS_MODULE" -> kind.token
      )
    )

  /** The child hands the chosen `node` to its `JSEnv`. Chekhov's Playwright
    * driver reads `PLAYWRIGHT_NODEJS_PATH`.
    */
  private[js] def nodeEnv(env: JsEnv, node: NodeBinary): Map[String, String] =
    env match
      case JsEnv.Node | JsEnv.Jsdom => Map("JS_NODE" -> node.path.toString)
      case JsEnv.Firefox => Map("PLAYWRIGHT_NODEJS_PATH" -> node.path.toString)

  /** The expression that builds the `JSEnv` on the `node` in `JS_NODE`. */
  private[js] def envExpr(env: JsEnv): String = env match
    case JsEnv.Node =>
      """new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withExecutable(sys.env("JS_NODE")))"""
    case JsEnv.Jsdom =>
      """new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv.Config().withExecutable(sys.env("JS_NODE")))"""
    case JsEnv.Firefox =>
      "new chekhov.jsenv.ChekhovJSEnv(chekhov.ChekhovBrowser.Firefox, true, false, scala.collection.immutable.Map.empty[String, String])"

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

  private def write(path: Path, text: String): IO[JsRunError, Unit] =
    Workspace.writeString(path, text).mapError(_ => JsRunError.Write(path))

  private def empty(path: Path): IO[JsRunError, Unit] =
    Workspace.recreate(path).mapError(_ => JsRunError.Write(path)).unit

enum JsRunError:
  case Resolved(error: ResolveError)
  case Unavailable
  case Compile(error: CompileError)
  case Write(path: Path)

  def message: String = this match
    case Resolved(error) => error.message
    case Unavailable     => "the JavaScript environment could not be fetched"
    case Compile(error)  => error.message
    case Write(path)     => s"could not write $path"
