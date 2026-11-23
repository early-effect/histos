package histos.js

import histos.jdk.JdkRuntime
import histos.node.NodeBinary
import histos.pack.MainClass
import histos.process.ChildRequest
import histos.workspace.Workspace
import histos.{JsEnv, JsModuleKind, ScalaVersion}

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
    node: NodeBinary,
    directory: Option[Path] = None,
)

/** Start a linked script in the module's JavaScript environment.
  */
object JsRun:
  private val SuiteMain: String = "histos.js.child.RunMain"

  def start(request: JsRunRequest): ZIO[Any, JsRunError, ChildRequest] =
    val JsRunRequest(env, kind, script, work, jdk, processEnv, node, directory) = request
    val child                                                                   = work.resolve("js-run-child")
    for
      host <- ZIO.fromOption(HostClasspath.child).orElseFail(JsRunError.Write(child))
      _    <- empty(child)
    yield ChildRequest(
      histos.Platform.Jvm,
      jdk.home.resolve("bin").resolve("java"),
      host,
      MainClass.trusted(SuiteMain),
      child,
      runEnvironment(processEnv, env, node, script, kind) + ("JS_ENV" -> envName(env)),
      directory = directory,
    )

  private def envName(env: JsEnv): String = env match
    case JsEnv.Node    => "node"
    case JsEnv.Jsdom   => "jsdom"
    case JsEnv.Firefox => "firefox"

  /** Program variables first. The bridge keys win: `JS_NODE`, `JS_SCRIPT`, `JS_MODULE`, and `PLAYWRIGHT_NODEJS_PATH`.
    */
  private[js] def runEnvironment(
      processEnv: Map[String, String],
      env: JsEnv,
      node: NodeBinary,
      script: Path,
      kind: JsModuleKind,
  ): Map[String, String] =
    processEnv ++ nodeEnv(env, node) ++ Map(
      "JS_SCRIPT" -> script.toAbsolutePath.toString,
      "JS_MODULE" -> kind.token,
    )

  /** The child hands the chosen `node` to its `JSEnv`. Chekhov's Playwright driver reads `PLAYWRIGHT_NODEJS_PATH`.
    */
  private[js] def nodeEnv(env: JsEnv, node: NodeBinary): Map[String, String] =
    env match
      case JsEnv.Node | JsEnv.Jsdom => Map("JS_NODE" -> node.path.toString)
      case JsEnv.Firefox            => Map("PLAYWRIGHT_NODEJS_PATH" -> node.path.toString)

  private def empty(path: Path): IO[JsRunError, Unit] =
    Workspace.recreate(path).mapError(_ => JsRunError.Write(path)).unit
end JsRun
