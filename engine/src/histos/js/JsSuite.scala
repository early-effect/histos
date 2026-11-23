package histos.js

import histos.jdk.JdkRuntime
import histos.node.NodeBinary
import histos.pack.MainClass
import histos.process.ChildRequest
import histos.testrun.SuitePattern
import histos.workspace.Workspace
import histos.{JsEnv, JsModuleKind, ScalaVersion}

import histos.resolve.LockedTools

import java.nio.file.Path

import zio.{IO, ZIO}

/** One Scala.js test-adapter run.
  */
final case class JsSuiteRequest(
    version: ScalaVersion,
    env: JsEnv,
    kind: JsModuleKind,
    framework: String,
    classpath: List[Path],
    work: Path,
    jdk: JdkRuntime,
    processEnv: Map[String, String],
    node: NodeBinary,
    patterns: List[SuitePattern] = Nil,
    revision: Option[histos.JsVersion] = None,
)

/** Link the Scala.js test bridge and run `org.scalajs.testing.adapter.TestAdapter` in a child JVM.
  */
object JsSuite:
  private val SuiteMain: String = "histos.js.child.SuiteMain"

  def run(
      request: JsSuiteRequest
  ): ZIO[LockedTools, JsSuiteError, ChildRequest] =
    val JsSuiteRequest(
      version,
      env,
      kind,
      framework,
      classpath,
      work,
      jdk,
      processEnv,
      node,
      patterns,
      revision,
    )          = request
    val output = work.resolve("out")
    val listed = work.resolve("classpath.txt")
    val child  = work.resolve("child")
    for
      view <- ZIO.service[LockedTools]
      selected = revision.getOrElse(view.catalog.js.revision)
      libraries <- ScalaJs
        .libraries(version, Some(selected))
        .mapError(JsSuiteError.Library(_))
      bridge <- view
        .materialize(histos.js.JsRequests.testBridge(selected, view.catalog, view.repositories))
        .mapError(JsSuiteError.Resolved(_))
        .map(_.artifacts.map(_.path))
      host <- ZIO.fromOption(HostClasspath.child).orElseFail(JsSuiteError.Run("the histos classpath is empty"))
      _    <- write(listed, (classpath ++ libraries ++ bridge).map(_.toAbsolutePath.toString).mkString("\n"))
      _    <- empty(output)
      _    <- empty(child)
    yield ChildRequest(
      histos.Platform.Jvm,
      jdk.home.resolve("bin").resolve("java"),
      host,
      MainClass.trusted(SuiteMain),
      child,
      processEnv ++ JsRun.nodeEnv(env, node) ++ Map(
        "JS_OUT"       -> output.toAbsolutePath.toString,
        "JS_CLASSPATH" -> listed.toAbsolutePath.toString,
        "JS_FRAMEWORK" -> framework,
        "JS_MODULE"    -> kind.token,
        "JS_ENV"       -> envName(env),
        "JS_PATTERNS"  -> patterns.map(_.value).mkString("\n"),
      ),
    )

  private def envName(env: JsEnv): String = env match
    case JsEnv.Node    => "node"
    case JsEnv.Jsdom   => "jsdom"
    case JsEnv.Firefox => "firefox"

  private def write(path: Path, text: String): IO[JsSuiteError, Unit] =
    Workspace.writeString(path, text).mapError(_ => JsSuiteError.Run(s"could not write $path"))

  private def empty(path: Path): IO[JsSuiteError, Unit] =
    Workspace.recreate(path).mapError(_ => JsSuiteError.Run(s"could not prepare $path")).unit
end JsSuite
