package histos.server

import histos.build.BuildEvent
import histos.cache.CacheCleanMode
import histos.central.DeploymentId
import histos.format.{FormatMode, FormatTargets}
import histos.plugin.{FlagName, NamedText, ParamName, PluginArgs}
import histos.publish.PublishToken
import histos.report.{BuildFault, CallResult}
import histos.schedule.CacheNamespace
import histos.task.RunId
import histos.testrun.{SuitePattern, TestMode}
import histos.workflow.WorkflowAction
import histos.{Platform, ProjectName}

import java.nio.file.Path

import zio.json.*
import histos.report.WireJson.given JsonCodec[CallResult]
import histos.report.WireJson.given JsonCodec[BuildEvent]
import histos.report.WireJson.given JsonCodec[Platform]
import histos.report.WireJson.given JsonCodec[ProjectName]

enum ServerError:
  case Toolchain(error: histos.jdk.ToolchainError)
  case Compiled(error: histos.zinc.GraphCompileError)
  case Resolve(error: histos.resolve.ResolveError)
  case Unreachable(path: Path)

  /** Another server already answers on this checkout's socket, so this one does not take it. */
  case Listening(socket: Path)
  case DifferentVersion
  case Restart(socket: Path, wanted: String, got: String)
  case NoServer
  case Protocol
  case Address(path: Path)
  case Io(op: ServerIo)

  def message: String = this match
    case Toolchain(error)  => error.message
    case Compiled(error)   => error.message
    case Resolve(error)    => error.message
    case Unreachable(path) => s"histos server at $path is not listening"
    case Listening(socket) => s"another histos server is listening at $socket"
    case DifferentVersion  =>
      "histos server is a different version and the restart did not finish"
    case Restart(socket, wanted, got) =>
      s"histos server at $socket did not record version $wanted; it has $got"
    case NoServer => "no histos server is running"
    case Protocol =>
      "histos server sent a frame this client does not understand"
    case Address(path) => s"histos server socket path is too long: $path"
    case Io(op)        => s"histos server failed while ${op.label}"
end ServerError

enum ServerIo:
  case Bind(path: Path)
  case Accept(path: Path)
  case Read(path: Path)
  case Write(path: Path)
  case Save(path: Path)

  def label: String = this match
    case Bind(_)   => "binding"
    case Accept(_) => "accepting"
    case Read(_)   => "reading"
    case Write(_)  => "writing"
    case Save(_)   => "writing the server record"
end ServerIo

/** One event from a command running in the daemon. */
enum Feed:
  case Event(event: BuildEvent)
  case Done(result: CallResult)

private object CallJson:
  given JsonCodec[ParamName] = JsonCodec.string.transformOrFail(
    raw => ParamName.from(raw).left.map(_.message),
    _.value,
  )
  given JsonCodec[FlagName] = JsonCodec.string.transformOrFail(
    raw => FlagName.from(raw).left.map(_.message),
    _.value,
  )
  given JsonCodec[NamedText]      = JsonCodec.derived
  given JsonCodec[PluginArgs]     = JsonCodec.derived
  given JsonCodec[LauncherAction] =
    JsonCodec.string.transformOrFail(
      raw => LauncherAction.from(raw).left.map(_.message),
      _.token,
    )
  given JsonCodec[SuitePattern] =
    JsonCodec.string.transformOrFail(
      raw => SuitePattern.from(raw).left.map(_.message),
      _.value,
    )
  given JsonCodec[RunId] =
    JsonCodec.string.transformOrFail(
      raw => RunId.from(raw).left.map(_.message),
      _.value,
    )
  given JsonCodec[CacheCleanMode] =
    JsonCodec.string.transformOrFail(
      raw => CacheCleanMode.from(raw).left.map(_.message),
      _.token,
    )
  given JsonCodec[CacheNamespace] =
    JsonCodec.string.transformOrFail(
      raw => CacheNamespace.from(raw).left.map(_.message),
      _.token,
    )

import CallJson.given

/** `main`, `pr/<n>`, and `dev/<name>` are namespaces. Any other token is a module, and a slash that is neither reports
  * the namespace error.
  */
enum ForgetFault:
  case Namespace(error: histos.schedule.NamespaceError)
  case Project(error: histos.ProjectNameError)

enum ForgetTarget derives JsonCodec:
  case Namespace(namespace: CacheNamespace)
  case Module(module: histos.ProjectName)

object ForgetTarget:
  def from(raw: String): Either[ForgetFault, ForgetTarget] =
    CacheNamespace.from(raw) match
      case Right(namespace) => Right(ForgetTarget.Namespace(namespace))
      case Left(namespace)  =>
        if raw.contains('/') then Left(ForgetFault.Namespace(namespace))
        else histos.ProjectName.from(raw).left.map(ForgetFault.Project(_)).map(ForgetTarget.Module(_))

/** How a watch repeats. `After` runs the call and then polls. `Restart` polls while a program runs and replaces it. */
enum WatchPolicy derives JsonCodec:
  case After
  case Restart

enum Call derives JsonCodec:
  case Compile(projects: List[ProjectName])
  case Test(projects: List[ProjectName], patterns: List[SuitePattern], mode: TestMode)
  case PublishLocal(tokens: List[PublishToken])
  case PublishSnapshots(tokens: List[String])
  case PublishRelease(tokens: List[String], automatic: Boolean)
  case PublishPromote(id: DeploymentId)
  case PublishDrop(id: DeploymentId)
  case PublishAssets(tokens: List[String])
  case Graph
  case Run(id: String, platform: Option[Platform])
  case Task(id: String, args: PluginArgs)
  case Cancel(id: RunId)
  case Resolve(action: ResolveAction)
  case Workflow(action: WorkflowAction)
  case Format(targets: FormatTargets, mode: FormatMode)
  case Assembly(modules: List[String])
  case Clean(projects: List[ProjectName], extent: CleanExtent)
  case Reload

  /** Replace the server. The successor starts as `successor` names, never as the server it replaces. */
  case Reboot(successor: Successor)
  case Version
  case CachePath
  case CacheList
  case CacheClean(mode: CacheCleanMode)
  case CachePromote(from: CacheNamespace, to: CacheNamespace)
  case CacheForget(target: ForgetTarget)
  case CacheVerify(projects: List[ProjectName])
  case Watch(inner: Call, policy: WatchPolicy)
  case Bsp
  case PreviewCompile(projects: List[ProjectName])
  case PreviewTest(projects: List[ProjectName], patterns: List[SuitePattern], mode: TestMode)
  case PreviewPublishLocal(tokens: List[PublishToken])
  case PreviewPublishSnapshots(tokens: List[String])
  case PreviewClean(projects: List[ProjectName], extent: CleanExtent)
  case PreviewRun(id: String, platform: Option[Platform])
end Call

object Call:
  /** The codec is on [[Call]]. A case value does not carry it. */
  def encode(call: Call): String =
    call.toJson

enum Message derives JsonCodec:
  case Ready
  case Event(event: BuildEvent)
  case Done(result: CallResult)
  case Marker(histosExit: Int)
