package histos.server

import histos.build.BuildEvent
import histos.cache.CacheCleanMode
import histos.format.{FormatMode, FormatTargets}
import histos.plugin.{FlagName, NamedText, ParamName, PluginArgs}
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
  case DifferentVersion
  case NoServer
  case Protocol
  case Address(path: Path)
  case Io(op: ServerIo)

  def message: String = this match
    case Toolchain(error)  => error.message
    case Compiled(error)   => error.message
    case Resolve(error)    => error.message
    case Unreachable(path) => s"histos server at $path is not listening"
    case DifferentVersion  =>
      "histos server is a different version and the restart did not finish"
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
  given JsonCodec[NamedText]     = JsonCodec.derived
  given JsonCodec[PluginArgs]    = JsonCodec.derived
  given JsonCodec[ResolveAction] =
    JsonCodec.string.transformOrFail(
      raw => ResolveAction.from(raw).left.map(_.message),
      _.token,
    )
  given JsonCodec[WorkflowAction] =
    JsonCodec.string.transformOrFail(
      raw => WorkflowAction.from(raw).left.map(_.message),
      _.token,
    )
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
  given JsonCodec[TestMode] =
    JsonCodec.string.transformOrFail(
      raw => TestMode.from(raw).left.map(_.message),
      _.token,
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

/** How a watch repeats. `After` runs the call and then polls. `Restart` polls while a program runs and replaces it. */
enum WatchPolicy derives JsonCodec:
  case After
  case Restart

enum Call derives JsonCodec:
  case Compile(projects: List[ProjectName])
  case Test(projects: List[ProjectName], patterns: List[SuitePattern], mode: TestMode)
  case PublishLocal(tokens: List[String])
  case PublishSnapshots(tokens: List[String])
  case PublishRelease(tokens: List[String], automatic: Boolean)
  case PublishPromote(id: String)
  case PublishDrop(id: String)
  case PublishAssets(tokens: List[String])
  case Graph
  case Run(id: String, platform: Option[Platform])

  /** Link the native client and write `dist/histos-client`. Does not start it. */
  case Client
  case Task(id: String, args: PluginArgs)
  case Cancel(id: RunId)
  case Toolchain
  case Resolve(action: ResolveAction)
  case Workflow(action: WorkflowAction)
  case Format(targets: FormatTargets, mode: FormatMode)
  case Assembly(destination: Option[String])
  case Clean(projects: List[ProjectName])
  case CleanFull
  case Reload
  case Reboot
  case Version
  case CachePath
  case CacheList
  case CacheClean(mode: CacheCleanMode)
  case CachePromote(from: CacheNamespace, to: CacheNamespace)
  case Launcher(action: LauncherAction)
  case Watch(inner: Call, policy: WatchPolicy)
  case Bsp
  case PreviewCompile(projects: List[ProjectName])
  case PreviewTest(projects: List[ProjectName])
  case PreviewPublishLocal(tokens: List[String])
  case PreviewPublishSnapshots(tokens: List[String])
  case PreviewClean(projects: List[ProjectName])
  case PreviewCleanFull
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
