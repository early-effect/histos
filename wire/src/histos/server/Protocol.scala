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
import histos.testrun.{SuitePattern, TagName, TestFilter, TestMode, TestTerm}
import histos.workflow.WorkflowAction
import histos.ProjectName

import java.nio.file.Path

import zio.Chunk
import zio.json.*
import histos.report.WireJson.given JsonCodec[CallResult]
import histos.report.WireJson.given JsonCodec[BuildEvent]
import histos.report.WireJson.given JsonCodec[Path]
import histos.report.WireJson.given JsonCodec[histos.MainClass]
import histos.report.WireJson.given JsonCodec[histos.PlatformSelection]
import histos.report.WireJson.given JsonCodec[histos.ScalaSelection]
import histos.report.WireJson.given JsonCodec[histos.CompileExtent]
import histos.report.WireJson.given JsonCodec[histos.TestSelection]
import histos.report.WireJson.given JsonCodec[ProjectName]

enum ServerError:
  case Toolchain(error: histos.jdk.ToolchainError)
  case Compiled(error: histos.zinc.GraphCompileError)
  case Resolve(error: histos.resolve.ResolveError)
  case Unreachable(path: Path)

  /** The spawned server exited before it listened. `detail` is the log text, or the exit code when that text is empty.
    */
  case Exited(detail: String)

  /** Another server already answers on this checkout's socket, so this one does not take it. */
  case Listening(socket: Path)
  case DifferentVersion
  case Restart(socket: Path, wanted: String, got: String)

  /** A stop was not honored, and the process is still listening. The record stays with that process. */
  case DidNotStop(socket: Path)
  case NoServer

  /** The socket closed before [[Message.Done]]. The client did not reject a payload. */
  case Closed
  case Protocol
  case Address(path: Path)
  case Io(op: ServerIo)

  def message: String = this match
    case Toolchain(error)  => error.message
    case Compiled(error)   => error.message
    case Resolve(error)    => error.message
    case Unreachable(path) => s"histos server at $path is not listening"
    case Exited(detail)    => detail
    case Listening(socket) => s"another histos server is listening at $socket"
    case DifferentVersion  =>
      "histos server is a different version and the restart did not finish"
    case Restart(socket, wanted, got) =>
      s"histos server at $socket did not record version $wanted; it has $got"
    case DidNotStop(socket) => s"histos server at $socket did not stop"
    case NoServer           => "no histos server is running"
    case Closed             => "histos server closed the frame stream before the command finished"
    case Protocol           =>
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
  case Output(bytes: Chunk[Byte])
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
  given JsonCodec[TestTerm] =
    JsonCodec.string.transformOrFail(
      raw => TestTerm.from(raw).left.map(_.message),
      _.value,
    )
  given JsonCodec[TagName] =
    JsonCodec.string.transformOrFail(
      raw => TagName.from(raw).left.map(_.message),
      _.value,
    )
  given JsonCodec[TestFilter] = JsonCodec.derived
  given JsonCodec[RunId]      =
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

/** `Shell` replaces the server environment with the caller's. `Tool` overlays the server environment.
  *
  * The program's standard streams are pipes. Histos keeps the terminal.
  */
enum RunLaunch derives JsonCodec:
  case Shell(env: Map[String, String], directory: Path)
  case Tool(env: Map[String, String], directory: Option[Path])

  def programEnv(server: Map[String, String]): Map[String, String] =
    this match
      case Shell(env, _) => env
      case Tool(env, _)  => server ++ env
end RunLaunch

/** A frame the client sends while a program runs. `Stdin` carries base64 bytes. */
enum RunInput derives JsonCodec:
  case Stdin(bytes: String)
  case StdinEnd
  case Interrupt

object RunBytes:
  def encode(bytes: Chunk[Byte]): String =
    java.util.Base64.getEncoder.encodeToString(bytes.toArray)

  def decode(text: String): Option[Chunk[Byte]] =
    try Some(Chunk.fromArray(java.util.Base64.getDecoder.decode(text)))
    catch case _: IllegalArgumentException => None
end RunBytes

/** What a running command does with one daemon frame. `None` is a closed stream, not a payload. */
object CommandFrame:
  def read(frame: Option[String]): Either[ServerError, Feed] =
    frame match
      case None          => Left(ServerError.Closed)
      case Some(payload) =>
        payload.fromJson[Message] match
          case Right(Message.Event(event))    => Right(Feed.Event(event))
          case Right(Message.Output(encoded)) =>
            RunBytes.decode(encoded) match
              case Some(bytes) => Right(Feed.Output(bytes))
              case None        => Left(ServerError.Protocol)
          case Right(Message.Done(result)) => Right(Feed.Done(result))
          case _                           => Left(ServerError.Protocol)

  /** The first frame is [[Message.Ready]]. A closed stream is [[ServerError.Closed]]. */
  def ready(frame: Option[String]): Either[ServerError, Unit] =
    frame match
      case None          => Left(ServerError.Closed)
      case Some(payload) =>
        payload.fromJson[Message] match
          case Right(Message.Ready) => Right(())
          case _                    => Left(ServerError.Protocol)
end CommandFrame

/** What the client does with the socket after Ready. */
enum Attendance:
  /** The handler reads the socket. Closing it is not a cancel. */
  case Held

  /** The client writes program input frames. */
  case Program

  /** The client writes nothing more. Closing the socket cancels the call. */
  case Quiet
end Attendance

enum Call derives JsonCodec:
  case Compile(
      projects: List[ProjectName],
      platforms: histos.PlatformSelection = histos.PlatformSelection.All,
      scala: histos.ScalaSelection = histos.ScalaSelection.All,
      sources: histos.CompileExtent = histos.CompileExtent.Main,
  )
  case Test(
      selection: histos.TestSelection,
      patterns: List[SuitePattern],
      mode: TestMode,
      filter: TestFilter,
  )
  case PublishCheck(tokens: List[PublishToken], scope: histos.publish.ProofScope)
  case PublishLocal(tokens: List[PublishToken], scope: histos.publish.ProofScope)

  /** Remove the local ivy revisions and toolchain directory for HEAD. The pin stays. */
  case PublishWithdraw
  case PublishSnapshots(tokens: List[String])
  case PublishRelease(tokens: List[String], automatic: Boolean)
  case PublishPromote(id: DeploymentId)
  case PublishDrop(id: DeploymentId)
  case PublishAssets(tokens: List[String])
  case Graph
  case Run(
      id: String,
      platform: Option[histos.PlatformSelection],
      launch: RunLaunch,
      args: List[String],
      main: Option[histos.MainClass] = None,
      scala: Option[histos.ScalaSelection] = None,
  )
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

  /** Exit. The server removes its record and does not start another process. */
  case Stop
  case Version
  case CachePath
  case CacheList
  case CacheClean(mode: CacheCleanMode)
  case CachePromote(from: CacheNamespace, to: CacheNamespace)
  case CacheForget(target: ForgetTarget)
  case CacheVerify(projects: List[ProjectName])
  case Watch(inner: Call, policy: WatchPolicy)
  case Bsp
  case PreviewCompile(
      projects: List[ProjectName],
      platforms: histos.PlatformSelection = histos.PlatformSelection.All,
      scala: histos.ScalaSelection = histos.ScalaSelection.All,
  )
  case PreviewTest(
      selection: histos.TestSelection,
      patterns: List[SuitePattern],
      mode: TestMode,
      filter: TestFilter,
  )
  case PreviewPublishLocal(tokens: List[PublishToken])
  case PreviewPublishSnapshots(tokens: List[String])
  case PreviewClean(projects: List[ProjectName], extent: CleanExtent)
  case PreviewRun(
      id: String,
      platform: Option[histos.PlatformSelection],
      main: Option[histos.MainClass] = None,
      scala: Option[histos.ScalaSelection] = None,
  )

  /** Project names, module names, plugin commands, or one plugin command's flags. Nothing here compiles. */
  case Catalog(query: CatalogQuery)

  /** A nested watch is quiet. Only the program under one watch reads stdin. */
  def attendance: Attendance =
    this match
      case Bsp | Reboot(_) | Stop                                  => Attendance.Held
      case Run(_, _, _, _, _, _) | Watch(Run(_, _, _, _, _, _), _) => Attendance.Program
      case _                                                       => Attendance.Quiet
end Call

object Call:
  /** The codec is on [[Call]]. A case value does not carry it. */
  def encode(call: Call): String =
    call.toJson

enum Message derives JsonCodec:
  case Ready
  case Event(event: BuildEvent)
  case Output(bytes: String)
  case Done(result: CallResult)
  case Marker(histosExit: Int)
