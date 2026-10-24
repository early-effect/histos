package histos.node

import histos.{Aggregate, Module, Platform, ProjectGraph}

import java.nio.file.Path
import histos.toolchain.ToolSelection

enum NodeOrigin:
  case Override, Path, Nvm, Fnm, Volta, Cache

  def label: String = this match
    case Override => "override"
    case Path     => "path"
    case Nvm      => "nvm"
    case Fnm      => "fnm"
    case Volta    => "volta"
    case Cache    => "cache"

enum NodeReject:
  case Absent
  case NoBinary
  case WrongMajor(found: NodeMajor)
  case UnreadableVersion(error: NodeVersionError)

  def message: String = this match
    case Absent                   => "absent"
    case NoBinary                 => "no node"
    case WrongMajor(found)        => s"major ${found.value}"
    case UnreadableVersion(error) => error.message

enum NodeCandidate:
  case Match(origin: NodeOrigin, home: Path, version: NodeVersion)
  case Rejected(origin: NodeOrigin, reason: NodeReject)
  case NotPresent(origin: NodeOrigin)
  case Empty(origin: NodeOrigin)

enum NodeInstall:
  case Nvm(major: NodeMajor)
  case Fnm(major: NodeMajor)
  case Volta(major: NodeMajor)

  def command: String = this match
    case Nvm(major)   => s"nvm install ${major.value}"
    case Fnm(major)   => s"fnm install ${major.value}"
    case Volta(major) => s"volta install node@${major.value}"

enum NodeResolution:
  case Selected(chosen: NodeCandidate.Match, trace: List[NodeCandidate])
  case OverrideRejected(reason: NodeReject, trace: List[NodeCandidate])
  case Install(hint: NodeInstall, trace: List[NodeCandidate])
  case Provision(major: NodeMajor, trace: List[NodeCandidate])

final case class NodeHome(
    path: Path,
    hasBinary: Boolean,
    version: Either[NodeVersionError, NodeVersion]
)

enum NodeLocated:
  case Unset
  case Missing(path: Path)
  case Found(home: NodeHome)

enum NodeManagerSlot:
  case Absent
  case Present(homes: List[NodeHome])

enum NodeManagerProbe:
  case Absent
  case Installed(versionsRoot: Option[Path])

final case class NodeView(
    explicit: NodeLocated,
    onPath: NodeLocated,
    nvm: NodeManagerSlot,
    fnm: NodeManagerSlot,
    volta: NodeManagerSlot,
    cached: List[NodeHome]
)

object Node:
  /** A JVM-only graph does not name Node. An aggregate does not either. */
  def required(graph: ProjectGraph): Boolean =
    graph.projects.exists:
      case module: Module => module.platforms.all.contains(Platform.Js)
      case _: Aggregate   => false

  def resolve(major: NodeMajor, view: NodeView): NodeResolution =
    val trace =
      located(NodeOrigin.Override, view.explicit, major) ++
        located(NodeOrigin.Path, view.onPath, major) ++
        manager(NodeOrigin.Nvm, view.nvm, major) ++
        manager(NodeOrigin.Fnm, view.fnm, major) ++
        manager(NodeOrigin.Volta, view.volta, major) ++
        homes(NodeOrigin.Cache, view.cached, major)
    ToolSelection.fold(trace):
      case chosen @ NodeCandidate.Match(NodeOrigin.Override, _, _) =>
        ToolSelection.Selected(chosen)
      case NodeCandidate.Rejected(NodeOrigin.Override, reason) =>
        ToolSelection.Rejected(reason)
      case _ => ToolSelection.Locked
    match
      case ToolSelection.Selected(chosen) =>
        NodeResolution.Selected(chosen, trace)
      case ToolSelection.Rejected(reason) =>
        NodeResolution.OverrideRejected(reason, trace)
      case ToolSelection.Locked => NodeResolution.Provision(major, trace)

  def report(resolution: NodeResolution): String =
    val lines = resolution match
      case NodeResolution.Selected(chosen, trace) =>
        trace.map(
          show
        ) :+ s"chosen ${chosen.origin.label} ${chosen.version.value} ${chosen.home}"
      case NodeResolution.OverrideRejected(reason, trace) =>
        trace.map(show) :+ s"override rejected ${showReason(reason)}"
      case NodeResolution.Install(hint, trace) =>
        trace.map(show) :+ s"install ${hint.command}"
      case NodeResolution.Provision(major, trace) =>
        trace.map(show) :+ s"provision node ${major.value}"
    lines.mkString("\n")

  private def located(
      origin: NodeOrigin,
      location: NodeLocated,
      wanted: NodeMajor
  ): List[NodeCandidate] =
    location match
      case NodeLocated.Unset      => List(NodeCandidate.NotPresent(origin))
      case NodeLocated.Missing(_) =>
        List(NodeCandidate.Rejected(origin, NodeReject.Absent))
      case NodeLocated.Found(home) => List(judge(origin, wanted, home))

  private def manager(
      origin: NodeOrigin,
      slot: NodeManagerSlot,
      wanted: NodeMajor
  ): List[NodeCandidate] =
    slot match
      case NodeManagerSlot.Absent => List(NodeCandidate.NotPresent(origin))
      case NodeManagerSlot.Present(Nil)   => List(NodeCandidate.Empty(origin))
      case NodeManagerSlot.Present(found) => homes(origin, found, wanted)

  private def homes(
      origin: NodeOrigin,
      installed: List[NodeHome],
      wanted: NodeMajor
  ): List[NodeCandidate] =
    if installed.isEmpty then List(NodeCandidate.NotPresent(origin))
    else installed.map(judge(origin, wanted, _))

  private def judge(
      origin: NodeOrigin,
      wanted: NodeMajor,
      home: NodeHome
  ): NodeCandidate =
    if !home.hasBinary then NodeCandidate.Rejected(origin, NodeReject.NoBinary)
    else
      home.version match
        case Left(error) =>
          NodeCandidate.Rejected(origin, NodeReject.UnreadableVersion(error))
        case Right(version) if version.major == wanted =>
          NodeCandidate.Match(origin, home.path, version)
        case Right(version) =>
          NodeCandidate.Rejected(origin, NodeReject.WrongMajor(version.major))

  private def show(candidate: NodeCandidate): String =
    candidate match
      case NodeCandidate.Match(origin, home, version) =>
        s"match ${origin.label} ${version.value} $home"
      case NodeCandidate.Rejected(origin, reason) =>
        s"reject ${origin.label} ${showReason(reason)}"
      case NodeCandidate.NotPresent(origin) => s"absent ${origin.label}"
      case NodeCandidate.Empty(origin)      => s"empty ${origin.label}"

  private def showReason(reason: NodeReject): String = reason.message
