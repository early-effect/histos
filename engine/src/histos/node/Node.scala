package histos.node

import histos.{Aggregate, Module, Platform, ProjectGraph}

import java.nio.file.Path

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

final case class NodeHome(path: Path, hasBinary: Boolean, version: Either[NodeVersionError, NodeVersion])

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
    cached: List[NodeHome],
)

object Node:
  /** A JVM-only graph does not name Node. An aggregate does not either. */
  def required(graph: ProjectGraph): Boolean =
    graph.projects.exists:
      case Module(_, platforms, _, _, _, _, _, _, _, _, _) => platforms.all.contains(Platform.Js)
      case Aggregate(_, _, _)               => false

  def resolve(major: NodeMajor, view: NodeView): NodeResolution =
    slot(NodeOrigin.Override, view.explicit, major, Nil, stop = true) match
      case Left(done) => done
      case Right(afterOverride) =>
        val continued =
          for
            afterPath  <- slot(NodeOrigin.Path, view.onPath, major, afterOverride, stop = false)
            afterNvm   <- manager(NodeOrigin.Nvm, view.nvm, major, afterPath)
            afterFnm   <- manager(NodeOrigin.Fnm, view.fnm, major, afterNvm)
            afterVolta <- manager(NodeOrigin.Volta, view.volta, major, afterFnm)
          yield afterVolta
        continued match
          case Left(done) => done
          case Right(seen) =>
            if managerPresent(view) then hint(view, major, seen)
            else
              homes(NodeOrigin.Cache, view.cached, major, seen) match
                case Left(done)  => done
                case Right(done) => NodeResolution.Provision(major, done)

  def report(resolution: NodeResolution): String =
    val lines = resolution match
      case NodeResolution.Selected(chosen, trace) =>
        trace.map(show) :+ s"chosen ${chosen.origin.label} ${chosen.version.value} ${chosen.home}"
      case NodeResolution.OverrideRejected(reason, trace) =>
        trace.map(show) :+ s"override rejected ${showReason(reason)}"
      case NodeResolution.Install(hint, trace) =>
        trace.map(show) :+ s"install ${hint.command}"
      case NodeResolution.Provision(major, trace) =>
        trace.map(show) :+ s"provision node ${major.value}"
    lines.mkString("\n")

  private def slot(
      origin: NodeOrigin,
      located: NodeLocated,
      wanted: NodeMajor,
      trace: List[NodeCandidate],
      stop: Boolean,
  ): Either[NodeResolution, List[NodeCandidate]] =
    located match
      case NodeLocated.Unset => Right(trace :+ NodeCandidate.NotPresent(origin))
      case NodeLocated.Missing(_) =>
        val next = trace :+ NodeCandidate.Rejected(origin, NodeReject.Absent)
        if stop then Left(NodeResolution.OverrideRejected(NodeReject.Absent, next)) else Right(next)
      case NodeLocated.Found(home) =>
        judge(origin, wanted, home) match
          case chosen @ NodeCandidate.Match(_, _, _) => Left(NodeResolution.Selected(chosen, trace :+ chosen))
          case NodeCandidate.Rejected(_, reason) if stop =>
            Left(NodeResolution.OverrideRejected(reason, trace :+ NodeCandidate.Rejected(origin, reason)))
          case other => Right(trace :+ other)

  private def manager(
      origin: NodeOrigin,
      slot: NodeManagerSlot,
      wanted: NodeMajor,
      trace: List[NodeCandidate],
  ): Either[NodeResolution, List[NodeCandidate]] =
    slot match
      case NodeManagerSlot.Absent       => Right(trace :+ NodeCandidate.NotPresent(origin))
      case NodeManagerSlot.Present(Nil) => Right(trace :+ NodeCandidate.Empty(origin))
      case NodeManagerSlot.Present(found) => homes(origin, found, wanted, trace)

  private def homes(
      origin: NodeOrigin,
      installed: List[NodeHome],
      wanted: NodeMajor,
      trace: List[NodeCandidate],
  ): Either[NodeResolution, List[NodeCandidate]] =
    if installed.isEmpty then Right(trace :+ NodeCandidate.NotPresent(origin))
    else
      val judged = installed.map(home => judge(origin, wanted, home))
      judged.collectFirst { case chosen @ NodeCandidate.Match(_, _, _) => chosen } match
        case Some(chosen) => Left(NodeResolution.Selected(chosen, trace ++ judged))
        case None         => Right(trace ++ judged)

  private def managerPresent(view: NodeView): Boolean =
    present(view.nvm) || present(view.fnm) || present(view.volta)

  private def present(slot: NodeManagerSlot): Boolean =
    slot match
      case NodeManagerSlot.Present(_) => true
      case NodeManagerSlot.Absent     => false

  private def hint(view: NodeView, major: NodeMajor, trace: List[NodeCandidate]): NodeResolution =
    view.nvm match
      case NodeManagerSlot.Present(_) => NodeResolution.Install(NodeInstall.Nvm(major), trace)
      case NodeManagerSlot.Absent =>
        view.fnm match
          case NodeManagerSlot.Present(_) => NodeResolution.Install(NodeInstall.Fnm(major), trace)
          case NodeManagerSlot.Absent =>
            view.volta match
              case NodeManagerSlot.Present(_) => NodeResolution.Install(NodeInstall.Volta(major), trace)
              case NodeManagerSlot.Absent     => NodeResolution.Provision(major, trace)

  private def judge(origin: NodeOrigin, wanted: NodeMajor, home: NodeHome): NodeCandidate =
    if !home.hasBinary then NodeCandidate.Rejected(origin, NodeReject.NoBinary)
    else
      home.version match
        case Left(error)                               => NodeCandidate.Rejected(origin, NodeReject.UnreadableVersion(error))
        case Right(version) if version.major == wanted => NodeCandidate.Match(origin, home.path, version)
        case Right(version)                            => NodeCandidate.Rejected(origin, NodeReject.WrongMajor(version.major))

  private def show(candidate: NodeCandidate): String =
    candidate match
      case NodeCandidate.Match(origin, home, version) => s"match ${origin.label} ${version.value} $home"
      case NodeCandidate.Rejected(origin, reason)     => s"reject ${origin.label} ${showReason(reason)}"
      case NodeCandidate.NotPresent(origin)           => s"absent ${origin.label}"
      case NodeCandidate.Empty(origin)                => s"empty ${origin.label}"

  private def showReason(reason: NodeReject): String =
    reason match
      case NodeReject.Absent                   => "absent"
      case NodeReject.NoBinary                 => "no node"
      case NodeReject.WrongMajor(found)        => s"major ${found.value}"
      case NodeReject.UnreadableVersion(error) => error.message
