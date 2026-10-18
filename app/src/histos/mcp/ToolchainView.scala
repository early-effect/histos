package histos.mcp

import heddle.endpoint.Schema
import histos.jdk.{Candidate, InstallHint, JdkLock, JdkSurvey, Origin, Reject, Resolution}
import histos.native.Clang
import histos.node.{NodeCandidate, NodeInstall, NodeLock, NodeOrigin, NodeReject, NodeResolution as DecidedNode, NodeSurvey}
import histos.toolchain.{Reached, Survey, SurveyError}

import zio.json.JsonCodec

/** The toolchain decision an agent reads. The CLI prints the same decision. */
object ToolchainView:
  enum JdkPlace derives Schema, JsonCodec:
    case Override, JavaHome, Path, Coursier, Sdkman, Cache

  enum JdkSkip derives Schema, JsonCodec:
    case Absent
    case NoJavac
    case WrongMajor(found: Int)
    case Unreadable(message: String)

  final case class JdkMatch(origin: JdkPlace, home: String, version: String) derives Schema, JsonCodec

  enum JdkSight derives Schema, JsonCodec:
    case Match(origin: JdkPlace, home: String, version: String)
    case Rejected(origin: JdkPlace, reason: JdkSkip)
    case NotPresent(origin: JdkPlace)
    case Empty(origin: JdkPlace)

  enum JdkResolution derives Schema, JsonCodec:
    case Selected(chosen: JdkMatch, trace: List[JdkSight])
    case OverrideRejected(reason: JdkSkip, trace: List[JdkSight])
    case Install(command: String, trace: List[JdkSight])
    case Provision(major: Int, trace: List[JdkSight])

  final case class JdkDecision(major: Int, lockVersion: String, archive: String, resolution: JdkResolution)
      derives Schema, JsonCodec

  enum NodePlace derives Schema, JsonCodec:
    case Override, Path, Nvm, Fnm, Volta, Cache

  enum NodeSkip derives Schema, JsonCodec:
    case Absent
    case NoBinary
    case WrongMajor(found: Int)
    case Unreadable(message: String)

  final case class NodeMatch(origin: NodePlace, home: String, version: String) derives Schema, JsonCodec

  enum NodeSight derives Schema, JsonCodec:
    case Match(origin: NodePlace, home: String, version: String)
    case Rejected(origin: NodePlace, reason: NodeSkip)
    case NotPresent(origin: NodePlace)
    case Empty(origin: NodePlace)

  enum NodeResolution derives Schema, JsonCodec:
    case Selected(chosen: NodeMatch, trace: List[NodeSight])
    case OverrideRejected(reason: NodeSkip, trace: List[NodeSight])
    case Install(command: String, trace: List[NodeSight])
    case Provision(major: Int, trace: List[NodeSight])

  final case class NodeDecision(major: Int, lockVersion: String, archive: String, resolution: NodeResolution)
      derives Schema, JsonCodec

  final case class ClangDecision(version: String, target: String, clang: String, clangpp: String)
      derives Schema, JsonCodec

  final case class ToolchainReport(jdk: JdkDecision, node: Option[NodeDecision], clang: Option[ClangDecision])
      derives Schema, JsonCodec

  enum ToolchainFault derives Schema, JsonCodec:
    case Jdk(message: String)
    case Node(message: String, jdk: JdkDecision)
    case Native(message: String, jdk: JdkDecision, node: Option[NodeDecision])
    case Clang(message: String, jdk: JdkDecision, node: Option[NodeDecision])

  def report(survey: Survey): ToolchainReport =
    ToolchainReport(jdk(survey.jdk), survey.node.map(node), survey.clang.map(clang))

  def fault(error: SurveyError): ToolchainFault = error match
    case SurveyError.Jdk(failure) => ToolchainFault.Jdk(failure.message)
    case SurveyError.Node(found, failure) => ToolchainFault.Node(failure.message, jdk(found))
    case SurveyError.Undeclared(reached, _) => native(reached, error.message)
    case SurveyError.Split(reached, _, _)   => native(reached, error.message)
    case SurveyError.Clang(reached, failure) =>
      ToolchainFault.Clang(failure.message, jdk(reached.jdk), reached.node.map(node))

  private def native(reached: Reached, message: String): ToolchainFault =
    ToolchainFault.Native(message, jdk(reached.jdk), reached.node.map(node))

  private def jdk(survey: JdkSurvey): JdkDecision =
    JdkDecision(survey.lock.major.value, survey.lock.version.value, JdkLock.url(survey.lock.source), resolve(survey.resolution))

  private def node(survey: NodeSurvey): NodeDecision =
    NodeDecision(
      survey.lock.major.value,
      survey.lock.version.value,
      NodeLock.url(survey.lock.source),
      resolveNode(survey.resolution),
    )

  private def clang(found: Clang): ClangDecision =
    ClangDecision(found.version.value, found.target.value, found.clang.toString, found.clangpp.toString)

  private def resolve(resolution: Resolution): JdkResolution = resolution match
    case Resolution.Selected(chosen, trace) =>
      JdkResolution.Selected(matched(chosen), trace.map(sight))
    case Resolution.OverrideRejected(reason, trace) =>
      JdkResolution.OverrideRejected(skip(reason), trace.map(sight))
    case Resolution.Install(hint, trace) =>
      JdkResolution.Install(command(hint), trace.map(sight))
    case Resolution.Provision(major, trace) =>
      JdkResolution.Provision(major.value, trace.map(sight))

  private def matched(chosen: Candidate.Match): JdkMatch =
    JdkMatch(place(chosen.origin), chosen.home.toString, chosen.version.value)

  private def sight(candidate: Candidate): JdkSight = candidate match
    case Candidate.Match(origin, home, version) => JdkSight.Match(place(origin), home.toString, version.value)
    case Candidate.Rejected(origin, reason)     => JdkSight.Rejected(place(origin), skip(reason))
    case Candidate.NotPresent(origin)           => JdkSight.NotPresent(place(origin))
    case Candidate.Empty(origin)                => JdkSight.Empty(place(origin))

  private def place(origin: Origin): JdkPlace = origin match
    case Origin.Override => JdkPlace.Override
    case Origin.JavaHome => JdkPlace.JavaHome
    case Origin.Path     => JdkPlace.Path
    case Origin.Coursier => JdkPlace.Coursier
    case Origin.Sdkman   => JdkPlace.Sdkman
    case Origin.Cache    => JdkPlace.Cache

  private def skip(reason: Reject): JdkSkip = reason match
    case Reject.Absent                   => JdkSkip.Absent
    case Reject.NoJavac                  => JdkSkip.NoJavac
    case Reject.WrongMajor(found)        => JdkSkip.WrongMajor(found.value)
    case Reject.UnreadableVersion(error) => JdkSkip.Unreadable(error.message)

  private def command(hint: InstallHint): String = hint.command

  private def resolveNode(resolution: DecidedNode): NodeResolution =
    resolution match
      case DecidedNode.Selected(chosen, trace) =>
        NodeResolution.Selected(matchedNode(chosen), trace.map(nodeSight))
      case DecidedNode.OverrideRejected(reason, trace) =>
        NodeResolution.OverrideRejected(nodeSkip(reason), trace.map(nodeSight))
      case DecidedNode.Install(hint, trace) =>
        NodeResolution.Install(nodeCommand(hint), trace.map(nodeSight))
      case DecidedNode.Provision(major, trace) =>
        NodeResolution.Provision(major.value, trace.map(nodeSight))

  private def matchedNode(chosen: NodeCandidate.Match): NodeMatch =
    NodeMatch(nodePlace(chosen.origin), chosen.home.toString, chosen.version.value)

  private def nodeSight(candidate: NodeCandidate): NodeSight = candidate match
    case NodeCandidate.Match(origin, home, version) => NodeSight.Match(nodePlace(origin), home.toString, version.value)
    case NodeCandidate.Rejected(origin, reason)     => NodeSight.Rejected(nodePlace(origin), nodeSkip(reason))
    case NodeCandidate.NotPresent(origin)           => NodeSight.NotPresent(nodePlace(origin))
    case NodeCandidate.Empty(origin)                => NodeSight.Empty(nodePlace(origin))

  private def nodePlace(origin: NodeOrigin): NodePlace = origin match
    case NodeOrigin.Override => NodePlace.Override
    case NodeOrigin.Path     => NodePlace.Path
    case NodeOrigin.Nvm      => NodePlace.Nvm
    case NodeOrigin.Fnm      => NodePlace.Fnm
    case NodeOrigin.Volta    => NodePlace.Volta
    case NodeOrigin.Cache    => NodePlace.Cache

  private def nodeSkip(reason: NodeReject): NodeSkip = reason match
    case NodeReject.Absent                   => NodeSkip.Absent
    case NodeReject.NoBinary                 => NodeSkip.NoBinary
    case NodeReject.WrongMajor(found)        => NodeSkip.WrongMajor(found.value)
    case NodeReject.UnreadableVersion(error) => NodeSkip.Unreadable(error.message)

  private def nodeCommand(hint: NodeInstall): String = hint.command
