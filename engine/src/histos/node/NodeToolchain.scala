package histos.node

import histos.jdk.Machine
import histos.toolchain.{LocalOverride, LocalOverrideError, RuntimeFiles, RuntimeIdentity}
import histos.workspace.{IoFailure, IoFault}
import histos.NodePath

import java.nio.file.{Files, Path}

import zio.{durationInt, IO, UIO, ZIO}

final case class NodeRuntime(
    binary: NodeBinary,
    version: NodeVersion,
    identity: RuntimeIdentity,
)

/** The Node binary that was accepted, and the source that named it. */
final case class NodeChoice(runtime: NodeRuntime, source: NodeSource)

/** One `node` binary found on `PATH`, in search order. */
final case class NodeSight(binary: Path, version: NodeVersion)

/** The first `node` on `PATH` is older than a later one. */
final case class NodeShadow(first: NodeSight, newer: NodeSight)

object NodeToolchain:
  /** Scala.js Wasm needs Node 25, so every Js launch uses that floor. */
  val Floor: NodeVersion =
    NodeVersion.trusted("25", NodeMajor.trusted(25))

  /** Absolute path of a `node` binary. Set, it wins over the build, `node.local`, and `PATH`. */
  val Key: String = "HISTOS_NODE"

  def explain(machine: Machine, nodePath: Option[NodePath]): IO[NodeMiss, NodeSurvey] =
    resolve(machine, nodePath).map(runtime => NodeSurvey(runtime.binary, runtime.version, runtime.identity))

  def text(survey: NodeSurvey): String =
    s"node ${survey.version.value} ${survey.binary.path}"

  /** The name a doctor line uses. `node.local` is the file `.histos/node.local`. */
  def origin(source: NodeSource): String = source match
    case NodeSource.Environment => "HISTOS_NODE"
    case NodeSource.Config      => "runnerTools.nodePath"
    case NodeSource.Path        => "PATH"
    case NodeSource.Local       => ".histos/node.local"

  /** Names `HISTOS_NODE`, `.histos/node.local`, and how to install Node 25. The miss text alone does not. */
  def failure(error: NodeMiss): String =
    s"${error.message}. $guidance"

  val guidance: String =
    "Set HISTOS_NODE to the absolute node binary, or write the Node 25 home to .histos/node.local. Install Node 25 from https://nodejs.org/. Histos does not fetch Node and does not try the next source."

  /** `HISTOS_NODE`, then a [[NodePath]] on the build, then the home in `node.local`, then `node` on `PATH`. A named
    * binary that is missing, not executable, or older than [[Floor]] fails. Histos does not fetch Node and does not try
    * the next source.
    */
  def choose(machine: Machine, nodePath: Option[NodePath]): IO[NodeMiss, NodeChoice] =
    machine.env.get(Key) match
      case Some(raw) =>
        ZIO
          .fromEither(NodePath.from(raw))
          .mapError(error => NodeMiss.Refused(NodeSource.Environment, error))
          .flatMap(chosen => acceptBinary(Path.of(chosen.value), NodeSource.Environment))
      case None =>
        nodePath match
          case Some(chosen) => acceptBinary(Path.of(chosen.value), NodeSource.Config)
          case None         => inherited(machine)

  def resolve(machine: Machine, nodePath: Option[NodePath]): IO[NodeMiss, NodeRuntime] =
    choose(machine, nodePath).map(_.runtime)

  /** Every executable `node` on `PATH`, first to last. A directory that is not a binary is skipped, and so is a binary
    * that does not answer `--version`.
    */
  def sights(machine: Machine): UIO[List[NodeSight]] =
    val entries =
      machine.env
        .get("PATH")
        .toList
        .flatMap(_.split(java.io.File.pathSeparator).toList)
        .filter(_.nonEmpty)
    ZIO.foreachPar(entries)(sight).map(distinct)

  /** The first binary is below [[Floor]] and a later binary is newer. The newer one is the highest version. */
  def shadow(sights: List[NodeSight]): Option[NodeShadow] =
    sights match
      case first :: rest =>
        val tooOld =
          !histos.toolchain.VersionOrder.atLeast(first.version.value, Floor.value, stripJavaOne = false)
        val newer = rest.filter(item =>
          histos.toolchain.VersionOrder.compare(item.version.value, first.version.value) > 0
        )
        if !tooOld || newer.isEmpty then None
        else
          val best = newer
            .sortWith((left, right) =>
              histos.toolchain.VersionOrder.compare(left.version.value, right.version.value) > 0
            )
            .head
          Some(NodeShadow(first, best))
      case Nil => None

  private def inherited(machine: Machine): IO[NodeMiss, NodeChoice] =
    readOverride(machine.repo)
      .mapError(NodeMiss.Unresolved(_))
      .flatMap:
        case Some(home) => acceptHome(home, NodeSource.Local)
        case None       => onPath(machine)

  private def onPath(machine: Machine): IO[NodeMiss, NodeChoice] =
    val name = "node"
    RuntimeFiles
      .firstOnPath(machine.env.get("PATH"), List(name))
      .mapError(error => NodeMiss.Unresolved(NodeError.Locations(error)))
      .flatMap:
        case Some(binary) =>
          RuntimeFiles.homeOf(binary) match
            case Some(home) => acceptHome(home, NodeSource.Path)
            case None       => ZIO.fail(NodeMiss.Absent(Floor, NodeSource.Path))
        case None => ZIO.fail(NodeMiss.Absent(Floor, NodeSource.Path))

  private def acceptBinary(path: Path, source: NodeSource): IO[NodeMiss, NodeChoice] =
    ready(path, source).flatMap: binary =>
      adopt(binary, source, placed(binary))

  private def acceptHome(home: Path, source: NodeSource): IO[NodeMiss, NodeChoice] =
    ready(home.resolve("bin").resolve("node"), source).flatMap: binary =>
      adopt(binary, source, home)

  private def adopt(binary: Path, source: NodeSource, home: Path): IO[NodeMiss, NodeChoice] =
    for
      version <- NodeProbe
        .version(binary)
        .mapError:
          case NodeError.Version(_, error) =>
            NodeMiss.Rejected(binary, NodeReject.UnreadableVersion(error), source)
          case other => NodeMiss.Unresolved(other)
      _ <- ZIO
        .fail(NodeMiss.TooOld(Floor, version, home, source))
        .unless(histos.toolchain.VersionOrder.atLeast(version.value, Floor.value, stripJavaOne = false))
      identity <- RuntimeFiles
        .identity(home, version.value)
        .mapError(failure => NodeMiss.Unresolved(NodeError.Io(failure)))
    yield NodeChoice(NodeRuntime(NodeBinary(binary), version, identity), source)

  private def sight(entry: String): UIO[Option[NodeSight]] =
    ZIO
      .attemptBlocking:
        val file = Path.of(entry).resolve("node")
        if Files.isRegularFile(file) && Files.isExecutable(file) then Some(file.toRealPath().normalize) else None
      .orElseSucceed(None)
      .flatMap:
        case None         => ZIO.succeed(None)
        case Some(binary) =>
          versionOf(binary)
            .map(version => Some(NodeSight(binary, version)))
            .timeout(2.seconds)
            .map(_.flatten)
            .orElseSucceed(None)

  /** One PATH entry. A JVM launch per binary spends the doctor check budget before `--version` returns. */
  private def versionOf(binary: Path): IO[NodeError, NodeVersion] =
    histos.process.Child
      .run(spawnVersion(binary), NodeError.Version(binary, NodeVersionError.MissingStatus))
      .flatMap: ended =>
        val text = ended.output.trim
        if ended.status == 0 then
          ZIO.fromEither(NodeVersion.from(text)).mapError(error => NodeError.Version(binary, error))
        else ZIO.fail(NodeError.Version(binary, NodeVersionError.Status(ended.status)))

  private def spawnVersion(binary: Path): IO[NodeError, Process] =
    ZIO
      .attemptBlocking {
        new ProcessBuilder(binary.toString, "--version")
          .redirectErrorStream(true)
          .redirectInput(ProcessBuilder.Redirect.from(new java.io.File("/dev/null")))
          .start()
      }
      .mapError(_ => NodeError.Version(binary, NodeVersionError.Missing))

  private def distinct(found: List[Option[NodeSight]]): List[NodeSight] =
    found.flatten
      .foldLeft((Set.empty[Path], List.empty[NodeSight])):
        case ((seen, acc), item) if seen.contains(item.binary) => (seen, acc)
        case ((seen, acc), item)                               => (seen + item.binary, acc :+ item)
      ._2

  /** A `bin/node` layout keeps the home. Any other file is its own identity. */
  private def placed(binary: Path): Path =
    Option(binary.getParent)
      .filter(parent => Option(parent.getFileName).exists(_.toString == "bin"))
      .flatMap(parent => Option(parent.getParent))
      .getOrElse(binary)

  private def ready(path: Path, source: NodeSource): IO[NodeMiss, Path] =
    ZIO
      .attemptBlocking(Files.exists(path))
      .mapError(_ => NodeMiss.Unresolved(NodeError.Io(IoFailure(path, IoFault.Stat))))
      .flatMap:
        case false => ZIO.fail(NodeMiss.Unusable(path, source, NodeFileFault.Missing))
        case true  =>
          ZIO
            .attemptBlocking(path.toRealPath().normalize)
            .mapError(_ => NodeMiss.Unresolved(NodeError.Io(IoFailure(path, IoFault.Stat))))
            .flatMap: canonical =>
              ZIO
                .attemptBlocking((Files.isRegularFile(canonical), Files.isExecutable(canonical)))
                .mapError(_ => NodeMiss.Unresolved(NodeError.Io(IoFailure(canonical, IoFault.Stat))))
                .flatMap:
                  case (false, _)    => ZIO.fail(NodeMiss.Unusable(path, source, NodeFileFault.NotAFile))
                  case (true, false) => ZIO.fail(NodeMiss.Unusable(path, source, NodeFileFault.NotExecutable))
                  case (true, true)  => ZIO.succeed(canonical)

  private def readOverride(repo: Path): IO[NodeError, Option[Path]] =
    LocalOverride
      .read(repo.resolve(".histos/node.local"))
      .mapError:
        case failure: IoFailure       => NodeError.Io(failure)
        case LocalOverrideError.Empty =>
          NodeError.Override(NodeOverrideError.Empty)
        case LocalOverrideError.Multiple =>
          NodeError.Override(NodeOverrideError.Multiple)
        case LocalOverrideError.Relative(raw) =>
          NodeError.Override(NodeOverrideError.Relative(raw))
        case LocalOverrideError.Invalid(raw) =>
          NodeError.Override(NodeOverrideError.Invalid(raw))
end NodeToolchain
