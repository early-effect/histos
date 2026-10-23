package histos.node

import histos.jdk.{ArchiveSource, Machine}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO}

/** The lock that was read and the decision it produced. Printing is [[NodeToolchain.text]]. */
final case class NodeSurvey(lock: NodeLock, resolution: NodeResolution)

object NodeToolchain:
  /** Read the Node decision. Does not download. A missing lock is an error: the caller already knows Js needs Node. */
  def explain(machine: Machine): IO[NodeError, NodeSurvey] =
    readLock(machine.repo).flatMap:
      case None       => ZIO.fail(NodeError.NoLock(lockPath(machine.repo)))
      case Some(lock) => resolution(machine, lock).map(found => NodeSurvey(lock, found))

  /** The `node` a Js launch runs: the one selected, or the locked archive staged into the cache. */
  def choose(machine: Machine): IO[NodeMiss, NodeBinary] =
    for
      survey <- explain(machine).mapError(NodeMiss.Unresolved(_))
      home <- survey.resolution match
        case NodeResolution.Selected(chosen, _)         => ZIO.succeed(chosen.home)
        case NodeResolution.OverrideRejected(reason, _) => ZIO.fail(NodeMiss.Rejected(reason))
        case NodeResolution.Install(hint, _)            => ZIO.fail(NodeMiss.Install(hint))
        case NodeResolution.Provision(_, _) =>
          NodeStage.stage(survey.lock, machine).mapBoth(error => NodeMiss.Unresolved(NodeError.Stage(error)), _.path)
      binary <- NodeProbe.binary(home).mapError(NodeMiss.Unresolved(_))
    yield binary

  def text(survey: NodeSurvey): String =
    render(survey.lock, survey.resolution)

  def render(lock: NodeLock, resolution: NodeResolution): String =
    val tail = resolution match
      case NodeResolution.Provision(_, _) => s"archive ${NodeLock.url(lock.source)}"
      case _                              => s"lock ${lock.version.value}"
    Node.report(resolution) + "\n" + tail

  private def resolution(machine: Machine, lock: NodeLock): IO[NodeError, NodeResolution] =
    for
      explicit <- readOverride(machine.repo)
      input    <- NodeLocations.probe(machine, explicit)
      view     <- NodeProbe.scan(input)
    yield Node.resolve(lock.major, view)

  private def readLock(repo: Path): IO[NodeError, Option[NodeLock]] =
    val file = lockPath(repo)
    ZIO.attempt(Files.isRegularFile(file)).mapError(_ => NodeError.Unreadable(file)).flatMap:
      case false => ZIO.none
      case true =>
        ZIO.attempt(Files.readString(file)).mapError(_ => NodeError.Unreadable(file)).flatMap: text =>
          ZIO.fromEither(NodeLock.parse(text)).mapBoth(NodeError.Lock(_), Some(_))

  private def readOverride(repo: Path): IO[NodeError, Option[Path]] =
    val file = repo.resolve(".histos").resolve("node.local")
    ZIO.attempt(Files.isRegularFile(file)).mapError(_ => NodeError.Unreadable(file)).flatMap:
      case false => ZIO.none
      case true =>
        ZIO.attempt(Files.readString(file)).mapError(_ => NodeError.Unreadable(file)).flatMap: text =>
          ZIO.fromEither(parseOverride(text)).mapBoth(NodeError.Override(_), Some(_))

  private def parseOverride(text: String): Either[NodeOverrideError, Path] =
    val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    lines match
      case Nil => Left(NodeOverrideError.Empty)
      case raw :: Nil =>
        try
          val path = Path.of(raw)
          if path.isAbsolute then Right(path) else Left(NodeOverrideError.Relative(raw))
        catch case _: java.nio.file.InvalidPathException => Left(NodeOverrideError.Invalid(raw))
      case _ => Left(NodeOverrideError.Multiple)

  private def lockPath(repo: Path): Path =
    repo.resolve(".histos").resolve("node.lock")
