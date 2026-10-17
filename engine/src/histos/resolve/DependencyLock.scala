package histos.resolve

import histos.jdk.Sha256
import histos.{DepKind, Platform, ProjectName, ScalaVersion}

/** One direct dependency, in graph order. */
final case class LockedDirect(organization: String, name: String, revision: String, kind: DepKind)

/** One classpath jar. `name` is the name after the Scala and platform suffix. */
final case class LockedArtifact(
    organization: String,
    name: String,
    revision: String,
    sha256: Sha256,
    repository: Repository,
)

/** One module, Scala version, and platform. An aggregate has no cell. */
final case class LockedCell(
    module: ProjectName,
    scalaVersion: ScalaVersion,
    platform: Platform,
    directs: List[LockedDirect],
    artifacts: List[LockedArtifact],
    evictions: List[Eviction],
)

/** The committed `.histos/dependencies.lock`. Cells stay in file order. */
final case class DependencyLock(cells: List[LockedCell])

object PlatformToken:
  def render(platform: Platform): String =
    platform match
      case Platform.Jvm    => "jvm"
      case Platform.Js     => "js"
      case Platform.Native => "native"

  def parse(raw: String): Option[Platform] =
    raw match
      case "jvm"    => Some(Platform.Jvm)
      case "js"     => Some(Platform.Js)
      case "native" => Some(Platform.Native)
      case _        => None

object DependencyLock:
  def parse(text: String): Either[LockError, DependencyLock] =
    val lines = text.linesIterator.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#")).toList
    if lines.isEmpty then Left(LockError.Empty)
    else lines.foldLeft[Either[LockError, Acc]](Right(Acc()))(step).flatMap(build)

  def render(lock: DependencyLock): String =
    if lock.cells.isEmpty then ""
    else lock.cells.map(renderCell).mkString("\n") + "\n"

  def renderCell(cell: LockedCell): String =
    val header = s"cell ${cell.module.value} ${cell.scalaVersion.value} ${PlatformToken.render(cell.platform)}"
    val body   =
      cell.directs.map(renderDirect) ++ cell.artifacts.map(renderArtifact) ++ cell.evictions.map(renderEvict)
    (header :: body.toList).mkString("\n")

  private def renderDirect(direct: LockedDirect): String =
    s"direct ${direct.organization} ${direct.name} ${direct.revision} ${kind(direct.kind)}"

  private def renderArtifact(artifact: LockedArtifact): String =
    s"artifact ${artifact.organization} ${artifact.name} ${artifact.revision} ${artifact.sha256.value} ${Repository.token(artifact.repository)}"

  private def renderEvict(eviction: Eviction): String =
    s"evict ${eviction.organization} ${eviction.name} ${eviction.requested} ${eviction.selected}"

  private def kind(kind: DepKind): String =
    kind match
      case DepKind.Java   => "java"
      case DepKind.Binary => "binary"
      case DepKind.Full   => "full"

  private final case class Building(
      module: ProjectName,
      scalaVersion: ScalaVersion,
      platform: Platform,
      directs: List[LockedDirect],
      artifacts: List[LockedArtifact],
      evictions: List[Eviction],
  )

  private final case class Acc(done: List[LockedCell] = Nil, open: Option[Building] = None)

  private def step(acc: Either[LockError, Acc], line: String): Either[LockError, Acc] =
    acc.flatMap(state => applyLine(state, line))

  private def applyLine(state: Acc, line: String): Either[LockError, Acc] =
    tokens(line) match
      case Nil => Left(LockError.BadLine(line))
      case head :: rest =>
        LockField.from(head) match
          case None        => Left(LockError.BadLine(line))
          case Some(field) =>
            val need = values(field)
            if rest.length < need then Left(LockError.Missing(field))
            else if rest.length > need then Left(LockError.BadLine(line))
            else accept(state, field, rest, line)

  private def values(field: LockField): Int =
    field match
      case LockField.Cell     => 3
      case LockField.Direct   => 4
      case LockField.Artifact => 5
      case LockField.Evict    => 4

  private def accept(state: Acc, field: LockField, rest: List[String], line: String): Either[LockError, Acc] =
    field match
      case LockField.Cell     => open(state, rest, line)
      case LockField.Direct   => withOpen(state, line)((cell, values) => direct(cell, values))
      case LockField.Artifact => withOpen(state, line)((cell, values) => artifact(cell, values))
      case LockField.Evict    => withOpen(state, line)((cell, values) => evict(cell, values))

  private def withOpen(state: Acc, line: String)(
      fill: (Building, List[String]) => Either[LockError, Building],
  ): Either[LockError, Acc] =
    tokens(line) match
      case _ :: rest =>
        state.open match
          case None       => Left(LockError.Missing(LockField.Cell))
          case Some(cell) => fill(cell, rest).map(next => state.copy(open = Some(next)))
      case Nil => Left(LockError.BadLine(line))

  private def open(state: Acc, rest: List[String], line: String): Either[LockError, Acc] =
    rest match
      case moduleRaw :: versionRaw :: platformRaw :: Nil =>
        val parsed =
          for
            module   <- ProjectName.from(moduleRaw).left.map(_ => LockError.BadLine(line))
            version  <- ScalaVersion.from(versionRaw).left.map(_ => LockError.BadLine(line))
            platform <- PlatformToken.parse(platformRaw).toRight(LockError.BadLine(line))
          yield (module, version, platform)
        parsed.flatMap: (module, version, platform) =>
          val closed = finish(state)
          if closed.exists(cell => cell.module == module && cell.scalaVersion == version && cell.platform == platform) then
            Left(LockError.DuplicateCell(module, version, platform))
          else
            Right(Acc(closed, Some(Building(module, version, platform, Nil, Nil, Nil))))
      case _ => Left(LockError.BadLine(line))

  private def direct(cell: Building, rest: List[String]): Either[LockError, Building] =
    rest match
      case organization :: name :: revision :: kindRaw :: Nil =>
        kindOf(kindRaw) match
          case None       => Left(LockError.BadLine(s"direct $organization $name $revision $kindRaw"))
          case Some(kind) => Right(cell.copy(directs = cell.directs :+ LockedDirect(organization, name, revision, kind)))
      case _ => Left(LockError.Missing(LockField.Direct))

  private def artifact(cell: Building, rest: List[String]): Either[LockError, Building] =
    rest match
      case organization :: name :: revision :: shaRaw :: repoRaw :: Nil =>
        for
          sha        <- Sha256.from(shaRaw).left.map(_ => LockError.BadSha256(shaRaw))
          repository <- Repository.parse(repoRaw)
        yield cell.copy(artifacts = cell.artifacts :+ LockedArtifact(organization, name, revision, sha, repository))
      case _ => Left(LockError.Missing(LockField.Artifact))

  private def evict(cell: Building, rest: List[String]): Either[LockError, Building] =
    rest match
      case organization :: name :: requested :: selected :: Nil =>
        Right(cell.copy(evictions = cell.evictions :+ Eviction(organization, name, requested, selected)))
      case _ => Left(LockError.Missing(LockField.Evict))

  private def kindOf(raw: String): Option[DepKind] =
    raw match
      case "java"   => Some(DepKind.Java)
      case "binary" => Some(DepKind.Binary)
      case "full"   => Some(DepKind.Full)
      case _        => None

  private def finish(state: Acc): List[LockedCell] =
    state.open match
      case None       => state.done
      case Some(cell) => state.done :+ LockedCell(cell.module, cell.scalaVersion, cell.platform, cell.directs, cell.artifacts, cell.evictions)

  private def build(state: Acc): Either[LockError, DependencyLock] =
    val cells = finish(state)
    if cells.isEmpty then Left(LockError.Empty) else Right(DependencyLock(cells))

  private def tokens(line: String): List[String] =
    line.split("\\s+").toList.filter(_.nonEmpty)
