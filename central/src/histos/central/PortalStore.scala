package histos.central

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

import zio.{Chunk, IO, ZIO}

/** Deployments on disk. Promote and drop are file writes, so a second process can run them. */
object PortalStore:
  final case class Record(state: DeploymentState, findings: Chunk[Finding], gavs: Chunk[Gav])

  /** `Read` and `Write` name the path that failed. An id or a gav that would leave the store fails as its call. */
  enum Error:
    case Read(path: Path)
    case Write(path: Path)
    case Missing(id: DeploymentId)
    case NotPromotable(id: DeploymentId)
    case Unreadable(path: Path, fault: Fault)

  /** Why a stored deployment does not decode. */
  enum Fault:
    case State(token: String)
    case Findings(error: FindingError)
    case Coordinates(error: GavError)

  def published(root: Path): IO[Error, Set[Gav]] =
    blocking(Error.Read(root)):
      val base = root.resolve("published")
      if !Files.exists(base) then Right(Set.empty)
      else
        val walk = Files.walk(base)
        try
          val found = Set.newBuilder[Gav]
          val items = walk.iterator()
          while items.hasNext do
            marker(base, items.next()) match
              case Some(gav) => found += gav
              case None      => ()
          Right(found.result())
        finally walk.close()

  def read(root: Path, id: DeploymentId): IO[Error, Option[Record]] =
    folder(root, id, Error.Read(_)) match
      case Left(error) => ZIO.fail(error)
      case Right(dir)  =>
        blocking(Error.Read(dir)):
          if !Files.isDirectory(dir) then Right(None)
          else
            for
              state  <- text(dir.resolve("state"))
              found  <- text(dir.resolve("report"))
              gavs   <- text(dir.resolve("gavs"))
              record <- decode(dir, state, found, gavs)
            yield Some(record)

  def create(root: Path, id: DeploymentId, record: Record): IO[Error, Unit] =
    val stored =
      if record.state == DeploymentState.Published then mark(root, record.gavs) *> write(root, id, record)
      else write(root, id, record)
    stored

  def promote(root: Path, id: DeploymentId): IO[Error, Unit] =
    require(root, id).flatMap: record =>
      record.state match
        case DeploymentState.Validated =>
          mark(root, record.gavs) *> write(root, id, record.copy(state = DeploymentState.Published))
        case DeploymentState.Published => ZIO.unit
        case _                         => ZIO.fail(Error.NotPromotable(id))

  def drop(root: Path, id: DeploymentId): IO[Error, Unit] =
    require(root, id).flatMap: record =>
      val unmarked =
        if record.state == DeploymentState.Published then unmark(root, record.gavs) else ZIO.unit
      unmarked *> remove(root, id)

  private def require(root: Path, id: DeploymentId): IO[Error, Record] =
    read(root, id).flatMap:
      case Some(record) => ZIO.succeed(record)
      case None         => ZIO.fail(Error.Missing(id))

  private def write(root: Path, id: DeploymentId, record: Record): IO[Error, Unit] =
    folder(root, id, Error.Write(_)) match
      case Left(error) => ZIO.fail(error)
      case Right(dir)  =>
        for
          _ <- atomic(dir.resolve("state"), wire(record.state) + "\n")
          _ <- atomic(
            dir.resolve("report"),
            record.findings.map(_.token).mkString("", "\n", if record.findings.isEmpty then "" else "\n"),
          )
          _ <- atomic(
            dir.resolve("gavs"),
            record.gavs.map(_.render).mkString("", "\n", if record.gavs.isEmpty then "" else "\n"),
          )
        yield ()

  private def mark(root: Path, gavs: Chunk[Gav]): IO[Error, Unit] =
    ZIO.foreachDiscard(gavs)(gav => placed(root, gav).flatMap(path => atomic(path, "")))

  private def unmark(root: Path, gavs: Chunk[Gav]): IO[Error, Unit] =
    ZIO.foreachDiscard(gavs): gav =>
      placed(root, gav).flatMap: path =>
        blocking(Error.Write(path))(Right(Files.deleteIfExists(path))).unit

  private def remove(root: Path, id: DeploymentId): IO[Error, Unit] =
    folder(root, id, Error.Write(_)) match
      case Left(error) => ZIO.fail(error)
      case Right(dir)  =>
        blocking(Error.Write(dir)):
          if !Files.exists(dir) then Right(())
          else
            val walk = Files.walk(dir)
            try
              val paths = List.newBuilder[Path]
              val items = walk.iterator()
              while items.hasNext do paths += items.next()
              paths.result().sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
              Right(())
            finally walk.close()

  /** Only a mark or an unmark asks, so a gav that leaves the store is a write. */
  private def placed(root: Path, gav: Gav): IO[Error, Path] =
    ZIO.fromEither:
      val base   = root.resolve("published").toAbsolutePath.normalize
      val target = gav.directory.split("/", -1).foldLeft(base)(_.resolve(_)).normalize
      if target.startsWith(base) then Right(target) else Left(Error.Write(root))

  private def folder(root: Path, id: DeploymentId, refused: Path => Error): Either[Error, Path] =
    val name = id.value
    if name.isEmpty || name.contains('/') || name.contains('\\') || name == "." || name == ".." then Left(refused(root))
    else
      val base = root.resolve("deployments").toAbsolutePath.normalize
      val dir  = base.resolve(name).normalize
      if dir.startsWith(base) then Right(dir) else Left(refused(root))

  private def marker(base: Path, path: Path): Option[Gav] =
    if !Files.isRegularFile(path) then None
    else
      val relative = base.relativize(path)
      val names    = (0 until relative.getNameCount).toList.map(index => relative.getName(index).toString)
      names match
        case group :+ artifact :+ version if group.nonEmpty =>
          Some(Gav(group.mkString("."), artifact, version))
        case _ => None

  private def decode(dir: Path, state: String, report: String, gavs: String): Either[Error, Record] =
    val decoded =
      for
        found       <- stored(state)
        findings    <- every(lines(report))(Finding.from).left.map(Fault.Findings(_))
        coordinates <- every(lines(gavs))(Gav.from).left.map(Fault.Coordinates(_))
      yield Record(found, findings, coordinates)
    decoded.left.map(Error.Unreadable(dir, _))

  /** Only the states a stored deployment can rest in. */
  private def stored(state: String): Either[Fault, DeploymentState] =
    state.trim match
      case "VALIDATED" => Right(DeploymentState.Validated)
      case "PUBLISHED" => Right(DeploymentState.Published)
      case "FAILED"    => Right(DeploymentState.Failed)
      case other       => Left(Fault.State(other))

  /** The first line that does not parse, or every parsed line. */
  private def every[E, A](rows: Chunk[String])(parse: String => Either[E, A]): Either[E, Chunk[A]] =
    val (bad, good) = rows.partitionMap(parse)
    bad.headOption.toLeft(good)

  private def lines(text: String): Chunk[String] =
    if text.isEmpty then Chunk.empty
    else Chunk.fromArray(text.split("\n", -1)).filter(_.nonEmpty)

  private def text(path: Path): Either[Error, String] =
    try Right(Files.readString(path, StandardCharsets.UTF_8))
    catch case _: IOException => Left(Error.Read(path))

  private def wire(state: DeploymentState): String = state match
    case DeploymentState.Pending    => "PENDING"
    case DeploymentState.Validating => "VALIDATING"
    case DeploymentState.Validated  => "VALIDATED"
    case DeploymentState.Publishing => "PUBLISHING"
    case DeploymentState.Published  => "PUBLISHED"
    case DeploymentState.Failed     => "FAILED"

  private def atomic(path: Path, text: String): IO[Error, Unit] =
    blocking(Error.Write(path))(writeAtomically(path, text))

  private def writeAtomically(path: Path, text: String): Either[Error, Unit] =
    val parent = path.getParent
    if parent == null then Left(Error.Write(path))
    else
      try
        Files.createDirectories(parent)
        val tmp = Files.createTempFile(parent, "." + path.getFileName.toString, ".tmp")
        try
          Files.writeString(tmp, text, StandardCharsets.UTF_8)
          move(tmp, path)
          Right(())
        catch
          case _: IOException =>
            Files.deleteIfExists(tmp)
            Left(Error.Write(path))
      catch case _: IOException => Left(Error.Write(path))

  private def move(tmp: Path, path: Path): Unit =
    try Files.move(tmp, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch
      case _: AtomicMoveNotSupportedException =>
        Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING)

  /** `failed` is the error when `body` throws. A `Left` from `body` is kept. */
  private def blocking[A](failed: Error)(body: => Either[Error, A]): IO[Error, A] =
    ZIO.attemptBlocking(body).mapError(_ => failed).flatMap(ZIO.fromEither(_))
end PortalStore
