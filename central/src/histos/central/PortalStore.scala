package histos.central

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

import zio.{Chunk, IO, ZIO}

/** Deployments on disk. Promote and drop are file writes, so a second process can run them. */
object PortalStore:
  final case class Record(state: DeploymentState, findings: Chunk[Finding], gavs: Chunk[Gav])

  enum Error:
    case Io(path: Path)
    case Missing(id: DeploymentId)
    case NotPromotable(id: DeploymentId)

  def published(root: Path): IO[Error, Set[Gav]] =
    blocking(root):
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
    folder(root, id) match
      case Left(error) => ZIO.fail(error)
      case Right(dir)  =>
        blocking(dir):
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
    folder(root, id) match
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
        blocking(path)(Right(Files.deleteIfExists(path))).unit

  private def remove(root: Path, id: DeploymentId): IO[Error, Unit] =
    folder(root, id) match
      case Left(error) => ZIO.fail(error)
      case Right(dir)  =>
        blocking(dir):
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

  private def placed(root: Path, gav: Gav): IO[Error, Path] =
    ZIO.fromEither:
      val base   = root.resolve("published").toAbsolutePath.normalize
      val target = gav.directory.split("/", -1).foldLeft(base)(_.resolve(_)).normalize
      if target.startsWith(base) then Right(target) else Left(Error.Io(root))

  private def folder(root: Path, id: DeploymentId): Either[Error, Path] =
    val name = id.value
    if name.isEmpty || name.contains('/') || name.contains('\\') || name == "." || name == ".." then
      Left(Error.Io(root))
    else
      val base = root.resolve("deployments").toAbsolutePath.normalize
      val dir  = base.resolve(name).normalize
      if dir.startsWith(base) then Right(dir) else Left(Error.Io(root))

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
    val parsed = state.trim match
      case "VALIDATED" => Some(DeploymentState.Validated)
      case "PUBLISHED" => Some(DeploymentState.Published)
      case "FAILED"    => Some(DeploymentState.Failed)
      case _           => None
    parsed match
      case None        => Left(Error.Io(dir))
      case Some(found) =>
        val findings    = lines(report).map(Finding.from)
        val coordinates = lines(gavs).map(Gav.from)
        if findings.exists(_.isEmpty) || coordinates.exists(_.isEmpty) then Left(Error.Io(dir))
        else
          Right(
            Record(
              found,
              findings.flatMap(item => Chunk.fromIterable(item.toList)),
              coordinates.flatMap(item => Chunk.fromIterable(item.toList)),
            )
          )

  private def lines(text: String): Chunk[String] =
    if text.isEmpty then Chunk.empty
    else Chunk.fromArray(text.split("\n", -1)).filter(_.nonEmpty)

  private def text(path: Path): Either[Error, String] =
    try Right(Files.readString(path, StandardCharsets.UTF_8))
    catch case _: IOException => Left(Error.Io(path))

  private def wire(state: DeploymentState): String = state match
    case DeploymentState.Pending    => "PENDING"
    case DeploymentState.Validating => "VALIDATING"
    case DeploymentState.Validated  => "VALIDATED"
    case DeploymentState.Publishing => "PUBLISHING"
    case DeploymentState.Published  => "PUBLISHED"
    case DeploymentState.Failed     => "FAILED"

  private def atomic(path: Path, text: String): IO[Error, Unit] =
    ZIO.attemptBlocking(writeAtomically(path, text)).mapError(_ => Error.Io(path)).flatMap(ZIO.fromEither(_))

  private def writeAtomically(path: Path, text: String): Either[Error, Unit] =
    val parent = path.getParent
    if parent == null then Left(Error.Io(path))
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
            Left(Error.Io(path))
      catch case _: IOException => Left(Error.Io(path))

  private def move(tmp: Path, path: Path): Unit =
    try Files.move(tmp, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
    catch
      case _: AtomicMoveNotSupportedException =>
        Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING)

  private def blocking[A](path: Path)(body: => Either[Error, A]): IO[Error, A] =
    ZIO.attemptBlocking(body).mapError(_ => Error.Io(path)).flatMap(ZIO.fromEither(_))
end PortalStore
