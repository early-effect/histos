package histos.zinc

import histos.cache.ContentStore
import histos.jdk.Sha256
import histos.schedule.{CacheError, Digest}
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  DataInputStream,
  DataOutputStream,
  IOException,
  UncheckedIOException,
}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.control.NonFatal

import sbt.internal.inc.FileAnalysisStore

import zio.{Chunk, IO, Ref, UIO, URIO, ZIO, ZLayer}

/** Starts of the Scala compiler. A hit does not tick. The ref is shared across the fibers a compile forks. */
final case class CompilerStarts(count: Ref[Int]):
  def tick: UIO[Unit] = count.update(_ + 1)
  def get: UIO[Int]   = count.get

object CompilerStarts:
  val live: ZLayer[Any, Nothing, CompilerStarts] =
    ZLayer.fromZIO(Ref.make(0).map(CompilerStarts(_)))

  def tick: URIO[CompilerStarts, Unit] =
    ZIO.serviceWithZIO[CompilerStarts](_.tick)
end CompilerStarts

/** Analysis and class files stored under the compile key. `a/inc.zip` is the analysis. `c/` is one file from the
  * classes directory. The prefix keeps one from being read as the other.
  */
object CompileReplay:
  private val Magic        = "HC1\n".getBytes(StandardCharsets.US_ASCII)
  private val AnalysisName = "inc.zip"

  /** A store that dies with this process. A layer that took `Scope` would change every resolver input. */
  def processStore: ZLayer[Any, Nothing, ContentStore] =
    ZLayer.fromZIO(
      ZIO.attemptBlocking {
        val dir = Files.createTempDirectory("histos-compile-cas")
        Runtime.getRuntime.addShutdownHook(new Thread(() => discard(dir)))
        ContentStore.in(dir)
      }.orDie
    )

  /** One call, one store. A later call of the same key misses, so Zinc still sees the analysis on disk. */
  def isolated[R, E, A](
      effect: ZIO[R & ContentStore & CompilerStarts, E, A]
  ): ZIO[R, E, A] =
    ZIO.scoped:
      for
        dir <- ZIO.acquireRelease(
          ZIO.attemptBlocking(Files.createTempDirectory("histos-compile-cas")).orDie
        )(path => Workspace.delete(path).orDieWith(error => new java.io.IOException(error.message)))
        starts <- Ref.make(0)
        out    <- effect.provideSomeLayer(
          ZLayer.succeed(ContentStore.in(dir)) ++ ZLayer.succeed(CompilerStarts(starts))
        )
      yield out

  def load(
      store: ContentStore,
      key: Sha256,
      request: CompileRequest,
  ): IO[CompileError, Option[CompileOutput]] =
    keyed(key).flatMap: id =>
      store
        .read(id)
        .mapError(CompileError.Stored(_))
        .flatMap:
          case None        => ZIO.none
          case Some(bytes) => restore(id, bytes, key, request).map(Some(_))

  def save(
      store: ContentStore,
      key: Sha256,
      request: CompileRequest,
  ): IO[CompileError, Unit] =
    for
      id    <- keyed(key)
      bytes <- ZIO.fromEither(archive(request))
      // File mtime. TestClock.instant arms a warning unless the test moves the clock.
      now <- zio.Clock.ClockLive.instant
      _   <- store.put(id, bytes, now).mapError(CompileError.Stored(_))
    yield ()

  private def keyed(key: Sha256): IO[CompileError, Digest] =
    Digest.from(key.value) match
      case Some(found) => ZIO.succeed(found)
      case None        => ZIO.fail(CompileError.Io(IoFailure(Path.of(key.value), IoFault.Read)))

  private def discard(dir: Path): Unit =
    try Workspace.deleteSync(dir)
    catch case NonFatal(_) => ()

  private enum ArchiveFault:
    case Magic, Truncated, Path, Prefix, Duplicate, Missing

  private enum StoredKind:
    case Analysis(bytes: Array[Byte])
    case Classes(relative: String, bytes: Array[Byte])

  private final case class Decoded(analysis: Array[Byte], classes: List[(String, Array[Byte])])

  private def restore(
      id: Digest,
      bytes: Chunk[Byte],
      key: Sha256,
      request: CompileRequest,
  ): IO[CompileError, CompileOutput] =
    ZIO
      .fromEither(decode(bytes))
      .mapError(_ => CompileError.Stored(CacheError.Corrupt(id)))
      .flatMap: decoded =>
        ZIO.fromEither(install(request, decoded)) *>
          ZIO.fromEither(finish(id, key, request))

  private def archive(request: CompileRequest): Either[CompileError, Chunk[Byte]] =
    val zip = request.analysis.resolve(AnalysisName)
    try
      if !Files.isRegularFile(zip) then Left(CompileError.Io(IoFailure(zip, IoFault.Read)))
      else
        filesOf(request.classes).flatMap: paths =>
          val rows =
            (s"a/$AnalysisName", Files.readAllBytes(zip)) :: paths.map: path =>
              val relative = request.classes.toAbsolutePath.normalize
                .relativize(path.toAbsolutePath.normalize)
                .iterator
                .asScala
                .map(_.toString)
                .mkString("/")
              (s"c/$relative", Files.readAllBytes(path))
          encode(rows).left.map(_ => CompileError.Io(IoFailure(zip, IoFault.Read)))
    catch
      case _: IOException | _: UncheckedIOException => Left(CompileError.Io(IoFailure(request.classes, IoFault.Read)))

  private def encode(entries: List[(String, Array[Byte])]): Either[ArchiveFault, Chunk[Byte]] =
    val out  = new ByteArrayOutputStream
    val data = new DataOutputStream(out)
    data.write(Magic)
    entries
      .foldLeft[Either[ArchiveFault, Unit]](Right(())):
        case (Left(fault), _)         => Left(fault)
        case (Right(_), (path, body)) =>
          val raw = path.getBytes(StandardCharsets.UTF_8)
          if raw.length == 0 || raw.length > 65535 then Left(ArchiveFault.Path)
          else
            data.writeShort(raw.length)
            data.write(raw)
            data.writeInt(body.length)
            data.write(body)
            Right(())
      .map: _ =>
        data.flush()
        Chunk.fromArray(out.toByteArray)

  private def decode(bytes: Chunk[Byte]): Either[ArchiveFault, Decoded] =
    val in = new DataInputStream(new ByteArrayInputStream(bytes.toArray))
    magic(in).flatMap: _ =>
      readRest(in, Nil, None, Set.empty).map: (analysis, classes) =>
        Decoded(analysis, classes)

  private def magic(in: DataInputStream): Either[ArchiveFault, Unit] =
    try
      val found = new Array[Byte](Magic.length)
      in.readFully(found)
      if java.util.Arrays.equals(found, Magic) then Right(())
      else Left(ArchiveFault.Magic)
    catch case _: IOException => Left(ArchiveFault.Truncated)

  @tailrec
  private def readRest(
      in: DataInputStream,
      classes: List[(String, Array[Byte])],
      analysis: Option[Array[Byte]],
      seen: Set[String],
  ): Either[ArchiveFault, (Array[Byte], List[(String, Array[Byte])])] =
    if in.available() == 0 then
      analysis match
        case Some(bytes) => Right((bytes, classes.reverse))
        case None        => Left(ArchiveFault.Missing)
    else
      readOne(in) match
        case Left(fault)         => Left(fault)
        case Right((path, body)) =>
          if seen.contains(path) then Left(ArchiveFault.Duplicate)
          else
            classify(path, body) match
              case Left(fault)                       => Left(fault)
              case Right(StoredKind.Analysis(bytes)) =>
                analysis match
                  case Some(_) => Left(ArchiveFault.Duplicate)
                  case None    => readRest(in, classes, Some(bytes), seen + path)
              case Right(StoredKind.Classes(relative, bytes)) =>
                readRest(in, (relative, bytes) :: classes, analysis, seen + path)

  private def readOne(in: DataInputStream): Either[ArchiveFault, (String, Array[Byte])] =
    try
      val length = in.readUnsignedShort()
      if length == 0 then Left(ArchiveFault.Path)
      else
        val raw = new Array[Byte](length)
        in.readFully(raw)
        val size = in.readInt()
        if size < 0 || size > in.available() then Left(ArchiveFault.Truncated)
        else
          val body = new Array[Byte](size)
          in.readFully(body)
          Right((new String(raw, StandardCharsets.UTF_8), body))
    catch case _: IOException => Left(ArchiveFault.Truncated)

  private def classify(path: String, body: Array[Byte]): Either[ArchiveFault, StoredKind] =
    if path == s"a/$AnalysisName" then Right(StoredKind.Analysis(body))
    else if path.startsWith("c/") && safe(path.drop(2)) then Right(StoredKind.Classes(path.drop(2), body))
    else if path.startsWith("c/") then Left(ArchiveFault.Path)
    else Left(ArchiveFault.Prefix)

  private def safe(relative: String): Boolean =
    val parts = relative.split("/", -1).toList
    parts.nonEmpty && parts.forall(part => part.nonEmpty && part != "." && part != "..")

  private def install(request: CompileRequest, decoded: Decoded): Either[CompileError, Unit] =
    clear(request.analysis).flatMap: _ =>
      clear(request.classes).flatMap: _ =>
        writeFile(request.analysis.resolve(AnalysisName), decoded.analysis).flatMap: _ =>
          decoded.classes.foldLeft[Either[CompileError, Unit]](Right(())):
            case (Left(error), _)             => Left(error)
            case (Right(_), (relative, body)) =>
              place(request.classes, relative) match
                case None       => Left(CompileError.Io(IoFailure(request.classes, IoFault.Write)))
                case Some(path) => writeFile(path, body)

  private def finish(
      id: Digest,
      key: Sha256,
      request: CompileRequest,
  ): Either[CompileError, CompileOutput] =
    readAnalysis(request.analysis, id).flatMap: analysis =>
      ApiDigest
        .write(request.analysis, analysis)
        .flatMap: _ =>
          ProductDigest
            .write(request.analysis, request.classes)
            .flatMap: _ =>
              countProducts(request.classes).map: count =>
                CompileOutput(
                  request.classes,
                  count,
                  key,
                  changed = false,
                  sources = Some(SourceCount(0, request.sources.size)),
                  classesWritten = Some(0),
                )

  private def readAnalysis(
      dir: Path,
      id: Digest,
  ): Either[CompileError, sbt.internal.inc.Analysis] =
    val zip = dir.resolve(AnalysisName)
    try
      FileAnalysisStore.binary(zip.toFile).get().toScala match
        case Some(contents) =>
          contents.getAnalysis match
            case found: sbt.internal.inc.Analysis => Right(found)
            case _                                => Left(CompileError.Stored(CacheError.Corrupt(id)))
        case None => Left(CompileError.Stored(CacheError.Corrupt(id)))
    catch case NonFatal(_) => Left(CompileError.Stored(CacheError.Corrupt(id)))

  private def countProducts(root: Path): Either[CompileError, Int] =
    filesOf(root).map: paths =>
      paths.count: path =>
        Option(path.getFileName).exists: name =>
          val text = name.toString
          text.endsWith(".class") || text.endsWith(".sjsir") || text.endsWith(".nir")

  private def filesOf(root: Path): Either[CompileError, List[Path]] =
    if !Files.isDirectory(root) then Left(CompileError.Io(IoFailure(root, IoFault.Stat)))
    else
      try
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.filter(Files.isRegularFile(_)).toList
          finally walk.close()
        Right(paths.sortBy(_.toString))
      catch
        case _: IOException | _: UncheckedIOException =>
          Left(CompileError.Io(IoFailure(root, IoFault.Walk)))

  private def clear(root: Path): Either[CompileError, Unit] =
    try
      if Files.exists(root) && !Files.isDirectory(root) then Left(CompileError.Io(IoFailure(root, IoFault.Stat)))
      else
        Files.createDirectories(root)
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.filter(_ != root).sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
        Right(())
    catch
      case _: IOException | _: UncheckedIOException =>
        Left(CompileError.Io(IoFailure(root, IoFault.Write)))

  private def place(root: Path, relative: String): Option[Path] =
    if !safe(relative) then None
    else
      val dest = relative.split("/", -1).foldLeft(root)(_.resolve(_)).toAbsolutePath.normalize
      val base = root.toAbsolutePath.normalize
      val back = base.relativize(dest).iterator.asScala.exists(_.toString == "..")
      if back || dest == base then None else Some(dest)

  private def writeFile(path: Path, body: Array[Byte]): Either[CompileError, Unit] =
    try
      Option(path.getParent).foreach(dir => Files.createDirectories(dir))
      Files.write(path, body)
      Right(())
    catch
      case _: IOException | _: UncheckedIOException =>
        Left(CompileError.Io(IoFailure(path, IoFault.Write)))
end CompileReplay
