package histos.toolchain

import histos.jdk.Sha256
import histos.archive.{ArchiveBudget, ArchiveBudgetError, ArchiveLimit}
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{
  FileVisitOption,
  Files,
  LinkOption,
  NoSuchFileException,
  Path
}
import java.nio.file.attribute.BasicFileAttributes
import java.security.MessageDigest

import scala.jdk.CollectionConverters.*
import scala.util.Try

import zio.{IO, Scope, ZIO}
import zio.stream.ZStream

enum RuntimeIdentity:
  case Locked(archive: Sha256)
  case Local(home: Path, content: Sha256)

enum RuntimeLocationError:
  case InvalidPath(raw: String)
  case Io(failure: IoFailure)

  def message: String = this match
    case InvalidPath(raw) => s"invalid runtime search path: $raw"
    case Io(failure)      => failure.message

object RuntimeFiles:
  def firstOnPath(
      pathEnv: Option[String],
      names: List[String]
  ): IO[RuntimeLocationError, Option[Path]] =
    for
      dirs <- ZIO.foreach(
        pathEnv.toList
          .flatMap(_.split(java.io.File.pathSeparator).toList)
          .filter(_.nonEmpty)
      )(raw =>
        ZIO.fromEither(
          Try(Path.of(raw)).toEither.left.map(_ =>
            RuntimeLocationError.InvalidPath(raw)
          )
        )
      )
      candidates = dirs.flatMap(dir => names.map(dir.resolve(_)))
      present <- ZIO.foreach(candidates)(path =>
        ZIO
          .attemptBlocking(Files.isRegularFile(path))
          .mapError(_ => RuntimeLocationError.Io(IoFailure(path, IoFault.Stat)))
          .map(found => Option.when(found)(path))
      )
      canonical <- ZIO.foreach(present.flatten.headOption)(path =>
        ZIO
          .attemptBlocking(path.toRealPath().normalize)
          .mapError(_ => RuntimeLocationError.Io(IoFailure(path, IoFault.Stat)))
      )
    yield canonical

  def homeOf(binary: Path): Option[Path] =
    Option(binary.getParent).flatMap(parent => Option(parent.getParent))

  def readLock[T, E](
      path: Path
  )(parse: String => Either[E, T]): IO[IoFailure | E, Option[T]] =
    readIfPresent(path).flatMap(text =>
      ZIO.foreach(text)(raw => ZIO.fromEither(parse(raw)))
    )

  def readIfPresent(path: Path): IO[IoFailure, Option[String]] =
    ZIO
      .attemptBlocking:
        try
          Some(
            Files.readAttributes(
              path,
              classOf[BasicFileAttributes],
              LinkOption.NOFOLLOW_LINKS
            )
          )
        catch case _: NoSuchFileException => None
      .mapError(_ => IoFailure(path, IoFault.Stat))
      .flatMap:
        case None    => ZIO.none
        case Some(_) => Workspace.readString(path).map(Some(_))

  def sha256(path: Path): IO[IoFailure, Sha256] =
    for
      digest <- newDigest(path)
      _ <- bytes(path).chunks.foreach(chunk =>
        ZIO
          .attemptBlocking(digest.update(chunk.toArray))
          .mapError(_ => IoFailure(path, IoFault.Read))
      )
      hash <- ZIO
        .attemptBlocking(Sha256.fromDigest(digest.digest()))
        .mapError(_ => IoFailure(path, IoFault.Read))
    yield hash

  def bytes(path: Path): ZStream[Any, IoFailure, Byte] =
    ZStream.unwrapScoped:
      ZIO
        .acquireRelease(
          ZIO
            .attemptBlocking(Files.newInputStream(path))
            .mapError(_ => IoFailure(path, IoFault.Read))
        )(input => ZIO.attemptBlocking(input.close()).ignore)
        .map(input =>
          ZStream
            .fromInputStream(input)
            .mapError(_ => IoFailure(path, IoFault.Read))
        )

  def paths(root: Path, maxDepth: Int): IO[IoFailure, List[Path]] =
    ZIO.scoped:
      for
        exists <- ZIO
          .attemptBlocking(Files.isDirectory(root))
          .mapError(_ => IoFailure(root, IoFault.Stat))
        found <-
          if !exists then ZIO.succeed(List.empty[Path])
          else
            walk(root, maxDepth).flatMap(stream =>
              ZIO
                .attemptBlocking(
                  stream.iterator.asScala.toList.sortBy(_.toString)
                )
                .mapError(_ => IoFailure(root, IoFault.Walk))
            )
      yield found

  def homes(root: Path, maxDepth: Int)(
      isHome: Path => Boolean
  ): IO[IoFailure, List[Path]] =
    for
      candidates <- paths(root, maxDepth)
      dirs <- ZIO
        .attemptBlocking(
          candidates.filter(dir => Files.isDirectory(dir) && isHome(dir))
        )
        .mapError(_ => IoFailure(root, IoFault.Walk))
      canonical <- ZIO.foreach(dirs)(dir =>
        ZIO
          .attemptBlocking(dir -> dir.toRealPath().normalize)
          .mapError(_ => IoFailure(dir, IoFault.Stat))
      )
    yield canonical
      .distinctBy(_._2)
      .map(_._1)
      .sortBy(path => (path.getNameCount, path.toString))
      .foldLeft(List.empty[Path]): (kept, dir) =>
        if kept.exists(dir.startsWith(_)) then kept else kept :+ dir

  /** Local overrides follow links, including linked file content. */
  def identity(home: Path): IO[IoFailure, RuntimeIdentity] =
    for
      canonical <- ZIO
        .attemptBlocking(home.toRealPath().normalize)
        .mapError(_ => IoFailure(home, IoFault.Stat))
      hash <- content(canonical)
    yield RuntimeIdentity.Local(canonical, hash)

  def contentBounded(
      home: Path,
      excluded: Set[Path],
      budget: ArchiveBudget
  ): IO[IoFailure | ArchiveBudgetError, Sha256] =
    ZIO.scoped:
      for
        root <- ZIO
          .attemptBlocking(home.toRealPath().normalize)
          .mapError(_ => IoFailure(home, IoFault.Stat))
        omitted <- ZIO.foreach(excluded)(path =>
          ZIO
            .attemptBlocking(path.toRealPath().normalize)
            .mapError(_ => IoFailure(path, IoFault.Stat))
        )
        stream <- ZIO.acquireRelease(
          ZIO
            .attemptBlocking(Files.walk(root, FileVisitOption.FOLLOW_LINKS))
            .mapError(_ => IoFailure(home, IoFault.Walk))
        )(stream => ZIO.attemptBlocking(stream.close()).ignore)
        paths <- ZIO
          .attemptBlocking(
            stream.iterator.asScala
              .filterNot(path => path == root || omitted.contains(path))
              .take(budget.entries + 1)
              .toList
          )
          .mapError(_ => IoFailure(home, IoFault.Walk))
        _ <- ZIO
          .fail(
            ArchiveBudgetError
              .Exceeded(ArchiveLimit.Entries, budget.entries.toLong)
          )
          .when(paths.size > budget.entries)
        _ <- ZIO.foldLeft(paths)(0L)((count, path) =>
          ZIO
            .attemptBlocking(if Files.isRegularFile(path) then Files.size(path)
            else 0L)
            .mapError(_ => IoFailure(path, IoFault.Stat))
            .flatMap(size =>
              if size > budget.entryBytes then
                ZIO.fail(
                  ArchiveBudgetError
                    .Exceeded(ArchiveLimit.EntryBytes, budget.entryBytes)
                )
              else if size > budget.extractedBytes - count then
                ZIO.fail(
                  ArchiveBudgetError.Exceeded(
                    ArchiveLimit.ExtractedBytes,
                    budget.extractedBytes
                  )
                )
              else ZIO.succeed(count + size)
            )
        )
        hash <- content(root, excluded)
      yield hash

  def content(
      home: Path,
      excluded: Set[Path] = Set.empty
  ): IO[IoFailure, Sha256] =
    ZIO.scoped:
      for
        canonical <- ZIO
          .attemptBlocking(home.toRealPath().normalize)
          .mapError(_ => IoFailure(home, IoFault.Stat))
        omitted <- ZIO.foreach(excluded)(path =>
          ZIO
            .attemptBlocking(path.toRealPath().normalize)
            .mapError(_ => IoFailure(path, IoFault.Stat))
        )
        stream <- ZIO.acquireRelease(
          ZIO
            .attemptBlocking(
              Files.walk(canonical, FileVisitOption.FOLLOW_LINKS)
            )
            .mapError(_ => IoFailure(home, IoFault.Walk))
        )(stream => ZIO.attemptBlocking(stream.close()).ignore)
        files <- ZIO
          .attemptBlocking(
            stream.iterator.asScala
              .filterNot(omitted.contains)
              .toList
              .sortBy(path => canonical.relativize(path).toString)
          )
          .mapError(_ => IoFailure(home, IoFault.Walk))
        digest <- newDigest(home)
        _ <- ZIO.foreachDiscard(files): file =>
          for
            tag <- ZIO
              .attemptBlocking:
                val name = canonical
                  .relativize(file)
                  .iterator
                  .asScala
                  .map(_.toString)
                  .mkString("/")
                val kind =
                  if Files.isDirectory(file) then "directory" else "file"
                val link = if Files.isSymbolicLink(file) then
                  Files.readSymbolicLink(file).toString
                else ""
                s"$name\u0000$kind\u0000$link\u0000${Files.isExecutable(file)}\u0000"
                  .getBytes(java.nio.charset.StandardCharsets.UTF_8)
              .mapError(_ => IoFailure(file, IoFault.Stat))
            _ <- ZIO
              .attemptBlocking(digest.update(tag))
              .mapError(_ => IoFailure(file, IoFault.Read))
            regular <- ZIO
              .attemptBlocking(Files.isRegularFile(file))
              .mapError(_ => IoFailure(file, IoFault.Stat))
            hash <-
              if regular then sha256(file).map(_.value) else ZIO.succeed("")
            _ <- ZIO
              .attemptBlocking(
                digest.update(
                  hash.getBytes(java.nio.charset.StandardCharsets.UTF_8)
                )
              )
              .mapError(_ => IoFailure(file, IoFault.Read))
          yield ()
        hash <- ZIO
          .attemptBlocking(Sha256.fromDigest(digest.digest()))
          .mapError(_ => IoFailure(home, IoFault.Read))
      yield hash

  def writeHashed[E](
      stream: ZStream[Any, E, Byte],
      path: Path
  ): IO[IoFailure | E, Sha256] =
    ZIO.scoped:
      for
        output <- ZIO.acquireRelease(
          ZIO
            .attemptBlocking(Files.newOutputStream(path))
            .mapError(_ => IoFailure(path, IoFault.Write))
        )(output => ZIO.attemptBlocking(output.close()).ignore)
        digest <- newDigest(path)
        _ <- stream.chunks.foreach(chunk =>
          ZIO
            .attemptBlocking:
              val bytes = chunk.toArray
              output.write(bytes)
              digest.update(bytes)
            .mapError(_ => IoFailure(path, IoFault.Write))
        )
        hash <- ZIO
          .attemptBlocking(Sha256.fromDigest(digest.digest()))
          .mapError(_ => IoFailure(path, IoFault.Read))
      yield hash

  private def newDigest(path: Path): IO[IoFailure, MessageDigest] =
    ZIO
      .attemptBlocking(MessageDigest.getInstance("SHA-256"))
      .mapError(_ => IoFailure(path, IoFault.Read))

  private def walk(
      root: Path,
      depth: Int
  ): ZIO[Scope, IoFailure, java.util.stream.Stream[Path]] =
    ZIO.acquireRelease(
      ZIO
        .attemptBlocking(Files.walk(root, depth))
        .mapError(_ => IoFailure(root, IoFault.Walk))
    )(stream => ZIO.attemptBlocking(stream.close()).ignore)
