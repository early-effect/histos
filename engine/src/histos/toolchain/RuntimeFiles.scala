package histos.toolchain

import histos.jdk.Sha256
import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, NoSuchFileException, Path}
import java.nio.file.attribute.BasicFileAttributes
import java.security.MessageDigest

import scala.jdk.CollectionConverters.*
import scala.util.Try

import zio.{Chunk, IO, ZIO}
import zio.stream.ZStream

enum RuntimeIdentity:
  /** `salt` is the canonical home and the build the runtime already published. */
  case Local(home: Path, salt: Sha256)

enum RuntimeLocationError:
  case InvalidPath(raw: String)
  case Io(failure: IoFailure)

  def message: String = this match
    case InvalidPath(raw) => s"invalid runtime search path: $raw"
    case Io(failure)      => failure.message
end RuntimeLocationError

object RuntimeFiles:
  def firstOnPath(
      pathEnv: Option[String],
      names: List[String],
  ): IO[RuntimeLocationError, Option[Path]] =
    for
      dirs <- ZIO.foreach(
        pathEnv.toList
          .flatMap(_.split(java.io.File.pathSeparator).toList)
          .filter(_.nonEmpty)
      )(raw =>
        ZIO.fromEither(
          Try(Path.of(raw)).toEither.left.map(_ => RuntimeLocationError.InvalidPath(raw))
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

  def readIfPresent(path: Path): IO[IoFailure, Option[String]] =
    ZIO
      .attemptBlocking:
        try
          Some(
            Files.readAttributes(
              path,
              classOf[BasicFileAttributes],
              LinkOption.NOFOLLOW_LINKS,
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
      _      <- bytes(path).chunks.foreach(chunk =>
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

  /** Canonical home and the build already read from it. A file replaced inside the home does not change the salt.
    */
  def identity(home: Path, build: String): IO[IoFailure, RuntimeIdentity] =
    ZIO
      .attemptBlocking(home.toRealPath().normalize)
      .mapError(_ => IoFailure(home, IoFault.Stat))
      .map: canonical =>
        val salt = Sha256.of(
          Chunk.fromArray(
            s"$canonical\n$build".getBytes(StandardCharsets.UTF_8)
          )
        )
        RuntimeIdentity.Local(canonical, salt)

  /** Regular files under a class directory, streamed into one digest. A JDK or Node home is [[identity]].
    */
  private[toolchain] def fingerprint(root: Path): IO[IoFailure, Sha256] =
    ZIO.scoped:
      for
        canonical <- ZIO
          .attemptBlocking(root.toRealPath().normalize)
          .mapError(_ => IoFailure(root, IoFault.Stat))
        stream <- ZIO.acquireRelease(
          ZIO
            .attemptBlocking(Files.walk(canonical))
            .mapError(_ => IoFailure(root, IoFault.Walk))
        )(open => ZIO.attemptBlocking(open.close()).ignore)
        files <- ZIO
          .attemptBlocking(
            stream.iterator.asScala
              .filter(path => Files.isRegularFile(path))
              .map: path =>
                val name = canonical
                  .relativize(path)
                  .iterator
                  .asScala
                  .map(_.toString)
                  .mkString("/")
                name -> path
              .toList
              .sortBy(_._1)
          )
          .mapError(_ => IoFailure(root, IoFault.Walk))
        digest <- newDigest(root)
        _      <- mark(digest, root, canonical.toString)
        _      <- ZIO.foreachDiscard(files): (name, file) =>
          mark(digest, file, name) *> bytes(file).chunks.foreach(chunk =>
            ZIO
              .attemptBlocking(digest.update(chunk.toArray))
              .mapError(_ => IoFailure(file, IoFault.Read))
          )
        hash <- ZIO
          .attemptBlocking(Sha256.fromDigest(digest.digest()))
          .mapError(_ => IoFailure(root, IoFault.Read))
      yield hash

  private def mark(
      digest: MessageDigest,
      path: Path,
      text: String,
  ): IO[IoFailure, Unit] =
    ZIO
      .attemptBlocking:
        digest.update(text.getBytes(StandardCharsets.UTF_8))
        digest.update(0.toByte)
      .mapError(_ => IoFailure(path, IoFault.Read))

  private def newDigest(path: Path): IO[IoFailure, MessageDigest] =
    ZIO
      .attemptBlocking(MessageDigest.getInstance("SHA-256"))
      .mapError(_ => IoFailure(path, IoFault.Read))
end RuntimeFiles
