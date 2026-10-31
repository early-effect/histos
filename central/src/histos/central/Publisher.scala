package histos.central

import histos.Organization

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.security.MessageDigest

import zio.{Chunk, IO, ULayer, ZIO, ZLayer}

trait Publisher:
  def publish(artifact: Artifact): IO[PublishError, Published]

object Publisher:
  /** A directory in Maven layout. The write is local. Nothing is uploaded. */
  def local(root: Path): ULayer[Publisher] =
    ZLayer.succeed(LocalPublisher(root))

  /** PUT a snapshot through a heddle `Client`. The layer closes that client. The clock is the fiber's. */
  def snapshots(
      repository: SnapshotRepository,
      credentials: RepositorySecret,
  ): ZLayer[Any, PublishError, Publisher] =
    val http =
      ZLayer.succeed(heddle.client.Client.Config.default) >>>
        heddle.client.Client.layer.mapError(error => PublishError.Transport(TransportFault.from(error)))
    http >>> ZLayer.fromFunction((client: heddle.client.Client) => SnapshotPublisher(repository, credentials, client))
end Publisher

private final class LocalPublisher(root: Path) extends Publisher:
  def publish(artifact: Artifact): IO[PublishError, Published] =
    for
      _       <- directory
      jar     <- ZIO.fromEither(JarCheck.accept(artifact.jar))
      sources <- accept(artifact.sources)
      javadoc <- accept(artifact.javadoc)
      paths   <- ZIO.fromEither(Layout.paths(root, artifact))
      pom = Chunk.fromArray(Layout.pom(artifact).getBytes(java.nio.charset.StandardCharsets.UTF_8))
      _ <- stage(paths.jar, jar)
      _ <- stageOptional(paths.sources, sources)
      _ <- stageOptional(paths.javadoc, javadoc)
      _ <- stage(paths.pom, pom)
    yield Published.Local(paths.jar, paths.pom, paths.sources, paths.javadoc, Checksum.sha256(jar))

  private def accept(bytes: Option[Chunk[Byte]]): IO[PublishError, Option[Chunk[Byte]]] =
    bytes match
      case None        => ZIO.succeed(None)
      case Some(value) => ZIO.fromEither(JarCheck.accept(value)).map(Some(_))

  private def stageOptional(path: Option[Path], bytes: Option[Chunk[Byte]]): IO[PublishError, Unit] =
    (path, bytes) match
      case (Some(file), Some(body)) => stage(file, body)
      case _                        => ZIO.unit

  private def stage(path: Path, bytes: Chunk[Byte]): IO[PublishError, Unit] =
    place(path, bytes) *>
      place(sidecar(path, "md5"), text(Digest.md5(bytes))) *>
      place(sidecar(path, "sha1"), text(Digest.sha1(bytes))) *>
      place(sidecar(path, "sha256"), text(Digest.sha256(bytes)))

  private def sidecar(path: Path, suffix: String): Path =
    val name   = Option(path.getFileName).map(_.toString).getOrElse("artifact")
    val parent = Option(path.getParent).getOrElse(path)
    parent.resolve(s"$name.$suffix")

  private def directory: IO[PublishError, Unit] =
    ZIO
      .attemptBlocking {
        if Files.exists(root) && !Files.isDirectory(root) then Left(PublishError.NotADirectory(root))
        else Right(())
      }
      .mapError(_ => PublishError.Io(root, PublishFault.Stat))
      .flatMap(ZIO.fromEither)

  private def text(hex: String): Chunk[Byte] =
    Chunk.fromArray(s"$hex\n".getBytes(java.nio.charset.StandardCharsets.UTF_8))

  private def place(path: Path, bytes: Chunk[Byte]): IO[PublishError, Unit] =
    val parent   = Option(path.getParent).getOrElse(path)
    val fileName = Option(path.getFileName).map(_.toString).getOrElse("artifact")
    val temp     = parent.resolve(s"$fileName.tmp")
    ZIO.acquireReleaseWith(ZIO.succeed(temp))(leftover => ZIO.attemptBlocking(Files.deleteIfExists(leftover)).ignore) {
      _ =>
        ZIO
          .attemptBlocking {
            Files.createDirectories(parent)
            Files.write(temp, bytes.toArray)
            try Files.move(temp, path, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
            catch
              case _: AtomicMoveNotSupportedException =>
                Files.move(temp, path, StandardCopyOption.REPLACE_EXISTING)
            ()
          }
          .mapError(_ => PublishError.Io(path, PublishFault.Write))
    }
end LocalPublisher
