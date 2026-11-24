package histos.cache

import histos.jdk.Sha256
import histos.schedule.{CacheError, Digest}
import histos.workspace.{IoFailure, IoFault}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption}

import zio.{Chunk, IO, ZIO}

/** One linked file under its link key. The bytes are the output. The work directory they are restored into is not.
  */
object LinkReplay:
  private val Magic = "LK1\n".getBytes(StandardCharsets.US_ASCII)

  def load(store: ContentStore, key: Sha256, work: Path): IO[CacheError, Option[Path]] =
    load(store, key, work, _ => ZIO.unit)

  private[cache] def load(
      store: ContentStore,
      key: Sha256,
      work: Path,
      beforePublish: Path => IO[CacheError, Unit],
  ): IO[CacheError, Option[Path]] =
    id(key).flatMap: digest =>
      store
        .read(digest)
        .flatMap:
          case None        => ZIO.none
          case Some(bytes) =>
            decode(digest, bytes) match
              case Left(error) => ZIO.fail(error)
              case Right(file) => place(work, file, beforePublish).asSome

  def save(store: ContentStore, key: Sha256, binary: Path): IO[CacheError, Unit] =
    for
      digest <- id(key)
      file   <- ZIO.attemptBlocking(captured(binary)).mapError(io(binary, IoFault.Read)).flatMap(ZIO.fromEither)
      now    <- ZIO.clockWith(_.instant)
      _      <- store.put(digest, Chunk.fromArray(encode(file)), now)
    yield ()

  private final case class Stored(name: String, executable: Boolean, bytes: Array[Byte])

  private def id(key: Sha256): IO[CacheError, Digest] =
    Digest.from(key.value) match
      case Some(found) => ZIO.succeed(found)
      case None        => ZIO.fail(CacheError.Io(IoFailure(Path.of(key.value), IoFault.Read)))

  private def captured(path: Path): Either[CacheError, Stored] =
    fileName(path).map: name =>
      Stored(name, canExecute(path), Files.readAllBytes(path))

  private def fileName(path: Path): Either[CacheError, String] =
    Option(path.getFileName).map(_.toString).filter(safe) match
      case Some(name) => Right(name)
      case None       => Left(CacheError.Io(IoFailure(path, IoFault.Read)))

  private def safe(name: String): Boolean =
    name.nonEmpty && name != "." && name != ".." && !name.contains("/") && !name.contains("\\") && !name.contains(
      '\u0000'
    )

  private def canExecute(path: Path): Boolean =
    try Files.getPosixFilePermissions(path).contains(PosixFilePermission.OWNER_EXECUTE)
    catch case _: UnsupportedOperationException => true

  private def place(work: Path, file: Stored, beforePublish: Path => IO[CacheError, Unit]): IO[CacheError, Path] =
    val parent                         = work.resolve("linked")
    val dest                           = parent.resolve(file.name)
    def valid: IO[CacheError, Boolean] =
      ZIO
        .attemptBlocking(
          Files.isRegularFile(dest, LinkOption.NOFOLLOW_LINKS) &&
            Files.readAllBytes(dest).sameElements(file.bytes) && (!file.executable || canExecute(dest))
        )
        .mapError(io(dest, IoFault.Read))
    for
      existing <- valid
      result   <-
        if existing then ZIO.succeed(dest)
        else
          ZIO.scoped:
            for
              _    <- ZIO.attemptBlocking(Files.createDirectories(parent)).mapError(io(parent, IoFault.Create))
              temp <- ZIO.acquireRelease(
                ZIO.attemptBlocking(Files.createTempFile(parent, ".replay-", ".tmp")).mapError(io(parent, IoFault.Temp))
              )(path => ZIO.attemptBlocking(Files.deleteIfExists(path)).orDie.unit)
              _ <- ZIO
                .attemptBlocking {
                  Files.write(temp, file.bytes)
                  val permissions = if file.executable then "rwxr-xr-x" else "rw-r--r--"
                  Files.setPosixFilePermissions(temp, PosixFilePermissions.fromString(permissions))
                }
                .mapError(io(temp, IoFault.Write))
              present <- valid
              _       <- ZIO.unless(present)(
                beforePublish(temp) *>
                  ZIO
                    .attemptBlocking(
                      Files.move(temp, dest, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
                    )
                    .mapError(io(dest, IoFault.Write))
                    .unit
                    .uninterruptible
              )
            yield dest
    yield result

  private def encode(file: Stored): Array[Byte] =
    val out  = new ByteArrayOutputStream
    val data = new DataOutputStream(out)
    data.write(Magic)
    data.writeBoolean(file.executable)
    val name = file.name.getBytes(StandardCharsets.UTF_8)
    data.writeShort(name.length)
    data.write(name)
    data.writeInt(file.bytes.length)
    data.write(file.bytes)
    data.flush()
    out.toByteArray

  private def decode(digest: Digest, bytes: Chunk[Byte]): Either[CacheError, Stored] =
    val raw = bytes.toArray
    if !magic(raw) then Left(CacheError.Corrupt(digest))
    else
      val input = new DataInputStream(new ByteArrayInputStream(raw, Magic.length, raw.length - Magic.length))
      try
        val executable = input.readBoolean()
        val length     = input.readUnsignedShort()
        val nameBytes  = new Array[Byte](length)
        input.readFully(nameBytes)
        val name = new String(nameBytes, StandardCharsets.UTF_8)
        val size = input.readInt()
        if !safe(name) || size < 0 || size > input.available() then Left(CacheError.Corrupt(digest))
        else
          val body = new Array[Byte](size)
          input.readFully(body)
          Right(Stored(name, executable, body))
      catch case _: IOException => Left(CacheError.Corrupt(digest))

  private def magic(raw: Array[Byte]): Boolean =
    raw.length >= Magic.length && Magic.indices.forall(index => raw(index) == Magic(index))

  private def io(path: Path, fault: IoFault): Throwable => CacheError =
    _ => CacheError.Io(IoFailure(path, fault))
end LinkReplay
