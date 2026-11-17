package histos.publish

import histos.jdk.Sha256
import histos.pack.{JarError, JarIo}
import histos.ToolchainIdentity

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipException, ZipFile, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.json.*
import zio.{Chunk, IO, ZIO}

/** The toolchain identity and shim digest written into a launcher jar after the class-jar cache runs. The cache key is
  * the class bytes, so a later commit is stamped here instead of being reused from the first publish.
  */
object LauncherStamp:
  /** Copy `jar` and store `identity` and `shim`. An older copy of either entry is replaced. Class entries stay. */
  def stamp(jar: Path, identity: ToolchainIdentity, shim: Sha256): IO[JarError, Chunk[Byte]] =
    ZIO
      .attemptBlocking(rewrite(jar, identity, shim))
      .mapError(_ => JarError.Io(JarIo.Read(jar)))
      .flatMap(ZIO.fromEither)

  private val replaced: Set[String] = Set(ToolchainIdentity.entry, PublishedShim.entry)

  private def rewrite(jar: Path, identity: ToolchainIdentity, shim: Sha256): Either[JarError, Chunk[Byte]] =
    open(jar).flatMap: zip =>
      try stored(zip, jar, identity, shim)
      finally zip.close()

  private def open(jar: Path): Either[JarError, ZipFile] =
    if !Files.isRegularFile(jar) then Left(JarError.Io(JarIo.Read(jar)))
    else
      try Right(new ZipFile(jar.toFile))
      catch case _: ZipException => Left(JarError.Io(JarIo.Read(jar)))

  private def stored(
      zip: ZipFile,
      jar: Path,
      identity: ToolchainIdentity,
      shim: Sha256,
  ): Either[JarError, Chunk[Byte]] =
    names(kept(zip)).flatMap: chosen =>
      val buffer  = new ByteArrayOutputStream
      val stream  = new ZipOutputStream(buffer)
      val written =
        try pack(zip, jar, stream, chosen, identity, shim)
        finally stream.close()
      written.map(_ => Chunk.fromArray(buffer.toByteArray))

  private def kept(zip: ZipFile): List[ZipEntry] =
    zip.entries.asScala.toList.filter: entry =>
      val name = Option(entry.getName).getOrElse("")
      !entry.isDirectory && !name.endsWith("/") && !replaced.contains(name)

  private def names(entries: List[ZipEntry]): Either[JarError, List[ZipEntry]] =
    val labeled = entries.map(entry => Option(entry.getName).getOrElse("") -> entry)
    labeled.collectFirst { case (name, _) if rejected(name) => name } match
      case Some(name) => Left(JarError.Escaped(name))
      case None       =>
        labeled.map(_._1).groupBy(name => name).collectFirst {
          case (name, found) if found.lengthIs > 1 => name
        } match
          case Some(name) => Left(JarError.Duplicate(name))
          case None       => Right(labeled.map(_._2))

  private def rejected(name: String): Boolean =
    name.isEmpty ||
      name.startsWith("/") ||
      name.contains('\\') ||
      name.split("/", -1).exists(part => part.isEmpty || part == "." || part == "..")

  private def pack(
      zip: ZipFile,
      jar: Path,
      stream: ZipOutputStream,
      entries: List[ZipEntry],
      identity: ToolchainIdentity,
      shim: Sha256,
  ): Either[JarError, Unit] =
    entries
      .foldLeft[Either[JarError, Unit]](Right(())): (acc, entry) =>
        acc.flatMap(_ => copy(zip, jar, stream, entry))
      .map: _ =>
        write(stream, ToolchainIdentity.entry, identity.toJson.getBytes(StandardCharsets.UTF_8))
        write(stream, PublishedShim.entry, (shim.value + "\n").getBytes(StandardCharsets.UTF_8))

  private def copy(zip: ZipFile, jar: Path, stream: ZipOutputStream, entry: ZipEntry): Either[JarError, Unit] =
    val name = Option(entry.getName).getOrElse("")
    Option(zip.getInputStream(entry)) match
      case None        => Left(JarError.Io(JarIo.Read(jar)))
      case Some(input) =>
        try
          stream.putNextEntry(new ZipEntry(name))
          input.transferTo(stream)
          stream.closeEntry()
          Right(())
        finally input.close()

  private def write(stream: ZipOutputStream, name: String, bytes: Array[Byte]): Unit =
    stream.putNextEntry(new ZipEntry(name))
    stream.write(bytes)
    stream.closeEntry()
end LauncherStamp
