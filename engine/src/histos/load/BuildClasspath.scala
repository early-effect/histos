package histos.load

import histos.jdk.Sha256
import histos.pack.{ClassJar, JarError}
import histos.workspace.{IoFailure, IoFault}

import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.security.MessageDigest
import java.util.zip.{ZipException, ZipFile}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, ZIO}

/** The classpath `histos.scala` compiles against.
  *
  * An assembly carries `bootstrap/histos-api.jar`, extracted into `cache/api/<sha256>`. A classes directory is packed
  * into that same cache. The loader hashes one jar, and a directory is not one. The digest is the class names and
  * bytes, not the zip: entry times would miss the cache on every call. A recompiled tree is a new jar. An unchanged
  * tree is left in place. Any other path is already the classpath.
  */
object BuildClasspath:
  val Entry: String   = "bootstrap/histos-api.jar"
  private val Program = "histos/cli/Main.class"

  def apply(codeSource: Path, cache: Path): IO[LoadError, Path] =
    ZIO
      .attemptBlocking(kind(codeSource))
      .mapError(_ => LoadError.Io(IoFailure(codeSource, IoFault.Stat)))
      .flatMap:
        case Kind.Assembly => extract(codeSource, cache)
        case Kind.Classes  => pack(codeSource, cache)
        case Kind.Plain    => ZIO.succeed(codeSource)

  private enum Kind:
    case Plain, Assembly, Classes

  private def kind(path: Path): Kind =
    if Files.isDirectory(path) then Kind.Classes
    else if !Files.isRegularFile(path) then Kind.Plain
    else
      try
        val zip = new ZipFile(path.toFile)
        try
          val program = zip.getEntry(Program) != null
          val api     = zip.getEntry(Entry) != null
          if program && api then Kind.Assembly else Kind.Plain
        finally zip.close()
      catch case _: ZipException => Kind.Plain

  private def extract(assembly: Path, cache: Path): IO[LoadError, Path] =
    ZIO
      .attemptBlocking {
        val zip = new ZipFile(assembly.toFile)
        try
          Option(zip.getEntry(Entry)) match
            case None        => Left(LoadError.MissingApi(assembly))
            case Some(entry) =>
              val input = zip.getInputStream(entry)
              val bytes =
                try input.readAllBytes()
                finally input.close()
              Right(bytes)
        finally zip.close()
      }
      .mapError(_ => LoadError.Io(IoFailure(assembly, IoFault.Read)))
      .flatMap(ZIO.fromEither)
      .flatMap(bytes => place(cache, bytes))

  private def place(cache: Path, bytes: Array[Byte]): IO[LoadError, Path] =
    val sha         = Sha256.of(Chunk.fromArray(bytes)).value
    val destination = cache.resolve("api").resolve(sha).resolve("histos-api.jar")
    ZIO
      .attemptBlocking {
        if !Files.isRegularFile(destination) then
          val parent = Option(destination.getParent).getOrElse(destination)
          Files.createDirectories(parent)
          val temp = parent.resolve("histos-api.jar.tmp")
          Files.write(temp, bytes)
          try Files.move(temp, destination, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
          catch
            case _: AtomicMoveNotSupportedException =>
              Files.move(temp, destination, StandardCopyOption.REPLACE_EXISTING)
        destination
      }
      .mapError(_ => LoadError.Io(IoFailure(destination, IoFault.Write)))

  private def pack(classes: Path, cache: Path): IO[LoadError, Path] =
    fingerprint(classes).flatMap:
      case Left(error) => ZIO.fail(LoadError.Jar(error))
      case Right(sha)  =>
        val destination = cache.resolve("api").resolve(sha.value).resolve("histos-api.jar")
        ZIO
          .attemptBlocking(Files.isRegularFile(destination))
          .mapError(_ => LoadError.Io(IoFailure(destination, IoFault.Stat)))
          .flatMap: present =>
            if present then ZIO.succeed(destination)
            else ClassJar.write(classes, destination).mapBoth(LoadError.Jar(_), _.path)

  /** Names and bytes, in path order. Zip bytes are not the identity: a zip entry's time changes on every write. */
  private def fingerprint(classes: Path): IO[LoadError, Either[JarError, Sha256]] =
    ZIO
      .attemptBlocking {
        val base = classes.toAbsolutePath.normalize
        if !Files.isDirectory(base) then Left(JarError.NotADirectory(classes))
        else
          val walk = Files.walk(base)
          val files =
            try walk.iterator.asScala.filter(path => Files.isRegularFile(path)).toList
            finally walk.close()
          val named = files
            .map: file =>
              val absolute = file.toAbsolutePath.normalize
              val name     = base.relativize(absolute).iterator.asScala.map(_.toString).mkString("/")
              (name, absolute)
            .sortBy(_._1)
          named match
            case Nil => Left(JarError.Empty(classes))
            case _   =>
              val digest = MessageDigest.getInstance("SHA-256")
              digest.update(domain)
              named.foreach: (name, file) =>
                digest.update(name.getBytes(StandardCharsets.UTF_8))
                digest.update(0.toByte)
                digest.update(Files.readAllBytes(file))
                digest.update(0.toByte)
              Right(Sha256.fromDigest(digest.digest()))
      }
      .mapError(_ => LoadError.Io(IoFailure(classes, IoFault.Read)))

  private val domain: Array[Byte] = "classes".getBytes(StandardCharsets.UTF_8)
end BuildClasspath
