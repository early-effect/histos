package histos.load

import histos.jdk.Sha256
import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.util.zip.{ZipException, ZipFile}

import zio.{Chunk, IO, ZIO}

/** The classpath `histos.scala` compiles against. An assembly carries a pure api jar. A plain api jar is that classpath. */
object BuildClasspath:
  val Entry: String = "bootstrap/histos-api.jar"
  private val Program = "histos/cli/Main.class"

  def apply(codeSource: Path, cache: Path): IO[LoadError, Path] =
    ZIO.attemptBlocking(kind(codeSource)).mapError(_ => LoadError.Io(IoFailure(codeSource, IoFault.Stat))).flatMap:
      case Kind.Assembly => extract(codeSource, cache)
      case Kind.Plain    => ZIO.succeed(codeSource)

  private enum Kind:
    case Plain, Assembly

  private def kind(path: Path): Kind =
    if !Files.isRegularFile(path) then Kind.Plain
    else
      try
        val zip = new ZipFile(path.toFile)
        try
          val program = zip.getEntry(Program) != null
          val api = zip.getEntry(Entry) != null
          if program && api then Kind.Assembly else Kind.Plain
        finally zip.close()
      catch case _: ZipException => Kind.Plain

  private def extract(assembly: Path, cache: Path): IO[LoadError, Path] =
    ZIO.attemptBlocking {
      val zip = new ZipFile(assembly.toFile)
      try
        Option(zip.getEntry(Entry)) match
          case None => Left(LoadError.MissingApi(assembly))
          case Some(entry) =>
            val input = zip.getInputStream(entry)
            val bytes =
              try input.readAllBytes()
              finally input.close()
            Right(bytes)
      finally zip.close()
    }.mapError(_ => LoadError.Io(IoFailure(assembly, IoFault.Read))).flatMap(ZIO.fromEither).flatMap(bytes => place(cache, bytes))

  private def place(cache: Path, bytes: Array[Byte]): IO[LoadError, Path] =
    val sha = Sha256.of(Chunk.fromArray(bytes)).value
    val destination = cache.resolve("api").resolve(sha).resolve("histos-api.jar")
    ZIO.attemptBlocking {
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
    }.mapError(_ => LoadError.Io(IoFailure(destination, IoFault.Write)))
