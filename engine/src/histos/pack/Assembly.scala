package histos.pack

import histos.jdk.Toolchain

import java.io.{BufferedOutputStream, InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipException, ZipFile, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

enum AssemblyError:
  case NotADirectory(path: Path)
  case Empty(path: Path)
  case Escaped(name: String)
  case DestinationInside(path: Path)
  case Overlap(path: Path)
  case NotAJar(path: Path)
  case MissingMain(main: MainClass)
  case Io(op: JarIo)

  def message: String = this match
    case NotADirectory(path)     => s"$path is not a directory of classes"
    case Empty(path)             => s"$path has no class files"
    case Escaped(name)           => s"'$name' leaves the assembly"
    case DestinationInside(path) => s"$path is inside the classes directory"
    case Overlap(path)           => s"$path is the assembly destination"
    case NotAJar(path)           => s"$path is not a jar"
    case MissingMain(main)       => s"${main.binary} is not in the assembly"
    case Io(op)                  => s"assembly failed while ${op.toString.toLowerCase}"

final case class Assembled(path: Path, entries: Int)

object Assembly:
  /** Merge application classes and runtime jars into one jar `java -jar` can exec.
    * Application classes win, then earlier jars. Service files are concatenated.
    * Signature files and `module-info.class` are omitted. The jar is not those signed artifacts and it is not a JPMS module.
    */
  def write(classes: Path, jars: List[Path], main: MainClass, destination: Path): IO[AssemblyError, Assembled] =
    for
      files <- entries(classes, destination)
      _     <- overlap(jars, destination)
      count <- place(destination, files, jars, main)
    yield Assembled(destination, count)

  private enum Classified:
    case Drop, Escape, Service, Keep

  private final class Merge(out: ZipOutputStream, mainClass: String):
    private var seen: Set[String] = Set.empty
    private var count: Int = 0
    private var foundMain: Boolean = false
    private var serviceOrder: List[String] = Nil
    private var serviceLines: Map[String, Vector[String]] = Map.empty
    private var serviceSeen: Map[String, Set[String]] = Map.empty

    def written: Int = count
    def hasMain: Boolean = foundMain

    def manifest(bytes: Array[Byte]): Unit =
      put("META-INF/MANIFEST.MF", bytes)

    def service(name: String, bytes: Array[Byte]): Unit =
      seen = seen + name
      val known = serviceSeen.getOrElse(name, Set.empty)
      val fresh = lines(bytes).filterNot(known.contains)
      if !serviceLines.contains(name) then serviceOrder = name :: serviceOrder
      serviceLines = serviceLines.updated(name, serviceLines.getOrElse(name, Vector.empty) ++ fresh)
      serviceSeen = serviceSeen.updated(name, known ++ fresh)

    def keep(name: String, input: InputStream): Unit =
      if seen.contains(name) then input.transferTo(OutputStream.nullOutputStream())
      else
        seen = seen + name
        out.putNextEntry(new ZipEntry(name))
        input.transferTo(out)
        out.closeEntry()
        count += 1
        if name == mainClass then foundMain = true

    def keepFile(name: String, file: Path): Unit =
      if !seen.contains(name) then
        seen = seen + name
        out.putNextEntry(new ZipEntry(name))
        Files.copy(file, out)
        out.closeEntry()
        count += 1
        if name == mainClass then foundMain = true

    def writeServices(): Unit =
      serviceOrder.reverse.foreach: name =>
        val body = serviceLines.getOrElse(name, Vector.empty).mkString("\n")
        if body.nonEmpty then put(name, (body + "\n").getBytes(StandardCharsets.UTF_8))

    private def put(name: String, bytes: Array[Byte]): Unit =
      out.putNextEntry(new ZipEntry(name))
      out.write(bytes)
      out.closeEntry()
      count += 1

    private def lines(bytes: Array[Byte]): List[String] =
      new String(bytes, StandardCharsets.UTF_8)
        .split('\n')
        .toList
        .map(_.stripSuffix("\r").trim)
        .filter(_.nonEmpty)

  private def entries(classes: Path, destination: Path): IO[AssemblyError, List[(String, Path)]] =
    ZIO.attemptBlocking {
      val base = classes.toAbsolutePath.normalize
      val dest = destination.toAbsolutePath.normalize
      if !Files.isDirectory(classes) then Left(AssemblyError.NotADirectory(classes))
      else if dest.startsWith(base) then Left(AssemblyError.DestinationInside(destination))
      else
        val walk = Files.walk(classes)
        val files =
          try walk.iterator.asScala.filter(path => Files.isRegularFile(path)).toList
          finally walk.close()
        files.sortBy(_.toString).foldLeft[Either[AssemblyError, List[(String, Path)]]](Right(Nil)): (acc, file) =>
          acc.flatMap: seen =>
            val absolute = file.toAbsolutePath.normalize
            val relative = base.relativize(absolute)
            val name = relative.iterator.asScala.map(_.toString).mkString("/")
            if !absolute.startsWith(base) || relative.startsWith("..") then Left(AssemblyError.Escaped(name))
            else Right(seen :+ (name, file))
        .flatMap: named =>
          if named.isEmpty then Left(AssemblyError.Empty(classes)) else Right(named)
    }.mapError(_ => AssemblyError.Io(JarIo.Read)).flatMap(ZIO.fromEither)

  private def overlap(jars: List[Path], destination: Path): IO[AssemblyError, Unit] =
    ZIO.attemptBlocking {
      val dest = destination.toAbsolutePath.normalize
      jars.foldLeft[Either[AssemblyError, Unit]](Right(())): (acc, jar) =>
        acc.flatMap: _ =>
          val path = jar.toAbsolutePath.normalize
          if path == dest then Left(AssemblyError.Overlap(jar)) else Right(())
    }.mapError(_ => AssemblyError.Io(JarIo.Read)).flatMap(ZIO.fromEither)

  private def place(
      destination: Path,
      files: List[(String, Path)],
      jars: List[Path],
      main: MainClass,
  ): IO[AssemblyError, Int] =
    val parent = Option(destination.getParent).getOrElse(destination)
    val fileName = Option(destination.getFileName).map(_.toString).getOrElse("histos.jar")
    val temp = parent.resolve(s"$fileName.tmp")
    ZIO.acquireReleaseWith(ZIO.succeed(temp))(leftover => ZIO.attemptBlocking(Files.deleteIfExists(leftover)).ignore): _ =>
      written(temp, parent, files, jars, main).flatMap: count =>
        ZIO.attemptBlocking {
          try Files.move(temp, destination, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
          catch
            case _: AtomicMoveNotSupportedException =>
              Files.move(temp, destination, StandardCopyOption.REPLACE_EXISTING)
          count
        }.mapError(_ => AssemblyError.Io(JarIo.Move))

  private def written(
      temp: Path,
      parent: Path,
      files: List[(String, Path)],
      jars: List[Path],
      main: MainClass,
  ): IO[AssemblyError, Int] =
    ZIO.attemptBlocking {
      Files.createDirectories(parent)
      val stream = new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(temp)))
      try
        val merge = Merge(stream, classFile(main))
        merge.manifest(manifest(main))
        writeOwn(merge, files).flatMap: _ =>
          writeJars(merge, jars).flatMap: _ =>
            merge.writeServices()
            if merge.hasMain then Right(merge.written) else Left(AssemblyError.MissingMain(main))
      finally stream.close()
    }.mapError(_ => AssemblyError.Io(JarIo.Write)).flatMap(ZIO.fromEither)

  private def writeOwn(merge: Merge, files: List[(String, Path)]): Either[AssemblyError, Unit] =
    files.foldLeft[Either[AssemblyError, Unit]](Right(())): (acc, file) =>
      acc.flatMap: _ =>
        val (name, path) = file
        classified(name) match
          case Classified.Drop    => Right(())
          case Classified.Escape  => Left(AssemblyError.Escaped(name))
          case Classified.Service =>
            merge.service(name, Files.readAllBytes(path))
            Right(())
          case Classified.Keep =>
            merge.keepFile(name, path)
            Right(())

  private def writeJars(merge: Merge, jars: List[Path]): Either[AssemblyError, Unit] =
    jars.foldLeft[Either[AssemblyError, Unit]](Right(())): (acc, jar) =>
      acc.flatMap(_ => writeJar(merge, jar))

  private def writeJar(merge: Merge, path: Path): Either[AssemblyError, Unit] =
    if !Files.isRegularFile(path) then Left(AssemblyError.NotAJar(path))
    else
      try
        val zip = new ZipFile(path.toFile)
        try
          zip.entries.asScala.toList.foldLeft[Either[AssemblyError, Unit]](Right(())): (acc, entry) =>
            acc.flatMap: _ =>
              val name = Option(entry.getName).getOrElse("")
              Option(zip.getInputStream(entry)) match
                case None =>
                  if entry.isDirectory || name.endsWith("/") then Right(())
                  else Left(AssemblyError.NotAJar(path))
                case Some(input) =>
                  try take(merge, name, input)
                  finally input.close()
        finally zip.close()
      catch case _: ZipException => Left(AssemblyError.NotAJar(path))

  private def take(merge: Merge, name: String, input: InputStream): Either[AssemblyError, Unit] =
    classified(name) match
      case Classified.Drop =>
        input.transferTo(OutputStream.nullOutputStream())
        Right(())
      case Classified.Escape => Left(AssemblyError.Escaped(name))
      case Classified.Service =>
        merge.service(name, input.readAllBytes())
        Right(())
      case Classified.Keep =>
        merge.keep(name, input)
        Right(())

  private def classified(name: String): Classified =
    if name.isEmpty || name.endsWith("/") then Classified.Drop
    else if escaped(name) then Classified.Escape
    else if name == "META-INF/MANIFEST.MF" || name == "META-INF/INDEX.LIST" then Classified.Drop
    else if name == "module-info.class" || versionedModule(name) then Classified.Drop
    else if signature(name) then Classified.Drop
    else if name.startsWith("META-INF/services/") then Classified.Service
    else Classified.Keep

  private def escaped(name: String): Boolean =
    name.startsWith("/") || name.split('/').exists(part => part.isEmpty || part == "..")

  private def versionedModule(name: String): Boolean =
    name.startsWith("META-INF/versions/") && name.endsWith("/module-info.class")

  private def signature(name: String): Boolean =
    name.startsWith("META-INF/SIG-") || topLevelSignature(name)

  private def topLevelSignature(name: String): Boolean =
    if !name.startsWith("META-INF/") then false
    else
      val rest = name.drop("META-INF/".length)
      if rest.contains('/') then false
      else
        val upper = rest.toUpperCase(java.util.Locale.ROOT)
        upper.endsWith(".SF") || upper.endsWith(".DSA") || upper.endsWith(".RSA") || upper.endsWith(".EC")

  private def classFile(main: MainClass): String =
    main.binary.replace('.', '/') + ".class"

  private def manifest(main: MainClass): Array[Byte] =
    val text =
      "Manifest-Version: 1.0\r\n" +
        s"Main-Class: ${main.binary}\r\n" +
        s"${Toolchain.NativeAccessManifest}\r\n" +
        "Multi-Release: true\r\n\r\n"
    text.getBytes(StandardCharsets.UTF_8)
