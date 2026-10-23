package histos.pack

import histos.jdk.Toolchain

import java.io.{BufferedOutputStream, InputStream, OutputStream}
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipException, ZipFile, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{IO, Scope, ZIO}

enum AssemblyError:
  case NotADirectory(path: Path)
  case Empty(path: Path)
  case Escaped(name: String)
  case DestinationInside(path: Path)
  case Overlap(path: Path)
  case NotAJar(path: Path)
  case MissingMain(main: MainClass)
  case NoClasses
  case Duplicate(name: String)
  case NotAFile(path: Path)
  case LazyVal(error: LazyValError)
  case Io(op: JarIo)

  def message: String = this match
    case NotADirectory(path)     => s"$path is not a directory of classes"
    case Empty(path)             => s"$path has no class files"
    case Escaped(name)           => s"'$name' leaves the assembly"
    case DestinationInside(path) => s"$path is inside the classes directory"
    case Overlap(path)           => s"$path is the assembly destination"
    case NotAJar(path)           => s"$path is not a jar"
    case MissingMain(main)       => s"${main.binary} is not in the assembly"
    case NoClasses               => "an assembly needs a classes directory"
    case Duplicate(name)         => s"'$name' is already in the assembly"
    case NotAFile(path)          => s"$path is not a file"
    case LazyVal(error)          => error.message
    case Io(op)                  => s"assembly failed while ${op.toString.toLowerCase}"

final case class Assembled(path: Path, entries: Int)

/** A file stored under one entry name. It is not unzipped. */
final case class AssemblyFile(entry: String, file: Path)

object Assembly:
  /** Merge application classes and runtime jars into one jar `java -jar` can exec.
    * Application classes win, then earlier jars. Service files are concatenated.
    * Signature files and `module-info.class` are omitted. The jar is not those signed artifacts and it is not a JPMS module.
    */
  def write(
      classes: List[Path],
      jars: List[Path],
      main: MainClass,
      destination: Path,
      resources: List[AssemblyFile] = Nil,
  ): IO[AssemblyError, Assembled] =
    for
      files <- classFiles(classes, destination)
      _     <- overlap(jars, destination)
      _     <- accept(resources, files.map(_._1).toSet)
      count <- place(destination, classes, files, jars, resources, main)
    yield Assembled(destination, count)

  private def classFiles(classes: List[Path], destination: Path): IO[AssemblyError, List[(String, Path)]] =
    if classes.isEmpty then ZIO.fail(AssemblyError.NoClasses)
    else
      ZIO.foldLeft(classes)(List.empty[(String, Path)]): (acc, dir) =>
        entries(dir, destination).map: files =>
          val taken = acc.map(_._1).toSet
          acc ++ files.filterNot((name, _) => taken.contains(name))

  private def accept(resources: List[AssemblyFile], taken: Set[String]): IO[AssemblyError, Unit] =
    ZIO.foreachDiscard(resources): resource =>
      if resource.entry.isEmpty || escaped(resource.entry) then ZIO.fail(AssemblyError.Escaped(resource.entry))
      else if taken.contains(resource.entry) then ZIO.fail(AssemblyError.Duplicate(resource.entry))
      else
        ZIO.attemptBlocking(Files.isRegularFile(resource.file)).mapError(_ => AssemblyError.Io(JarIo.Read)).flatMap: present =>
          if present then ZIO.unit else ZIO.fail(AssemblyError.NotAFile(resource.file))

  private enum Classified:
    case Drop, Escape, Service, Keep

  private final class Merge(
      out: ZipOutputStream,
      mainClass: String,
      handles: Map[(String, String), LazyVals.Handle],
      loader: ClassLoader,
  ):
    private var seen: Set[String] = Set.empty
    private var count: Int = 0
    private var foundMain: Boolean = false
    private var serviceOrder: List[String] = Nil
    private var serviceLines: Map[String, Vector[String]] = Map.empty
    private var serviceSeen: Map[String, Set[String]] = Map.empty

    def written: Int = count
    def hasMain: Boolean = foundMain
    def has(name: String): Boolean = seen.contains(name)

    def manifest(bytes: Array[Byte]): Unit =
      put("META-INF/MANIFEST.MF", bytes)

    def service(name: String, bytes: Array[Byte]): Unit =
      seen = seen + name
      val known = serviceSeen.getOrElse(name, Set.empty)
      val fresh = lines(bytes).filterNot(known.contains)
      if !serviceLines.contains(name) then serviceOrder = name :: serviceOrder
      serviceLines = serviceLines.updated(name, serviceLines.getOrElse(name, Vector.empty) ++ fresh)
      serviceSeen = serviceSeen.updated(name, known ++ fresh)

    def keep(name: String, input: InputStream): Either[AssemblyError, Unit] =
      if seen.contains(name) then
        input.transferTo(OutputStream.nullOutputStream())
        Right(())
      else if name.endsWith(".class") then store(name, input.readAllBytes())
      else
        seen = seen + name
        out.putNextEntry(new ZipEntry(name))
        input.transferTo(out)
        out.closeEntry()
        count += 1
        if name == mainClass then foundMain = true
        Right(())

    def keepFile(name: String, file: Path): Either[AssemblyError, Unit] =
      if seen.contains(name) then Right(())
      else if name.endsWith(".class") then store(name, Files.readAllBytes(file))
      else
        seen = seen + name
        out.putNextEntry(new ZipEntry(name))
        Files.copy(file, out)
        out.closeEntry()
        count += 1
        if name == mainClass then foundMain = true
        Right(())

    private def store(name: String, bytes: Array[Byte]): Either[AssemblyError, Unit] =
      LazyVals.rewrite(name, bytes, handles, loader).left.map(AssemblyError.LazyVal(_)).map: rewritten =>
        seen = seen + name
        out.putNextEntry(new ZipEntry(name))
        out.write(rewritten)
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
      classes: List[Path],
      files: List[(String, Path)],
      jars: List[Path],
      resources: List[AssemblyFile],
      main: MainClass,
  ): IO[AssemblyError, Int] =
    val parent = Option(destination.getParent).getOrElse(destination)
    val fileName = Option(destination.getFileName).map(_.toString).getOrElse("histos.jar")
    val temp = parent.resolve(s"$fileName.tmp")
    ZIO.acquireReleaseWith(ZIO.succeed(temp))(leftover => ZIO.attemptBlocking(Files.deleteIfExists(leftover)).ignore): _ =>
      written(temp, parent, classes, files, jars, resources, main).flatMap: count =>
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
      classes: List[Path],
      files: List[(String, Path)],
      jars: List[Path],
      resources: List[AssemblyFile],
      main: MainClass,
  ): IO[AssemblyError, Int] =
    ZIO.scoped:
      hierarchy(classes, jars).flatMap: loader =>
        ZIO.attemptBlocking {
          Files.createDirectories(parent)
          val stream = new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(temp)))
          try
            val packed = LazyVals.catalog(ownBytes(files) ++ jarBytes(jars)) match
              case Left(error) => Left(AssemblyError.LazyVal(error))
              case Right(handles) =>
                val merge = Merge(stream, classFile(main), handles, loader)
                merge.manifest(manifest(main))
                writeOwn(merge, files).flatMap: _ =>
                  writeResources(merge, resources).flatMap: _ =>
                    writeJars(merge, jars).flatMap: _ =>
                      merge.writeServices()
                      if merge.hasMain then Right(merge.written) else Left(AssemblyError.MissingMain(main))
            packed
          finally stream.close()
        }.mapError(_ => AssemblyError.Io(JarIo.Write)).flatMap(ZIO.fromEither)

  private def ownBytes(files: List[(String, Path)]): List[(String, Array[Byte])] =
    files.collect:
      case (name, path) if name.endsWith(".class") => name -> Files.readAllBytes(path)

  private def jarBytes(jars: List[Path]): List[(String, Array[Byte])] =
    jars.flatMap: path =>
      if !Files.isRegularFile(path) then Nil
      else
        try
          val zip = new ZipFile(path.toFile)
          try
            zip.entries.asScala.toList.flatMap: entry =>
              val name = Option(entry.getName).getOrElse("")
              if !name.endsWith(".class") then Nil
              else
                Option(zip.getInputStream(entry)) match
                  case None => Nil
                  case Some(input) =>
                    try List(name -> input.readAllBytes())
                    finally input.close()
          finally zip.close()
        catch case _: ZipException => Nil

  /** Every class directory and jar, for the frames of rewritten lazy vals. */
  private[pack] def hierarchy(classes: List[Path], jars: List[Path]): ZIO[Scope, AssemblyError, URLClassLoader] =
    ZIO.acquireRelease(
      ZIO.attempt(URLClassLoader((classes ++ jars).map(_.toUri.toURL).toArray, ClassLoader.getPlatformClassLoader))
        .mapError(_ => AssemblyError.Io(JarIo.Read))
    )(loader => ZIO.attemptBlocking(loader.close()).ignore)

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
          case Classified.Keep => merge.keepFile(name, path)

  private def writeResources(merge: Merge, resources: List[AssemblyFile]): Either[AssemblyError, Unit] =
    resources.foldLeft[Either[AssemblyError, Unit]](Right(())): (acc, resource) =>
      acc.flatMap: _ =>
        if merge.has(resource.entry) then Left(AssemblyError.Duplicate(resource.entry))
        else merge.keepFile(resource.entry, resource.file)

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
      case Classified.Keep => merge.keep(name, input)

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
