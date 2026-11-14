package histos.program

import histos.pack.MainClass
import histos.{ClangMajor, NativeVersion, ScalaVersion}

import java.io.{BufferedInputStream, BufferedOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** Compiled native products and the versions a later link needs. The archive has no clang and no native libraries.
  */
final case class ClientKit(
    main: MainClass,
    clang: ClangMajor,
    scala: ScalaVersion,
    native: NativeVersion,
    classpath: List[Path],
)

object ClientKit:
  val FileName: String = "client-kit.zip"

  private val Descriptor: String = "kit.txt"

  enum Failure:
    case Write(path: Path)
    case Read(path: Path)
    case Corrupt(detail: String)

    def message: String = this match
      case Write(path)     => s"could not write $path"
      case Read(path)      => s"could not read $path"
      case Corrupt(detail) => detail
  end Failure

  def write(destination: Path, kit: ClientKit): IO[Failure, Path] =
    ZIO
      .attemptBlocking {
        Option(destination.getParent).foreach(Files.createDirectories(_))
        val stream = new ZipOutputStream(new BufferedOutputStream(Files.newOutputStream(destination)))
        try
          entry(stream, Descriptor, descriptor(kit).getBytes(StandardCharsets.UTF_8))
          kit.classpath.zipWithIndex.foreach((path, index) => add(stream, index, path))
        finally stream.close()
        destination
      }
      .mapError(_ => Failure.Write(destination))

  def read(archive: Path, into: Path): IO[Failure, ClientKit] =
    ZIO
      .attemptBlocking(load(archive, into))
      .mapError(_ => Failure.Read(archive))
      .flatMap(ZIO.fromEither)

  private def descriptor(kit: ClientKit): String =
    s"""main=${kit.main.binary}
       |clang=${kit.clang.value}
       |scala=${kit.scala.value}
       |native=${kit.native.value}
       |count=${kit.classpath.length}
       |""".stripMargin

  private def entry(stream: ZipOutputStream, name: String, bytes: Array[Byte]): Unit =
    stream.putNextEntry(new ZipEntry(name))
    stream.write(bytes)
    stream.closeEntry()

  private def add(stream: ZipOutputStream, index: Int, path: Path): Unit =
    if Files.isDirectory(path) then
      stream.putNextEntry(new ZipEntry(s"cp/$index/"))
      stream.closeEntry()
      val root = path.toAbsolutePath.normalize
      val walk = Files.walk(root)
      try
        walk.iterator.asScala.foreach: child =>
          if child != root && Files.isRegularFile(child) && !Files.isSymbolicLink(child) then
            val relative = root.relativize(child).toString.replace('\\', '/')
            val bytes    = Files.readAllBytes(child)
            entry(stream, s"cp/$index/$relative", bytes)
      finally walk.close()
    else
      val suffix = extension(path)
      entry(stream, s"cp/$index$suffix", Files.readAllBytes(path))

  private def extension(path: Path): String =
    Option(path.getFileName).map(_.toString) match
      case Some(name) if name.contains('.') => name.substring(name.lastIndexOf('.'))
      case _                                => ""

  private final case class Header(
      main: MainClass,
      clang: ClangMajor,
      scala: ScalaVersion,
      native: NativeVersion,
      count: Int,
  )

  private def load(archive: Path, into: Path): Either[Failure, ClientKit] =
    val zip = new ZipFile(archive.toFile)
    try
      val root = into.toAbsolutePath.normalize
      Files.createDirectories(root)
      for
        text   <- descriptorText(zip)
        header <- parse(text)
        paths  <- unpack(zip, root, header.count)
      yield ClientKit(header.main, header.clang, header.scala, header.native, paths)
    finally zip.close()

  private def descriptorText(zip: ZipFile): Either[Failure, String] =
    Option(zip.getEntry(Descriptor)) match
      case None        => Left(Failure.Corrupt("kit descriptor is missing"))
      case Some(found) =>
        val input = new BufferedInputStream(zip.getInputStream(found))
        try Right(String(input.readAllBytes, StandardCharsets.UTF_8))
        finally input.close()

  private def parse(text: String): Either[Failure, Header] =
    val lines                                       = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
    def field(key: String): Either[Failure, String] =
      lines.collectFirst:
        case line if line.startsWith(s"$key=") => line.drop(key.length + 1)
      match
        case Some(value) if value.nonEmpty => Right(value)
        case _                             => Left(Failure.Corrupt(s"kit descriptor has no $key"))
    for
      main     <- field("main").flatMap(raw => MainClass.from(raw).left.map(error => Failure.Corrupt(error.message)))
      clangRaw <- field("clang")
      clangN   <- clangRaw.toIntOption.toRight(Failure.Corrupt(s"clang major $clangRaw is not a number"))
      clang    <- ClangMajor.from(clangN).left.map(error => Failure.Corrupt(error.message))
      scala  <- field("scala").flatMap(raw => ScalaVersion.from(raw).left.map(error => Failure.Corrupt(error.message)))
      native <- field("native").flatMap(raw =>
        NativeVersion.from(raw).left.map(error => Failure.Corrupt(error.message))
      )
      countRaw <- field("count")
      count <- countRaw.toIntOption.filter(_ >= 0).toRight(Failure.Corrupt(s"classpath count $countRaw is not a count"))
    yield Header(main, clang, scala, native, count)

  private def unpack(zip: ZipFile, into: Path, count: Int): Either[Failure, List[Path]] =
    val names = zip.entries.asScala.map(_.getName).filter(_ != Descriptor).toList
    if names.exists(name => !safe(name)) then Left(Failure.Corrupt("kit entry escapes the archive"))
    else
      names
        .foldLeft[Either[Failure, Unit]](Right(()))((acc, name) => acc.flatMap(_ => place(zip, into, name)))
        .flatMap { _ =>
          (0 until count).foldLeft[Either[Failure, List[Path]]](Right(Nil)): (acc, index) =>
            acc.flatMap: seen =>
              rootOf(into, names, index).map(seen :+ _)
        }

  private def safe(name: String): Boolean =
    val parts = name.split("/", -1).toList
    val body  = if name.endsWith("/") then parts.dropRight(1) else parts
    name.nonEmpty && !name.startsWith("/") && body.forall(part => part.nonEmpty && part != "..")

  private def place(zip: ZipFile, into: Path, name: String): Either[Failure, Unit] =
    if name.endsWith("/") then
      val dir = into.resolve(name).normalize
      if dir.startsWith(into) then
        Files.createDirectories(dir)
        Right(())
      else Left(Failure.Corrupt("kit entry escapes the archive"))
    else
      Option(zip.getEntry(name)) match
        case None        => Left(Failure.Corrupt(s"kit entry $name is missing"))
        case Some(found) =>
          val target = into.resolve(name).normalize
          if !target.startsWith(into) then Left(Failure.Corrupt("kit entry escapes the archive"))
          else
            Option(target.getParent).foreach(Files.createDirectories(_))
            val input = new BufferedInputStream(zip.getInputStream(found))
            try
              Files.copy(input, target, StandardCopyOption.REPLACE_EXISTING)
              Right(())
            finally input.close()

  private def rootOf(into: Path, names: List[String], index: Int): Either[Failure, Path] =
    val prefix    = s"cp/$index"
    val owned     = names.filter(name => name == prefix || name.startsWith(s"$prefix/") || name.startsWith(s"$prefix."))
    val directory = owned.exists(name => name == s"$prefix/" || name.startsWith(s"$prefix/"))
    val file      = owned.find(name => name == prefix || name.startsWith(s"$prefix."))
    (directory, file) match
      case (true, Some(_))     => Left(Failure.Corrupt(s"classpath entry $index is both a directory and a file"))
      case (true, None)        => Right(into.resolve(prefix))
      case (false, Some(name)) => Right(into.resolve(name))
      case (false, None)       => Left(Failure.Corrupt(s"classpath entry $index is missing"))
end ClientKit
