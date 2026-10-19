package histos.load

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import histos.cache.ContentStore
import histos.jdk.Sha256
import histos.plugin.{ApiDigest, PluginIo, PluginLoadError}
import histos.schedule.Digest

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, UIO, ZIO}

/**
 * Compiles a plugin before the build file. The class directory lives under `plugins/<digest>`
 * and outlives the load `Scope`. A directory is reused only after `.complete` is in place.
 */
private[load] object PluginCompile:
  private val Compiler = "3.10.0"

  def fromJar(
      launcher: ApiDigest,
      jar: Path,
      checksums: List[String],
      classpath: List[Path],
  ): ZIO[ContentStore, LoadError, Path] =
    fileSha(jar).flatMap(sources => cached(launcher, sources, checksums, fillJar(jar, classpath)))

  def fromTree(launcher: ApiDigest, tree: Path, classpath: List[Path]): ZIO[ContentStore, LoadError, Path] =
    treeSha(tree).flatMap(sources => cached(launcher, sources, Nil, fillTree(tree, classpath)))

  /** A `.class` file under `histos`, `zio`, or `scala` in a compiled plugin tree. */
  def engineClass(root: Path): Option[String] =
    if !Files.isDirectory(root) then None
    else
      val walk = Files.walk(root)
      try
        walk.iterator.asScala.collectFirst:
          case path if classFile(path) && reserved(rel(root, path)) =>
            rel(root, path).stripSuffix(".class").replace('/', '.')
      finally walk.close()

  private def cached(
      launcher: ApiDigest,
      sources: String,
      checksums: List[String],
      fill: Path => IO[LoadError, Unit],
  ): ZIO[ContentStore, LoadError, Path] =
    ZIO.serviceWithZIO[ContentStore]: store =>
      val hex = key(launcher, sources, checksums)
      val plugins = store.root.resolve("plugins")
      val dest = plugins.resolve(hex)
      ZIO.attempt(ready(dest)).mapError(_ => LoadError.Io(LoadIo.Stat)).flatMap:
        case true => touch(store, dest, hex)
        case false =>
          ZIO.attempt(Files.createDirectories(plugins)).mapError(_ => LoadError.Io(LoadIo.Temp)) *>
            ZIO.acquireReleaseWith(partial(plugins))(deleteTree): tmp =>
              fill(tmp) *> mark(tmp) *> publish(tmp, dest) *> touch(store, dest, hex)

  private def fillJar(jar: Path, classpath: List[Path])(classes: Path): IO[LoadError, Unit] =
    parentOf(classes).flatMap: plugins =>
      ZIO.acquireReleaseWith(partial(plugins, ".partial-src-"))(deleteTree): src =>
        for
          _       <- extract(jar, src)
          sources <- scalaSources(src)
          _       <- scalac(sources, classpath, classes)
          _       <- copyResources(src, classes)
        yield ()

  private def fillTree(tree: Path, classpath: List[Path])(classes: Path): IO[LoadError, Unit] =
    for
      sources <- scalaSources(tree)
      _       <- scalac(sources, classpath, classes)
      _       <- copyResources(tree, classes)
    yield ()

  private def key(launcher: ApiDigest, sources: String, checksums: List[String]): String =
    val lines =
      List(s"api ${launcher.value}", s"scala $Compiler", s"sources $sources") ++
        checksums.sorted.map(sha => s"artifact $sha")
    Sha256.of(Chunk.fromArray(lines.mkString("\n").getBytes(StandardCharsets.UTF_8))).value

  private def partial(parent: Path, prefix: String = ".partial-"): IO[LoadError, Path] =
    ZIO.attempt(Files.createTempDirectory(parent, prefix)).mapError(_ => LoadError.Io(LoadIo.Temp))

  private def parentOf(path: Path): IO[LoadError, Path] =
    Option(path.getParent) match
      case Some(parent) => ZIO.succeed(parent)
      case None         => ZIO.fail(LoadError.Io(LoadIo.Temp))

  private def extract(jar: Path, dest: Path): IO[LoadError, Unit] =
    ZIO.attempt {
      val zip = new ZipFile(jar.toFile)
      try
        zip.entries.asScala.toList.foldLeft[Either[LoadError, Unit]](Right(())):
          case (Left(error), _) => Left(error)
          case (Right(_), entry) =>
            val name = entry.getName
            safe(dest, name) match
              case Left(error) => Left(error)
              case Right(target) if entry.isDirectory =>
                Files.createDirectories(target)
                Right(())
              case Right(target) =>
                Option(target.getParent).foreach(Files.createDirectories(_))
                val stream = zip.getInputStream(entry)
                try Files.copy(stream, target, StandardCopyOption.REPLACE_EXISTING)
                finally stream.close()
                Right(())
      finally zip.close()
    }.mapError(_ => LoadError.Plugin(PluginLoadError.Io(PluginIo.Read))).flatMap(ZIO.fromEither)

  private def safe(root: Path, name: String): Either[LoadError, Path] =
    if name.isEmpty || name.startsWith("/") || name.contains('\\') then
      Left(LoadError.Plugin(PluginLoadError.Io(PluginIo.Read)))
    else
      val target = root.resolve(name).normalize
      val base = root.normalize
      if target.startsWith(base) then Right(target)
      else Left(LoadError.Plugin(PluginLoadError.Io(PluginIo.Read)))

  private def scalaSources(root: Path): IO[LoadError, List[Path]] =
    ZIO.attempt {
      if !Files.isDirectory(root) then Nil
      else
        val walk = Files.walk(root)
        try
          walk.iterator.asScala
            .filter(path => Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS) && isScala(path))
            .toList
            .sortBy(_.toString)
        finally walk.close()
    }.mapError(_ => LoadError.Plugin(PluginLoadError.Io(PluginIo.Read)))

  private def scalac(sources: List[Path], classpath: List[Path], out: Path): IO[LoadError, Unit] =
    sources match
      case Nil => ZIO.fail(LoadError.CompileFailed(List(CompilerMessage.fromCompiler("plugin sources are empty"))))
      case _ =>
        ZIO.attempt {
          val reporter = new StoreReporter(null)
          val args = List(
            "-classpath",
            classpath.map(_.toString).mkString(java.io.File.pathSeparator),
            "-d",
            out.toString,
            "-color:never",
          ) ++ sources.map(_.toString)
          new Driver().process(args.toArray, reporter)
          if reporter.hasErrors then
            Left(LoadError.CompileFailed(reporter.allErrors.map(error => CompilerMessage.fromCompiler(error.message))))
          else Right(())
        }.mapError(_ => LoadError.Io(LoadIo.Compile)).flatMap(ZIO.fromEither)

  private def copyResources(from: Path, to: Path): IO[LoadError, Unit] =
    ZIO.attempt {
      if Files.isDirectory(from) then
        val walk = Files.walk(from)
        try
          walk.iterator.asScala.foreach: path =>
            if Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS) && keep(path) then
              val dest = to.resolve(from.relativize(path))
              Option(dest.getParent).foreach(Files.createDirectories(_))
              Files.copy(path, dest, StandardCopyOption.REPLACE_EXISTING)
        finally walk.close()
    }.mapError(_ => LoadError.Plugin(PluginLoadError.Io(PluginIo.Read)))

  private def fileSha(path: Path): IO[LoadError, String] =
    ZIO.attempt(Sha256.of(Chunk.fromArray(Files.readAllBytes(path))).value)
      .mapError(_ => LoadError.Plugin(PluginLoadError.Io(PluginIo.Read)))

  private def treeSha(root: Path): IO[LoadError, String] =
    ZIO.attempt {
      val files =
        if !Files.isDirectory(root) then Nil
        else
          val walk = Files.walk(root)
          try
            walk.iterator.asScala
              .filter(path => Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS))
              .toList
              .sortBy(path => rel(root, path))
          finally walk.close()
      val lines = files.map: path =>
        val sha = Sha256.of(Chunk.fromArray(Files.readAllBytes(path))).value
        s"${rel(root, path)} $sha"
      Sha256.of(Chunk.fromArray(lines.mkString("\n").getBytes(StandardCharsets.UTF_8))).value
    }.mapError(_ => LoadError.Plugin(PluginLoadError.Io(PluginIo.Read)))

  private def mark(dir: Path): IO[LoadError, Unit] =
    ZIO.attempt(Files.writeString(dir.resolve(".complete"), "ok\n")).mapError(_ => LoadError.Io(LoadIo.Write)).unit

  private def publish(tmp: Path, dest: Path): IO[LoadError, Unit] =
    ZIO.attempt {
      if ready(dest) then deleteTreeSync(tmp)
      else
        if Files.exists(dest) then deleteTreeSync(dest)
        try Files.move(tmp, dest, StandardCopyOption.ATOMIC_MOVE)
        catch
          case _: java.nio.file.AtomicMoveNotSupportedException =>
            Files.move(tmp, dest)
      ()
    }.mapError(_ => LoadError.Io(LoadIo.Write))

  private def touch(store: ContentStore, dest: Path, hex: String): IO[LoadError, Path] =
    val stamped =
      ZIO.attempt(Files.setLastModifiedTime(dest, java.nio.file.attribute.FileTime.from(java.time.Instant.now)))
        .mapError(_ => LoadError.Io(LoadIo.Write))
    val remembered = Digest.from(hex) match
      case Some(digest) => store.remember(digest).mapError(_ => LoadError.Io(LoadIo.Write))
      case None         => ZIO.fail(LoadError.Io(LoadIo.Write))
    stamped *> remembered.as(dest)

  private def ready(dir: Path): Boolean =
    Files.isRegularFile(dir.resolve(".complete"))

  private def deleteTree(root: Path): UIO[Unit] =
    ZIO.attempt(deleteTreeSync(root)).ignore

  private def deleteTreeSync(root: Path): Unit =
    if Files.exists(root) then
      val walk = Files.walk(root)
      val paths =
        try walk.iterator.asScala.toList
        finally walk.close()
      paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))

  private def rel(root: Path, path: Path): String =
    root.relativize(path).toString.replace('\\', '/')

  private def isScala(path: Path): Boolean =
    Option(path.getFileName).exists(_.toString.endsWith(".scala"))

  private def keep(path: Path): Boolean =
    Option(path.getFileName).map(_.toString) match
      case Some(name) => !name.endsWith(".scala") && !name.endsWith(".class")
      case None       => false

  private def classFile(path: Path): Boolean =
    Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS) &&
      Option(path.getFileName).exists(_.toString.endsWith(".class"))

  private def reserved(name: String): Boolean =
    name.startsWith("histos/") || name.startsWith("zio/") || name.startsWith("scala/")
