package histos.load

import histos.cache.ContentStore
import histos.jdk.Sha256Digest
import histos.plugin.BootstrapError
import histos.schedule.{Digest, RemoteCache}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompilerStarts, Compilers}
import histos.ScalacFlags

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipInputStream, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Clock, Ref, Scope, ZIO, ZLayer}

/** Content-addressed build-file compile. A repeated key unpacks and does not start the compiler. */
private[histos] object BuildCache:
  type Env = ContentStore & CompilerStarts & Compilers & RemoteCache & PluginFlights

  def ephemeral: ZLayer[Scope, Nothing, Env] =
    ZLayer.makeSome[Scope, Env](
      ZLayer.scoped(
        Workspace
          .work("histos-build-cache")
          .foldZIO(failure => ZIO.dieMessage(failure.message), path => ZIO.succeed(ContentStore.in(path)))
      ),
      CompilerStarts.live,
      Compilers.live,
      RemoteCache.off,
      PluginFlights.live,
    )

  /** A store that stays at `root` after this process exits. */
  def at(root: Path): ZLayer[Scope, Nothing, Env] =
    ZLayer.makeSome[Scope, Env](
      ZLayer.succeed(ContentStore.in(root)),
      CompilerStarts.live,
      Compilers.live,
      RemoteCache.off,
      PluginFlights.live,
    )

  /** `~/.histos/build-cache`, or `build-cache` under `HISTOS_HOME`. */
  def durable: ZLayer[Any, Nothing, Env] =
    ZLayer.scopedEnvironment[Any](
      for
        base <- home
        root <- ZIO.attemptBlocking(Files.createDirectories(base.resolve("build-cache"))).orDie
        env  <- at(root).build
      yield env
    )

  /** `HISTOS_HOME`, or `.histos` under the user home. */
  private def home: zio.UIO[Path] =
    zio.System
      .env("HISTOS_HOME")
      .orElseSucceed(None)
      .flatMap:
        case Some(dir) if dir.trim.nonEmpty => ZIO.succeed(Path.of(dir.trim))
        case _                              =>
          zio.System
            .property("user.home")
            .orElseSucceed(None)
            .map: user =>
              Path.of(user.getOrElse("."), ".histos")

  def compile(
      sources: List[Path],
      classpath: List[Path],
      out: Path,
      extra: String,
  ): ZIO[ContentStore & CompilerStarts, LoadError, Unit] =
    compile(sources, classpath, out, extra, BuildScalac.bootstrapFlags)

  def compile(
      sources: List[Path],
      classpath: List[Path],
      out: Path,
      extra: String,
      flags: ScalacFlags,
  ): ZIO[ContentStore & CompilerStarts, LoadError, Unit] =
    for
      key   <- identity(sources, classpath, extra, flags)
      store <- ZIO.service[ContentStore]
      hit   <- store.read(Digest.trusted(key)).mapError(failed(out, _))
      _     <- hit match
        case Some(bytes) =>
          unpack(out, bytes)
        case None =>
          sayCompiling(sources) *>
            CompilerStarts.tick *>
            BuildFile.compileSources(sources, classpath, out, flags).catchAll(late) *>
            storeZip(store, key, out)
    yield ()

  /** One flushed line per source file. A hit, and an empty list, print nothing. */
  private def sayCompiling(sources: List[Path]): zio.UIO[Unit] =
    ZIO.foreachDiscard(names(sources)): name =>
      zio.Console.printLineError(s"compiling $name").ignore *>
        ZIO.attempt(java.lang.System.err.flush()).ignore

  private def names(sources: List[Path]): List[String] =
    sources.flatMap(source => Option(source.getFileName).map(_.toString))

  private def late(error: LoadError): ZIO[Any, LoadError, Unit] =
    error match
      case LoadError.CompileFailed(messages)
          if messages.exists(message =>
            message.text.contains("withPluginLoading") && message.text.contains("is not a member")
          ) =>
        ZIO.fail(LoadError.Bootstrap(BootstrapError.LateLoading))
      case other =>
        ZIO.fail(other)

  private def identity(
      sources: List[Path],
      classpath: List[Path],
      extra: String,
      flags: ScalacFlags,
  ): ZIO[Any, LoadError, String] =
    for
      files <- ZIO.foreach(sources)(hashFile)
      jars  <- ZIO.foreach(classpath)(hashFile)
      lines = files ++ jars ++ List(s"flags:${flags.canonical}", s"extra:$extra")
    yield Sha256Digest
      .of(Chunk.fromArray(lines.mkString("\n").getBytes(java.nio.charset.StandardCharsets.UTF_8)))
      .value

  private def hashFile(path: Path): ZIO[Any, LoadError, String] =
    ZIO
      .attemptBlocking(hashNow(path))
      .mapError(_ => LoadError.Io(IoFailure(path, IoFault.Read)))

  private def hashNow(path: Path): String =
    if Files.isDirectory(path) then
      val walk = Files.walk(path)
      try
        val lines = walk.iterator.asScala.toList
          .filter(Files.isRegularFile(_))
          .sortBy(_.toString)
          .map: file =>
            val name = path.relativize(file).toString.replace('\\', '/')
            s"$name:${Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(file))).value}"
        Sha256Digest.of(Chunk.fromArray(lines.mkString("\n").getBytes(java.nio.charset.StandardCharsets.UTF_8))).value
      finally walk.close()
    else Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(path))).value

  private def storeZip(store: ContentStore, key: String, out: Path): ZIO[Any, LoadError, Unit] =
    for
      bytes <- zip(out)
      now   <- Clock.instant
      _     <- store.put(Digest.trusted(key), bytes, now).mapError(failed(out, _))
    yield ()

  private def zip(root: Path): ZIO[Any, LoadError, Chunk[Byte]] =
    ZIO
      .attemptBlocking {
        val buffer = new ByteArrayOutputStream
        val zip    = new ZipOutputStream(buffer)
        if Files.isDirectory(root) then
          val walk = Files.walk(root)
          try
            walk.iterator.asScala.foreach: path =>
              if Files.isRegularFile(path) then
                val name = root.relativize(path).toString.replace('\\', '/')
                if safe(name) then
                  zip.putNextEntry(new ZipEntry(name))
                  zip.write(Files.readAllBytes(path))
                  zip.closeEntry()
          finally
            walk.close()
        zip.close()
        Chunk.fromArray(buffer.toByteArray)
      }
      .mapError(_ => LoadError.Io(IoFailure(root, IoFault.Read)))

  private def unpack(root: Path, bytes: Chunk[Byte]): ZIO[Any, LoadError, Unit] =
    ZIO
      .attemptBlocking(unpackNow(root, bytes))
      .mapError(_ => LoadError.Io(IoFailure(root, IoFault.Write)))

  private def unpackNow(root: Path, bytes: Chunk[Byte]): Unit =
    val base = root.toAbsolutePath.normalize
    if Files.isDirectory(base) then
      val walk   = Files.walk(base)
      val doomed =
        try walk.iterator.asScala.filterNot(_.toAbsolutePath.normalize == base).toList
        finally walk.close()
      doomed.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    Files.createDirectories(base)
    val zip = new ZipInputStream(new ByteArrayInputStream(bytes.toArray))
    try
      var entry = Option(zip.getNextEntry)
      while entry.nonEmpty do
        entry.foreach: found =>
          val name = found.getName
          if safe(name) && !found.isDirectory then
            val dest = base.resolve(name).normalize
            if dest.startsWith(base) then
              Option(dest.getParent).foreach(parent => Files.createDirectories(parent))
              Files.copy(zip, dest, StandardCopyOption.REPLACE_EXISTING)
          zip.closeEntry()
        entry = Option(zip.getNextEntry)
    finally zip.close()

  private def safe(name: String): Boolean =
    name.nonEmpty && !name.startsWith("/") && !name.contains("..") && !name.contains('\\')

  private def failed(path: Path, error: histos.schedule.CacheError): LoadError =
    error match
      case _ => LoadError.Io(IoFailure(path, IoFault.Write))
end BuildCache
