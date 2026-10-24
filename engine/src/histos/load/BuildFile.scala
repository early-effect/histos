package histos.load

import dotty.tools.dotc.reporting.StoreReporter
import histos.cache.{CacheProject, ContentStore}
import histos.jdk.Sha256
import histos.plugin.{
  ApiDigest,
  EntryName,
  HistosPlugin,
  LinkStrategy,
  PluginCoordinate,
  PluginLinked,
  PluginLoadError,
  RegisteredPlugin
}
import histos.resolve.{
  ArtifactName,
  DependencyLock,
  LockedPlugin,
  PluginCells,
  PluginRequest,
  Resolve,
  ResolveError,
  ResolvedArtifact
}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.zinc.{CompileProblem, CompileSeverity, Dotty, PositionBase}
import histos.{Dep, HistosBuild, Platform, ProjectPath, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Scope, UIO, ZIO}

/** Diagnostics from compiling `histos.scala`. Positions are 0-based dotty
  * positions.
  */
private[load] object BuildFile:
  private[load] def tempDir(prefix: String): ZIO[Scope, LoadError, Path] =
    Workspace.work(prefix).mapError(LoadError.Io(_))

  private[load] def writeTemp(text: String): ZIO[Scope, LoadError, Path] =
    ZIO.acquireRelease(
      ZIO
        .attemptBlocking {
          val path = Files.createTempFile("histos-build", ".scala")
          Files.writeString(path, text)
          path
        }
        .mapError(_ =>
          LoadError.Io(IoFailure(Path.of("histos-build.scala"), IoFault.Temp))
        )
    )(path => ZIO.attemptBlocking(Files.deleteIfExists(path)).ignore)

  private[load] def compile(
      source: Path,
      classpath: List[Path],
      out: Path
  ): IO[LoadError, Unit] =
    drive(source, classpath, out).flatMap: reporter =>
      if reporter.hasErrors then
        ZIO.fail(
          LoadError.CompileFailed(
            reporter.allErrors.map(error =>
              CompilerMessage.fromCompiler(rewrite(error.message, source))
            )
          )
        )
      else ZIO.unit

  private[load] def drive(
      source: Path,
      classpath: List[Path],
      out: Path
  ): IO[LoadError, StoreReporter] =
    Dotty
      .process(
        List(
          "-classpath",
          classpath.map(_.toString).mkString(java.io.File.pathSeparator),
          "-d",
          out.toString,
          "-color:never",
          source.toString
        )
      )
      .mapError(_ => LoadError.Io(IoFailure(out, IoFault.Write)))

  private[load] def notesOf(
      reporter: StoreReporter,
      display: Path,
      temp: Path
  ): BuildNotes =
    val problems = reporter.allErrors.map(error => noted(error, display, temp))
    BuildNotes(reporter.hasErrors, problems)

  private[load] def noted(
      diagnostic: dotty.tools.dotc.reporting.Diagnostic,
      display: Path,
      temp: Path
  ): CompileProblem =
    val pos = diagnostic.pos
    val severity = diagnostic.level match
      case 2 => CompileSeverity.Error
      case 1 => CompileSeverity.Warn
      case _ => CompileSeverity.Info
    val placed = pos.exists
    CompileProblem(
      rewrite(diagnostic.message, temp),
      if placed then Some(display) else None,
      severity,
      if placed then Some(pos.startLine) else None,
      if placed then Some(pos.startColumn) else None,
      if placed then Some(pos.endLine) else None,
      if placed then Some(pos.endColumn) else None,
      PositionBase.Dotty
    )

  private[load] def rewrite(message: String, temp: Path): String =
    List(temp.toString, temp.toAbsolutePath.toString).distinct
      .foldLeft(message)((text, from) => text.replace(from, "histos.scala"))

  private[load] def open(
      classes: Path,
      jars: List[Path]
  ): IO[LoadError, Opened] =
    ZIO
      .attemptBlocking {
        val host = Option(classOf[HistosBuild].getClassLoader).getOrElse(
          ClassLoader.getPlatformClassLoader
        )
        val parent = new ApiBoundary(host)
        val build =
          new BuildLoader((classes :: jars).map(_.toUri.toURL).toArray, parent)
        Opened(build)
      }
      .mapError(failed(classes, IoFault.Read))

  private[load] def close(opened: Opened): UIO[Unit] =
    ZIO.attemptBlocking(opened.build.close()).ignore

  private[load] def entries(
      out: Path,
      loader: ClassLoader
  ): IO[LoadError, List[HistosBuild]] =
    classFiles(out).flatMap: files =>
      ZIO.foreach(files)(loadOne(loader, out, _)).map(_.flatten)

  private[load] def classFiles(out: Path): IO[LoadError, List[Path]] =
    ZIO
      .attemptBlocking {
        val walk = Files.walk(out)
        try walk.iterator.asScala.filter(isClass).toList
        finally walk.close()
      }
      .mapError(failed(out, IoFault.Walk))

  private[load] def isClass(path: Path): Boolean =
    Option(path.getFileName).map(_.toString) match
      case Some(name) => name.endsWith(".class") && !name.contains("$anon")
      case None       => false

  private[load] def loadOne(
      loader: ClassLoader,
      out: Path,
      file: Path
  ): IO[LoadError, Option[HistosBuild]] =
    ZIO
      .attemptBlocking(module(loader.loadClass(className(out, file))))
      .mapError(failed(file, IoFault.Read))

  private[load] def className(out: Path, file: Path): String =
    val relative = out.relativize(file).toString
    relative.stripSuffix(".class").replace(java.io.File.separatorChar, '.')

  private[load] def module(cls: Class[?]): Option[HistosBuild] =
    cls.getDeclaredFields.find(_.getName == "MODULE$") match
      case None        => None
      case Some(field) =>
        try
          field.setAccessible(true)
          field.get(null) match
            case build: HistosBuild => Some(build)
            case _                  => None
        catch
          case _: IllegalAccessException |
              _: java.lang.reflect.InaccessibleObjectException =>
            None

  private[load] def select(
      builds: List[HistosBuild]
  ): IO[LoadError, HistosBuild] =
    builds match
      case build :: Nil => ZIO.succeed(build)
      case Nil          => ZIO.fail(LoadError.NoEntry)
      case many         =>
        ZIO.fail(
          LoadError.AmbiguousEntry(
            many.map(build => BinaryName.fromClass(build.getClass))
          )
        )

  private[load] def failed(path: Path, fault: IoFault): Throwable => LoadError =
    _ => LoadError.Io(IoFailure(path, fault))

end BuildFile
