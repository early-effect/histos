package histos.load

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import histos.HistosBuild

import java.net.URLClassLoader
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.*

trait Loader:
  def load(repo: Path, apiJar: Path): ZIO[Scope, LoadError, LoadedBuild]

object Loader:
  val live: ULayer[Loader] = ZLayer.succeed(LoaderLive)

private object LoaderLive extends Loader:
  def load(repo: Path, apiJar: Path): ZIO[Scope, LoadError, LoadedBuild] =
    val source = repo.resolve("histos.scala")
    for
      _       <- ZIO.unlessZIO(exists(source))(ZIO.fail(LoadError.MissingFile(source.toString)))
      out     <- ZIO.acquireRelease(tempDir("histos-classes"))(deleteTree)
      _       <- compile(source, apiJar, out)
      loader  <- ZIO.acquireRelease(classLoader(out))(closeLoader)
      builds  <- entries(out, loader)
      build   <- select(builds)
      graph   <- ZIO.fromEither(build.graph).mapError(LoadError.BrokenGraph(_))
    yield LoadedBuild(graph)

  private def exists(path: Path): IO[LoadError, Boolean] =
    ZIO.attempt(Files.isRegularFile(path)).mapError(failed)

  private def tempDir(prefix: String): IO[LoadError, Path] =
    ZIO.attempt(Files.createTempDirectory(prefix)).mapError(failed)

  private def compile(source: Path, apiJar: Path, out: Path): IO[LoadError, Unit] =
    classpath(apiJar).flatMap: cp =>
      ZIO.attempt {
        val reporter = new StoreReporter(null)
        new Driver().process(
          Array(
          "-classpath",
          cp,
          "-d",
          out.toString,
          "-color:never",
          source.toString,
          ),
          reporter,
        )
        if reporter.hasErrors then Left(LoadError.CompileFailed(reporter.allErrors.map(_.message)))
        else Right(())
      }.mapError(failed).flatMap(ZIO.fromEither)

  /** The build compiles against the api jar and the Scala libraries, not the engine. */
  private def classpath(apiJar: Path): IO[LoadError, String] =
    ZIO.attempt {
      val libs = List(classOf[scala.collection.immutable.Nil.type], classOf[scala.quoted.Quotes]).flatMap(codeSource)
      (apiJar :: libs).distinct.mkString(java.io.File.pathSeparator)
    }.mapError(failed)

  private def codeSource(cls: Class[?]): Option[Path] =
    Option(cls.getProtectionDomain).flatMap(domain => Option(domain.getCodeSource)).flatMap: source =>
      Option(source.getLocation).map(url => Path.of(url.toURI))

  private def classLoader(out: Path): IO[LoadError, URLClassLoader] =
    ZIO.attempt(new URLClassLoader(Array(out.toUri.toURL), getClass.getClassLoader)).mapError(failed)

  private def closeLoader(loader: URLClassLoader): UIO[Unit] =
    ZIO.attempt(loader.close()).ignore

  private def entries(out: Path, loader: ClassLoader): IO[LoadError, List[HistosBuild]] =
    classFiles(out).flatMap: files =>
      ZIO.foreach(files)(loadOne(loader, out, _)).map(_.flatten)

  private def classFiles(out: Path): IO[LoadError, List[Path]] =
    ZIO.attempt {
      val walk = Files.walk(out)
      try walk.iterator.asScala.filter(isClass).toList
      finally walk.close()
    }.mapError(failed)

  private def isClass(path: Path): Boolean =
    Option(path.getFileName).map(_.toString) match
      case Some(name) => name.endsWith(".class") && !name.contains("$anon")
      case None       => false

  private def loadOne(loader: ClassLoader, out: Path, file: Path): IO[LoadError, Option[HistosBuild]] =
    ZIO.attempt(module(loader.loadClass(className(out, file)))).mapError(failed)

  private def className(out: Path, file: Path): String =
    val relative = out.relativize(file).toString
    relative.stripSuffix(".class").replace(java.io.File.separatorChar, '.')

  private def module(cls: Class[?]): Option[HistosBuild] =
    cls.getDeclaredFields.find(_.getName == "MODULE$") match
      case None        => None
      case Some(field) =>
        try
          field.setAccessible(true)
          field.get(null) match
            case build: HistosBuild => Some(build)
            case _                  => None
        catch
          case _: IllegalAccessException | _: java.lang.reflect.InaccessibleObjectException => None

  private def select(builds: List[HistosBuild]): IO[LoadError, HistosBuild] =
    builds match
      case build :: Nil => ZIO.succeed(build)
      case Nil          => ZIO.fail(LoadError.NoEntry)
      case many         => ZIO.fail(LoadError.AmbiguousEntry(many.map(_.getClass.getName)))

  private def deleteTree(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.ignore

  private def failed(cause: Throwable): LoadError =
    LoadError.Failed(cause.getClass.getName)
