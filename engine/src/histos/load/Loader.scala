package histos.load

import dotty.tools.dotc.Driver
import dotty.tools.dotc.reporting.StoreReporter
import histos.cache.ContentStore
import histos.jdk.Sha256
import histos.plugin.{
  ApiDigest,
  EntryName,
  HistosPlugin,
  LinkStrategy,
  PluginCoordinate,
  PluginLinked,
  PluginLoadError,
  RegisteredPlugin,
}
import histos.resolve.{ArtifactName, DependencyLock, LockedPlugin, PluginCells, Resolve, ResolveError, ResolvedArtifact}
import histos.zinc.{CompileProblem, CompileSeverity, PositionBase}
import histos.{Dep, HistosBuild, Platform, ProjectPath, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Scope, UIO, ZIO}

/** Diagnostics from compiling `histos.scala`. Positions are 0-based dotty positions. */
final case class BuildNotes(failed: Boolean, problems: List[CompileProblem])

trait Loader:
  def scan(repo: Path): IO[LoadError, ScannedBuild]
  def load(repo: Path, apiJar: Path): ZIO[Scope & Resolve & ContentStore, LoadError, LoadedBuild]
  def buildClasspath(apiJar: Path): IO[LoadError, List[Path]]
  def reportBuild(repo: Path, apiJar: Path): IO[LoadError, BuildNotes]

object Loader:
  val live: zio.ULayer[Loader] = zio.ZLayer.succeed(LoaderLive)

private final case class Matched(
    classpath: List[Path],
    origin: Path,
    entry: EntryName,
    coordinate: PluginCoordinate,
    linked: PluginLinked,
)

private final case class Opened(build: BuildLoader)

private object LoaderLive extends Loader:
  def buildClasspath(apiJar: Path): IO[LoadError, List[Path]] =
    Boundary.jars(apiJar)

  def reportBuild(repo: Path, apiJar: Path): IO[LoadError, BuildNotes] =
    val display = repo.resolve("histos.scala")
    for
      _        <- ZIO.unlessZIO(exists(display))(ZIO.fail(LoadError.MissingFile(display)))
      text     <- ZIO.attempt(Files.readString(display)).mapError(failed(LoadIo.Stat))
      boundary <- Boundary.jars(apiJar)
      out      <- tempDir("histos-bsp-build")
      file     <- writeTemp(text)
      notes    <- drive(file, boundary, out).map(reporter => notesOf(reporter, display, file))
                    .ensuring(deleteFile(file) *> deleteTree(out))
    yield notes

  def scan(repo: Path): IO[LoadError, ScannedBuild] =
    val source = repo.resolve("histos.scala")
    for
      _    <- ZIO.unlessZIO(exists(source))(ZIO.fail(LoadError.MissingFile(source)))
      text <- ZIO.attempt(Files.readString(source)).mapError(failed(LoadIo.Stat))
      out  <- ZIO.fromEither(HeaderScan.scan(text)).mapError(LoadError.Plugin(_))
    yield out

  def load(repo: Path, apiJar: Path): ZIO[Scope & Resolve & ContentStore, LoadError, LoadedBuild] =
    for
      scanned  <- scan(repo)
      launcher <- digestOf(apiJar)
      boundary <- Boundary.jars(apiJar)
      matched  <- materialize(repo, scanned.requests, launcher, boundary)
      out      <- ZIO.acquireRelease(tempDir("histos-classes"))(deleteTree)
      source   <- ZIO.acquireRelease(writeTemp(scanned.source))(deleteFile)
      jars      = pluginJars(matched)
      _        <- compile(source, boundary ++ jars, out)
      opened   <- ZIO.acquireRelease(open(out, jars))(close)
      live     <- ZIO.foreach(matched)(row => define(opened.build, row))
      builds   <- entries(out, opened.build)
      build    <- select(builds)
      graph    <- graphOf(opened.build, build, matched)
      _        <- checkUses(opened.build, matched, graph.plugins)
    yield LoadedBuild(graph, PluginRuntime.loaded(repo, graph, opened.build, live, links(matched, live), launcher))

  private def materialize(
      repo: Path,
      requests: List[PluginRequest],
      launcher: ApiDigest,
      boundary: List[Path],
  ): ZIO[Resolve & ContentStore, LoadError, List[Matched]] =
    val dependencies = requests.collect { case PluginRequest.Dep(dependency, _) => dependency }
    val locked =
      if dependencies.isEmpty then ZIO.succeed(DependencyLock(Nil))
      else Resolve.readLock(repo).mapError(absent(dependencies))
    locked.flatMap: lock =>
      ZIO.foreach(requests):
        case PluginRequest.Path(path)       => pathPlugin(repo, path, launcher, boundary)
        case PluginRequest.Dep(dependency, _) => one(lock, dependency, launcher, boundary)

  private def one(
      lock: DependencyLock,
      dependency: Dep,
      launcher: ApiDigest,
      boundary: List[Path],
  ): ZIO[Resolve & ContentStore, LoadError, Matched] =
    ZIO.fromEither(PluginCells.locate(lock, dependency)).mapError(LoadError.Plugin(_)).flatMap: locked =>
      for
        artifacts <- ZIO.foreach(locked.artifacts): artifact =>
          ZIO.serviceWithZIO[Resolve](_.fetch(artifact)).mapError(LoadError.Resolve(_))
        direct <- ZIO.fromEither(directJar(dependency, artifacts)).mapError(LoadError.Plugin(_))
        read   <- PluginJar.read(dependency, direct.path)
        linked <- ZIO.fromEither(decide(dependency, locked, read, launcher)).mapError(LoadError.Plugin(_))
        row    <- link(dependency, locked, read, launcher, boundary, artifacts, direct.path, linked)
      yield row

  /** The lock strategy wins. A header change without `--update` does not retarget this cell. */
  private def decide(
      dependency: Dep,
      locked: LockedPlugin,
      read: ReadPlugin,
      launcher: ApiDigest,
  ): Either[PluginLoadError, PluginLinked] =
    val coordinate = PluginCoordinate.Published(dependency)
    locked.link match
      case LinkStrategy.Api => Left(PluginLoadError.ApiLink(dependency))
      case LinkStrategy.ExactBinary =>
        if read.published == launcher then Right(PluginLinked.FromBinary(launcher))
        else Left(PluginLoadError.VersionMismatch(dependency, read.published, launcher))
      case LinkStrategy.Source =>
        if locked.source.isEmpty then Left(PluginLoadError.NoSource(coordinate))
        else Right(PluginLinked.FromSource(read.published, launcher))
      case LinkStrategy.BinaryOrSource =>
        if read.published == launcher then Right(PluginLinked.FromBinary(launcher))
        else if locked.source.isEmpty then Left(PluginLoadError.NoSource(coordinate))
        else Right(PluginLinked.FromSource(read.published, launcher))

  private def link(
      dependency: Dep,
      locked: LockedPlugin,
      read: ReadPlugin,
      launcher: ApiDigest,
      boundary: List[Path],
      artifacts: List[ResolvedArtifact],
      direct: Path,
      linked: PluginLinked,
  ): ZIO[Resolve & ContentStore, LoadError, Matched] =
    val coordinate = PluginCoordinate.Published(dependency)
    linked match
      case PluginLinked.FromBinary(_) =>
        read.leak match
          case Some(binary) => ZIO.fail(LoadError.Plugin(PluginLoadError.EngineLeak(coordinate, binary)))
          case None =>
            val classpath = artifacts.filterNot(artifact => Boundary.supplied(artifact.name)).map(_.path)
            ZIO.succeed(Matched(classpath, direct, read.entry, coordinate, linked))
      case PluginLinked.FromSource(_, _) =>
        locked.source match
          case None => ZIO.fail(LoadError.Plugin(PluginLoadError.NoSource(coordinate)))
          case Some(source) =>
            val libraries = artifacts.filter(artifact => !same(artifact.path, direct) && !Boundary.supplied(artifact.name)).map(_.path)
            for
              fetched <- ZIO.serviceWithZIO[Resolve](_.sources(source.organization, source.name, source.revision, source.repository))
                .mapError(LoadError.Resolve(_))
              _ <-
                if fetched.sha256 == source.sha256 then ZIO.unit
                else ZIO.fail(LoadError.Resolve(ResolveError.Checksum(fetched.path, source.sha256, fetched.sha256)))
              classes <- PluginCompile.fromJar(launcher, fetched.path, locked.artifacts.map(_.sha256.value), boundary ++ libraries)
              _       <- refuseLeak(classes, coordinate)
            yield Matched(classes :: libraries, classes, read.entry, coordinate, linked)

  private def pathPlugin(
      repo: Path,
      path: ProjectPath,
      launcher: ApiDigest,
      boundary: List[Path],
  ): ZIO[ContentStore, LoadError, Matched] =
    val root = repo.resolve(path.value)
    val coordinate = PluginCoordinate.Path(path)
    for
      present <- ZIO.attempt(Files.isDirectory(root)).mapError(failed(LoadIo.Stat))
      _       <- ZIO.unless(present)(ZIO.fail(LoadError.Plugin(PluginLoadError.NoEntry(coordinate.render))))
      classes <- PluginCompile.fromTree(launcher, root, boundary)
      entry   <- PluginJar.readTree(coordinate, classes)
      _       <- refuseLeak(classes, coordinate)
    yield Matched(List(classes), classes, entry, coordinate, PluginLinked.FromSource(launcher, launcher))

  private def refuseLeak(classes: Path, coordinate: PluginCoordinate): IO[LoadError, Unit] =
    ZIO.attempt(PluginCompile.engineClass(classes)).mapError(failed(LoadIo.Walk)).flatMap:
      case Some(binary) => ZIO.fail(LoadError.Plugin(PluginLoadError.EngineLeak(coordinate, binary)))
      case None         => ZIO.unit

  private def directJar(dependency: Dep, artifacts: List[ResolvedArtifact]): Either[PluginLoadError, ResolvedArtifact] =
    val published = ArtifactName.of(dependency, ScalaVersion.trusted("3.10.0"), Platform.Jvm)
    artifacts.find(artifact =>
      artifact.organization == dependency.organization &&
        artifact.name == published &&
        artifact.revision == dependency.revision
    ) match
      case Some(artifact) => Right(artifact)
      case None           => Left(PluginLoadError.NoEntry(PluginCoordinate.Published(dependency).render))

  private def pluginJars(matched: List[Matched]): List[Path] =
    matched.flatMap(_.classpath).distinct

  private def absent(dependencies: List[Dep])(error: ResolveError): LoadError =
    error match
      case ResolveError.NoLock(_) =>
        dependencies match
          case head :: _ => LoadError.Plugin(PluginLoadError.Unlocked(PluginCoordinate.Published(head).render))
          case Nil       => LoadError.Io(LoadIo.Stat)
      case other => LoadError.Resolve(other)

  private def define(loader: BuildLoader, row: Matched): IO[LoadError, HistosPlugin] =
    ZIO.attempt(loader.loadClass(row.entry.value)).mapError(blown(loader, row, _)).flatMap: cls =>
      pluginOf(cls) match
        case Some(plugin) => ZIO.succeed(plugin)
        case None         => ZIO.fail(LoadError.Plugin(PluginLoadError.NoEntry(row.coordinate.render)))

  private def blown(loader: BuildLoader, row: Matched, err: Throwable): LoadError =
    loader.takeRefused match
      case Some(binary) => LoadError.Plugin(PluginLoadError.EngineLeak(row.coordinate, binary))
      case None         =>
        err match
          case _: IllegalAccessException | _: java.lang.reflect.InaccessibleObjectException =>
            LoadError.Plugin(PluginLoadError.NoEntry(row.coordinate.render))
          case _ =>
            LoadError.Plugin(PluginLoadError.Defect(row.coordinate, err.getClass.getName))

  private def graphOf(loader: BuildLoader, build: HistosBuild, matched: List[Matched]): IO[LoadError, histos.ProjectGraph] =
    withContext(loader, ZIO.fromEither(build.graph).mapError(LoadError.BrokenGraph(_))).either.flatMap: outcome =>
      loader.takeRefused match
        case Some(binary) =>
          matched match
            case row :: _ => ZIO.fail(LoadError.Plugin(PluginLoadError.EngineLeak(row.coordinate, binary)))
            case Nil      => outcome.fold(error => ZIO.fail(error), value => ZIO.succeed(value))
        case None => outcome.fold(error => ZIO.fail(error), value => ZIO.succeed(value))

  private def withContext[A](loader: ClassLoader, effect: IO[LoadError, A]): IO[LoadError, A] =
    ZIO.acquireReleaseWith(
      ZIO.attempt {
        val previous = Thread.currentThread.getContextClassLoader
        Thread.currentThread.setContextClassLoader(loader)
        previous
      }.mapError(failed(LoadIo.OpenLoader))
    )(previous => ZIO.attempt(Thread.currentThread.setContextClassLoader(previous)).ignore)(_ => effect)

  private def checkUses(loader: BuildLoader, matched: List[Matched], plugins: List[RegisteredPlugin]): IO[LoadError, Unit] =
    ZIO.foreachDiscard(plugins)(plugin => checkOne(loader, matched, plugin))

  private def checkOne(loader: BuildLoader, matched: List[Matched], plugin: RegisteredPlugin): IO[LoadError, Unit] =
    ZIO.attempt(loader.loadClass(plugin.entry.value)).mapError(missed(loader, matched, plugin, _)).flatMap: cls =>
      val source = Boundary.codeSource(cls)
      matched.find(row => source.exists(path => same(path, row.origin))) match
        case None => ZIO.fail(LoadError.Plugin(PluginLoadError.ForeignEntry(plugin.id, plugin.entry)))
        case Some(row) if plugin.entry != row.entry =>
          ZIO.fail(LoadError.Plugin(PluginLoadError.NotEntry(plugin.id, plugin.entry, row.entry)))
        case Some(row) =>
          pluginOf(cls) match
            case None => ZIO.fail(LoadError.Plugin(PluginLoadError.NoEntry(row.coordinate.render)))
            case Some(found) if found.id != plugin.id =>
              ZIO.fail(LoadError.Plugin(PluginLoadError.IdMismatch(plugin.id, found.id)))
            case Some(_) => ZIO.unit

  private def missed(loader: BuildLoader, matched: List[Matched], plugin: RegisteredPlugin, err: Throwable): LoadError =
    loader.takeRefused match
      case Some(binary) =>
        matched match
          case row :: _ => LoadError.Plugin(PluginLoadError.EngineLeak(row.coordinate, binary))
          case Nil      => LoadError.Plugin(PluginLoadError.Undeclared(plugin.id))
      case None =>
        err match
          case _: ClassNotFoundException => LoadError.Plugin(PluginLoadError.Undeclared(plugin.id))
          case _                         =>
            matched match
              case row :: _ => LoadError.Plugin(PluginLoadError.Defect(row.coordinate, err.getClass.getName))
              case Nil      => LoadError.Plugin(PluginLoadError.Undeclared(plugin.id))

  private def pluginOf(cls: Class[?]): Option[HistosPlugin] =
    val field =
      try Some(cls.getField("MODULE$"))
      catch case _: NoSuchFieldException => None
    field.flatMap: found =>
      try
        found.setAccessible(true)
        found.get(null) match
          case plugin: HistosPlugin => Some(plugin)
          case _                    => None
      catch case _: IllegalAccessException | _: java.lang.reflect.InaccessibleObjectException => None

  private def links(matched: List[Matched], instances: List[HistosPlugin]): List[PluginLink] =
    matched.zip(instances).map: (row, plugin) =>
      PluginLink(plugin.id, row.linked, row.classpath.map(_.toString))

  private def same(left: Path, right: Path): Boolean =
    try Files.isSameFile(left, right)
    catch case scala.util.control.NonFatal(_) => left.toAbsolutePath.normalize == right.toAbsolutePath.normalize

  private def digestOf(apiJar: Path): IO[LoadError, ApiDigest] =
    ZIO.attempt(Sha256.of(Chunk.fromArray(Files.readAllBytes(apiJar)))).mapError(failed(LoadIo.Stat)).flatMap: sha =>
      ApiDigest.from(sha.value) match
        case Right(digest) => ZIO.succeed(digest)
        case Left(_)       => ZIO.fail(LoadError.Io(LoadIo.Stat))

  private def exists(path: Path): IO[LoadError, Boolean] =
    ZIO.attempt(Files.isRegularFile(path)).mapError(failed(LoadIo.Stat))

  private def tempDir(prefix: String): IO[LoadError, Path] =
    ZIO.attempt(Files.createTempDirectory(prefix)).mapError(failed(LoadIo.Temp))

  private def writeTemp(text: String): IO[LoadError, Path] =
    ZIO.attempt {
      val path = Files.createTempFile("histos-build", ".scala")
      Files.writeString(path, text)
      path
    }.mapError(failed(LoadIo.Temp))

  private def deleteFile(path: Path): UIO[Unit] =
    ZIO.attempt(Files.deleteIfExists(path)).ignore

  private def compile(source: Path, classpath: List[Path], out: Path): IO[LoadError, Unit] =
    drive(source, classpath, out).flatMap: reporter =>
      if reporter.hasErrors then
        ZIO.fail(LoadError.CompileFailed(reporter.allErrors.map(error => CompilerMessage.fromCompiler(rewrite(error.message, source)))))
      else ZIO.unit

  private def drive(source: Path, classpath: List[Path], out: Path): IO[LoadError, StoreReporter] =
    ZIO.attempt {
      val reporter = new StoreReporter(null)
      new Driver().process(
        Array(
          "-classpath",
          classpath.map(_.toString).mkString(java.io.File.pathSeparator),
          "-d",
          out.toString,
          "-color:never",
          source.toString,
        ),
        reporter,
      )
      reporter
    }.mapError(failed(LoadIo.Compile))

  private def notesOf(reporter: StoreReporter, display: Path, temp: Path): BuildNotes =
    val problems = reporter.allErrors.map(error => noted(error, display, temp))
    BuildNotes(reporter.hasErrors, problems)

  private def noted(diagnostic: dotty.tools.dotc.reporting.Diagnostic, display: Path, temp: Path): CompileProblem =
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
      PositionBase.Dotty,
    )

  private def rewrite(message: String, temp: Path): String =
    List(temp.toString, temp.toAbsolutePath.toString).distinct.foldLeft(message)((text, from) => text.replace(from, "histos.scala"))

  private def open(classes: Path, jars: List[Path]): IO[LoadError, Opened] =
    ZIO.attempt {
      val host = Option(classOf[HistosBuild].getClassLoader).getOrElse(ClassLoader.getPlatformClassLoader)
      val parent = new ApiBoundary(host)
      val build = new BuildLoader((classes :: jars).map(_.toUri.toURL).toArray, parent)
      Opened(build)
    }.mapError(failed(LoadIo.OpenLoader))

  private def close(opened: Opened): UIO[Unit] =
    ZIO.attempt(opened.build.close()).ignore

  private def entries(out: Path, loader: ClassLoader): IO[LoadError, List[HistosBuild]] =
    classFiles(out).flatMap: files =>
      ZIO.foreach(files)(loadOne(loader, out, _)).map(_.flatten)

  private def classFiles(out: Path): IO[LoadError, List[Path]] =
    ZIO.attempt {
      val walk = Files.walk(out)
      try walk.iterator.asScala.filter(isClass).toList
      finally walk.close()
    }.mapError(failed(LoadIo.Walk))

  private def isClass(path: Path): Boolean =
    Option(path.getFileName).map(_.toString) match
      case Some(name) => name.endsWith(".class") && !name.contains("$anon")
      case None       => false

  private def loadOne(loader: ClassLoader, out: Path, file: Path): IO[LoadError, Option[HistosBuild]] =
    ZIO.attempt(module(loader.loadClass(className(out, file)))).mapError(failed(LoadIo.LoadClass))

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
        catch case _: IllegalAccessException | _: java.lang.reflect.InaccessibleObjectException => None

  private def select(builds: List[HistosBuild]): IO[LoadError, HistosBuild] =
    builds match
      case build :: Nil => ZIO.succeed(build)
      case Nil          => ZIO.fail(LoadError.NoEntry)
      case many         => ZIO.fail(LoadError.AmbiguousEntry(many.map(build => BinaryName.fromClass(build.getClass))))

  private def deleteTree(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.ignore

  private def failed(op: LoadIo): Throwable => LoadError =
    _ => LoadError.Io(op)
