package histos.load

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
import histos.zinc.{CompileProblem, CompileSeverity, PositionBase}
import histos.{Dep, HistosBuild, Platform, ProjectPath, ScalaVersion}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Scope, UIO, ZIO}

/** Diagnostics from compiling `histos.scala`. Positions are 0-based dotty
  * positions.
  */
final case class BuildNotes(failed: Boolean, problems: List[CompileProblem])

trait Loader:
  def scan(repo: Path): IO[LoadError, ScannedBuild]
  def loadLocked(
      repo: Path,
      apiJar: Path,
      lock: DependencyLock
  ): ZIO[Scope, LoadError, LoadedBuild]
  def load(repo: Path, apiJar: Path): ZIO[Scope, LoadError, LoadedBuild]
  def buildClasspath(apiJar: Path): IO[LoadError, List[Path]]
  def reportBuild(repo: Path, apiJar: Path): IO[LoadError, BuildNotes]

object Loader:
  val live: zio.ZLayer[Resolve & ContentStore, Nothing, Loader] =
    zio.ZLayer.fromFunction(LoaderLive(_, _))

private final case class Matched(
    classpath: List[Path],
    origin: Path,
    entry: EntryName,
    coordinate: PluginCoordinate,
    linked: PluginLinked
)

private[load] final case class Opened(build: BuildLoader)

/** The blanked build file and its class directory live as long as the `Scope`.
  */
private final case class Prepared(
    launcher: ApiDigest,
    boundary: List[Path],
    matched: List[Matched],
    source: Path,
    out: Path
)

private final class LoaderLive(resolve: Resolve, store: ContentStore)
    extends Loader:
  def buildClasspath(apiJar: Path): IO[LoadError, List[Path]] =
    Boundary.jars(apiJar)

  def reportBuild(repo: Path, apiJar: Path): IO[LoadError, BuildNotes] =
    ZIO.scoped:
      for
        prepared <- prepare(repo, apiJar)
        reporter <- BuildFile.drive(
          prepared.source,
          prepared.boundary ++ pluginJars(prepared.matched),
          prepared.out
        )
      yield BuildFile.notesOf(
        reporter,
        repo.resolve("histos.scala"),
        prepared.source
      )

  def scan(repo: Path): IO[LoadError, ScannedBuild] =
    val source = repo.resolve("histos.scala")
    for
      _ <- ZIO.unlessZIO(exists(source))(
        ZIO.fail(LoadError.MissingFile(source))
      )
      text <- ZIO
        .attemptBlocking(Files.readString(source))
        .mapError(BuildFile.failed(source, IoFault.Read))
      out <- ZIO.fromEither(HeaderScan.scan(text)).mapError(LoadError.Plugin(_))
    yield out

  def load(repo: Path, apiJar: Path): ZIO[Scope, LoadError, LoadedBuild] =
    loadWith(repo, apiJar, None)

  override def loadLocked(
      repo: Path,
      apiJar: Path,
      lock: DependencyLock
  ): ZIO[Scope, LoadError, LoadedBuild] = loadWith(repo, apiJar, Some(lock))

  private def loadWith(
      repo: Path,
      apiJar: Path,
      lock: Option[DependencyLock]
  ): ZIO[Scope, LoadError, LoadedBuild] =
    for
      prepared <- prepare(repo, apiJar, lock)
      matched = prepared.matched
      jars = pluginJars(matched)
      _ <- BuildFile.compile(
        prepared.source,
        prepared.boundary ++ jars,
        prepared.out
      )
      opened <- ZIO.acquireRelease(BuildFile.open(prepared.out, jars))(
        BuildFile.close
      )
      threads <- PluginThreads.make(opened.build)
      live <- ZIO.foreach(matched)(row => define(opened.build, matched, row))
      builds <- BuildFile.entries(prepared.out, opened.build)
      build <- BuildFile.select(builds)
      graph <- graphOf(opened.build, threads, build, matched)
      _ <- checkUses(opened.build, matched, graph.plugins)
      runtime <- PluginRuntime.loaded(
        repo,
        graph,
        opened.build,
        threads,
        live,
        links(matched, live),
        prepared.launcher
      )
    yield LoadedBuild(graph, runtime)

  private def prepare(
      repo: Path,
      apiJar: Path,
      supplied: Option[DependencyLock] = None
  ): ZIO[Scope, LoadError, Prepared] =
    for
      scanned <- scan(repo)
      launcher <- digestOf(apiJar)
      boundary <- Boundary.jars(apiJar)
      matched <- materialize(
        repo,
        scanned.requests,
        launcher,
        boundary,
        supplied
      )
      out <- BuildFile.tempDir("histos-classes")
      source <- BuildFile.writeTemp(scanned.source)
    yield Prepared(launcher, boundary, matched, source, out)

  private def materialize(
      repo: Path,
      requests: List[PluginRequest],
      launcher: ApiDigest,
      boundary: List[Path],
      supplied: Option[DependencyLock]
  ): IO[LoadError, List[Matched]] =
    val dependencies = requests.collect {
      case PluginRequest.Dep(dependency, _) =>
        dependency
    }
    val locked = supplied match
      case Some(lock)                   => ZIO.succeed(lock)
      case None if dependencies.isEmpty => ZIO.succeed(DependencyLock(Nil))
      case None => Resolve.readLock(repo).mapError(absent(dependencies))
    locked.flatMap: lock =>
      ZIO
        .foreach(requests):
          case PluginRequest.Path(path) =>
            pathPlugin(repo, path, launcher, boundary)
          case PluginRequest.Dep(dependency, _) =>
            one(lock, dependency, launcher, boundary)
        .flatMap: matched =>
          val pins = matched.flatMap(row => digestName(row.origin)).toSet
          store
            .replace(CacheProject.of(repo), pins)
            .mapError(_ => LoadError.Io(IoFailure(repo, IoFault.Write)))
            .as(matched)

  private def one(
      lock: DependencyLock,
      dependency: Dep,
      launcher: ApiDigest,
      boundary: List[Path]
  ): IO[LoadError, Matched] =
    ZIO
      .fromEither(PluginCells.locate(lock, dependency))
      .mapError(LoadError.Plugin(_))
      .flatMap: locked =>
        for
          artifacts <- ZIO.foreach(locked.artifacts): artifact =>
            resolve.fetch(artifact).mapError(LoadError.Resolve(_))
          compiler <- histos.toolchain.BuildCompiler.version
            .mapError(LoadError.Resolve(_))
          direct <- ZIO
            .fromEither(directJar(dependency, artifacts, compiler))
            .mapError(LoadError.Plugin(_))
          read <- PluginJar.read(dependency, direct.path)
          linked <- ZIO
            .fromEither(decide(dependency, locked, read, launcher))
            .mapError(LoadError.Plugin(_))
          row <- link(
            LinkJob(
              dependency,
              locked,
              read,
              launcher,
              boundary,
              artifacts,
              direct.path,
              linked
            )
          )
        yield row

  /** The lock strategy wins. A header change without `--update` does not
    * retarget this cell.
    */
  private def decide(
      dependency: Dep,
      locked: LockedPlugin,
      read: ReadPlugin,
      launcher: ApiDigest
  ): Either[PluginLoadError, PluginLinked] =
    val coordinate = PluginCoordinate.Published(dependency)
    locked.link match
      case LinkStrategy.Api         => Left(PluginLoadError.ApiLink(dependency))
      case LinkStrategy.ExactBinary =>
        if read.published == launcher then
          Right(PluginLinked.FromBinary(launcher))
        else
          Left(
            PluginLoadError.VersionMismatch(
              dependency,
              read.published,
              launcher
            )
          )
      case LinkStrategy.Source =>
        if locked.source.isEmpty then Left(PluginLoadError.NoSource(coordinate))
        else Right(PluginLinked.FromSource(read.published, launcher))
      case LinkStrategy.BinaryOrSource =>
        if read.published == launcher then
          Right(PluginLinked.FromBinary(launcher))
        else if locked.source.isEmpty then
          Left(PluginLoadError.NoSource(coordinate))
        else Right(PluginLinked.FromSource(read.published, launcher))

  private final case class LinkJob(
      dependency: Dep,
      locked: LockedPlugin,
      read: ReadPlugin,
      launcher: ApiDigest,
      boundary: List[Path],
      artifacts: List[ResolvedArtifact],
      direct: Path,
      linked: PluginLinked
  )

  private def link(job: LinkJob): IO[LoadError, Matched] =
    val LinkJob(
      dependency,
      locked,
      read,
      launcher,
      boundary,
      artifacts,
      direct,
      linked
    ) = job
    val coordinate = PluginCoordinate.Published(dependency)
    linked match
      case PluginLinked.FromBinary(_) =>
        read.leak match
          case Some(binary) =>
            ZIO.fail(
              LoadError.Plugin(PluginLoadError.EngineLeak(coordinate, binary))
            )
          case None =>
            val classpath = artifacts
              .filterNot(artifact => Boundary.supplied(artifact.name))
              .map(_.path)
            ZIO.succeed(
              Matched(classpath, direct, read.entry, coordinate, linked)
            )
      case PluginLinked.FromSource(_, _) =>
        locked.source match
          case None =>
            ZIO.fail(LoadError.Plugin(PluginLoadError.NoSource(coordinate)))
          case Some(source) =>
            val libraries = artifacts
              .filter(artifact =>
                !same(artifact.path, direct) && !Boundary.supplied(
                  artifact.name
                )
              )
              .map(_.path)
            for
              fetched <- resolve
                .sources(
                  source.organization,
                  source.name,
                  source.revision,
                  source.repository
                )
                .mapError(LoadError.Resolve(_))
              _ <-
                if fetched.sha256 == source.sha256 then ZIO.unit
                else
                  ZIO.fail(
                    LoadError.Resolve(
                      ResolveError
                        .Checksum(fetched.path, source.sha256, fetched.sha256)
                    )
                  )
              classes <- PluginCompile.fromJar(
                store,
                launcher,
                fetched.path,
                locked.artifacts.map(_.sha256.value),
                boundary ++ libraries
              )
              _ <- refuseLeak(classes, coordinate)
            yield Matched(
              classes :: libraries,
              classes,
              read.entry,
              coordinate,
              linked
            )

  private def pathPlugin(
      repo: Path,
      path: ProjectPath,
      launcher: ApiDigest,
      boundary: List[Path]
  ): IO[LoadError, Matched] =
    val root = repo.resolve(path.value)
    val coordinate = PluginCoordinate.Path(path)
    for
      present <- ZIO
        .attemptBlocking(Files.isDirectory(root))
        .mapError(BuildFile.failed(root, IoFault.Stat))
      _ <- ZIO.unless(present)(
        ZIO.fail(LoadError.Plugin(PluginLoadError.NoEntry(coordinate.render)))
      )
      classes <- PluginCompile.fromTree(store, launcher, root, boundary)
      entry <- PluginJar.readTree(coordinate, classes)
      _ <- refuseLeak(classes, coordinate)
    yield Matched(
      List(classes),
      classes,
      entry,
      coordinate,
      PluginLinked.FromSource(launcher, launcher)
    )

  private def digestName(path: Path): Option[histos.schedule.Digest] =
    Option(path.getFileName)
      .map(_.toString)
      .flatMap(histos.schedule.Digest.from)

  private def refuseLeak(
      classes: Path,
      coordinate: PluginCoordinate
  ): IO[LoadError, Unit] =
    PluginCompile
      .engineClass(classes)
      .flatMap:
        case Some(binary) =>
          ZIO.fail(
            LoadError.Plugin(PluginLoadError.EngineLeak(coordinate, binary))
          )
        case None => ZIO.unit

  private def directJar(
      dependency: Dep,
      artifacts: List[ResolvedArtifact],
      compiler: ScalaVersion
  ): Either[PluginLoadError, ResolvedArtifact] =
    val published =
      ArtifactName.of(dependency, compiler, Platform.Jvm)
    artifacts.find(artifact =>
      artifact.organization == dependency.organization &&
        artifact.name == published &&
        artifact.revision == dependency.revision
    ) match
      case Some(artifact) => Right(artifact)
      case None           =>
        Left(
          PluginLoadError.NoEntry(PluginCoordinate.Published(dependency).render)
        )

  private def pluginJars(matched: List[Matched]): List[Path] =
    matched.flatMap(_.classpath).distinct

  private def absent(dependencies: List[Dep])(error: ResolveError): LoadError =
    error match
      case ResolveError.NoLock(_) =>
        dependencies match
          case head :: _ =>
            LoadError.Plugin(
              PluginLoadError.Unlocked(PluginCoordinate.Published(head).render)
            )
          case Nil =>
            LoadError.Io(
              IoFailure(Path.of(".histos/dependencies.lock"), IoFault.Read)
            )
      case other => LoadError.Resolve(other)

  private def define(
      loader: BuildLoader,
      matched: List[Matched],
      row: Matched
  ): IO[LoadError, HistosPlugin] =
    ZIO
      .attemptBlocking(loader.loadClass(row.entry.value))
      .mapError(blown(loader, matched, row, _))
      .flatMap: cls =>
        pluginOf(cls, blown(loader, matched, row, _)).flatMap:
          case Some(plugin) => ZIO.succeed(plugin)
          case None         =>
            ZIO.fail(
              LoadError.Plugin(PluginLoadError.NoEntry(row.coordinate.render))
            )

  /** `row` is the entry being defined. A refusal asked for by another plugin's
    * class blames that plugin.
    */
  private def blown(
      loader: BuildLoader,
      matched: List[Matched],
      row: Matched,
      err: Throwable
  ): LoadError =
    loader.takeRefused match
      case Some(refusal) =>
        LoadError.Plugin(
          PluginLoadError.EngineLeak(
            ownerOf(matched, refusal).getOrElse(row).coordinate,
            refusal.binary
          )
        )
      case None =>
        err match
          case _: IllegalAccessException |
              _: java.lang.reflect.InaccessibleObjectException =>
            LoadError.Plugin(PluginLoadError.NoEntry(row.coordinate.render))
          case _ =>
            LoadError.Plugin(
              PluginLoadError.Defect(row.coordinate, err.getClass.getName)
            )

  private def graphOf(
      loader: BuildLoader,
      threads: PluginThreads,
      build: HistosBuild,
      matched: List[Matched]
  ): IO[LoadError, histos.ProjectGraph] =
    threads
      .run(ZIO.fromEither(build.graph).mapError(LoadError.BrokenGraph(_)))
      .either
      .flatMap: outcome =>
        loader.takeRefused match
          case Some(refusal) => ZIO.fail(leaked(matched, refusal))
          case None          => ZIO.fromEither(outcome)

  private def leaked(matched: List[Matched], refusal: Refusal): LoadError =
    ownerOf(matched, refusal) match
      case Some(row) =>
        LoadError.Plugin(
          PluginLoadError.EngineLeak(row.coordinate, refusal.binary)
        )
      case None => LoadError.Plugin(PluginLoadError.BuildLeak(refusal.binary))

  /** The plugin row whose classpath defined the class that asked for the
    * refused name.
    */
  private def ownerOf(
      matched: List[Matched],
      refusal: Refusal
  ): Option[Matched] =
    refusal.requester
      .flatMap(Boundary.codeSource)
      .flatMap: source =>
        matched.find(row =>
          (row.origin :: row.classpath).exists(same(source, _))
        )

  private def checkUses(
      loader: BuildLoader,
      matched: List[Matched],
      plugins: List[RegisteredPlugin]
  ): IO[LoadError, Unit] =
    ZIO.foreachDiscard(plugins)(plugin => checkOne(loader, matched, plugin))

  private def checkOne(
      loader: BuildLoader,
      matched: List[Matched],
      plugin: RegisteredPlugin
  ): IO[LoadError, Unit] =
    ZIO
      .attemptBlocking(loader.loadClass(plugin.entry.value))
      .mapError(missed(loader, matched, plugin, _))
      .flatMap: cls =>
        val source = Boundary.codeSource(cls)
        matched.find(row => source.exists(path => same(path, row.origin))) match
          case None =>
            ZIO.fail(
              LoadError
                .Plugin(PluginLoadError.ForeignEntry(plugin.id, plugin.entry))
            )
          case Some(row) if plugin.entry != row.entry =>
            ZIO.fail(
              LoadError.Plugin(
                PluginLoadError.NotEntry(plugin.id, plugin.entry, row.entry)
              )
            )
          case Some(row) =>
            pluginOf(cls, blown(loader, matched, row, _)).flatMap:
              case None =>
                ZIO.fail(
                  LoadError
                    .Plugin(PluginLoadError.NoEntry(row.coordinate.render))
                )
              case Some(found) if found.id != plugin.id =>
                ZIO.fail(
                  LoadError
                    .Plugin(PluginLoadError.IdMismatch(plugin.id, found.id))
                )
              case Some(_) => ZIO.unit

  private def missed(
      loader: BuildLoader,
      matched: List[Matched],
      plugin: RegisteredPlugin,
      err: Throwable
  ): LoadError =
    val entered = matched.find(_.entry == plugin.entry)
    loader.takeRefused match
      case Some(refusal) =>
        ownerOf(matched, refusal).orElse(entered) match
          case Some(row) =>
            LoadError.Plugin(
              PluginLoadError.EngineLeak(row.coordinate, refusal.binary)
            )
          case None =>
            LoadError.Plugin(PluginLoadError.BuildLeak(refusal.binary))
      case None =>
        (err, entered) match
          case (_: ClassNotFoundException, _) =>
            LoadError.Plugin(PluginLoadError.Undeclared(plugin.id))
          case (_, Some(row)) =>
            LoadError.Plugin(
              PluginLoadError.Defect(row.coordinate, err.getClass.getName)
            )
          case (_, None) =>
            LoadError.Plugin(PluginLoadError.Undeclared(plugin.id))

  /** Reading `MODULE$` runs the object's static initializer, so a throw there
    * goes to `fault`.
    */
  private def pluginOf(
      cls: Class[?],
      fault: Throwable => LoadError
  ): IO[LoadError, Option[HistosPlugin]] =
    ZIO
      .attemptBlocking {
        cls.getFields
          .find(_.getName == "MODULE$")
          .map: field =>
            field.setAccessible(true)
            field.get(null)
      }
      .mapBoth(fault, _.collect { case plugin: HistosPlugin => plugin })

  private def links(
      matched: List[Matched],
      instances: List[HistosPlugin]
  ): List[PluginLink] =
    matched
      .zip(instances)
      .map: (row, plugin) =>
        PluginLink(plugin.id, row.linked, row.classpath.map(_.toString))

  private def same(left: Path, right: Path): Boolean =
    try Files.isSameFile(left, right)
    catch
      case scala.util.control.NonFatal(_) =>
        left.toAbsolutePath.normalize == right.toAbsolutePath.normalize

  private def digestOf(apiJar: Path): IO[LoadError, ApiDigest] =
    ZIO
      .attemptBlocking(Sha256.of(Chunk.fromArray(Files.readAllBytes(apiJar))))
      .mapError(BuildFile.failed(apiJar, IoFault.Read))
      .flatMap: sha =>
        ApiDigest.from(sha.value) match
          case Right(digest) => ZIO.succeed(digest)
          case Left(_)       =>
            ZIO.fail(LoadError.Io(IoFailure(apiJar, IoFault.Stat)))

  private def exists(path: Path): IO[LoadError, Boolean] =
    ZIO
      .attemptBlocking(Files.isRegularFile(path))
      .mapError(BuildFile.failed(path, IoFault.Stat))
