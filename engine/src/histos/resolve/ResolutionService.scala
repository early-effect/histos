package histos.resolve

import histos.plugin.{LinkStrategy, PluginLoadError}
import histos.{Dep, ProjectGraph, Resolution}

import java.nio.file.{Files, Path}

import zio.{IO, Ref, Semaphore, UIO, ZIO, ZLayer}

/** The jars a loaded build fetches. The build file selects [[Resolution]]. A cell is solved once and kept until
  * [[ResolutionService.adopt]], which a project reload calls. Solve writes `.histos/resolution`. Lock reads the pin and
  * does not solve.
  */
trait ResolutionService:
  def resolution: UIO[Resolution]
  def current: UIO[DependencyLock]
  def adopt(graph: ProjectGraph): IO[ResolveError, Unit]
  def module(cell: ResolveCell): IO[ResolveError, Classpath]
  def tool(request: ToolRequest): IO[ResolveError, Classpath]
  def plugin(
      dependency: Dep,
      link: LinkStrategy,
      repositories: Repositories,
  ): IO[ResolveError, Classpath]

object ResolutionService:
  def reportPath(repo: Path): Path =
    repo.resolve(".histos").resolve("resolution")

  def file(repo: Path, resolution: Resolution): Path =
    resolution match
      case Resolution.Solve => reportPath(repo)
      case Resolution.Lock  => Resolve.lockPath(repo)

  def open(repo: Path, resolution: Resolution): ZIO[Resolve, ResolveError, ResolutionService] =
    ZIO.serviceWithZIO[Resolve]: resolve =>
      load(repo, resolution, resolve)

  def open(repo: Path, graph: ProjectGraph): ZIO[Resolve, ResolveError, ResolutionService] =
    open(repo, graph.resolution)

  def layer(repo: Path, graph: ProjectGraph): ZLayer[Resolve, ResolveError, ResolutionService] =
    ZLayer.fromZIO(open(repo, graph))

  /** An in-memory pin. A miss fails. Nothing is written. */
  def pinned(lock: DependencyLock): ZLayer[Resolve, Nothing, ResolutionService] =
    ZLayer.fromZIO:
      ZIO.serviceWithZIO[Resolve]: resolve =>
        live(Path.of(""), Resolution.Lock, lock, resolve, persist = false)

  private def load(
      repo: Path,
      resolution: Resolution,
      resolve: Resolve,
  ): IO[ResolveError, ResolutionService] =
    Resolve
      .readStored(file(repo, resolution), resolution)
      .flatMap: lock =>
        live(repo, resolution, lock, resolve, persist = true)

  private def live(
      repo: Path,
      resolution: Resolution,
      lock: DependencyLock,
      resolve: Resolve,
      persist: Boolean,
  ): UIO[ResolutionService] =
    for
      mode   <- Ref.make(resolution)
      state  <- Ref.make(lock)
      memory <- Ref.make(Map.empty[String, Classpath])
      gate   <- Semaphore.make(1)
    yield Live(repo, mode, state, memory, gate, resolve, persist)

  private final class Live(
      repo: Path,
      mode: Ref[Resolution],
      state: Ref[DependencyLock],
      memory: Ref[Map[String, Classpath]],
      gate: Semaphore,
      resolve: Resolve,
      persist: Boolean,
  ) extends ResolutionService:
    def resolution: UIO[Resolution] = mode.get

    def current: UIO[DependencyLock] = state.get

    def adopt(graph: ProjectGraph): IO[ResolveError, Unit] =
      if !persist then ZIO.unit
      else
        val selected = graph.resolution
        Resolve
          .readStored(file(repo, selected), selected)
          .flatMap: lock =>
            mode.set(selected) *> state.set(lock) *> memory.set(Map.empty)

    def module(cell: ResolveCell): IO[ResolveError, Classpath] =
      val key = moduleKey(cell)
      remembered(key).flatMap:
        case Some(classpath) => ZIO.succeed(classpath)
        case None            =>
          mode.get
            .zip(state.get)
            .flatMap: (selected, lock) =>
              Resolve.locate(cell, lock) match
                case Right(held) =>
                  fetch(held.artifacts, held.evictions)
                    .flatMap(keep(key, _))
                    .catchSome:
                      case _: ResolveError.Checksum if selected == Resolution.Solve =>
                        solveModule(cell, key)
                case Left(error) if selected == Resolution.Lock => ZIO.fail(error)
                case Left(_)                                    => solveModule(cell, key)

    def tool(request: ToolRequest): IO[ResolveError, Classpath] =
      val key = toolKey(request)
      remembered(key).flatMap:
        case Some(classpath) => ZIO.succeed(classpath)
        case None            =>
          mode.get
            .zip(state.get)
            .flatMap: (selected, lock) =>
              lock.tools.find(_.id == request.id) match
                case None if selected == Resolution.Lock => ZIO.fail(ResolveError.NoTool(request.id))
                case None                                => solveTool(request, key)
                case Some(held)                          =>
                  LockedTools.validate(request, held) match
                    case Left(error) if selected == Resolution.Lock => ZIO.fail(error)
                    case Left(_)                                    => solveTool(request, key)
                    case Right(_)                                   =>
                      fetch(held.artifacts, held.evictions)
                        .flatMap(keep(key, _))
                        .catchSome:
                          case _: ResolveError.Checksum if selected == Resolution.Solve =>
                            solveTool(request, key)

    def plugin(
        dependency: Dep,
        link: LinkStrategy,
        repositories: Repositories,
    ): IO[ResolveError, Classpath] =
      val key = pluginKey(dependency, link)
      remembered(key).flatMap:
        case Some(classpath) => ZIO.succeed(classpath)
        case None            =>
          mode.get
            .zip(state.get)
            .flatMap: (selected, lock) =>
              PluginCells.locate(lock, dependency) match
                case Right(held) if held.link == link =>
                  fetch(held.artifacts, Nil)
                    .flatMap(keep(key, _))
                    .catchSome:
                      case _: ResolveError.Checksum if selected == Resolution.Solve =>
                        solvePlugin(dependency, link, repositories, key)
                case Left(error) if selected == Resolution.Lock =>
                  ZIO.fail(ResolveError.Plugin(error))
                case Right(_) if selected == Resolution.Lock =>
                  ZIO.fail(
                    ResolveError.Plugin(
                      PluginLoadError.Unlocked(histos.plugin.PluginCoordinate.Published(dependency).render)
                    )
                  )
                case _ => solvePlugin(dependency, link, repositories, key)

    private def solveModule(cell: ResolveCell, key: String): IO[ResolveError, Classpath] =
      gate.withPermit:
        remembered(key).flatMap:
          case Some(classpath) => ZIO.succeed(classpath)
          case None            =>
            mode.get
              .zip(state.get)
              .flatMap: (selected, lock) =>
                Resolve.locate(cell, lock) match
                  case Right(held) =>
                    fetch(held.artifacts, held.evictions).flatMap(keep(key, _))
                  case Left(error) if selected == Resolution.Lock => ZIO.fail(error)
                  case Left(_)                                    =>
                    for
                      classpath <- resolve.resolve(cell)
                      held = Resolve.lockCell(cell, classpath)
                      _ <- state.update(putCell(_, held))
                      _ <- save
                      _ <- memory.update(_.updated(key, classpath))
                    yield classpath

    private def solveTool(request: ToolRequest, key: String): IO[ResolveError, Classpath] =
      gate.withPermit:
        remembered(key).flatMap:
          case Some(classpath) => ZIO.succeed(classpath)
          case None            =>
            mode.get
              .zip(state.get)
              .flatMap: (selected, lock) =>
                lock.tools.find(_.id == request.id) match
                  case Some(held) if LockedTools.validate(request, held).isRight =>
                    fetch(held.artifacts, held.evictions).flatMap(keep(key, _))
                  case None if selected == Resolution.Lock =>
                    ZIO.fail(ResolveError.NoTool(request.id))
                  case Some(held) if selected == Resolution.Lock =>
                    ZIO.fromEither(LockedTools.validate(request, held)) *>
                      fetch(held.artifacts, held.evictions).flatMap(keep(key, _))
                  case _ =>
                    for
                      classpath <- resolve.resolveTool(request)
                      held = Resolve.lockTool(request, classpath)
                      _ <- ZIO.fromEither(LockedTools.validate(request, held))
                      _ <- state.update(putTool(_, held))
                      _ <- save
                      _ <- memory.update(_.updated(key, classpath))
                    yield classpath

    private def solvePlugin(
        dependency: Dep,
        link: LinkStrategy,
        repositories: Repositories,
        key: String,
    ): IO[ResolveError, Classpath] =
      gate.withPermit:
        remembered(key).flatMap:
          case Some(classpath) => ZIO.succeed(classpath)
          case None            =>
            for
              classpath <- resolve.resolvePlugin(dependency, repositories)
              held      <- Resolve
                .lockPlugin(dependency, link, classpath)
                .provideLayer(zio.ZLayer.succeed(resolve))
              _ <- state.update(putPlugin(_, held))
              _ <- save
              _ <- memory.update(_.updated(key, classpath))
            yield classpath

    private def remembered(key: String): UIO[Option[Classpath]] =
      memory.get.flatMap: cached =>
        cached.get(key) match
          case None            => ZIO.succeed(None)
          case Some(classpath) =>
            present(classpath).flatMap: kept =>
              if kept then ZIO.succeed(Some(classpath))
              else memory.update(_.removed(key)).as(None)

    private def present(classpath: Classpath): UIO[Boolean] =
      ZIO
        .attemptBlocking(classpath.artifacts.forall(artifact => Files.isRegularFile(artifact.path)))
        .orElseSucceed(false)

    private def fetch(
        artifacts: List[LockedArtifact],
        evictions: List[Eviction],
    ): IO[ResolveError, Classpath] =
      ZIO.foreach(artifacts)(resolve.fetch).map(Classpath(_, evictions))

    private def keep(key: String, classpath: Classpath): UIO[Classpath] =
      memory.update(_.updated(key, classpath)).as(classpath)

    private def save: IO[ResolveError, Unit] =
      mode.get.flatMap: selected =>
        if !persist || selected != Resolution.Solve then ZIO.unit
        else state.get.flatMap(lock => Resolve.write(reportPath(repo), lock))
  end Live

  private def moduleKey(cell: ResolveCell): String =
    val directs = cell.dependencies
      .map(dep => s"${dep.organization}:${dep.name}:${dep.revision}:${dep.kind}")
      .mkString(",")
    s"module ${cell.module.value} ${cell.scalaVersion.value} ${cell.platform.token} ${cell.scope.header} $directs"

  private def toolKey(request: ToolRequest): String =
    val roots = request.roots
      .map(root => s"${root.organization}:${root.name}:${root.revision}")
      .mkString(",")
    s"tool ${request.id.token} $roots"

  private def pluginKey(dependency: Dep, link: LinkStrategy): String =
    s"plugin ${dependency.organization}:${dependency.name}:${dependency.revision}:${link}"

  private def putCell(lock: DependencyLock, cell: LockedCell): DependencyLock =
    val rest = lock.cells.filterNot: held =>
      held.module == cell.module &&
        held.scalaVersion == cell.scalaVersion &&
        held.platform == cell.platform &&
        held.scope == cell.scope
    lock.copy(cells = rest :+ cell)

  private def putTool(lock: DependencyLock, tool: LockedTool): DependencyLock =
    lock.copy(tools = lock.tools.filterNot(_.id == tool.id) :+ tool)

  private def putPlugin(lock: DependencyLock, plugin: LockedPlugin): DependencyLock =
    val rest = lock.plugins.filterNot: held =>
      held.organization == plugin.organization && held.name == plugin.name && held.revision == plugin.revision
    lock.copy(plugins = rest :+ plugin)
end ResolutionService
