package histos.load

import histos.cache.ContentStore
import histos.jdk.Sha256Digest
import histos.lock.{
  ExecutionKind,
  LockDecision,
  LockDefaults,
  LockMode,
  LockPhase,
  LockRefusal,
  LockStage,
  LockStrategy,
}
import histos.plugin.{
  ActivationManifest,
  ApiDigest,
  ArtifactEntry,
  ArtifactFormat,
  ArtifactIdentity,
  ArtifactPath,
  BootstrapCatalog,
  BootstrapError,
  BootstrapSequencer,
  BootstrapSteps,
  ContentHash,
  Digests,
  GenerationId,
  HistosPlugins,
  HistosStrategies,
  InvocationContext,
  InvocationId,
  PluginCoordinate,
  PluginEvents,
  PluginIo,
  PluginLoadError,
  PluginLoading,
  PluginOrigin,
  ResolutionIntent,
  SealedArtifactManifest,
  ServiceActivationError,
  ServiceKey,
  SourceId,
  VerifiedPluginManifest,
}
import histos.PluginRef
import histos.report.WireJson.given
import histos.resolve.Resolve
import histos.toolchain.{HostCompilerInputs, HostCompilerSession, HostToolRole}
import histos.workspace.{IoFailure, IoFault}
import histos.{
  CatalogSpec,
  HistosBuild,
  Organized,
  PluginPublication,
  ProjectDef,
  ProjectGraph,
  ProjectName,
  Projects,
  Resolution,
  Splits,
}
import java.lang.reflect.Modifier
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

import zio.json.*
import zio.json.ast.Json
import zio.{Chunk, Scope, UIO, ZIO, ZLayer}

/** Core seed, optional strategies, custom policy, then main. One loader, one catalog instance. */
private[load] object BootstrapLoad:
  final case class Prepare(
      repo: Path,
      apiJar: Path,
      payload: Option[Path] = None,
      notice: String => UIO[Unit] = _ => ZIO.unit,
      execution: ExecutionKind = ExecutionKind.Local,
      lockMode: Option[LockMode] = None,
  )

  def prepare(resolve: Resolve, request: Prepare): ZIO[Scope & BuildCache.Env, LoadError, LoadedBuild] =
    for
      leases  <- GenerationLeases.make
      _       <- ZIO.addFinalizer(leases.retire)
      session <- materialize(request)
      loaded  <- open(resolve, request, leases, session).onError(_ => leases.discard)
    yield loaded

  private def materialize(request: Prepare) =
    request.payload match
      case None =>
        ZIO.succeed(None)
      case Some(path) =>
        for
          store <- ZIO.service[ContentStore]
          home  <- jdkHome
          live  <- HostCompilerInputs.materialize(path, home, store).mapError(LoadError.HostCompiler(_))
        yield Some(live)

  private def open(
      resolve: Resolve,
      request: Prepare,
      leases: GenerationLeases,
      session: Option[histos.toolchain.HostCompilerSession],
  ): ZIO[Scope & BuildCache.Env, LoadError, LoadedBuild] =
    val catalogFile = request.repo.resolve("histos.plugins.scala")
    leases.use:
      openGeneration(resolve, request, leases, session, catalogFile)

  /** Catalog loader, strategy classes, and the main class directory. The caller scope does not acquire them. */
  private def openGeneration(
      resolve: Resolve,
      request: Prepare,
      leases: GenerationLeases,
      session: Option[histos.toolchain.HostCompilerSession],
      catalogFile: Path,
  ): ZIO[Scope & BuildCache.Env, LoadError, LoadedBuild] =
    for
      launcher   <- digestOf(request.apiJar)
      boundary   <- Boundary.jars(request.apiJar)
      catalogOut <- BuildFile.tempDir("histos-catalog")
      _          <- BuildCache.compile(List(catalogFile), boundary, catalogOut, "catalog")
      opened     <- ZIO.acquireRelease(BuildFile.open(catalogOut, Nil))(BuildFile.close)
      threads    <- PluginThreads.make(opened.build)
      catalog    <- select[HistosPlugins](catalogOut, opened.build)
      defs       <- definitions(catalog)
      full       <- localGraph(catalog, defs)
      steps      <- ZIO.fromEither(plan(catalog)).mapError(LoadError.Bootstrap(_))
      _          <- request.notice("seed")
      seedPaths  <- phase(
        resolve,
        request,
        full,
        steps.seed,
        coreLocks(catalog, request),
        None,
        boundary,
        launcher,
        opened.build,
        session,
      )
      strategy <- strategies(request, catalog, steps, boundary ++ seedPaths, opened.build, leases, launcher)
      _        <- request.notice("strategy")
      rest     <- phase(
        resolve,
        request,
        full,
        steps.remaining,
        strategy.locks.getOrElse(coreLocks(catalog, request)),
        strategy.loading,
        boundary ++ seedPaths,
        launcher,
        opened.build,
        session,
      )
      mainOut <- BuildFile.tempDir("histos-main")
      _       <- BuildCache.compile(
        List(request.repo.resolve("histos.scala")),
        boundary ++ seedPaths ++ rest,
        mainOut,
        "main",
      )
      _      <- ZIO.attemptBlocking(opened.build.add(mainOut)).mapError(BuildFile.failed(mainOut, IoFault.Read))
      builds <- BuildFile.entries(mainOut, opened.build)
      build  <- BuildFile.select(builds)
      graph  <- threads.run(ZIO.fromEither(build.graph).mapError(LoadError.BrokenGraph(_)))
      agreed <- ZIO.fromEither(normalize(catalog.resolution, graph)).mapError(LoadError.Bootstrap(_))
      tasks  <- activateMain(build, strategy, leases, launcher, request.repo)
      linked = agreed.withPlugins(tasks)
      proven  <- VerifiedClosure(launcher, (seedPaths ++ rest).distinct)
      runtime <- PluginRuntime.loaded(
        request.repo,
        linked,
        opened.build,
        threads,
        Nil,
        launcher,
        leases,
        proven,
      )
    yield LoadedBuild(linked, runtime, build, Some(catalog), proven)

  private def plan(plugins: HistosPlugins): Either[BootstrapError, BootstrapSteps.CoreFirst] =
    BootstrapSequencer.coreFirst.plan(BootstrapCatalog.from(plugins)) match
      case Left(_) =>
        Left(BootstrapError.SeedOutsideCatalog)
      case Right(steps) =>
        steps match
          case BootstrapSteps.CoreFirst(seed, early, remaining) =>
            Right(BootstrapSteps.CoreFirst(seed, early, remaining))

  private def strategies(
      request: Prepare,
      plugins: HistosPlugins,
      steps: BootstrapSteps.CoreFirst,
      classpath: List[Path],
      loader: BuildLoader,
      leases: GenerationLeases,
      launcher: ApiDigest,
  ): ZIO[Scope & BuildCache.Env, LoadError, histos.plugin.ActivatedStrategies] =
    steps.early match
      case None =>
        ZIO.succeed(histos.plugin.ActivatedStrategies(None, None))
      case Some(path) =>
        val file = request.repo.resolve(path.value)
        ZIO.ifZIO(exists(file))(
          for
            out   <- BuildFile.tempDir("histos-strategies")
            _     <- BuildCache.compile(List(file), classpath, out, "strategies")
            _     <- ZIO.attemptBlocking(loader.add(out)).mapError(BuildFile.failed(out, IoFault.Read))
            found <- select[HistosStrategies](out, loader)
            live  <- activateStrategies(found, leases, launcher, request.repo)
          yield live,
          ZIO.fail(LoadError.Bootstrap(BootstrapError.MissingStrategy(path))),
        )

  private def phase(
      resolve: Resolve,
      request: Prepare,
      full: ProjectGraph,
      catalog: histos.plugin.PluginCatalog,
      locks: LockStrategy,
      loading: Option[histos.plugin.PluginLoadingStrategy],
      classpath: List[Path],
      launcher: ApiDigest,
      loader: BuildLoader,
      session: Option[histos.toolchain.HostCompilerSession],
  ): ZIO[Scope & BuildCache.Env, LoadError, List[Path]] =
    val wanted = catalog.entries
    val graph  = slice(full, wanted)
    for
      decision <- locks
        .decide(lockContext(request))
        .mapError(error => LoadError.Plugin(PluginLoadError.Policy(error)))
      solving = decision match
        case LockDecision.OfferRepair | LockDecision.Update | LockDecision.LocalSolve => Some(resolve)
        case LockDecision.Frozen | LockDecision.UsePinned | LockDecision.Reject(_)    => None
      modules <-
        if graph.coordinates.exists { case PluginCoordinate.Module(_) => true; case _ => false } then
          PluginModules.compileCached(request.repo, graph, classpath, solving, session)
        else ZIO.succeed(Nil)
      ships <- ships(
        resolve,
        request,
        graph,
        decision,
        launcher,
        session,
        classpath ++ modules,
        hostHashes(session),
        loading,
      )
      paths = modules ++ ships
      _ <- ClasspathIndex.includeAll(classpath ++ paths)
      _ <- ZIO.foreachDiscard(paths)(path =>
        ZIO.attemptBlocking(loader.add(path)).mapError(BuildFile.failed(path, IoFault.Read))
      )
    yield paths

  private def ships(
      resolve: Resolve,
      request: Prepare,
      graph: ProjectGraph,
      decision: LockDecision,
      launcher: ApiDigest,
      session: Option[histos.toolchain.HostCompilerSession],
      classpath: List[Path],
      hosts: Set[String],
      loading: Option[histos.plugin.PluginLoadingStrategy],
  ): ZIO[Scope & BuildCache.Env, LoadError, List[Path]] =
    decision match
      case LockDecision.Frozen | LockDecision.UsePinned =>
        frozen(resolve, request.repo, graph, launcher, hosts)
      case LockDecision.Reject(refusal) =>
        refused(refusal, graph)
      case LockDecision.OfferRepair | LockDecision.Update | LockDecision.LocalSolve =>
        PluginShips.inspect(graph, resolve, launcher, None, hosts).flatMap { stamped =>
          ZIO.foreach(stamped)(item => rebuild(item, session, classpath, loading, request)).map(_.flatten)
        }

  private def frozen(
      resolve: Resolve,
      repo: Path,
      graph: ProjectGraph,
      launcher: ApiDigest,
      hosts: Set[String],
  ): ZIO[Any, LoadError, List[Path]] =
    graph.coordinates.collect { case PluginCoordinate.Published(dependency, _) => dependency } match
      case Nil =>
        ZIO.succeed(Nil)
      case first :: _ =>
        Resolve
          .readLock(repo)
          .foldZIO(
            _ => ZIO.fail(LoadError.Plugin(PluginLoadError.Unlocked(PluginCoordinate.Published(first).render))),
            lock => PluginShips.jars(graph, resolve, launcher, Some(lock), hosts),
          )

  private def refused(refusal: LockRefusal, graph: ProjectGraph): ZIO[Any, LoadError, Nothing] =
    val name = graph.coordinates.headOption match
      case Some(coordinate) => coordinate.render
      case None             => "plugin"
    refusal match
      case LockRefusal.FrozenDrift =>
        ZIO.fail(LoadError.Plugin(PluginLoadError.Unlocked(name)))
      case LockRefusal.Policy(detail) =>
        ZIO.fail(LoadError.Plugin(PluginLoadError.Policy(histos.lock.LockStrategyError.Policy(detail))))

  private def rebuild(
      stamped: PluginShips.Stamped,
      session: Option[histos.toolchain.HostCompilerSession],
      classpath: List[Path],
      loading: Option[histos.plugin.PluginLoadingStrategy],
      request: Prepare,
  ): ZIO[Scope & BuildCache.Env, LoadError, List[Path]] =
    described(stamped) match
      case None =>
        ZIO.succeed(Nil)
      case Some(path) =>
        val binding = stamped.drift match
          case Some(_) => histos.plugin.BindingMatch.Drift
          case None    => histos.plugin.BindingMatch.Exact
        for
          publication <- publicationOf(path)
          recipe      <- recipeOf(path)
          strategy    <- loading match
            case Some(found) => ZIO.succeed(found)
            case None        =>
              ZIO.service[histos.plugin.PluginLoadingStrategy].provide(PluginLoading.exactOrRebuild)
          plan <- strategy
            .plan(loadContext(request, publication, binding, recipe))
            .mapError(error => LoadError.Plugin(PluginLoadError.Loading(error)))
          paths <- follow(plan, stamped, session, classpath, path)
        yield paths

  private def described(stamped: PluginShips.Stamped): Option[Path] =
    stamped.drift match
      case Some(drift) => Some(drift.path)
      case None        =>
        stamped.paths match
          case path :: _ => Some(path)
          case Nil       => None

  private def follow(
      plan: histos.plugin.PluginLoadPlan,
      stamped: PluginShips.Stamped,
      session: Option[histos.toolchain.HostCompilerSession],
      classpath: List[Path],
      path: Path,
  ): ZIO[Scope & BuildCache.Env, LoadError, List[Path]] =
    plan match
      case histos.plugin.PluginLoadPlan.UseExactBinary =>
        ZIO.succeed(stamped.paths)
      case histos.plugin.PluginLoadPlan.Reject(refusal) =>
        ZIO.fail(LoadError.Plugin(PluginLoadError.Refused(path, refusal)))
      case histos.plugin.PluginLoadPlan.Rebuild =>
        stamped.drift match
          case None =>
            ZIO.succeed(stamped.paths)
          case Some(drift) =>
            extract(drift.path).flatMap: sources =>
              if sources.isEmpty then ZIO.fail(LoadError.Plugin(drift))
              else
                for
                  out <- BuildFile.tempDir("histos-rebuild")
                  _   <- session match
                    case None =>
                      BuildCache.compile(sources, classpath, out, drift.path.toString)
                    case Some(hosted) =>
                      PluginCompiler
                        .compile(
                          PluginCompileJob(
                            ProjectName.trusted("rebuilt"),
                            hosted,
                            out,
                            out,
                            sources,
                            classpath,
                            classes = out,
                            analysis = out.resolve("analysis"),
                          )
                        )
                        .mapError:
                          case PluginCompileError.Host(error) => LoadError.HostCompiler(error)
                          case other                          =>
                            LoadError.CompileFailed(List(CompilerMessage.fromCompiler(other.message)))
                  kept = stamped.paths.filterNot(_ == drift.path)
                yield kept :+ out

  private def publicationOf(path: Path): ZIO[Any, LoadError, histos.plugin.PublicationFact] =
    entry(path, "META-INF/histos/plugin.json").flatMap:
      case None =>
        ZIO.succeed(histos.plugin.PublicationFact.Missing)
      case Some(text) =>
        modeOf(text) match
          case Some(PluginPublication.Rebuildable) => ZIO.succeed(histos.plugin.PublicationFact.Rebuildable)
          case Some(PluginPublication.BinaryOnly)  => ZIO.succeed(histos.plugin.PublicationFact.BinaryOnly)
          case None                                => ZIO.fail(LoadError.Plugin(PluginLoadError.Publication(path)))

  private def modeOf(text: String): Option[PluginPublication] =
    text.fromJson[Json] match
      case Right(obj: Json.Obj) =>
        obj.get("mode") match
          case Some(value) =>
            value.as[PluginPublication] match
              case Right(mode) => Some(mode)
              case Left(_)     => None
          case None => None
      case _ => None

  private def recipeOf(path: Path): ZIO[Any, LoadError, histos.plugin.RecipeFact] =
    ZIO
      .attemptBlocking(rebuildSources(path))
      .mapError(BuildFile.failed(path, IoFault.Read))
      .map: present =>
        if present then histos.plugin.RecipeFact.Present else histos.plugin.RecipeFact.Missing

  private def rebuildSources(path: Path): Boolean =
    val prefix = "META-INF/histos/rebuild/"
    val zip    = new ZipFile(path.toFile)
    try
      zip.entries.asScala.exists: entry =>
        val name = entry.getName
        !entry.isDirectory && name.startsWith(prefix) && safe(name) && sourceFile(name)
    finally zip.close()

  private def sourceFile(name: String): Boolean =
    name.endsWith(".scala") || name.endsWith(".java")

  private def extract(path: Path): ZIO[Scope, LoadError, List[Path]] =
    val prefix = "META-INF/histos/rebuild/"
    for
      root  <- BuildFile.tempDir("histos-recipe")
      names <- ZIO
        .attemptBlocking(recipeEntries(path, prefix, root))
        .mapError(BuildFile.failed(path, IoFault.Read))
    yield names

  private def recipeEntries(path: Path, prefix: String, root: Path): List[Path] =
    val zip = new ZipFile(path.toFile)
    try
      zip.entries.asScala.toList.flatMap: entry =>
        val name = entry.getName
        if entry.isDirectory || !name.startsWith(prefix) || !safe(name) then None
        else
          val relative = name.stripPrefix(prefix)
          val dest     = root.resolve(relative).normalize
          if !dest.startsWith(root.toAbsolutePath.normalize) then None
          else
            Option(dest.getParent).foreach(parent => Files.createDirectories(parent))
            val stream = zip.getInputStream(entry)
            try Files.copy(stream, dest)
            finally stream.close()
            if relative.endsWith(".scala") || relative.endsWith(".java") then Some(dest) else None
    finally zip.close()

  private def entry(path: Path, name: String): ZIO[Any, LoadError, Option[String]] =
    ZIO
      .attemptBlocking {
        val zip = new ZipFile(path.toFile)
        try
          Option(zip.getEntry(name)).map: found =>
            val stream = zip.getInputStream(found)
            try new String(stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
            finally stream.close()
        finally zip.close()
      }
      .mapError(BuildFile.failed(path, IoFault.Read))

  private def coreLocks(plugins: HistosPlugins, request: Prepare): LockStrategy =
    val mode = request.lockMode.getOrElse:
      request.execution match
        case ExecutionKind.Local => plugins.bootstrapLocks.local
        case ExecutionKind.Ci    => plugins.bootstrapLocks.ci
    CoreLocks(mode)

  private def localGraph(plugins: HistosPlugins, defs: List[ProjectDef]): ZIO[Any, LoadError, ProjectGraph] =
    val settings = Organized.Settings(
      resolution = plugins.resolution,
      pluginSources = plugins.sources.entries,
      intent = ResolutionIntent.Select(plugins.resolution),
    )
    ZIO
      .fromEither(Projects.build(settings, Splits.aggregate, CatalogSpec.empty, defs))
      .mapError(LoadError.BrokenGraph(_))

  private def slice(graph: ProjectGraph, entries: List[PluginRef]): ProjectGraph =
    val coordinates = graph.coordinates.filter: coordinate =>
      entries.exists:
        case PluginRef.Module(name, _) =>
          coordinate match
            case PluginCoordinate.Module(found)   => found.value == name
            case PluginCoordinate.Published(_, _) => false
        case PluginRef.Ship(dependency, _, _) =>
          coordinate match
            case PluginCoordinate.Published(found, _) =>
              found.organization == dependency.organization && found.name == dependency.name &&
              found.revision == dependency.revision
            case PluginCoordinate.Module(_) => false
    copyCoordinates(graph, coordinates)

  private def normalize(catalog: Resolution, graph: ProjectGraph): Either[BootstrapError, ProjectGraph] =
    graph.intent match
      case ResolutionIntent.Inherit =>
        Right(copyResolution(graph, catalog))
      case ResolutionIntent.Select(selected) if selected == catalog =>
        Right(graph)
      case ResolutionIntent.Select(selected) =>
        Left(BootstrapError.ResolutionConflict(catalog, selected))

  private def copyResolution(graph: ProjectGraph, resolution: Resolution): ProjectGraph =
    copy(graph, resolution, graph.coordinates)

  private def copyCoordinates(graph: ProjectGraph, coordinates: List[PluginCoordinate]): ProjectGraph =
    copy(graph, graph.resolution, coordinates)

  private def copy(
      graph: ProjectGraph,
      resolution: Resolution,
      coordinates: List[PluginCoordinate],
  ): ProjectGraph =
    ProjectGraph.trusted(
      graph.projects,
      graph.splits,
      graph.catalog,
      graph.organization,
      graph.resolvers,
      graph.plugins,
      graph.tools,
      graph.runnerTools,
      graph.facts,
      graph.promote,
      graph.testLog,
      resolution,
      graph.docCheck,
      graph.ci,
      graph.supply,
      graph.coverage,
      graph.coverageExcludes,
      graph.formatting,
      coordinates,
      graph.ciSetup,
      graph.theme,
      graph.intent,
    )

  /** A module `val` is a field on some Scala encodings and only an accessor on others. The accessor and the field are
    * the same value.
    */
  private def definitions(instance: AnyRef): ZIO[Any, LoadError, List[ProjectDef]] =
    ZIO
      .attemptBlocking(members(instance))
      .mapError(BuildFile.failed(Path.of("histos.plugins.scala"), IoFault.Read))

  private def members(instance: AnyRef): List[ProjectDef] =
    val cls    = instance.getClass
    val fields = cls.getDeclaredFields.toList.flatMap: field =>
      if Modifier.isStatic(field.getModifiers) then Nil
      else
        field.setAccessible(true)
        projectOf(field.get(instance))
    val methods = cls.getDeclaredMethods.toList.flatMap: method =>
      if Modifier.isStatic(method.getModifiers) || method.getParameterCount != 0 || method.getName.contains("$") then
        Nil
      else
        method.setAccessible(true)
        try projectOf(method.invoke(instance))
        catch case _: ReflectiveOperationException => Nil
    (fields ++ methods).foldLeft(List.empty[ProjectDef]): (seen, defn) =>
      if seen.exists(_ eq defn) then seen else seen :+ defn

  private def projectOf(value: Any): List[ProjectDef] =
    value match
      case defn: ProjectDef => List(defn)
      case _                => Nil

  private def select[A: ClassTag](out: Path, loader: ClassLoader): ZIO[Any, LoadError, A] =
    BuildFile
      .classFiles(out)
      .flatMap: files =>
        ZIO
          .foreach(files)(loadOne[A](loader, out, _))
          .map(_.flatten)
          .flatMap:
            case one :: Nil => ZIO.succeed(one)
            case Nil        => ZIO.fail(LoadError.NoEntry)
            case many       =>
              ZIO.fail(LoadError.AmbiguousEntry(many.map(value => histos.load.BinaryName.fromClass(value.getClass))))

  private def loadOne[A: ClassTag](loader: ClassLoader, out: Path, file: Path): ZIO[Any, LoadError, Option[A]] =
    ZIO
      .attemptBlocking(module[A](loader.loadClass(BuildFile.className(out, file))))
      .mapError(BuildFile.failed(file, IoFault.Read))

  private def module[A: ClassTag](cls: Class[?]): Option[A] =
    cls.getDeclaredFields.find(_.getName == "MODULE$") match
      case None =>
        None
      case Some(field) =>
        field.setAccessible(true)
        field.get(null) match
          case value: A => Some(value)
          case _        => None

  private def activateStrategies(
      strategies: HistosStrategies,
      leases: GenerationLeases,
      launcher: ApiDigest,
      repo: Path,
  ): ZIO[Scope, LoadError, histos.plugin.ActivatedStrategies] =
    strategies.services match
      case plan: histos.plugin.StrategyPlan[?] =>
        leases
          .use(plan.activate.provideSomeLayer(ambient(launcher, repo)))
          .mapError:
            case error: ServiceActivationError =>
              LoadError.Plugin(PluginLoadError.Activation(error))
            case setup: histos.plugin.ServiceSetupError =>
              LoadError.Plugin(PluginLoadError.Activation(ServiceActivationError.Setup(setup.diagnosis)))
      case _ =>
        ZIO.succeed(histos.plugin.ActivatedStrategies(None, None))

  private def activateMain(
      build: HistosBuild,
      strategies: histos.plugin.ActivatedStrategies,
      leases: GenerationLeases,
      launcher: ApiDigest,
      repo: Path,
  ): ZIO[Scope, LoadError, List[histos.plugin.RegisteredPlugin]] =
    build.services match
      case plan: histos.plugin.Plan[?, ?] =>
        val locks   = strategies.locks.getOrElse(CoreLocks(LockMode.Offer))
        val loading = strategies.loading match
          case Some(found) => ZLayer.succeed(found)
          case None        => PluginLoading.exactOrRebuild
        leases
          .use(
            plan.activate.provideSomeLayer[Scope](
              ambient(launcher, repo) ++
                ZLayer.succeed(locks) ++
                loading
            )
          )
          .mapError:
            case error: ServiceActivationError =>
              LoadError.Plugin(PluginLoadError.Activation(error))
            case setup: histos.plugin.ServiceSetupError =>
              LoadError.Plugin(PluginLoadError.Activation(ServiceActivationError.Setup(setup.diagnosis)))
          .flatMap: activated =>
            ZIO.foreach(activated.tasks): plugin =>
              ZIO
                .attempt(histos.plugin.RegisteredPlugin(plugin.id, plugin.contribute)(plugin))
                .mapError(thrown => LoadError.Plugin(PluginLoadError.Construction(thrown.getClass.getName)))
      case _ =>
        ZIO.succeed(Nil)

  private def ambient(launcher: ApiDigest, repo: Path): ZLayer[Any, Nothing, histos.plugin.GenerationAmbient] =
    val hash    = Digests.sha256(repo.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    val context = new histos.plugin.BuildContext:
      def generation   = GenerationId.trusted(hash)
      def repository   = repo.toString
      def declarations = ContentHash.trusted(hash)
      def host         = launcher
    val events = new PluginEvents.Generation:
      def emit(event: histos.plugin.GenerationPluginEvent) = ZIO.unit
    ZLayer.succeed[histos.plugin.BuildContext](context) ++ ZLayer.succeed[PluginEvents.Generation](events)

  private def lockContext(request: Prepare): histos.lock.LockContext =
    histos.lock.LockContext(
      LockPhase.Plugins,
      LockStage.Discovery,
      request.execution,
      ResolutionIntent.Inherit,
      histos.lock.LockRevisionState.Absent,
      histos.lock.DeclarationDrift.Same,
      histos.lock.NecessaryScope(histos.plugin.PluginDetail.trusted("plugins"), histos.lock.LockDiff(Nil)),
      None,
      InvocationContext(
        InvocationId.derive("prepare"),
        histos.plugin.ClientId.trusted("loader"),
        histos.plugin.CommandIdentity.Other(histos.plugin.CommandToken.trusted("load")),
        request.execution,
        histos.plugin.Interaction.Unattended,
        histos.plugin.OfflineSelection.Online,
      ),
      PluginEvents.closedInvocation,
    )

  private def loadContext(
      request: Prepare,
      publication: histos.plugin.PublicationFact,
      binding: histos.plugin.BindingMatch,
      recipe: histos.plugin.RecipeFact,
  ): histos.plugin.PluginLoadContext =
    histos.plugin.PluginLoadContext(
      publication,
      binding,
      recipe,
      histos.plugin.ClosureFact.Valid,
      histos.plugin.OfflineSelection.Online,
      request.execution,
      InvocationContext(
        InvocationId.derive("prepare"),
        histos.plugin.ClientId.trusted("loader"),
        histos.plugin.CommandIdentity.Other(histos.plugin.CommandToken.trusted("load")),
        request.execution,
        histos.plugin.Interaction.Unattended,
        histos.plugin.OfflineSelection.Online,
      ),
      PluginEvents.closedInvocation,
    )

  private def hostHashes(session: Option[HostCompilerSession]): Set[String] =
    session.toList
      .flatMap(_.tools)
      .collect:
        case tool if tool.entry.role == HostToolRole.Runtime => tool.entry.digest.value
      .toSet

  private def jdkHome: ZIO[Any, LoadError, Path] =
    ZIO
      .attemptBlocking(Option(java.lang.System.getProperty("java.home")))
      .mapError(_ => LoadError.Io(IoFailure(Path.of("java.home"), IoFault.Stat)))
      .flatMap:
        case Some(home) if home.nonEmpty => ZIO.succeed(Path.of(home))
        case _                           => ZIO.fail(LoadError.Io(IoFailure(Path.of("java.home"), IoFault.Stat)))

  private def digestOf(apiJar: Path): ZIO[Any, LoadError, ApiDigest] =
    ZIO
      .attemptBlocking(Sha256Digest.of(Chunk.fromArray(Files.readAllBytes(apiJar))))
      .mapError(BuildFile.failed(apiJar, IoFault.Read))
      .flatMap: sha =>
        ApiDigest.from(sha.value) match
          case Right(digest) => ZIO.succeed(digest)
          case Left(_)       => ZIO.fail(LoadError.Io(IoFailure(apiJar, IoFault.Stat)))

  private def exists(path: Path): ZIO[Any, LoadError, Boolean] =
    ZIO.attemptBlocking(Files.isRegularFile(path)).mapError(BuildFile.failed(path, IoFault.Stat))

  private def safe(name: String): Boolean =
    !name.startsWith("/") && !name.contains("..")

  private final class CoreLocks(mode: LockMode) extends LockStrategy:
    def decide(context: histos.lock.LockContext): zio.IO[histos.lock.LockStrategyError, LockDecision] =
      ZIO.succeed:
        mode match
          case LockMode.Frozen => LockDecision.Frozen
          case LockMode.Offer  => LockDecision.OfferRepair
          case LockMode.Update => LockDecision.Update
          case LockMode.Local  => LockDecision.LocalSolve
end BootstrapLoad

/** Verified closure for jars this load accepted. Empty is `Some(Nil)`, not an unverified result. */
private[load] object VerifiedClosure:
  def apply(host: ApiDigest, paths: List[Path]): ZIO[Any, LoadError, List[ProvenanceResult]] =
    ZIO
      .foreach(paths)(seal)
      .map: manifests =>
        val hash                = Digests.content(manifests.map(_.identity.digest.value).mkString("\n").getBytes(utf8))
        val (defining, closure) = manifests match
          case Nil =>
            (ArtifactIdentity(ArtifactFormat.Jar, hash), Nil)
          case first :: rest =>
            (first.identity, rest.map(_.identity))
        val activation = ActivationManifest(
          SourceId.trusted("closure"),
          ServiceKey.trusted("closure"),
          PluginOrigin.Source(SourceId.trusted("closure"), hash),
          defining,
          closure,
          host,
          hash,
        )
        List(ProvenanceResult.Verified(VerifiedPluginManifest(activation, manifests)))

  private def seal(path: Path): ZIO[Any, LoadError, SealedArtifactManifest] =
    ZIO
      .attemptBlocking(entriesOf(path))
      .mapError(_ => unread(path))
      .flatMap:
        case None          => ZIO.fail(unread(path))
        case Some(entries) =>
          ZIO.fromEither(SealedArtifactManifest.from(entries)).mapError(_ => unread(path))

  private def entriesOf(path: Path): Option[List[ArtifactEntry]] =
    if Files.isDirectory(path) then tree(path)
    else if Files.isRegularFile(path) then archive(path)
    else Some(Nil)

  private def tree(root: Path): Option[List[ArtifactEntry]] =
    val walk = Files.walk(root)
    try
      walk.iterator.asScala.foldLeft[Option[List[ArtifactEntry]]](Some(Nil)): (acc, file) =>
        acc.flatMap: found =>
          if !Files.isRegularFile(file) then Some(found)
          else
            val name = root.relativize(file).toString.replace('\\', '/')
            recorded(name, Files.readAllBytes(file)).map(found :+ _)
    finally walk.close()

  private def archive(path: Path): Option[List[ArtifactEntry]] =
    val zip = new ZipFile(path.toFile)
    try
      zip.entries.asScala.foldLeft[Option[List[ArtifactEntry]]](Some(Nil)): (acc, entry) =>
        acc.flatMap: found =>
          val name = entry.getName
          if entry.isDirectory || !safe(name) then Some(found)
          else
            val stream = zip.getInputStream(entry)
            try recorded(name, stream.readAllBytes()).map(found :+ _)
            finally stream.close()
    finally zip.close()

  private def recorded(name: String, bytes: Array[Byte]): Option[ArtifactEntry] =
    ArtifactPath.from(name) match
      case Left(_)     => None
      case Right(path) => Some(ArtifactEntry(path, bytes.length.toLong, Digests.content(bytes)))

  private def safe(name: String): Boolean =
    name.nonEmpty && !name.startsWith("/") && !name.contains("..") && !name.contains('\\')

  private def unread(path: Path): LoadError =
    LoadError.Plugin(PluginLoadError.Io(PluginIo.Read(path)))

  private val utf8 = java.nio.charset.StandardCharsets.UTF_8
end VerifiedClosure
