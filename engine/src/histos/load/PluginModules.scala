package histos.load

import histos.cache.ContentStore
import histos.plugin.{BootstrapError, InputKey, PluginCoordinate, PluginLoadError, SemanticSetting}
import histos.resolve.{BuildResolvers, ClasspathScope, Repositories, Resolve, ResolveCell}
import histos.schedule.RemoteCache
import histos.toolchain.{HostCompilerError, HostCompilerSession, HostToolRole}
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.{CompileError, CompilerStarts, Compilers, GraphCompile, Sources}
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path, StandardCopyOption}

import scala.jdk.CollectionConverters.*

import zio.{IO, Scope, ZIO}

/** In-repo plugin modules, compiled so a later build file can name them. Upstream modules compile first. A ship pin is
  * not resolved here.
  */
private[load] object PluginModules:
  def compile(
      repo: Path,
      graph: ProjectGraph,
      boundary: List[Path],
  ): ZIO[Scope, LoadError, List[Path]] =
    run(repo, graph, boundary, None, None, Set.empty).provideSomeLayer[Scope](BuildCache.ephemeral)

  def compile(
      repo: Path,
      graph: ProjectGraph,
      boundary: List[Path],
      resolve: Resolve,
  ): ZIO[Scope, LoadError, List[Path]] =
    run(repo, graph, boundary, Some(resolve), None, Set.empty).provideSomeLayer[Scope](BuildCache.ephemeral)

  /** Java, resources, flags, and a content-addressed class directory. A missing host bridge is typed. */
  def compileCached(
      repo: Path,
      graph: ProjectGraph,
      boundary: List[Path],
      resolve: Option[Resolve],
      session: Option[HostCompilerSession],
      skip: Set[ProjectName] = Set.empty,
  ): ZIO[Scope & BuildCache.Env, LoadError, List[Path]] =
    run(repo, graph, boundary, resolve, session, skip)

  private def run(
      repo: Path,
      graph: ProjectGraph,
      boundary: List[Path],
      resolve: Option[Resolve],
      session: Option[HostCompilerSession],
      skip: Set[ProjectName],
  ): ZIO[Scope & BuildCache.Env, LoadError, List[Path]] =
    walk(repo, graph, boundary, resolve, session, skip)

  private def walk(
      repo: Path,
      graph: ProjectGraph,
      boundary: List[Path],
      resolve: Option[Resolve],
      session: Option[HostCompilerSession],
      skip: Set[ProjectName],
  ): ZIO[Scope & BuildCache.Env, LoadError, List[Path]] =
    ZIO
      .fromEither(ordered(graph))
      .flatMap: modules =>
        val all          = graph.projects.collect { case module: Module => module }
        val roots        = coordinates(graph)
        val repositories = BuildResolvers(graph.resolvers)
        ZIO.foldLeft(modules)(List.empty[Path]): (classpath, module) =>
          if skip.contains(module.name) then ZIO.succeed(classpath)
          else
            val need = if roots.contains(module.name) then Need.Required else Need.Optional
            compileOne(repo, all, module, boundary ++ classpath, need, resolve, session, repositories).map: added =>
              classpath ++ added

  private def coordinates(graph: ProjectGraph): Set[ProjectName] =
    graph.coordinates.collect { case PluginCoordinate.Module(name) => name }.toSet

  private def ordered(graph: ProjectGraph): Either[LoadError, List[Module]] =
    val modules = graph.projects.collect { case module: Module => module }
    val byName  = modules.map(module => module.name -> module).toMap
    val roots   = graph.coordinates.collect { case PluginCoordinate.Module(name) => name }
    val ready   = roots.foldLeft[Either[LoadError, Unit]](Right(())): (acc, name) =>
      acc.flatMap: _ =>
        byName.get(name) match
          case Some(module) if jvm(module) => Right(())
          case _                           =>
            Left(LoadError.Plugin(PluginLoadError.NoSource(PluginCoordinate.Module(name))))
    ready.map: _ =>
      topo(Module.closure(modules, roots.toSet).filter(jvm))

  private def jvm(module: Module): Boolean =
    module.platforms.all.contains(Platform.Jvm)

  private enum Need:
    case Required, Optional

  /** Dependencies before the module that uses them. A cycle cannot occur: the graph already refused one. */
  private def topo(modules: List[Module]): List[Module] =
    def step(left: List[Module], done: Set[ProjectName], acc: List[Module]): List[Module] =
      left match
        case Nil => acc.reverse
        case _   =>
          left.find(module => module.upstream(Platform.Jvm).forall(done.contains)) match
            case None =>
              acc.reverse ++ left
            case Some(next) =>
              step(left.filterNot(_.name == next.name), done + next.name, next :: acc)
    step(modules, Set.empty, Nil)

  private def compileOne(
      repo: Path,
      modules: List[Module],
      module: Module,
      classpath: List[Path],
      need: Need,
      resolve: Option[Resolve],
      session: Option[HostCompilerSession],
      repositories: Repositories,
  ): ZIO[Scope & BuildCache.Env, LoadError, List[Path]] =
    versionOf(module) match
      case None if module.versions.all.length > 1 =>
        ZIO.fail(LoadError.Bootstrap(BootstrapError.AmbiguousScala(module.name.value)))
      case None =>
        missing(module, need)
      case Some(version) =>
        GraphCompile
          .sourceBase(repo, module, modules)
          .mapError(LoadError.Runtime(_))
          .flatMap:
            case None =>
              missing(module, need)
            case Some(base) =>
              Sources
                .select(base, version, Platform.Jvm)
                .mapError(failed)
                .flatMap: found =>
                  if found.isEmpty then missing(module, need)
                  else
                    for
                      extra <- externals(module, version, resolve, repositories)
                      out   <- BuildFile.tempDir("histos-plugin")
                      _     <- emit(repo, module, base, found, classpath ++ extra, out, session)
                      _     <- copyResources(base, out)
                    yield extra :+ out

  /** The plugin module compiler is this process. A module that names it uses that source tree. One declared version
    * uses that version. Several versions, none of them this compiler, are not a guess.
    */
  private def versionOf(module: Module): Option[ScalaVersion] =
    val versions = module.versions.all
    val host     = scala.util.Properties.versionNumberString
    versions
      .find(_.value == host)
      .orElse:
        versions match
          case only :: Nil => Some(only)
          case _           => None

  private def emit(
      repo: Path,
      module: Module,
      base: Path,
      sources: List[Path],
      classpath: List[Path],
      out: Path,
      session: Option[HostCompilerSession],
  ): ZIO[BuildCache.Env, LoadError, Unit] =
    val java = sources.exists(path => Option(path.getFileName).exists(_.toString.endsWith(".java")))
    session match
      case Some(hosted) =>
        hostedCompile(repo, module, base, sources, classpath, out, hosted)
      case None if java =>
        ZIO.fail(LoadError.HostCompiler(HostCompilerError.EntryAbsent(HostToolRole.Bridge)))
      case None =>
        for
          mark <- resourceMark(base)
          _    <- BuildCache.compile(sources, classpath, out, module.flags.canonical + mark)
        yield ()

  private def hostedCompile(
      repo: Path,
      module: Module,
      base: Path,
      sources: List[Path],
      classpath: List[Path],
      out: Path,
      session: HostCompilerSession,
  ): ZIO[BuildCache.Env, LoadError, Unit] =
    for
      settings <- resourceSettings(base)
      _        <- PluginCompiler
        .compile(
          PluginCompileJob(
            module.name,
            session,
            repo,
            repo,
            sources,
            classpath,
            classes = out,
            analysis = out.resolve("analysis"),
            settings = settings,
            flags = module.flags,
          )
        )
        .mapError(compiled)
        .unit
    yield ()

  /** A catalog module's libraries use that graph's repositories. Main is compiled after this, so its resolvers are not
    * this classpath. An empty declaration stays Central. A resolved `scala-library` or `scala3-library` is dropped: the
    * compiler already supplies that runtime, and a second copy disagrees with the assembly.
    */
  private def externals(
      module: Module,
      version: ScalaVersion,
      resolve: Option[Resolve],
      repositories: Repositories,
  ): IO[LoadError, List[Path]] =
    resolve match
      case None                                   => ZIO.succeed(Nil)
      case Some(_) if module.dependencies.isEmpty => ZIO.succeed(Nil)
      case Some(service)                          =>
        val cell = ResolveCell(
          module.name,
          module.dependencies,
          version,
          Platform.Jvm,
          repositories,
          ClasspathScope.Compile,
        )
        service
          .resolve(cell)
          .mapBoth(
            LoadError.Resolve(_),
            found => found.artifacts.map(_.path).filterNot(histos.zinc.JvmCompile.isStandardLibrary),
          )

  private def copyResources(base: Path, out: Path): IO[LoadError, Unit] =
    val resources = base.resolve("main").resolve("resources")
    ZIO
      .attemptBlocking(copyTree(resources, out))
      .mapError(_ => LoadError.Io(IoFailure(resources, IoFault.Read)))

  private def copyTree(resources: Path, out: Path): Unit =
    if Files.isDirectory(resources) then
      val walk = Files.walk(resources)
      try
        walk.iterator.asScala.foreach: path =>
          if Files.isRegularFile(path) then
            val dest = out.resolve(resources.relativize(path))
            Option(dest.getParent).foreach(parent => Files.createDirectories(parent))
            Files.copy(path, dest, StandardCopyOption.REPLACE_EXISTING)
      finally walk.close()

  private def resourceMark(base: Path): IO[LoadError, String] =
    resourceSettings(base).map(settings => settings.map(_.value).mkString(","))

  private def resourceSettings(base: Path): IO[LoadError, List[SemanticSetting]] =
    val resources = base.resolve("main").resolve("resources")
    ZIO
      .attemptBlocking(resourceHashes(resources))
      .mapError(_ => LoadError.Io(IoFailure(resources, IoFault.Read)))
      .flatMap: hashes =>
        hashes match
          case Nil => ZIO.succeed(Nil)
          case _   =>
            ZIO.succeed(List(SemanticSetting(InputKey.trusted("resources"), hashes.mkString(","))))

  private def resourceHashes(resources: Path): List[String] =
    if !Files.isDirectory(resources) then Nil
    else
      val walk = Files.walk(resources)
      try
        walk.iterator.asScala.toList
          .filter(Files.isRegularFile(_))
          .sortBy(_.toString)
          .map: path =>
            val relative = resources.relativize(path).toString.replace('\\', '/')
            val bytes    = Files.readAllBytes(path)
            s"$relative:${histos.plugin.Digests.sha256(bytes)}"
      finally walk.close()

  private def compiled(error: PluginCompileError): LoadError =
    error match
      case PluginCompileError.Host(found)    => LoadError.HostCompiler(found)
      case PluginCompileError.Compile(found) =>
        LoadError.CompileFailed(List(CompilerMessage.fromCompiler(found.message)))
      case other =>
        LoadError.CompileFailed(List(CompilerMessage.fromCompiler(other.message)))

  private def missing(module: Module, need: Need): ZIO[Any, LoadError, List[Path]] =
    need match
      case Need.Required =>
        ZIO.fail(LoadError.Plugin(PluginLoadError.NoSource(PluginCoordinate.Module(module.name))))
      case Need.Optional => ZIO.succeed(Nil)

  private def failed(error: CompileError): LoadError =
    error match
      case CompileError.Io(failure) => LoadError.Io(failure)
      case other                    => LoadError.CompileFailed(List(CompilerMessage.fromCompiler(other.message)))
end PluginModules
