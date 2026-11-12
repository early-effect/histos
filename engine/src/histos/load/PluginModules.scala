package histos.load

import histos.plugin.{PluginCoordinate, PluginLoadError}
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.{CompileError, GraphCompile, Sources}
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion}

import java.nio.file.{Files, Path}
import java.util.zip.{ZipException, ZipFile}

import scala.jdk.CollectionConverters.*

import zio.{IO, Scope, ZIO}

/** In-repo plugin modules, compiled so the wiring can name them. Upstream modules compile first. A ship pin is not
  * resolved here.
  */
private[load] object PluginModules:
  def compile(
      repo: Path,
      graph: ProjectGraph,
      boundary: List[Path],
  ): ZIO[Scope, LoadError, List[Path]] =
    ZIO
      .fromEither(ordered(graph))
      .flatMap: modules =>
        val all   = graph.projects.collect { case module: Module => module }
        val roots = coordinates(graph)
        ZIO.foldLeft(modules)(List.empty[Path]): (classpath, module) =>
          val need = if roots.contains(module.name) then Need.Required else Need.Optional
          compileOne(repo, all, module, boundary ++ classpath, need).map: added =>
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
  ): ZIO[Scope, LoadError, List[Path]] =
    versionOf(module) match
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
              onBoundary(base, classpath).flatMap:
                case true  => ZIO.succeed(Nil)
                case false =>
                  Sources
                    .select(base, version, Platform.Jvm)
                    .mapError(failed)
                    .flatMap: found =>
                      val sources = scalaSources(found)
                      if sources.isEmpty then missing(module, need)
                      else
                        for
                          out <- BuildFile.tempDir("histos-plugin")
                          _   <- BuildFile.compileSources(sources, classpath, out)
                        yield List(out)

  /** The wiring compiler is this process. A module that names it uses that source tree. One declared version uses that
    * version. Several versions, none of them this compiler, are not a guess.
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

  /** The running process already loaded this module: its sources define [[histos.HistosBuild]] and the boundary jar
    * contains that class. Compiling it again, against an assembly, collides with packages the assembly also contains.
    * The plugin links the boundary jar.
    */
  private def onBoundary(base: Path, boundary: List[Path]): IO[LoadError, Boolean] =
    ZIO
      .attemptBlocking(definesHost(base) && boundary.exists(hasHost))
      .mapError(_ => LoadError.Io(IoFailure(base, IoFault.Walk)))

  private def definesHost(base: Path): Boolean =
    if !Files.isDirectory(base) then false
    else
      val walk = Files.walk(base)
      try
        walk.iterator.asScala.exists: path =>
          Files.isRegularFile(path) &&
            Option(path.getFileName).exists(_.toString.endsWith(".scala")) &&
            Files.readString(path).contains("trait HistosBuild")
      finally walk.close()

  private def hasHost(jar: Path): Boolean =
    if !Files.isRegularFile(jar) then false
    else
      try
        val zip = new ZipFile(jar.toFile)
        try zip.getEntry(HostClass) != null
        finally zip.close()
      catch case _: ZipException => false

  private val HostClass: String = "histos/HistosBuild.class"

  private def scalaSources(paths: List[Path]): List[Path] =
    paths.filter: path =>
      Option(path.getFileName).exists(_.toString.endsWith(".scala"))

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
