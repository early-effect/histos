package histos.doc

import histos.resolve.{ClasspathScope, LockedTools, ResolutionService, Resolve}
import histos.zinc.{CompiledCell, GraphCompileError, JvmCompile, ScalacClasspath, UpstreamCells}
import histos.{Module, ProjectGraph}

import java.nio.file.Path

import zio.{ZIO, ZLayer}

/** The classpath scaladoc reads: this cell's classes, then the resolved jars and upstream class directories. The jars
  * are the ones compile used: the lock when the build is locked, the resolution report when it solves. The tool's own
  * Scala library stays off this list, so `scala.caps` is not both an object and a package.
  */
object DocClasspath:
  def of(
      repo: Path,
      graph: ProjectGraph,
      module: Module,
      cell: CompiledCell,
  ): ZIO[Resolve, GraphCompileError, List[Path]] =
    val modules = graph.projects.collect { case item: Module => item }
    val chain   = Module.chain(module, modules, cell.platform)
    for
      lock  <- ResolutionService.open(repo, graph).flatMap(_.current).mapError(GraphCompileError.Lock(_))
      tools <- LockedTools.make(lock, graph)
      found <- UpstreamCells
        .read(module, chain, cell.version, cell.platform, UpstreamCells.declared(repo, modules, cell.platform))
        .flatMap: upstream =>
          ScalacClasspath.of(
            repo,
            module,
            upstream,
            cell.version,
            cell.platform,
            lock,
            tools.repositories,
            ClasspathScope.Compile,
            histos.load.ApiDep.modules(graph),
          )
        .provideSomeLayer[Resolve](ZLayer.succeed(tools))
    yield (cell.classes :: found.paths).filterNot(scalaLibrary)

  private def scalaLibrary(path: Path): Boolean =
    JvmCompile.isStandardLibrary(path) ||
      Option(path.getFileName)
        .map(_.toString)
        .exists: name =>
          name.startsWith("scala3lib_") || name.startsWith("scalalib_")
end DocClasspath
