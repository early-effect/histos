package histos.doc

import histos.resolve.{ClasspathScope, LockedTools, Resolve}
import histos.zinc.{CompiledCell, GraphCompileError, JvmCompile, ScalacClasspath, UpstreamCells}
import histos.{Module, ProjectGraph}

import java.nio.file.Path

import zio.{ZIO, ZLayer}

/** The classpath scaladoc reads: this cell's classes, then the lock and upstream class directories. Each upstream is
  * the cell its compile read. The tool's own Scala library stays off this list, so `scala.caps` is not both an object
  * and a package.
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
      lock  <- Resolve.readLock(repo).mapError(GraphCompileError.Lock(_))
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
