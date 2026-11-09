package histos.format

import histos.select.Selection
import histos.workspace.{IoFailure, IoFault}
import histos.{Module, ProjectGraph}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** The files a [[FormatTargets]] names. A module's trees are `<module>/src` and `<module>/test`, and the repository's
  * `src` and `test` when the graph has one module, the same roots compile and test read.
  */
object FormatScope:
  val buildFile: Path = Path.of("histos.scala")

  def select(repo: Path, graph: ProjectGraph, targets: FormatTargets): IO[FormatError, FormatSelection] =
    val modules = graph.projects.collect { case module: Module => module }
    targets match
      case FormatTargets.Build =>
        for
          trees <- sources(repo, roots(modules, modules))
          build <- ZIO.attemptBlocking(Files.isRegularFile(repo.resolve(buildFile))).orElseSucceed(false)
        yield FormatSelection(trees ++ Option.when(build)(buildFile).toList, GitFilter.Tracked)
      case FormatTargets.Projects(names) =>
        ZIO
          .fromEither(Selection.select(graph, names.toList))
          .mapError(FormatError.Selected(_))
          .flatMap(selected => sources(repo, roots(selected.modules, modules)))
          .map(FormatSelection(_, GitFilter.Tracked))
      case FormatTargets.Files(paths) =>
        ZIO.succeed(FormatSelection(paths.toList.map(_.normalize).distinct, GitFilter.Named))

  private def roots(chosen: List[Module], all: List[Module]): List[Path] =
    val shared = if all.length == 1 then List(Path.of("src"), Path.of("test")) else Nil
    (chosen.flatMap(module => List(Path.of(module.baseDirectory, "src"), Path.of(module.baseDirectory, "test"))) ++
      shared).distinct

  private def sources(repo: Path, roots: List[Path]): IO[FormatError, List[Path]] =
    ZIO
      .foreach(roots)(root => scala(repo, root))
      .map(_.flatten.distinct.sortBy(_.toString))

  private def scala(repo: Path, root: Path): IO[FormatError, List[Path]] =
    val directory = repo.resolve(root)
    ZIO
      .attemptBlocking(Files.isDirectory(directory))
      .mapError(_ => FormatError.Io(IoFailure(directory, IoFault.Stat)))
      .flatMap: present =>
        if !present then ZIO.succeed(Nil)
        else
          ZIO
            .acquireReleaseWith(ZIO.attemptBlocking(Files.walk(directory)))(stream => ZIO.succeed(stream.close())):
              stream =>
                ZIO.attemptBlocking(
                  stream.iterator.asScala
                    .filter(path =>
                      Option(path.getFileName).exists(_.toString.endsWith(".scala")) && Files.isRegularFile(path)
                    )
                    .map(repo.relativize)
                    .toList
                )
            .mapError(_ => FormatError.Io(IoFailure(directory, IoFault.Walk)))
end FormatScope
