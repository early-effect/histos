package histos.doc

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.pack.ScaladocSite
import histos.resolve.Resolve
import histos.schedule.CacheConfig
import histos.task.{TaskError, TaskResult}
import histos.zinc.{CompiledCell, GraphCompile}
import histos.{Module, ProjectGraph}

import java.nio.file.Path

import zio.{NonEmptyChunk, ZIO, ZLayer}

/** Scaladoc every module a ship publishes. A warning or an error fails the task. Publish itself does not.
  */
object DocTask:
  def fromRepo(
      graph: ProjectGraph,
      repo: Path,
      machine: Machine,
  ): ZIO[Resolve & ContentStore, TaskError, TaskResult] =
    check(graph, repo, machine).as(TaskResult.Doc)

  private def check(
      graph: ProjectGraph,
      repo: Path,
      machine: Machine,
  ): ZIO[Resolve & ContentStore, TaskError, Unit] =
    val names = graph.catalog.ships.flatMap(_.members.toList).distinct
    if names.isEmpty then ZIO.unit
    else
      val modules = graph.projects.collect { case module: Module => module }
      ZIO.scoped:
        for
          process <- CacheConfig.processEnv
          remote  <- CacheConfig.open(process).mapError(error => refused(error.message))
          jdk     <- GraphCompile.jdk(machine).mapError(error => refused(error.message))
          cells   <- GraphCompile
            .run(repo, graph, names, jdk, NativeOrigin.read(machine.env), None, _ => ZIO.unit)
            .provideSomeLayer(ZLayer.succeed(remote))
            .mapError(error => refused(error.message))
          found <- ZIO.foreach(cells.filter(cell => names.contains(cell.module))): cell =>
            one(repo, graph, modules, cell)
          _ <- NonEmptyChunk.fromIterableOption(found.flatten) match
            case Some(lines) => ZIO.fail(TaskError.Doc(lines))
            case None        => ZIO.unit
        yield ()

  private def one(
      repo: Path,
      graph: ProjectGraph,
      modules: List[Module],
      cell: CompiledCell,
  ): ZIO[Resolve, TaskError, Option[String]] =
    modules.find(_.name == cell.module) match
      case None         => ZIO.fail(refused(s"'${cell.module.value}' is not a module"))
      case Some(module) =>
        val output = repo
          .resolve(".histos")
          .resolve("doc")
          .resolve(cell.module.value)
          .resolve(cell.version.value)
          .resolve(cell.platform.token)
        DocClasspath
          .of(repo, graph, module, cell)
          .mapError(error => refused(error.message))
          .flatMap: classpath =>
            ScaladocSite
              .render(cell.classes, classpath, cell.module.value, cell.version.value, output, failOnWarning = true)
              .fold(fault => Some(fault.text), _ => None)

  private def refused(line: String): TaskError =
    TaskError.Doc(NonEmptyChunk(line))
end DocTask
