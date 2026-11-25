package histos.doc

import histos.build.{BuildNode, BuildRuntime, ExecutionRequest, StageProduct}
import histos.cache.ContentStore
import histos.jdk.Machine
import histos.load.PluginRuntime
import histos.native.NativeOrigin
import histos.pack.ScaladocSite
import histos.plugin.SourceSet
import histos.resolve.{LockedTools, ResolutionService, Resolve}
import histos.schedule.{CacheConfig, Parallelism}
import histos.task.{TaskError, TaskResult}
import histos.zinc.{CompileSession, GraphCompile}
import histos.{Cell, CompilationStage, Module, ProjectGraph}

import java.nio.file.Path

import zio.{NonEmptyChunk, ZIO, ZLayer}

/** Scaladoc every module a ship publishes. A warning or an error fails the task. Publish itself does not.
  */
object DocTask:
  def fromRepo(
      graph: ProjectGraph,
      repo: Path,
      machine: Machine,
      plugins: PluginRuntime,
  ): ZIO[Resolve & ContentStore & ResolutionService, TaskError, TaskResult] =
    check(graph, repo, machine, plugins).as(TaskResult.Doc)

  private def check(
      graph: ProjectGraph,
      repo: Path,
      machine: Machine,
      plugins: PluginRuntime,
  ): ZIO[Resolve & ContentStore & ResolutionService, TaskError, Unit] =
    val names  = graph.catalog.ships.flatMap(_.members.toList).distinct
    val stages = graph.projects
      .collect { case module: Module if names.contains(module.name) => module }
      .flatMap(module =>
        for
          version  <- module.versions.all
          platform <- module.platforms.all
        yield module -> CompilationStage(Cell(module.name, version, platform), SourceSet.Main)
      )
    if stages.isEmpty then ZIO.unit
    else
      ZIO.scoped:
        for
          process <- CacheConfig.processEnv
          remote  <- CacheConfig.open(process).mapError(error => refused(error.message))
          jdk     <- GraphCompile.jdk(machine).mapError(error => refused(error.message))
          lock    <- ZIO.serviceWithZIO[ResolutionService](_.current)
          runtime <- BuildRuntime
            .open(Some(plugins), parallelism = Parallelism.of(graph.parallelism))
            .provideSomeLayer(ZLayer.succeed(remote))
          products <- runtime
            .ensure(
              ExecutionRequest(
                CompileSession(repo, graph, jdk, NativeOrigin.read(machine.env), lock),
                stages.map((_, stage) => BuildNode.Compile(stage)),
              )
            )
            .mapError(error => refused(histos.testrun.TestFailures(error).message))
          resolution <- ZIO.service[ResolutionService]
          tools      <- LockedTools.make(lock, graph, resolution)
          found      <- ZIO
            .foreach(stages) { (module, stage) =>
              ZIO
                .fromEither(products.require(stage))
                .mapError(error => refused(histos.testrun.TestFailures(error).message))
                .flatMap(product => one(repo, module, product))
            }
            .provideLayer(ZLayer.succeed(tools))
          _ <- NonEmptyChunk.fromIterableOption(found.flatten) match
            case Some(lines) => ZIO.fail(TaskError.Doc(lines))
            case None        => ZIO.unit
        yield ()

  private def one(repo: Path, module: Module, product: StageProduct): ZIO[LockedTools, TaskError, Option[String]] =
    val cell   = product.compiled
    val output = repo
      .resolve(".histos")
      .resolve("doc")
      .resolve(cell.module.value)
      .resolve(cell.version.value)
      .resolve(cell.platform.token)
    DocClasspath
      .of(module, product)
      .mapError(error => refused(error.message))
      .flatMap: classpath =>
        ScaladocSite
          .render(
            cell.classes,
            classpath,
            cell.module.value,
            cell.version.value,
            output,
            failOnWarning = true,
            platform = cell.platform,
          )
          .fold(fault => Some(fault.text), _ => None)

  private def refused(line: String): TaskError =
    TaskError.Doc(NonEmptyChunk(line))
end DocTask
