package histos.mcp

import heddle.LinePipe
import heddle.endpoint.Schema
import heddle.mcp.{Mcp, McpBuildError}
import histos.jdk.Machine
import histos.load.{LoadError, Loader}
import histos.task.{TaskError, TaskId, Tasks}
import histos.toolchain.Survey
import histos.version.VersionTask
import histos.{Aggregate, Module, Project, ProjectGraph}

import java.nio.file.Path

import zio.*
import zio.json.JsonCodec

/** MCP tools over a loaded build: `graph`, `run`, `cancel`, and `toolchain`. */
object HistosMcp:
  final case class NoArgs() derives Schema, JsonCodec
  final case class RunArgs(id: String) derives Schema, JsonCodec
  final case class CancelArgs(id: String) derives Schema, JsonCodec
  final case class ToolFailure(message: String) derives Schema, JsonCodec

  enum ServeError:
    case Load(error: LoadError)
    case Build(errors: NonEmptyChunk[McpBuildError])
    case Stdio

    def message: String = this match
      case Load(error)  => error.message
      case Build(errors) => errors.map(_.message).mkString("\n")
      case Stdio         => "the mcp pipe failed"

  def serve(repo: Path, apiJar: Path, pipe: LinePipe, machine: Machine): ZIO[Loader & Scope, ServeError, Unit] =
    for
      loaded <- ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).mapError(ServeError.Load(_))
      runners = Map(TaskId.Versions -> VersionTask.fromRepo(loaded.graph, repo, machine.env))
      tasks  <- Tasks.live(loaded.graph, runners).build.map(_.get[Tasks])
      mcp    <- ZIO.fromEither(tools(loaded.graph, tasks, machine)).mapError(ServeError.Build(_))
      _      <- mcp.stdio(pipe).mapError(_ => ServeError.Stdio)
    yield ()

  def tools(graph: ProjectGraph, tasks: Tasks, machine: Machine): Either[NonEmptyChunk[McpBuildError], Mcp[Scope]] =
    for
      base    <- Mcp.from()
      listed  <- base.tool[NoArgs]("graph", "Show the project graph")(_ => ZIO.succeed(render(graph.projects)))
      running <- listed.tool[RunArgs]("run", "Run a task and return its text")(args => run(tasks, args.id))
      stopped <- running.tool[CancelArgs]("cancel", "Cancel a run")(args => cancel(tasks, args.id))
      ready   <- stopped.tool[NoArgs](
                   "toolchain",
                   "Show the JDK decision, and the Node or clang decision when the build needs them",
                 )(_ => Survey.read(machine, graph).mapBoth(ToolchainView.fault, ToolchainView.report))
    yield ready

  private def run(tasks: Tasks, raw: String): ZIO[Scope, ToolFailure, String] =
    ZIO.fromEither(TaskId.from(raw)).mapError(failed).flatMap: id =>
      tasks.run(id).mapError(failed).flatMap(run => run.await.mapError(failed).map(_.text))

  private def cancel(tasks: Tasks, raw: String): ZIO[Any, ToolFailure, String] =
    ZIO.fromEither(histos.task.RunId.from(raw)).mapError(failed).flatMap: id =>
      tasks.cancel(id).mapError(failed).as("cancelled")

  private def failed(err: TaskError): ToolFailure = ToolFailure(err.message)

  private def render(projects: List[Project]): String =
    projects.map:
      case Module(name, platforms, versions, _, _, _, _, _, _, _, _) =>
        s"${name.value} ${platforms.all.map(_.toString.toLowerCase).mkString(",")} ${versions.all.map(_.value).mkString(",")}"
      case Aggregate(name, children, _) =>
        s"${name.value} aggregate ${children.map(_.value).mkString(",")}"
    .mkString("\n")
end HistosMcp
