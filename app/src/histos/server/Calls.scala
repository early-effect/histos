package histos.server

import histos.assembly.Assemble
import histos.cache.ContentStore
import histos.central.Publisher
import histos.cli.{CliExit, GraphText}
import histos.jdk.{Machine, Os, Settled}
import histos.jdk.Sha256
import histos.load.{LoadError, PluginLink, PluginRuntime}
import histos.plugin.{PluginArgs, PluginRunError, PluginTaskId}
import histos.publish.LocalPublish
import histos.resolve.{BuildResolvers, Repositories, Resolve, ResolveError}
import histos.schedule.RemoteCache
import histos.task.{RunId, TaskError, TaskId, TaskResult}
import histos.toolchain.Survey
import histos.version.VersionTask
import histos.workflow.{Workflow, WorkflowAction}
import histos.zinc.{GraphCompileError, ResolvedJdk}
import histos.{Platform, ProjectGraph}

import zio.json.*

import java.io.OutputStream
import java.nio.file.{Files, Path}

import zio.{Chunk, Fiber, IO, Ref, ZIO, ZLayer}

/** One graph command, written as the CLI would print it. */
object Calls:
  val quiet: PluginRuntime = Quiet

  def idleMachine: Machine =
    Machine(
      Map.empty,
      ".",
      Os.Other,
      Path.of(".histos"),
      Path.of("."),
      None,
      Settled.No
    )

  def graph(
      graph: ProjectGraph,
      plugins: PluginRuntime,
      output: OutputStream
  ): IO[ServerError, Unit] =
    told(output, GraphText.render(graph, plugins.links)) *> done(
      output,
      CliExit.Success.code,
      ""
    )

  def publish(
      repo: Path,
      graph: ProjectGraph,
      machine: Machine,
      jdk: ResolvedJdk,
      repository: String,
      projects: List[String],
      output: OutputStream
  ): ZIO[Resolve, ServerError, Unit] =
    placed(repo, repository) match
      case None =>
        done(output, CliExit.PublishFailed.code, s"'$repository' is not a path")
      case Some(destination) =>
        LocalPublish
          .run(repo, graph, projects, jdk, machine.env)
          .provideSome[Resolve](Publisher.local(destination))
          .foldZIO(
            failure =>
              done(output, CliExit.PublishFailed.code, failure.message),
            written =>
              val lines =
                written.skipped ++ written.modules
                  .map(line => published(destination, line))
              told(output, lines.mkString("\n")) *> done(
                output,
                CliExit.Success.code,
                ""
              )
          )

  def run(
      repo: Path,
      graph: ProjectGraph,
      jdk: ResolvedJdk,
      machine: Machine,
      id: String,
      platform: Option[String],
      output: OutputStream
  ): ZIO[Resolve, ServerError, Unit] =
    chosen(platform) match
      case Left(message) => done(output, CliExit.TaskFailed.code, message)
      case Right(choice) =>
        Ref.make(0).flatMap { status =>
          histos.program.Program
            .console(repo, graph, id, jdk, machine.env, choice)
            .foreach {
              case histos.program.ProgramFeed.Begin(module, version, target) =>
                told(
                  output,
                  s"${module.value} ${version.value} ${target.token}"
                )
              case histos.program.ProgramFeed.Out(text) => told(output, text)
              case histos.program.ProgramFeed.Err(text) =>
                emit(output, Message.Err(text))
              case histos.program.ProgramFeed.Exited(_, _, _, code) =>
                ZIO.when(code != 0)(status.set(code)).unit
            }
            .foldZIO(
              {
                case error: histos.program.ProgramError =>
                  done(output, programCode(error), error.message)
                case error: ServerError => ZIO.fail(error)
              },
              _ =>
                status.get.flatMap(code =>
                  done(
                    output,
                    if code == 0 then CliExit.Success.code else code,
                    ""
                  )
                )
            )
        }

  def task(
      repo: Path,
      graph: ProjectGraph,
      plugins: PluginRuntime,
      machine: Machine,
      id: String,
      output: OutputStream
  ): ZIO[Resolve & histos.load.Loader & ContentStore, ServerError, Unit] =
    TaskId.from(id) match
      case Left(error) => done(output, CliExit.TaskFailed.code, error.message)
      case Right(found) if found == TaskId.Inspect =>
        told(output, graph.projects.map(_.name.value).mkString(",")) *> done(
          output,
          CliExit.Success.code,
          ""
        )
      case Right(found) if found == TaskId.Versions =>
        VersionTask
          .fromRepo(graph, repo, machine.env)
          .foldZIO(
            error => done(output, CliExit.TaskFailed.code, error.message),
            result =>
              told(output, result.text) *> done(
                output,
                CliExit.Success.code,
                ""
              )
          )
      case Right(_) =>
        PluginTaskId.parse(id) match
          case Left(error) =>
            done(output, CliExit.TaskFailed.code, error.message)
          case Right(taskId) => plugin(plugins, taskId, output)

  def toolchain(
      machine: Machine,
      graph: ProjectGraph,
      output: OutputStream
  ): IO[ServerError, Unit] =
    Survey
      .read(machine, graph)
      .foldZIO(
        error =>
          told(output, Survey.shown(error)) *> done(
            output,
            CliExit.ToolchainFailed.code,
            error.message
          ),
        found =>
          told(output, Survey.text(found)) *> done(
            output,
            CliExit.Success.code,
            ""
          )
      )

  def resolve(
      repo: Path,
      graph: ProjectGraph,
      action: String,
      output: OutputStream
  ): ZIO[Resolve & histos.load.Loader, ServerError, Unit] =
    resolveAction(action) match
      case None =>
        done(output, CliExit.Usage.code, s"'$action' is not a resolve action")
      case Some(kind) =>
        ZIO
          .serviceWithZIO[histos.load.Loader](_.scan(repo))
          .either
          .flatMap:
            case Left(error) =>
              done(output, CliExit.LoadFailed.code, error.message)
            case Right(scanned) =>
              val followed: ZIO[
                Resolve,
                LoadError | ResolveError,
                String
              ] =
                kind match
                  case histos.cli.ResolveAction.Verify =>
                    Resolve.requirePlugins(repo, scanned.requests) *>
                      Resolve.verify(
                        repo,
                        graph,
                        BuildResolvers(graph.resolvers)
                      )
                  case histos.cli.ResolveAction.Check =>
                    Resolve.requirePlugins(repo, scanned.requests) *>
                      Resolve.checkPlugins(
                        repo,
                        scanned.requests,
                        Repositories.central
                      ) *>
                      Resolve.check(
                        repo,
                        graph,
                        BuildResolvers(graph.resolvers)
                      )
                  case histos.cli.ResolveAction.Update =>
                    Resolve
                      .updatePlugins(
                        repo,
                        scanned.requests,
                        Repositories.central
                      )
                      .unit *>
                      Resolve.update(
                        repo,
                        graph,
                        BuildResolvers(graph.resolvers)
                      )
              followed.foldZIO(
                {
                  case error: LoadError =>
                    done(output, CliExit.LoadFailed.code, error.message)
                  case error: ResolveError =>
                    done(output, CliExit.ResolveFailed.code, error.message)
                },
                text =>
                  told(output, text) *> done(output, CliExit.Success.code, "")
              )

  def workflow(
      repo: Path,
      graph: ProjectGraph,
      action: String,
      output: OutputStream
  ): IO[ServerError, Unit] =
    workflowAction(action) match
      case None =>
        done(output, CliExit.Usage.code, s"'$action' is not a workflow action")
      case Some(kind) =>
        val effect = kind match
          case WorkflowAction.Check  => Workflow.check(repo, graph)
          case WorkflowAction.Update => Workflow.update(repo, graph)
        effect.foldZIO(
          error => done(output, CliExit.WorkflowFailed.code, error.message),
          file =>
            told(output, file.toString) *> done(
              output,
              CliExit.Success.code,
              ""
            )
        )

  def assembly(
      repo: Path,
      graph: ProjectGraph,
      jdk: ResolvedJdk,
      destination: Option[String],
      output: OutputStream
  ): ZIO[Resolve, ServerError, Unit] =
    placed(repo, destination) match
      case None =>
        done(output, CliExit.CompileFailed.code, "assembly path is not a path")
      case Some(path) =>
        Assemble
          .run(repo, graph, jdk, path)
          .foldZIO(
            error => done(output, CliExit.CompileFailed.code, error.message),
            assembled =>
              pin(repo, assembled.path).foldZIO(
                message => done(output, CliExit.CompileFailed.code, message),
                _ =>
                  told(output, relative(repo, assembled.path)) *> done(
                    output,
                    CliExit.Success.code,
                    ""
                  )
              )
          )

  def cancel(
      runs: Ref[Map[String, Fiber[ServerError, Unit]]],
      id: String,
      output: OutputStream
  ): IO[ServerError, Unit] =
    RunId.from(id) match
      case Left(error)  => done(output, CliExit.TaskFailed.code, error.message)
      case Right(runId) =>
        runs.modify(map => (map.get(id), map - id)).flatMap {
          case Some(fiber) =>
            fiber.interrupt *> told(output, "cancelled") *> done(
              output,
              CliExit.Success.code,
              ""
            )
          case None =>
            done(
              output,
              CliExit.TaskFailed.code,
              TaskError.UnknownRun(runId).message
            )
        }

  def tracked[R](
      runs: Ref[Map[String, Fiber[ServerError, Unit]]],
      id: String,
      output: OutputStream
  )(body: ZIO[R, ServerError, Unit]): ZIO[R, ServerError, Unit] =
    body.fork.flatMap { fiber =>
      runs.update(_ + (id -> fiber)) *>
        fiber.join.foldCauseZIO(
          cause =>
            runs.update(_ - id) *>
              (if cause.isInterruptedOnly then
                 done(output, CliExit.Interrupted.code, "")
               else
                 cause.failureOrCause match
                   case Left(error) => ZIO.fail(error)
                   case Right(_)    =>
                     done(output, CliExit.TaskFailed.code, "the run failed")),
          _ => runs.update(_ - id)
        )
    }

  private def plugin(
      plugins: PluginRuntime,
      id: PluginTaskId,
      output: OutputStream
  ): ZIO[ContentStore, ServerError, Unit] =
    ZIO.serviceWithZIO[ContentStore] { store =>
      plugins
        .run(id, PluginArgs.empty)
        .provide(
          ZLayer.succeed(ContentStore.actions(store)),
          RemoteCache.off
        )
        .foldZIO(
          error => done(output, CliExit.TaskFailed.code, error.message),
          result =>
            told(output, result.text) *> done(output, CliExit.Success.code, "")
        )
    }

  private def chosen(
      platform: Option[String]
  ): Either[String, Option[Platform]] =
    platform match
      case None      => Right(None)
      case Some(raw) =>
        Platform.from(raw) match
          case Left(error)  => Left(error.message)
          case Right(found) => Right(Some(found))

  private def resolveAction(raw: String): Option[histos.cli.ResolveAction] =
    raw match
      case "verify" => Some(histos.cli.ResolveAction.Verify)
      case "update" => Some(histos.cli.ResolveAction.Update)
      case "check"  => Some(histos.cli.ResolveAction.Check)
      case _        => None

  private def workflowAction(raw: String): Option[WorkflowAction] =
    raw match
      case "check"  => Some(WorkflowAction.Check)
      case "update" => Some(WorkflowAction.Update)
      case _        => None

  private def programCode(error: histos.program.ProgramError): Int =
    error match
      case histos.program.ProgramError.Compile(
            GraphCompileError.Lock(_) | GraphCompileError.LockRead(_)
          ) =>
        CliExit.ResolveFailed.code
      case histos.program.ProgramError.Lock(_) |
          histos.program.ProgramError.LockRead(_) =>
        CliExit.ResolveFailed.code
      case histos.program.ProgramError.Compile(_) =>
        CliExit.CompileFailed.code
      case _ =>
        CliExit.TaskFailed.code

  private def placed(repo: Path, raw: Option[String]): Option[Path] =
    raw match
      case None       => Some(repo.resolve("dist").resolve("histos.jar"))
      case Some(text) =>
        try
          val path = Path.of(text)
          Some(if path.isAbsolute then path else repo.resolve(path))
        catch case _: java.nio.file.InvalidPathException => None

  private def placed(repo: Path, raw: String): Option[Path] =
    try
      val path = Path.of(raw)
      Some(if path.isAbsolute then path else repo.resolve(path))
    catch case _: java.nio.file.InvalidPathException => None

  private def published(
      destination: Path,
      line: histos.publish.PublishedModule
  ): String =
    val base = destination.toAbsolutePath.normalize
    val full = line.jar.toAbsolutePath.normalize
    val shown =
      (if full.startsWith(base) then base.relativize(full) else full).toString
        .replace('\\', '/')
    s"${line.group.value} ${line.artifact} ${line.revision} $shown"

  private def relative(repo: Path, path: Path): String =
    val base = repo.toAbsolutePath.normalize
    val full = path.toAbsolutePath.normalize
    (if full.startsWith(base) then base.relativize(full) else full).toString
      .replace('\\', '/')

  /** The launcher verifies this checksum of `dist/histos.jar`. */
  private def pin(repo: Path, jar: Path): IO[String, Unit] =
    val pinned =
      repo.resolve("dist").resolve("histos.jar").toAbsolutePath.normalize
    if jar.toAbsolutePath.normalize != pinned then ZIO.unit
    else
      ZIO
        .attempt {
          val sha = Sha256.of(Chunk.fromArray(Files.readAllBytes(jar))).value
          val dir = repo.resolve(".histos")
          Files.createDirectories(dir)
          Files.writeString(dir.resolve("assembly.sha256"), sha + "\n")
          ()
        }
        .mapError(_ =>
          s"could not write ${repo.resolve(".histos").resolve("assembly.sha256")}"
        )

  private def told(output: OutputStream, text: String): IO[ServerError, Unit] =
    if text.isEmpty then ZIO.unit else emit(output, Message.Out(text))

  private def done(
      output: OutputStream,
      code: Int,
      err: String
  ): IO[ServerError, Unit] =
    emit(output, Message.Exit(code, err))

  private def emit(
      output: OutputStream,
      message: Message
  ): IO[ServerError, Unit] =
    histos.bsp.Frame
      .write(output, message.toJson)
      .mapError(_ => ServerError.Io(ServerIo.Write))

  private object Quiet extends PluginRuntime:
    def run(
        id: PluginTaskId,
        args: PluginArgs
    ): ZIO[
      histos.schedule.ActionCache & RemoteCache,
      PluginRunError,
      histos.plugin.PluginOutput
    ] =
      ZIO.fail(PluginRunError.Unknown(id))

    def bind: ZIO[
      histos.schedule.ActionCache & RemoteCache,
      Nothing,
      Map[TaskId, IO[TaskError, TaskResult]]
    ] =
      ZIO.succeed(Map.empty)

    def links: List[PluginLink] = Nil
end Calls
