package histos.cli

import heddle.LinePipe
import histos.acp.Acp
import histos.load.Loader
import histos.mcp.HistosMcp
import histos.task.{TaskError, TaskId, Tasks}
import histos.{Aggregate, Module, Project}

import java.nio.file.Path

import zio.*

enum Command:
  case Version
  case Graph
  case Run(id: String)
  case Mcp
  case Acp

enum CliExit:
  case Success, TaskFailed, Usage, LoadFailed

  def code: Int = this match
    case Success     => 0
    case TaskFailed  => 1
    case Usage       => 2
    case LoadFailed  => 3

final case class Completed(exit: CliExit, out: String, err: String)

object Cli:
  val VersionText: String = "histos 0.0.1-SNAPSHOT"

  val usageText: String = "usage: histos (version | graph | run <id> | mcp | acp)"

  def parse(args: List[String]): Either[String, Command] =
    args match
      case "version" :: Nil => Right(Command.Version)
      case "version" :: _   => Left("version takes no arguments")
      case "graph" :: Nil   => Right(Command.Graph)
      case "graph" :: _     => Left("graph takes no arguments")
      case "run" :: id :: Nil => Right(Command.Run(id))
      case "run" :: _       => Left("run needs one task id")
      case "mcp" :: Nil     => Right(Command.Mcp)
      case "mcp" :: _       => Left("mcp takes no arguments")
      case "acp" :: Nil     => Right(Command.Acp)
      case "acp" :: _       => Left("acp takes no arguments")
      case command :: _     => Left(s"unknown command $command")
      case Nil              => Left(usageText)

  def execute(args: List[String], repo: Path, apiJar: Path): URIO[Loader & Scope, Completed] =
    parse(args) match
      case Left(detail) => ZIO.succeed(Completed(CliExit.Usage, "", detail))
      case Right(command) =>
        command match
          case Command.Version => ZIO.succeed(Completed(CliExit.Success, VersionText, ""))
          case Command.Graph   => graph(repo, apiJar)
          case Command.Run(id) => run(repo, apiJar, id)
          case Command.Mcp     => mcp(repo, apiJar)
          case Command.Acp     => acp

  private def graph(repo: Path, apiJar: Path): URIO[Loader & Scope, Completed] =
    ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.map:
      case Left(err)     => Completed(CliExit.LoadFailed, "", err.message)
      case Right(loaded) => Completed(CliExit.Success, render(loaded.graph.projects), "")

  private def run(repo: Path, apiJar: Path, raw: String): URIO[Loader & Scope, Completed] =
    TaskId.from(raw) match
      case Left(err) => ZIO.succeed(Completed(CliExit.TaskFailed, "", err.message))
      case Right(id) =>
        ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap:
          case Left(err) => ZIO.succeed(Completed(CliExit.LoadFailed, "", err.message))
          case Right(loaded) =>
            ZIO.scoped:
              for
                run <- ZIO.serviceWithZIO[Tasks](_.run(id))
                out <- run.await
              yield Completed(CliExit.Success, out.text, "")
            .provide(Tasks.live(loaded.graph))
            .catchAll(err => ZIO.succeed(Completed(taskExit(err), "", err.message)))

  private def mcp(repo: Path, apiJar: Path): URIO[Loader & Scope, Completed] =
    HistosMcp.serve(repo, apiJar, LinePipe.standard).fold(
      err => Completed(CliExit.LoadFailed, "", err.message),
      _ => Completed(CliExit.Success, "", ""),
    )

  private def acp: UIO[Completed] =
    def loop: UIO[Completed] =
      LinePipe.standard.readLine.foldZIO(
        t => ZIO.succeed(Completed(CliExit.LoadFailed, "", t.getClass.getName)),
        {
          case None => ZIO.succeed(Completed(CliExit.Success, "", ""))
          case Some(line) =>
            LinePipe.standard.writeLine(Acp.answer(line)).foldZIO(
              t => ZIO.succeed(Completed(CliExit.LoadFailed, "", t.getClass.getName)),
              _ => loop,
            )
        },
      )
    loop

  private def taskExit(err: TaskError): CliExit =
    err match
      case TaskError.EmptyId | TaskError.NoRunner(_) | TaskError.UnknownRun(_) => CliExit.TaskFailed

  private def render(projects: List[Project]): String =
    projects.map:
      case Module(name, platforms, versions, _, _, _) =>
        s"${name.value} ${platforms.all.map(_.toString.toLowerCase).mkString(",")} ${versions.all.map(_.value).mkString(",")}"
      case Aggregate(name, children, _) =>
        s"${name.value} aggregate ${children.map(_.value).mkString(",")}"
    .mkString("\n")

object Main extends ZIOAppDefault:
  def run =
    apiJar.foldZIO(
      exit => ZIO.succeed(zio.ExitCode(exit.code)),
      jar =>
        for
          args <- getArgs
          done <- ZIO.scoped:
            Cli.execute(args.toList, Path.of("").toAbsolutePath, jar).provideSome[Scope](Loader.live)
          _ <- Console.printLine(done.out).when(done.out.nonEmpty)
          _ <- Console.printLineError(done.err).when(done.err.nonEmpty)
        yield zio.ExitCode(done.exit.code),
    )

  private def apiJar: IO[CliExit, Path] =
    val location = Option(classOf[histos.HistosBuild].getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
    location match
      case None => ZIO.fail(CliExit.LoadFailed)
      case Some(url) =>
        ZIO.attempt(Path.of(url.toURI)).orElseFail(CliExit.LoadFailed)
