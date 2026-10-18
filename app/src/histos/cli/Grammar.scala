package histos.cli

import histos.{Platform, ProjectName}
import histos.task.TaskId
import histos.testrun.SuitePattern
import histos.workflow.WorkflowAction

import zio.{IO, ZIO}
import zio.cli.{Args, CliConfig, Command as CliCommand, CommandDirective, Options, ValidationError, ValidationErrorType}

/** zio-cli grammar. The wire value still has the raw name. [[parse]] turns that name into a [[TaskId]] and, for a module, a platform. */
object Grammar:
  private enum Wire:
    case Version, Graph, Mcp, Acp, Bsp, Server, CachePath, CacheList, Toolchain
    case Run(name: String, platform: Option[String])
    case Test(projects: List[String])
    case TestOnly(args: List[String])
    case TestQuick(args: List[String])
    case Compile(projects: List[String])
    case Assembly(destination: Option[String])
    case Publish(repository: String, projects: List[String])
    case CacheClean(dryRun: Boolean)
    case Resolve(update: Boolean, check: Boolean)
    case Workflow(check: Boolean, update: Boolean)

  private val version   = CliCommand("version").map(_ => Wire.Version)
  private val graph     = CliCommand("graph").map(_ => Wire.Graph)
  private val run       =
    CliCommand("run", Args.text("name") ++ Args.text("platform").atMost(1)).map { case (name, platform) =>
      Wire.Run(name, platform.headOption)
    }
  private val test      = CliCommand("test", Args.text("project").*).map(names => Wire.Test(names.toList))
  private val testOnly  = CliCommand("testOnly", Args.text("pattern").*).map(args => Wire.TestOnly(args.toList))
  private val testQuick = CliCommand("testQuick", Args.text("pattern").*).map(args => Wire.TestQuick(args.toList))
  private val compile   = CliCommand("compile", Args.text("project").*).map(names => Wire.Compile(names.toList))
  private val assembly  =
    CliCommand("assembly", Args.text("destination").atMost(1)).map(paths => Wire.Assembly(paths.headOption))
  private val publish  =
    CliCommand("publish", Args.text("repository") ++ Args.text("project").*).map { case (repository, projects) =>
      Wire.Publish(repository, projects.toList)
    }
  private val mcp       = CliCommand("mcp").map(_ => Wire.Mcp)
  private val acp       = CliCommand("acp").map(_ => Wire.Acp)
  private val bsp       = CliCommand("bsp").map(_ => Wire.Bsp)
  private val server    = CliCommand("server").map(_ => Wire.Server)
  private val toolchain = CliCommand("toolchain").map(_ => Wire.Toolchain)
  private val path      = CliCommand("path").map(_ => Wire.CachePath)
  private val list      = CliCommand("list").map(_ => Wire.CacheList)
  private val clean     = CliCommand("clean", Options.boolean("dry-run")).map(Wire.CacheClean(_))
  private val cache     = CliCommand("cache").subcommands(path | list | clean)
  private val resolve   =
    CliCommand("resolve", Options.boolean("update") ++ Options.boolean("check")).map { case (update, check) =>
      Wire.Resolve(update, check)
    }
  private val workflow  =
    CliCommand("workflow", Options.boolean("check") ++ Options.boolean("update")).map { case (check, update) =>
      Wire.Workflow(check, update)
    }
  private val root      =
    CliCommand("histos").subcommands(
      version | graph | run | test | testOnly | testQuick | compile | assembly | publish | mcp | acp | bsp | server | toolchain | cache | resolve | workflow,
    )

  def parse(args: List[String]): IO[CliParseError, Command] =
    args match
      case "~" :: Nil     => ZIO.fail(CliParseError.WatchCommand("watch needs a command"))
      case "~" :: "~" :: _ => ZIO.fail(CliParseError.WatchCommand("watch does not nest"))
      case "~" :: rest    => parse(rest).map(Command.Watch(_))
      case _              => parseRoot(args)

  private def parseRoot(args: List[String]): IO[CliParseError, Command] =
    root.parse("histos" :: args, CliConfig.default).mapError(reject).flatMap:
      case CommandDirective.UserDefined(Nil, wire) => command(wire)
      case CommandDirective.UserDefined(_, _)      => ZIO.fail(CliParseError.UnexpectedArguments)
      case CommandDirective.BuiltIn(_)             => ZIO.fail(CliParseError.Help)

  private def command(wire: Wire): IO[CliParseError, Command] =
    wire match
      case Wire.Version            => ZIO.succeed(Command.Version)
      case Wire.Graph              => ZIO.succeed(Command.Graph)
      case Wire.Mcp                => ZIO.succeed(Command.Mcp)
      case Wire.Acp                => ZIO.succeed(Command.Acp)
      case Wire.Bsp                => ZIO.succeed(Command.Bsp)
      case Wire.Server            => ZIO.succeed(Command.Server)
      case Wire.CachePath          => ZIO.succeed(Command.CachePath)
      case Wire.CacheList          => ZIO.succeed(Command.CacheList)
      case Wire.Toolchain          => ZIO.succeed(Command.Toolchain)
      case Wire.CacheClean(dryRun) => ZIO.succeed(Command.CacheClean(dryRun))
      case Wire.Resolve(true, true) => ZIO.fail(CliParseError.Rejected)
      case Wire.Resolve(update, check) =>
        val action =
          if update then ResolveAction.Update
          else if check then ResolveAction.Check
          else ResolveAction.Verify
        ZIO.succeed(Command.Resolve(action))
      case Wire.Workflow(true, true) => ZIO.fail(CliParseError.Rejected)
      case Wire.Workflow(_, update) =>
        ZIO.succeed(Command.Workflow(if update then WorkflowAction.Update else WorkflowAction.Check))
      case Wire.Test(projects) => ZIO.succeed(Command.Test(projects))
      case Wire.TestOnly(args) => ZIO.fromEither(selection("testOnly", args, requirePattern = true)).map {
        case (modules, patterns) => Command.TestOnly(modules, patterns)
      }
      case Wire.TestQuick(args) => ZIO.fromEither(selection("testQuick", args, requirePattern = false)).map {
        case (modules, patterns) => Command.TestQuick(modules, patterns)
      }
      case Wire.Compile(projects) => ZIO.succeed(Command.Compile(projects))
      case Wire.Assembly(destination) => ZIO.succeed(Command.Assembly(destination))
      case Wire.Publish(repository, projects) => ZIO.succeed(Command.Publish(repository, projects))
      case Wire.Run(raw, token) =>
        ZIO.fromEither(TaskId.from(raw)).mapError(_ => CliParseError.BadTaskId).flatMap: id =>
          val task = id == TaskId.Inspect || id == TaskId.Versions
          token match
            case Some(_) if task => ZIO.fail(CliParseError.TaskPlatform(id.value))
            case None            => ZIO.succeed(Command.Run(id, None))
            case Some(text) =>
              ZIO.fromEither(Platform.from(text)).mapBoth(
                error => CliParseError.BadPlatform(error.message),
                platform => Command.Run(id, Some(platform)),
              )

  private def selection(
      command: String,
      args: List[String],
      requirePattern: Boolean,
  ): Either[CliParseError, (List[String], List[SuitePattern])] =
    walk(command, requirePattern, args, Nil, Nil)

  private def walk(
      command: String,
      requirePattern: Boolean,
      args: List[String],
      modules: List[String],
      patterns: List[SuitePattern],
  ): Either[CliParseError, (List[String], List[SuitePattern])] =
    args match
      case Nil =>
        if requirePattern && patterns.isEmpty then Left(CliParseError.MissingPattern)
        else Right((distinct(modules.reverse), patterns.reverse))
      case "--module" :: Nil =>
        Left(CliParseError.Pattern(s"$command --module needs a project"))
      case "--module" :: name :: rest =>
        ProjectName.from(name) match
          case Left(error) => Left(CliParseError.Pattern(error.message))
          case Right(found) => walk(command, requirePattern, rest, found.value :: modules, patterns)
      case flag :: _ if flag.startsWith("-") =>
        Left(CliParseError.Pattern(s"$command does not take $flag"))
      case token :: rest =>
        SuitePattern.from(token) match
          case Left(error) => Left(CliParseError.Pattern(error.message))
          case Right(pattern) => walk(command, requirePattern, rest, modules, pattern :: patterns)

  private def distinct(names: List[String]): List[String] =
    names.foldLeft(List.empty[String])((acc, name) => if acc.contains(name) then acc else acc :+ name)

  private def reject(error: ValidationError): CliParseError =
    error.validationErrorType match
      case ValidationErrorType.CommandMismatch                     => CliParseError.UnknownCommand
      case ValidationErrorType.InvalidArgument | ValidationErrorType.MissingValue =>
        CliParseError.MissingArgument
      case _                                                       => CliParseError.Rejected

