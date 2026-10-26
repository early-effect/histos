package histos.cli

import histos.cache.CacheCleanMode
import histos.plugin.{CommandName, PluginTaskId}
import histos.server.ResolveAction
import histos.task.{RunId, TaskId}
import histos.testrun.SuitePattern
import histos.workflow.WorkflowAction
import histos.{Platform, ProjectName}

import zio.{IO, ZIO}
import zio.cli.{
  Args,
  CliConfig,
  Command as CliCommand,
  CommandDirective,
  HelpDoc,
  Options,
  ValidationError,
  ValidationErrorType,
}

/** zio-cli grammar. The wire value still has the raw name. [[parse]] turns that name into a [[TaskId]] and, for a
  * module, a platform.
  */
object Grammar:
  private enum Wire:
    case Version, Graph, Mcp, Bsp, Server, CachePath, CacheList, Toolchain,
      Reload, Reboot
    case Cancel(id: String)
    case Run(name: String, platform: Option[String])
    case Test(projects: List[String])
    case TestOnly(args: List[String])
    case TestQuick(args: List[String])
    case Compile(projects: List[String])
    case Clean(projects: List[String])
    case CleanFull(projects: List[String])
    case Assembly(destination: Option[String])
    case Publish(repository: String, projects: List[String])
    case CacheClean(mode: CacheCleanMode)
    case Resolve(action: ResolveAction)
    case LauncherUpdate
    case Workflow(action: WorkflowAction)
    case PreviewCompile(projects: List[String])
    case PreviewTest(projects: List[String])
    case PreviewPublish(projects: List[String])
    case PreviewClean(projects: List[String])
    case PreviewCleanFull(projects: List[String])
    case PreviewRun(name: String, platform: Option[String])
    case Observe

  private val version = CliCommand("version").map(_ => Wire.Version)
  private val graph   = CliCommand("graph").map(_ => Wire.Graph)
  private val run     =
    CliCommand("run", Args.text("name") ++ Args.text("platform").atMost(1))
      .map { case (name, platform) =>
        Wire.Run(name, platform.headOption)
      }
  private val test       = CliCommand("test", Args.text("project").*).map(names => Wire.Test(names.toList))
  private val testOnly   = CliCommand("testOnly", Args.text("pattern").*).map(args => Wire.TestOnly(args.toList))
  private val testQuick  = CliCommand("testQuick", Args.text("pattern").*).map(args => Wire.TestQuick(args.toList))
  private val compile    = CliCommand("compile", Args.text("project").*).map(names => Wire.Compile(names.toList))
  private val cleanBuild = CliCommand("clean", Args.text("project").*).map(names => Wire.Clean(names.toList))
  private val cleanFull  = CliCommand("cleanFull", Args.text("project").*).map(names => Wire.CleanFull(names.toList))
  private val assembly   =
    CliCommand("assembly", Args.text("destination").atMost(1)).map(paths => Wire.Assembly(paths.headOption))
  private val publish =
    CliCommand("publish", Args.text("repository") ++ Args.text("project").*)
      .map { case (repository, projects) =>
        Wire.Publish(repository, projects.toList)
      }
  private val mcp       = CliCommand("mcp").map(_ => Wire.Mcp)
  private val bsp       = CliCommand("bsp").map(_ => Wire.Bsp)
  private val server    = CliCommand("server").map(_ => Wire.Server)
  private val toolchain = CliCommand("toolchain").map(_ => Wire.Toolchain)
  private val reload    = CliCommand("reload").map(_ => Wire.Reload)
  private val reboot    = CliCommand("reboot").map(_ => Wire.Reboot)
  private val cancel    =
    CliCommand("cancel", Args.text("id")).map(Wire.Cancel(_))
  private val path    = CliCommand("path").map(_ => Wire.CachePath)
  private val list    = CliCommand("list").map(_ => Wire.CacheList)
  private val refused =
    Left(ValidationError(ValidationErrorType.InvalidValue, HelpDoc.empty))
  private val cacheMode =
    Options.boolean("dry-run").map(dry => if dry then CacheCleanMode.DryRun else CacheCleanMode.Remove)
  private val clean         = CliCommand("clean", cacheMode).map(Wire.CacheClean(_))
  private val cache         = CliCommand("cache").subcommands(path | list | clean)
  private val resolveAction =
    (Options.boolean("update") ++ Options.boolean("check")).mapOrFail:
      case (false, false) => Right(ResolveAction.Verify)
      case (true, false)  => Right(ResolveAction.Update)
      case (false, true)  => Right(ResolveAction.Check)
      case (true, true)   => refused
  private val resolve =
    CliCommand("resolve", resolveAction).map(Wire.Resolve(_))
  private val workflowAction =
    (Options.boolean("check") ++ Options.boolean("update")).mapOrFail:
      case (true, true) => refused
      case (_, true)    => Right(WorkflowAction.Update)
      case _            => Right(WorkflowAction.Check)
  private val workflow =
    CliCommand("workflow", workflowAction).map(Wire.Workflow(_))
  private val launcherFlag =
    Options
      .boolean("update")
      .mapOrFail:
        case true  => Right(())
        case false => refused
  private val launcher =
    CliCommand("launcher", launcherFlag).map(_ => Wire.LauncherUpdate)
  private val previewCompile =
    CliCommand("compile", Args.text("project").*).map(names => Wire.PreviewCompile(names.toList))
  private val previewTest =
    CliCommand("test", Args.text("project").*).map(names => Wire.PreviewTest(names.toList))
  private val previewPublish =
    CliCommand("publish", Args.text("project").*).map(names => Wire.PreviewPublish(names.toList))
  private val previewClean =
    CliCommand("clean", Args.text("project").*).map(names => Wire.PreviewClean(names.toList))
  private val previewCleanFull =
    CliCommand("cleanFull", Args.text("project").*).map(names => Wire.PreviewCleanFull(names.toList))
  private val previewRun =
    CliCommand("run", Args.text("name") ++ Args.text("platform").atMost(1))
      .map { case (name, platform) =>
        Wire.PreviewRun(name, platform.headOption)
      }
  private val observe = CliCommand("observe").map(_ => Wire.Observe)
  private val preview = CliCommand("preview").subcommands(
    previewCompile | previewTest | previewPublish | previewClean | previewCleanFull | previewRun
  )
  private val root =
    CliCommand("histos").subcommands(
      version | graph | run | test | testOnly | testQuick | compile | cleanBuild | cleanFull | assembly | publish | mcp | bsp | server | toolchain | cache | resolve | workflow | launcher | reload | reboot | cancel | observe | preview
    )

  /** Reserved root words never fall through to plugin dispatch, including a word held for a later surface.
    */
  private val roots: Set[String] = CommandName.reserved.map(_.value)

  def parse(args: List[String]): IO[CliParseError, Command] =
    args match
      case "~" :: Nil =>
        ZIO.fail(CliParseError.WatchCommand("watch needs a command"))
      case "~" :: "~" :: _ =>
        ZIO.fail(CliParseError.WatchCommand("watch does not nest"))
      case "~" :: rest =>
        parseRoot(rest).map(command => Command.Watch(command, rest))
      case _ => parseRoot(args)

  private def parseRoot(args: List[String]): IO[CliParseError, Command] =
    root
      .parse("histos" :: args, CliConfig.default)
      .mapError(error => reject(error, args))
      .flatMap:
        case CommandDirective.UserDefined(Nil, wire) => command(wire)
        case CommandDirective.UserDefined(_, _)      =>
          ZIO.fail(CliParseError.UnexpectedArguments)
        case CommandDirective.BuiltIn(_) => ZIO.fail(CliParseError.Help)

  private def command(wire: Wire): IO[CliParseError, Command] =
    wire match
      case Wire.Version    => ZIO.succeed(Command.Version)
      case Wire.Graph      => ZIO.succeed(Command.Graph)
      case Wire.Mcp        => ZIO.succeed(Command.Mcp)
      case Wire.Bsp        => ZIO.succeed(Command.Bsp)
      case Wire.Server     => ZIO.succeed(Command.Server)
      case Wire.CachePath  => ZIO.succeed(Command.CachePath)
      case Wire.CacheList  => ZIO.succeed(Command.CacheList)
      case Wire.Toolchain  => ZIO.succeed(Command.Toolchain)
      case Wire.Reload     => ZIO.succeed(Command.Reload)
      case Wire.Reboot     => ZIO.succeed(Command.Reboot)
      case Wire.Cancel(id) =>
        ZIO.fromEither(
          RunId.from(id).left.map(error => CliParseError.Pattern(error.message)).map(Command.Cancel(_))
        )
      case Wire.CacheClean(mode) => ZIO.succeed(Command.CacheClean(mode))
      case Wire.Resolve(action)  => ZIO.succeed(Command.Resolve(action))
      case Wire.LauncherUpdate   => ZIO.succeed(Command.LauncherUpdate)
      case Wire.Workflow(action) => ZIO.succeed(Command.Workflow(action))
      case Wire.Test(projects)   =>
        projectList("test", projects).map(Command.Test(_))
      case Wire.TestOnly(args) =>
        ZIO.fromEither(selection("testOnly", args, requirePattern = true)).map { case (modules, patterns) =>
          Command.TestOnly(modules, patterns)
        }
      case Wire.TestQuick(args) =>
        ZIO
          .fromEither(selection("testQuick", args, requirePattern = false))
          .map { case (modules, patterns) =>
            Command.TestQuick(modules, patterns)
          }
      case Wire.Observe           => ZIO.succeed(Command.Observe)
      case Wire.Compile(projects) =>
        projectList("compile", projects).map(Command.Compile(_))
      case Wire.Clean(projects) =>
        projectList("clean", projects).map(Command.Clean(_))
      case Wire.CleanFull(Nil) => ZIO.succeed(Command.CleanFull)
      case Wire.CleanFull(_)   =>
        ZIO.fail(CliParseError.Pattern("cleanFull does not take a project"))
      case Wire.Assembly(destination) =>
        ZIO.succeed(Command.Assembly(destination))
      case Wire.Publish(repository, projects) =>
        projectList("publish", projects).map(Command.Publish(repository, _))
      case Wire.PreviewCompile(projects) =>
        projectList("preview compile", projects).map(Command.PreviewCompile(_))
      case Wire.PreviewTest(projects) =>
        projectList("preview test", projects).map(Command.PreviewTest(_))
      case Wire.PreviewPublish(projects) =>
        projectList("preview publish", projects).map(Command.PreviewPublish(_))
      case Wire.PreviewClean(projects) =>
        projectList("preview clean", projects).map(Command.PreviewClean(_))
      case Wire.PreviewCleanFull(Nil) => ZIO.succeed(Command.PreviewCleanFull)
      case Wire.PreviewCleanFull(_)   =>
        ZIO.fail(CliParseError.Pattern("cleanFull does not take a project"))
      case Wire.PreviewRun(raw, token) =>
        namedRun(raw, token).map:
          case NamedRun.Module(id, platform) =>
            Command.PreviewRun(id, platform)
          case NamedRun.Plugin(id) => Command.PreviewPlugin(id)
      case Wire.Run(raw, token) =>
        namedRun(raw, token).map:
          case NamedRun.Module(id, platform) => Command.Run(id, platform)
          case NamedRun.Plugin(id)           =>
            Command.Plugin(id, histos.plugin.PluginArgs.empty)

  private def selection(
      command: String,
      args: List[String],
      requirePattern: Boolean,
  ): Either[CliParseError, (List[ProjectName], List[SuitePattern])] =
    walk(command, requirePattern, args, Nil, Nil)

  private def walk(
      command: String,
      requirePattern: Boolean,
      args: List[String],
      modules: List[ProjectName],
      patterns: List[SuitePattern],
  ): Either[CliParseError, (List[ProjectName], List[SuitePattern])] =
    args match
      case Nil =>
        if requirePattern && patterns.isEmpty then Left(CliParseError.MissingPattern)
        else Right((modules.reverse, patterns.reverse))
      case "--module" :: Nil =>
        Left(CliParseError.Pattern(s"$command --module needs a project"))
      case "--module" :: name :: rest =>
        ProjectName.from(name) match
          case Left(error)  => Left(CliParseError.Pattern(error.message))
          case Right(found) =>
            walk(
              command,
              requirePattern,
              rest,
              found :: modules,
              patterns,
            )
      case flag :: _ if flag.startsWith("-") =>
        Left(CliParseError.Pattern(s"$command does not take $flag"))
      case token :: rest =>
        SuitePattern.from(token) match
          case Left(error)    => Left(CliParseError.Pattern(error.message))
          case Right(pattern) =>
            walk(command, requirePattern, rest, modules, pattern :: patterns)

  private enum NamedRun:
    case Module(id: TaskId, platform: Option[Platform])
    case Plugin(id: PluginTaskId)

  private def projectList(
      command: String,
      projects: List[String],
  ): IO[CliParseError, List[ProjectName]] =
    projects.find(_.startsWith("-")) match
      case Some(flag) =>
        ZIO.fail(CliParseError.Pattern(s"$command does not take $flag"))
      case None => ZIO.fromEither(parsedNames(projects))

  private def parsedNames(raw: List[String]): Either[CliParseError, List[ProjectName]] =
    raw.foldLeft[Either[CliParseError, List[ProjectName]]](Right(Nil)): (acc, token) =>
      for
        have <- acc
        name <- ProjectName.from(token).left.map(error => CliParseError.Pattern(error.message))
      yield have :+ name

  /** The same id `run` accepts: a module, `inspect`, `versions`, or a plugin task.
    */
  private def namedRun(
      raw: String,
      token: Option[String],
  ): IO[CliParseError, NamedRun] =
    if raw.contains('/') then
      token match
        case Some(_) => ZIO.fail(CliParseError.TaskPlatform(raw))
        case None    =>
          ZIO
            .fromEither(PluginTaskId.parse(raw))
            .mapBoth(
              error => CliParseError.BadPluginTask(error.message),
              NamedRun.Plugin(_),
            )
    else
      ZIO
        .fromEither(TaskId.from(raw))
        .mapError(_ => CliParseError.BadTaskId)
        .flatMap: id =>
          val task = id == TaskId.Inspect || id == TaskId.Versions
          token match
            case Some(_) if task =>
              ZIO.fail(CliParseError.TaskPlatform(id.value))
            case None       => ZIO.succeed(NamedRun.Module(id, None))
            case Some(text) =>
              ZIO
                .fromEither(Platform.from(text))
                .mapBoth(
                  error => CliParseError.BadPlatform(error.message),
                  platform => NamedRun.Module(id, Some(platform)),
                )

  private def reject(
      error: ValidationError,
      args: List[String],
  ): CliParseError =
    error.validationErrorType match
      case ValidationErrorType.CommandMismatch =>
        args match
          case head :: _ if roots.contains(head) => CliParseError.UnknownCommand
          case _                                 => CliParseError.UnknownRoot
      case ValidationErrorType.InvalidArgument | ValidationErrorType.MissingValue =>
        CliParseError.MissingArgument
      case _ => CliParseError.Rejected
end Grammar
