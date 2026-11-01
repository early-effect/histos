package histos.cli

import histos.cache.CacheCleanMode
import histos.plugin.{CommandName, PluginTaskId}
import histos.schedule.CacheNamespace
import histos.server.{LauncherAction, ResolveAction}
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
    case Version, Graph, Mcp, Bsp, Server, Portal, CachePath, CacheList, Toolchain,
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
    case PublishLocal(tokens: List[String])
    case PublishSnapshots(projects: List[String])
    case PublishRelease(automatic: Boolean, tokens: List[String])
    case PublishPromote(id: String)
    case PublishDrop(id: String)
    case PublishAssets(tokens: List[String])
    case CacheClean(mode: CacheCleanMode)
    case CachePromote(from: String, to: String)
    case Resolve(action: ResolveAction)
    case Launcher(action: LauncherAction)
    case Workflow(action: WorkflowAction)
    case PreviewCompile(projects: List[String])
    case PreviewTest(projects: List[String])
    case PreviewPublishLocal(tokens: List[String])
    case PreviewPublishSnapshots(projects: List[String])
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
  private val publishLocal =
    CliCommand("publishLocal", Args.text("token").*).map(tokens => Wire.PublishLocal(tokens.toList))
  private val publishSnapshots =
    CliCommand("snapshots", Args.text("project").*).map(names => Wire.PublishSnapshots(names.toList))
  private val publishRelease =
    CliCommand("release", Options.boolean("automatic"), Args.text("token").*).map { case (automatic, names) =>
      val raw    = names.toList
      val tokens = raw.filterNot(_ == "--automatic")
      // An option is read only while it is the next token. A flag after a ship stays in the args.
      Wire.PublishRelease(automatic || raw.exists(_ == "--automatic"), tokens)
    }
  private val publishPromote =
    CliCommand("promote", Args.text("deployment")).map(Wire.PublishPromote(_))
  private val publishDrop =
    CliCommand("drop", Args.text("deployment")).map(Wire.PublishDrop(_))
  private val publishAssets =
    CliCommand("assets", Args.text("token").*).map(tokens => Wire.PublishAssets(tokens.toList))
  private val publish =
    CliCommand("publish").subcommands(
      publishSnapshots | publishRelease | publishAssets | publishPromote | publishDrop
    )
  private val mcp       = CliCommand("mcp").map(_ => Wire.Mcp)
  private val bsp       = CliCommand("bsp").map(_ => Wire.Bsp)
  private val server    = CliCommand("server").map(_ => Wire.Server)
  private val portal    = CliCommand("portal").map(_ => Wire.Portal)
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
  private val clean   = CliCommand("clean", cacheMode).map(Wire.CacheClean(_))
  private val promote =
    CliCommand("promote", Options.text("from") ++ Options.text("to")).map { (from, to) =>
      Wire.CachePromote(from, to)
    }
  private val cache         = CliCommand("cache").subcommands(path | list | clean | promote)
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
    (Options.boolean("update") ++ Options.boolean("latest")).mapOrFail:
      case (true, false) => Right(LauncherAction.Update)
      case (false, true) => Right(LauncherAction.Latest)
      case _             => refused
  private val launcher =
    CliCommand("launcher", launcherFlag).map(Wire.Launcher(_))
  private val previewCompile =
    CliCommand("compile", Args.text("project").*).map(names => Wire.PreviewCompile(names.toList))
  private val previewTest =
    CliCommand("test", Args.text("project").*).map(names => Wire.PreviewTest(names.toList))
  private val previewPublishSnapshots =
    CliCommand("snapshots", Args.text("project").*).map(names => Wire.PreviewPublishSnapshots(names.toList))
  private val previewPublish =
    CliCommand("publish").subcommands(previewPublishSnapshots)
  private val previewPublishLocal =
    CliCommand("publishLocal", Args.text("token").*).map(tokens => Wire.PreviewPublishLocal(tokens.toList))
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
    previewCompile | previewTest | previewPublish | previewPublishLocal | previewClean | previewCleanFull | previewRun
  )
  private val root =
    CliCommand("histos").subcommands(
      version | graph | run | test | testOnly | testQuick | compile | cleanBuild | cleanFull | assembly | publishLocal | publish | mcp | bsp | server | portal | toolchain | cache | resolve | workflow | launcher | reload | reboot | cancel | observe | preview
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
      .mapError(error => failure(error, args))
      .flatMap:
        case CommandDirective.UserDefined(Nil, wire) => command(wire)
        case CommandDirective.UserDefined(_, _)      =>
          ZIO.fail(CliParseError.UnexpectedArguments)
        case CommandDirective.BuiltIn(_) =>
          args match
            case "publish" :: Nil              => ZIO.fail(CliParseError.PublishForm)
            case "preview" :: "publish" :: Nil => ZIO.fail(CliParseError.PublishForm)
            case _                             => ZIO.fail(CliParseError.Help)

  private def command(wire: Wire): IO[CliParseError, Command] =
    wire match
      case Wire.Version    => ZIO.succeed(Command.Version)
      case Wire.Graph      => ZIO.succeed(Command.Graph)
      case Wire.Mcp        => ZIO.succeed(Command.Mcp)
      case Wire.Bsp        => ZIO.succeed(Command.Bsp)
      case Wire.Server     => ZIO.succeed(Command.Server)
      case Wire.Portal     => ZIO.succeed(Command.Portal)
      case Wire.CachePath  => ZIO.succeed(Command.CachePath)
      case Wire.CacheList  => ZIO.succeed(Command.CacheList)
      case Wire.Toolchain  => ZIO.succeed(Command.Toolchain)
      case Wire.Reload     => ZIO.succeed(Command.Reload)
      case Wire.Reboot     => ZIO.succeed(Command.Reboot)
      case Wire.Cancel(id) =>
        ZIO.fromEither(
          RunId.from(id).left.map(CliParseError.RunId(_)).map(Command.Cancel(_))
        )
      case Wire.CacheClean(mode)       => ZIO.succeed(Command.CacheClean(mode))
      case Wire.CachePromote(from, to) =>
        ZIO.fromEither(namespaces(from, to)).map { (source, dest) =>
          Command.CachePromote(source, dest)
        }
      case Wire.Resolve(action)  => ZIO.succeed(Command.Resolve(action))
      case Wire.Launcher(action) => ZIO.succeed(Command.Launcher(action))
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
      case Wire.PublishLocal(tokens) =>
        ZIO.succeed(Command.PublishLocal(tokens))
      case Wire.PublishSnapshots(tokens) =>
        ZIO.succeed(Command.PublishSnapshots(tokens))
      case Wire.PublishRelease(automatic, tokens) =>
        ZIO.succeed(Command.PublishRelease(automatic, tokens))
      case Wire.PublishPromote(id) =>
        ZIO.succeed(Command.PublishPromote(id))
      case Wire.PublishDrop(id) =>
        ZIO.succeed(Command.PublishDrop(id))
      case Wire.PublishAssets(tokens) =>
        ZIO.succeed(Command.PublishAssets(tokens))
      case Wire.PreviewCompile(projects) =>
        projectList("preview compile", projects).map(Command.PreviewCompile(_))
      case Wire.PreviewTest(projects) =>
        projectList("preview test", projects).map(Command.PreviewTest(_))
      case Wire.PreviewPublishLocal(tokens) =>
        ZIO.succeed(Command.PreviewPublishLocal(tokens))
      case Wire.PreviewPublishSnapshots(tokens) =>
        ZIO.succeed(Command.PreviewPublishSnapshots(tokens))
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
          case Left(error)  => Left(CliParseError.ProjectName(error))
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
          case Left(error)    => Left(CliParseError.SuitePattern(error))
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
        name <- ProjectName.from(token).left.map(CliParseError.ProjectName(_))
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
              error => CliParseError.BadPluginTask(error),
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
                  error => CliParseError.BadPlatform(error),
                  platform => NamedRun.Module(id, Some(platform)),
                )

  private def namespaces(from: String, to: String): Either[CliParseError, (CacheNamespace, CacheNamespace)] =
    for
      source <- CacheNamespace.from(from).left.map(CliParseError.Namespace(_))
      dest   <- CacheNamespace.from(to).left.map(CliParseError.Namespace(_))
    yield (source, dest)

  private def failure(error: ValidationError, args: List[String]): CliParseError =
    args match
      case "cache" :: "promote" :: _ =>
        error.validationErrorType match
          case ValidationErrorType.MissingValue | ValidationErrorType.InvalidArgument =>
            CliParseError.Pattern("cache promote needs --from and --to")
          case _ => reject(error, args)
      case "publish" :: ("snapshots" | "release" | "assets" | "promote" | "drop") :: _ => reject(error, args)
      case "publish" :: _                                                              => CliParseError.PublishForm
      case "preview" :: "publish" :: "snapshots" :: _                                  => reject(error, args)
      case "preview" :: "publish" :: _                                                 => CliParseError.PublishForm
      case _                                                                           => reject(error, args)

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
