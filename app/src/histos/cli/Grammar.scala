package histos.cli

import histos.doctor.{FixMode, Suggest}
import histos.cache.CacheCleanMode
import histos.central.DeploymentId
import histos.plugin.{CommandName, PluginTaskId}
import histos.publish.PublishToken
import histos.schedule.CacheNamespace
import histos.server.{CleanExtent, ResolveAction}
import histos.task.{RunId, TaskId}
import histos.testrun.{SuitePattern, TestMode}
import histos.workflow.WorkflowAction
import histos.format.{FormatMode, FormatTargets}
import histos.supply.{PolicyWrite, SetupSource, ZipxConfirm}

import java.nio.file.Path

import zio.NonEmptyChunk
import histos.scaffold.ImportSource
import histos.version.{BumpPart, ShipName}
import histos.{Platform, ProjectName}

import zio.json.*
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

/** zio-cli grammar. The wire value still has the raw name. [[Grammar.parse]] turns that name into a
  * [[histos.task.TaskId]] and, for a module, a platform.
  */
object Grammar:
  private enum Wire:
    case Version, Graph, Mcp, Bsp, Server, Portal, CachePath, CacheList, Reload, Reboot
    case Cancel(id: String)
    case New(name: String, bare: Boolean, yes: Boolean, branch: Option[String])
    case Init(bare: Boolean, yes: Boolean, from: Option[String], branch: Option[String])
    case Run(name: String, platform: Option[String])
    case Test(tokens: List[String])
    case Coverage(projects: List[String], floors: histos.FloorInput)
    case Affected(base: String)
    case Compile(projects: List[String])
    case Clean(tokens: List[String])
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
    case Workflow(action: WorkflowAction)
    case Fmt(check: Boolean, files: Boolean, args: List[String])
    case PreviewCompile(projects: List[String])
    case PreviewTest(tokens: List[String])
    case PreviewPublishLocal(tokens: List[String])
    case PreviewPublishSnapshots(projects: List[String])
    case PreviewClean(tokens: List[String])
    case PreviewRun(name: String, platform: Option[String])
    case Observe
    case Doctor(
        area: Option[String],
        json: Boolean,
        preflight: Option[String],
        fix: Boolean,
        yes: Boolean,
        verbose: Boolean,
        projects: List[String],
    )
    case Deps(action: DepsAction)
    case Setup(from: Option[String], fix: Boolean, yes: Boolean)
    case Bump(part: BumpPart, ships: List[String])
    case Help(words: List[String])

  private val version = CliCommand("version").map(_ => Wire.Version)
  private val graph   = CliCommand("graph").map(_ => Wire.Graph)
  private val run     =
    CliCommand("run", Args.text("name") ++ Args.text("platform").atMost(1))
      .map { case (name, platform) =>
        Wire.Run(name, platform.headOption)
      }
  private val test =
    CliCommand("test", Args.text("token").*).map(tokens => Wire.Test(tokens.toList))
  private val coverageFloors =
    Options.integer("min-stmt").optional ++
      Options.integer("min-branch").optional ++
      Options.integer("min-stmt-file").optional ++
      Options.integer("min-branch-file").optional ++
      Options.integer("min-stmt-package").optional ++
      Options.integer("min-branch-package").optional
  private val coverage =
    CliCommand("coverage", coverageFloors, Args.text("project").*).map {
      case (
            (statements, branches, fileStatements, fileBranches, packageStatements, packageBranches),
            projects,
          ) =>
        Wire.Coverage(
          projects.toList,
          histos.FloorInput(
            statements,
            branches,
            fileStatements,
            fileBranches,
            packageStatements,
            packageBranches,
          ),
        )
    }
  private val affected   = CliCommand("affected", Options.text("base")).map(Wire.Affected(_))
  private val compile    = CliCommand("compile", Args.text("project").*).map(names => Wire.Compile(names.toList))
  private val cleanBuild = CliCommand("clean", Args.text("token").*).map(tokens => Wire.Clean(tokens.toList))
  private val assembly   =
    CliCommand("assembly", Args.text("destination").atMost(1)).map(paths => Wire.Assembly(paths.headOption))
  private val publishLocal =
    CliCommand("local", Args.text("token").*).map(tokens => Wire.PublishLocal(tokens.toList))
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
      publishLocal | publishSnapshots | publishRelease | publishAssets | publishPromote | publishDrop
    )
  private val mcp    = CliCommand("mcp").map(_ => Wire.Mcp)
  private val bsp    = CliCommand("bsp").map(_ => Wire.Bsp)
  private val server = CliCommand("server").map(_ => Wire.Server)
  private val portal = CliCommand("portal").map(_ => Wire.Portal)
  private val reload = CliCommand("reload").map(_ => Wire.Reload)
  private val reboot = CliCommand("reboot").map(_ => Wire.Reboot)
  private val cancel =
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
  private val fmt =
    CliCommand("fmt", Options.boolean("check") ++ Options.boolean("file"), Args.text("target").*).map {
      case ((check, files), args) => Wire.Fmt(check, files, args.toList)
    }
  private val previewCompile =
    CliCommand("compile", Args.text("project").*).map(names => Wire.PreviewCompile(names.toList))
  private val previewTest =
    CliCommand("test", Args.text("token").*).map(tokens => Wire.PreviewTest(tokens.toList))
  private val previewPublishSnapshots =
    CliCommand("snapshots", Args.text("project").*).map(names => Wire.PreviewPublishSnapshots(names.toList))
  private val previewPublishLocal =
    CliCommand("local", Args.text("token").*).map(tokens => Wire.PreviewPublishLocal(tokens.toList))
  private val previewPublish =
    CliCommand("publish").subcommands(previewPublishLocal | previewPublishSnapshots)
  private val previewClean =
    CliCommand("clean", Args.text("token").*).map(tokens => Wire.PreviewClean(tokens.toList))
  private val previewRun =
    CliCommand("run", Args.text("name") ++ Args.text("platform").atMost(1))
      .map { case (name, platform) =>
        Wire.PreviewRun(name, platform.headOption)
      }
  private val doctor =
    CliCommand(
      "doctor",
      Options.text("area").optional ++ Options.boolean("json") ++ Options.text("preflight").optional ++
        Options.boolean("fix") ++ Options.boolean("yes") ++ Options.boolean("verbose"),
      Args.text("project").*,
    ).map { case ((area, json, preflight, fix, yes, verbose), projects) =>
      Wire.Doctor(area, json, preflight, fix, yes, verbose, projects.toList)
    }
  private val depsCheck  = CliCommand("check").map(_ => Wire.Deps(DepsAction.Check))
  private val depsUpdate =
    CliCommand("update", Options.boolean("yes") ++ Options.boolean("security")).map { (yes, security) =>
      Wire.Deps(DepsAction.Update(yes, security))
    }
  private val depsAudit =
    CliCommand("audit", Options.boolean("report")).map(report => Wire.Deps(DepsAction.Audit(report)))
  private val depsPr =
    CliCommand("pr", Options.boolean("security") ++ Options.boolean("open")).map { (security, open) =>
      Wire.Deps(DepsAction.Pr(security, open))
    }
  private val depsAlert    = CliCommand("alert").map(_ => Wire.Deps(DepsAction.Alert))
  private val depsSnapshot =
    CliCommand("snapshot", Options.boolean("submit")).map(submit => Wire.Deps(DepsAction.Snapshot(submit)))
  private val deps =
    CliCommand("deps").subcommands(depsCheck | depsUpdate | depsAudit | depsPr | depsAlert | depsSnapshot)
  private val supplyChain =
    CliCommand("supply-chain", Options.text("from").optional ++ Options.boolean("fix") ++ Options.boolean("yes"))
      .map { case (from, fix, yes) => Wire.Setup(from, fix, yes) }
  private val setup   = CliCommand("setup").subcommands(supplyChain)
  private val helpCmd =
    CliCommand("help", Args.text("topic").*).map(words => Wire.Help(words.toList))
  private val freshFlags =
    Options.boolean("bare") ++ Options.boolean("yes") ++ Options.text("branch").optional
  private val fresh =
    CliCommand("new", freshFlags, Args.text("name")).map { case ((bare, yes, branch), name) =>
      Wire.New(name, bare, yes, branch)
    }
  private val init =
    CliCommand("init", freshFlags ++ Options.text("from").optional).map { case (bare, yes, branch, from) =>
      Wire.Init(bare, yes, from, branch)
    }
  private val observe  = CliCommand("observe").map(_ => Wire.Observe)
  private val bumpPart =
    (Options.boolean("major") ++ Options.boolean("minor") ++ Options.boolean("patch")).mapOrFail:
      case (true, false, false)  => Right(BumpPart.Major)
      case (false, true, false)  => Right(BumpPart.Minor)
      case (false, false, true)  => Right(BumpPart.Patch)
      case (false, false, false) => Right(BumpPart.Patch)
      case _                     => refused
  private val bump =
    CliCommand("bump", bumpPart, Args.text("ship").*).map { (part, ships) =>
      Wire.Bump(part, ships.toList)
    }
  private val preview = CliCommand("preview").subcommands(
    previewCompile | previewTest | previewPublish | previewClean | previewRun
  )
  private val commands =
    version | graph | run | test | coverage | affected | compile | fresh | init | cleanBuild | assembly | publish | mcp | bsp | server | portal | cache | resolve | workflow | fmt | reload | reboot | cancel | observe | doctor | deps | setup | bump | preview | helpCmd
  private val root = CliCommand("histos").subcommands(commands)

  /** The root commands. Each one's word is in `CommandName.reserved`, so no plugin command can take it. */
  private[cli] val builtIns: CliCommand[?] = commands

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
        val (policy, body) = rest match
          case "--restart" :: tail => (histos.server.WatchPolicy.Restart, tail)
          case other               => (histos.server.WatchPolicy.After, other)
        prepared(body).flatMap: ready =>
          parseRoot(ready.args)
            .flatMap(command => withExcludes(command, ready))
            .flatMap(command => watched(command, policy))
            .map(command => Command.Watch(command, rest))
      case "--explain" :: Nil =>
        ZIO.fail(CliParseError.Pattern("--explain needs a check id"))
      case "--explain" :: id :: Nil =>
        ZIO.succeed(Command.Explain(id))
      case "--explain" :: _ =>
        ZIO.fail(CliParseError.Pattern("--explain takes one check id"))
      case _ =>
        prepared(args).flatMap: ready =>
          parseRoot(ready.args).flatMap(command => withExcludes(command, ready))

  /** `--restart` repeats a program. Every other watched command runs, then polls. */
  private def watched(command: Command, policy: histos.server.WatchPolicy): IO[CliParseError, Command] =
    policy match
      case histos.server.WatchPolicy.After   => ZIO.succeed(command)
      case histos.server.WatchPolicy.Restart =>
        command match
          case Command.Run(id, _) if !id.builtIn => ZIO.succeed(command)
          case _                                 =>
            ZIO.fail(CliParseError.WatchCommand("--restart watches a program"))

  private def parseRoot(args: List[String]): IO[CliParseError, Command] =
    gate(args) match
      case Some(error) => ZIO.fail(error)
      case None        =>
        root
          .parse("histos" :: args, CliConfig.default)
          .mapError(error => failure(error, args))
          .flatMap:
            case CommandDirective.UserDefined(Nil, wire) => command(wire)
            case CommandDirective.UserDefined(_, _)      =>
              ZIO.fail(shim(args).getOrElse(CliParseError.UnexpectedArguments))
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
      case Wire.Resolve(action)         => ZIO.succeed(Command.Resolve(action))
      case Wire.Workflow(action)        => ZIO.succeed(Command.Workflow(action))
      case Wire.Fmt(check, files, args) => ZIO.fromEither(fmtOf(check, files, args))
      case Wire.Test(tokens)            => tested("test", tokens, Command.Test.apply)
      case Wire.Coverage(projects, raw) =>
        ZIO
          .fromEither(histos.CoverageFloors.parse(raw))
          .mapError(error => CliParseError.Pattern(error.message))
          .flatMap: floors =>
            projectList("coverage", projects).map: names =>
              Command.Coverage(names, floors, histos.coverage.Excludes(Nil, Nil))
      case Wire.Affected(base) =>
        ZIO.succeed(Command.Affected(base))
      case Wire.New(name, bare, yes, branch) =>
        ZIO.succeed(Command.New(name, ScaffoldChoice.from(bare, yes), branch))
      case Wire.Init(bare, yes, from, branch) =>
        val choice = ScaffoldChoice.from(bare, yes)
        from match
          case None      => ZIO.succeed(Command.Init(choice, None, branch))
          case Some(raw) =>
            ImportSource.from(raw) match
              case Some(source) => ZIO.succeed(Command.Init(choice, Some(source), branch))
              case None         => ZIO.fail(CliParseError.Pattern("init --from is sbt or zipx"))
      case Wire.Observe                                                    => ZIO.succeed(Command.Observe)
      case Wire.Doctor(area, json, preflight, fix, yes, verbose, projects) =>
        doctorOf(area, json, preflight, fix, yes, verbose, projects)
      case Wire.Deps(action)      => ZIO.succeed(Command.Deps(action))
      case Wire.Bump(part, ships) =>
        ZIO
          .foreach(ships)(raw =>
            ZIO.fromEither(ShipName.from(raw).left.map(error => CliParseError.Pattern(error.message)))
          )
          .map(names => Command.Bump(part, names))
      case Wire.Setup(from, fix, yes) =>
        setupOf(from, fix, yes)
      case Wire.Help(words) =>
        ZIO.succeed(Command.Help(words))
      case Wire.Compile(projects) =>
        projectList("compile", projects).map(Command.Compile(_))
      case Wire.Clean(tokens) =>
        cleaned("clean", tokens, Command.Clean.apply)
      case Wire.Assembly(destination) =>
        ZIO.succeed(Command.Assembly(destination))
      case Wire.PublishLocal(tokens) =>
        publishTokens(tokens).map(Command.PublishLocal(_))
      case Wire.PublishSnapshots(tokens) =>
        ZIO.succeed(Command.PublishSnapshots(tokens))
      case Wire.PublishRelease(automatic, tokens) =>
        ZIO.succeed(Command.PublishRelease(automatic, tokens))
      case Wire.PublishPromote(id) =>
        deployment(id).map(Command.PublishPromote(_))
      case Wire.PublishDrop(id) =>
        deployment(id).map(Command.PublishDrop(_))
      case Wire.PublishAssets(tokens) =>
        ZIO.succeed(Command.PublishAssets(tokens))
      case Wire.PreviewCompile(projects) =>
        projectList("preview compile", projects).map(Command.PreviewCompile(_))
      case Wire.PreviewTest(tokens) =>
        tested("preview test", tokens, Command.PreviewTest.apply)
      case Wire.PreviewPublishLocal(tokens) =>
        publishTokens(tokens).map(Command.PreviewPublishLocal(_))
      case Wire.PreviewPublishSnapshots(tokens) =>
        ZIO.succeed(Command.PreviewPublishSnapshots(tokens))
      case Wire.PreviewClean(tokens) =>
        cleaned("preview clean", tokens, Command.PreviewClean.apply)
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

  private final case class Prepared(args: List[String], classlikes: List[String], files: List[String])

  private def prepared(args: List[String]): IO[CliParseError, Prepared] =
    ZIO.fromEither(splitCoverage(args))

  private def splitCoverage(args: List[String]): Either[CliParseError, Prepared] =
    args match
      case "coverage" :: rest =>
        takeExcludes(rest, Nil, Nil, Nil).map: (kept, classlikes, files) =>
          Prepared("coverage" :: kept, classlikes.reverse, files.reverse)
      case _ => Right(Prepared(args, Nil, Nil))

  private def takeExcludes(
      args: List[String],
      classlikes: List[String],
      files: List[String],
      kept: List[String],
  ): Either[CliParseError, (List[String], List[String], List[String])] =
    args match
      case Nil                      => Right((kept.reverse, classlikes, files))
      case "--exclude-class" :: Nil =>
        Left(CliParseError.Pattern("coverage --exclude-class needs a regex"))
      case "--exclude-class" :: value :: rest =>
        takeExcludes(rest, value :: classlikes, files, kept)
      case "--exclude-file" :: Nil =>
        Left(CliParseError.Pattern("coverage --exclude-file needs a regex"))
      case "--exclude-file" :: value :: rest =>
        takeExcludes(rest, classlikes, value :: files, kept)
      case flag :: Nil if histos.FloorName.known(flag) =>
        Left(CliParseError.Pattern(s"coverage $flag needs an integer"))
      case flag :: value :: rest if histos.FloorName.known(flag) =>
        if kept.contains(flag) then Left(CliParseError.Pattern(s"coverage $flag is already set"))
        else takeExcludes(rest, classlikes, files, value :: flag :: kept)
      case head :: rest =>
        takeExcludes(rest, classlikes, files, head :: kept)

  private def withExcludes(command: Command, ready: Prepared): IO[CliParseError, Command] =
    command match
      case Command.Coverage(projects, floors, _) =>
        ZIO
          .fromEither(histos.coverage.Excludes.check(ready.classlikes, ready.files))
          .mapBoth(
            error => CliParseError.Pattern(error.message),
            excludes => Command.Coverage(projects, floors, excludes),
          )
      case other => ZIO.succeed(other)

  /** Shim words win. A removed word fails before zio-cli and before a plugin can take it. */
  private def gate(args: List[String]): Option[CliParseError] =
    shim(args).orElse(retired(args))

  private def retired(args: List[String]): Option[CliParseError] =
    args match
      case "testOnly" :: _                  => Some(CliParseError.Retired("testOnly", "test --only"))
      case "testQuick" :: _                 => Some(CliParseError.Retired("testQuick", "test --quick"))
      case "cleanFull" :: _                 => Some(CliParseError.Retired("cleanFull", "clean --full"))
      case "publishLocal" :: _              => Some(CliParseError.Retired("publishLocal", "publish local"))
      case "client" :: _                    => Some(CliParseError.Retired("client", "publish"))
      case "launcher" :: _                  => Some(CliParseError.Retired("launcher", "update"))
      case "toolchain" :: _                 => Some(CliParseError.Retired("toolchain", "doctor"))
      case "preview" :: "testOnly" :: _     => Some(CliParseError.Retired("preview testOnly", "preview test --only"))
      case "preview" :: "testQuick" :: _    => Some(CliParseError.Retired("preview testQuick", "preview test --quick"))
      case "preview" :: "cleanFull" :: _    => Some(CliParseError.Retired("preview cleanFull", "preview clean --full"))
      case "preview" :: "publishLocal" :: _ =>
        Some(CliParseError.Retired("preview publishLocal", "preview publish local"))
      case _ => None

  private def tested(
      command: String,
      tokens: List[String],
      build: (List[ProjectName], AffectedChoice, TestMode, List[SuitePattern]) => Command,
  ): IO[CliParseError, Command] =
    ZIO.fromEither(splitTest(command, tokens)).flatMap { parts =>
      for
        names    <- projectList(command, parts.projects)
        patterns <- ZIO.fromEither(suitePatterns(parts.patterns))
        picked   <- choice(parts.affected)
      yield build(names, picked, parts.mode, patterns)
    }

  private def cleaned(
      command: String,
      tokens: List[String],
      build: (List[ProjectName], CleanExtent) => Command,
  ): IO[CliParseError, Command] =
    ZIO.fromEither(splitClean(command, tokens)).flatMap { (raw, extent) =>
      if extent == CleanExtent.Full && raw.nonEmpty then
        ZIO.fail(CliParseError.Pattern(s"$command --full does not take a project"))
      else projectList(command, raw).map(names => build(names, extent))
    }

  private final case class TestParts(
      projects: List[String],
      affected: Option[String],
      mode: TestMode,
      patterns: List[String],
  )

  /** Projects are the words before `--only`. Patterns are the words after it. Flags may sit anywhere. */
  private def splitTest(command: String, args: List[String]): Either[CliParseError, TestParts] =
    def loop(
        rest: List[String],
        projects: List[String],
        affected: Option[String],
        quick: Boolean,
        only: Boolean,
        patterns: List[String],
    ): Either[CliParseError, TestParts] =
      rest match
        case Nil =>
          if only && patterns.isEmpty then Left(CliParseError.Pattern(s"$command --only needs a pattern"))
          else
            val mode = if quick then TestMode.Quick else TestMode.Run
            Right(TestParts(projects.reverse, affected, mode, patterns.reverse))
        case "--quick" :: tail =>
          if quick then Left(CliParseError.Pattern(s"$command --quick is already set"))
          else loop(tail, projects, affected, true, only, patterns)
        case "--only" :: Nil =>
          Left(CliParseError.Pattern(s"$command --only needs a pattern"))
        case "--only" :: tail =>
          loop(tail, projects, affected, quick, true, patterns)
        case "--affected" :: Nil =>
          Left(CliParseError.Pattern(s"$command --affected needs a JSON list"))
        case "--affected" :: value :: tail if value.startsWith("-") =>
          Left(CliParseError.Pattern(s"$command --affected needs a JSON list"))
        case "--affected" :: value :: tail =>
          affected match
            case Some(_) => Left(CliParseError.Pattern(s"$command --affected is already set"))
            case None    => loop(tail, projects, Some(value), quick, only, patterns)
        case flag :: _ if flag.startsWith("-") =>
          Left(CliParseError.Pattern(Help.flag(command, flag)))
        case token :: tail if only =>
          loop(tail, projects, affected, quick, true, token :: patterns)
        case token :: tail =>
          loop(tail, token :: projects, affected, quick, only, patterns)
    loop(args, Nil, None, false, false, Nil)

  private def splitClean(
      command: String,
      args: List[String],
  ): Either[CliParseError, (List[String], CleanExtent)] =
    def loop(
        rest: List[String],
        projects: List[String],
        full: Boolean,
    ): Either[CliParseError, (List[String], CleanExtent)] =
      rest match
        case Nil =>
          val extent = if full then CleanExtent.Full else CleanExtent.Products
          Right((projects.reverse, extent))
        case "--full" :: tail =>
          if full then Left(CliParseError.Pattern(s"$command --full is already set"))
          else loop(tail, projects, true)
        case flag :: _ if flag.startsWith("-") =>
          Left(CliParseError.Pattern(Help.flag(command, flag)))
        case token :: tail =>
          loop(tail, token :: projects, full)
    loop(args, Nil, false)

  private def suitePatterns(raw: List[String]): Either[CliParseError, List[SuitePattern]] =
    raw.foldLeft[Either[CliParseError, List[SuitePattern]]](Right(Nil)): (acc, token) =>
      for
        have    <- acc
        pattern <- SuitePattern.from(token).left.map(CliParseError.SuitePattern(_))
      yield have :+ pattern

  private def publishTokens(raw: List[String]): IO[CliParseError, List[PublishToken]] =
    ZIO.foreach(raw): token =>
      ZIO.fromEither(PublishToken.from(token).left.map(error => CliParseError.Pattern(error.message)))

  private def deployment(raw: String): IO[CliParseError, DeploymentId] =
    ZIO.fromEither(DeploymentId.from(raw).left.map(error => CliParseError.Pattern(error.message)))

  private def doctorOf(
      area: Option[String],
      json: Boolean,
      preflight: Option[String],
      fix: Boolean,
      yes: Boolean,
      verbose: Boolean,
      projects: List[String],
  ): IO[CliParseError, Command] =
    val form   = if json then ReportForm.Json else ReportForm.Text
    val detail = if verbose then DoctorDetail.Verbose else DoctorDetail.Brief
    val mode   = FixMode.from(fix, yes)
    for
      scope <- ZIO.foreach(area)(raw =>
        ZIO.fromEither(DoctorScope.parse(raw).left.map(error => CliParseError.Pattern(error.message)))
      )
      job <- ZIO.foreach(preflight)(raw =>
        ZIO.fromEither(DoctorScope.preflight(raw).left.map(error => CliParseError.Pattern(error.message)))
      )
      names <- projectList("doctor", projects)
    yield Command.Doctor(scope, form, job, mode, detail, names)

  private def setupOf(from: Option[String], fix: Boolean, yes: Boolean): IO[CliParseError, Command] =
    val write   = if fix then PolicyWrite.Write else PolicyWrite.Preview
    val confirm = if yes then ZipxConfirm.Delete else ZipxConfirm.Ask
    setupSource(from).map(source => Command.Setup(source, write, confirm))

  private def setupSource(raw: Option[String]): IO[CliParseError, Option[SetupSource]] =
    raw match
      case None         => ZIO.succeed(None)
      case Some("zipx") => ZIO.succeed(Some(SetupSource.Zipx))
      case Some(text)   =>
        if text.isEmpty then ZIO.fail(CliParseError.Pattern("setup --from names zipx or a relative directory"))
        else
          scala.util.Try(Path.of(text)).toOption match
            case None =>
              ZIO.fail(CliParseError.Pattern("setup --from names zipx or a relative directory"))
            case Some(path) if path.isAbsolute =>
              ZIO.fail(CliParseError.Pattern("setup --from names zipx or a relative directory"))
            case Some(_) =>
              ZIO.succeed(Some(SetupSource.Directory(text)))

  private enum NamedRun:
    case Module(id: TaskId, platform: Option[Platform])
    case Plugin(id: PluginTaskId)

  private def choice(raw: Option[String]): IO[CliParseError, AffectedChoice] =
    raw match
      case None       => ZIO.succeed(AffectedChoice.Unset)
      case Some(text) =>
        text.fromJson[List[String]] match
          case Left(_) =>
            ZIO.fail(CliParseError.Pattern("affected is a JSON list of module names"))
          case Right(Nil) =>
            ZIO.succeed(AffectedChoice.NoneSelected)
          case Right(names) if names.contains("all") =>
            ZIO.succeed(AffectedChoice.Everything)
          case Right(names) =>
            ZIO
              .foreach(names)(name => ZIO.fromEither(ProjectName.from(name)).mapError(CliParseError.ProjectName(_)))
              .map(parsed => AffectedChoice.Only(parsed))

  /** zio-cli reads an option only while it is the next token, so a flag after a target is still in `args`. No target is
    * the build. `--file` makes every target a repository-relative file.
    */
  private def fmtOf(check: Boolean, files: Boolean, args: List[String]): Either[CliParseError, Command] =
    val mode    = if check || args.contains("--check") then FormatMode.Check else FormatMode.Write
    val named   = files || args.contains("--file")
    val targets = args.filterNot(token => token == "--check" || token == "--file")
    targets.find(_.startsWith("-")) match
      case Some(flag) => Left(CliParseError.Fmt(FmtUsage.Flag(flag)))
      case None       =>
        val chosen =
          if named then
            targets
              .foldRight[Either[FmtUsage, List[Path]]](Right(Nil))((raw, rest) =>
                rest.flatMap(paths => path(raw).map(_ :: paths))
              )
              .flatMap(paths => NonEmptyChunk.fromIterableOption(paths).toRight(FmtUsage.NoFiles))
              .map(FormatTargets.Files(_))
              .left
              .map(CliParseError.Fmt(_))
          else
            parsedNames(targets).map(names =>
              NonEmptyChunk.fromIterableOption(names).fold(FormatTargets.Build)(FormatTargets.Projects(_))
            )
        chosen.map(Command.Fmt(_, mode))

  private def path(raw: String): Either[FmtUsage, Path] =
    scala.util.Try(Path.of(raw)).toOption match
      case None                            => Left(FmtUsage.BadPath(raw))
      case Some(found) if found.isAbsolute => Left(FmtUsage.Absolute(raw))
      case Some(found)                     => Right(found)

  private def projectList(
      command: String,
      projects: List[String],
  ): IO[CliParseError, List[ProjectName]] =
    projects.find(_.startsWith("-")) match
      case Some(flag) =>
        ZIO.fail(CliParseError.Pattern(Help.flag(command, flag)))
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
          val task = id.builtIn
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
      case "publish" :: ("local" | "snapshots" | "release" | "assets" | "promote" | "drop") :: _ =>
        reject(error, args)
      case "publish" :: _                                         => CliParseError.PublishForm
      case "preview" :: "publish" :: ("local" | "snapshots") :: _ => reject(error, args)
      case "preview" :: "publish" :: _                            => CliParseError.PublishForm
      case _                                                      => reject(error, args)

  private def shim(args: List[String]): Option[CliParseError] =
    args match
      case "update" :: _ =>
        Some(CliParseError.Pattern("histos update is run by the installed shim\nhistos help update"))
      case "self" :: _ =>
        Some(CliParseError.Pattern("histos self update is run by the installed shim\nhistos help self"))
      case "toolchain" :: "install" :: _ =>
        Some(
          CliParseError.Pattern(
            "histos toolchain install <version> is run by the installed shim\nhistos help toolchain"
          )
        )
      case "toolchain" :: "list" :: _ =>
        Some(CliParseError.Pattern("histos toolchain list is run by the installed shim\nhistos help toolchain"))
      case _ => None

  private def reject(
      error: ValidationError,
      args: List[String],
  ): CliParseError =
    shim(args) match
      case Some(held) => held
      case None       =>
        error.validationErrorType match
          case ValidationErrorType.CommandMismatch                                    => mismatched(args)
          case ValidationErrorType.InvalidArgument | ValidationErrorType.MissingValue =>
            CliParseError.MissingArgument
          case _ => CliParseError.Rejected

  private def mismatched(args: List[String]): CliParseError =
    args match
      case "cache" :: token :: _ =>
        CliParseError.UnknownCommand(token, Suggest.near(token, List("path", "list", "clean", "promote")))
      case "deps" :: token :: _ =>
        CliParseError.UnknownCommand(
          token,
          Suggest.near(token, List("check", "update", "audit", "pr", "alert", "snapshot")),
        )
      case "preview" :: token :: _ =>
        CliParseError.UnknownCommand(
          token,
          Suggest.near(token, List("compile", "test", "publish", "clean", "run")),
        )
      case "setup" :: token :: _ =>
        CliParseError.UnknownCommand(token, Suggest.near(token, List("supply-chain")))
      case "help" :: token :: _ =>
        CliParseError.UnknownCommand(token, Suggest.near(token, Help.names))
      case head :: _ if roots.contains(head) =>
        CliParseError.UnknownCommand(head, Suggest.near(head, Help.names))
      case head :: _ =>
        CliParseError.UnknownRoot(head)
      case Nil =>
        CliParseError.UnknownRoot("")
end Grammar
