package histos.mcp

import heddle.LinePipe
import heddle.endpoint.{Schema, SchemaDoc}
import heddle.mcp.{Mcp, McpBuildError}
import heddle.mcp.protocol.ToolName
import histos.cache.{CacheCleanMode, ContentStore}
import histos.cli.{Appearance, Cli, Depth, Ink, Paint, TruecolorChoice}
import histos.client.{ClientJson, Session}
import histos.format.{FormatMode, FormatTargets}
import histos.report.{BuildFault, Rejection, Stored, WireSchema}
import WireSchema.given
import histos.jdk.Machine
import histos.load.Loader
import histos.plugin.{FlagName, NamedText, ParamName, PluginArgs}
import histos.publish.{IvyRoot, PublishToken}
import histos.resolve.Resolve
import histos.server.{Call, CleanExtent, ForgetFault, ForgetTarget, ResolveAction, Successor}
import histos.supply.DoctorSurface
import histos.task.RunId
import histos.testrun.{SuitePattern, TagName, TestFilter, TestMode, TestTerm}
import histos.workflow.WorkflowAction
import histos.doctor.{DoctorLaunch, FaultText}
import histos.select.SelectError
import histos.task.TaskId
import histos.{PlatformSelection, ProjectName, ProjectNameError, ScalaSelection, SelectionError}

import ClientJson.given

import java.nio.file.Path

import zio.*
import zio.json.JsonCodec

/** MCP tools. Each call reads `.histos/server.json` and sends [[histos.server.Call]] when it matches.
  */
object HistosMcp:
  /** Where `publish` writes. `local` is the Ivy repository. `snapshots` is the snapshot coordinate. */
  enum PublishPlace:
    case Local, Snapshots

    def token: String = this match
      case Local     => "local"
      case Snapshots => "snapshots"
  end PublishPlace

  enum PublishPlaceError:
    case Unknown(raw: String)

    def message: String = this match
      case Unknown(raw) => s"'$raw' is not a publish place"
  end PublishPlaceError

  object PublishPlace:
    given JsonCodec[PublishPlace] =
      JsonCodec.string.transformOrFail(
        raw => from(raw).left.map(_.message),
        _.token,
      )

    def from(raw: String): Either[PublishPlaceError, PublishPlace] =
      PublishPlace.values.find(_.token == raw).toRight(PublishPlaceError.Unknown(raw))
  end PublishPlace

  /** What `preview` shows. `local` is `preview publish local`. */
  enum PreviewOp:
    case Compile, Test, Local, Snapshots, Clean, Run

    def token: String = this match
      case Compile   => "compile"
      case Test      => "test"
      case Local     => "local"
      case Snapshots => "snapshots"
      case Clean     => "clean"
      case Run       => "run"
  end PreviewOp

  enum PreviewOpError:
    case Unknown(raw: String)

    def message: String = this match
      case Unknown(raw) => s"'$raw' is not a preview operation"
  end PreviewOpError

  object PreviewOp:
    given JsonCodec[PreviewOp] =
      JsonCodec.string.transformOrFail(
        raw => from(raw).left.map(_.message),
        _.token,
      )

    def from(raw: String): Either[PreviewOpError, PreviewOp] =
      PreviewOp.values.find(_.token == raw).toRight(PreviewOpError.Unknown(raw))
  end PreviewOp

  given Schema[TestMode] =
    Schema.of(SchemaDoc.Enum(Some("test mode"), TestMode.values.toList.map(_.token)))
  given Schema[ResolveAction] =
    Schema.of(SchemaDoc.Enum(Some("resolve action"), ResolveAction.tokens))
  given Schema[WorkflowAction] =
    Schema.of(SchemaDoc.Enum(Some("workflow action"), WorkflowAction.values.toList.map(_.token)))
  given Schema[FormatMode] =
    Schema.of(SchemaDoc.Enum(Some("fmt mode"), FormatMode.values.toList.map(_.toString)))
  given Schema[CleanExtent] =
    Schema.of(SchemaDoc.Enum(Some("clean extent"), CleanExtent.values.toList.map(_.token)))
  given Schema[PublishPlace] =
    Schema.of(SchemaDoc.Enum(Some("publish place"), PublishPlace.values.toList.map(_.token)))
  given Schema[PreviewOp] =
    Schema.of(SchemaDoc.Enum(Some("preview operation"), PreviewOp.values.toList.map(_.token)))

  final case class NoArgs() derives Schema, JsonCodec
  final case class NamedArg(name: String, value: String) derives Schema, JsonCodec
  final case class RunArgs(
      id: String,
      platform: Option[String] = None,
      texts: List[NamedArg] = Nil,
      flags: List[String] = Nil,
      env: List[NamedArg] = Nil,
      directory: Option[String] = None,
      args: List[String] = Nil,
      scala: Option[String] = None,
  ) derives Schema,
        JsonCodec
  final case class CancelArgs(id: String) derives Schema, JsonCodec
  final case class CompileArgs(
      projects: List[String] = Nil,
      color: Boolean = false,
      truecolor: Option[Boolean] = None,
      platform: List[String] = Nil,
      scala: List[String] = Nil,
  ) derives Schema,
        JsonCodec
  final case class TestArgs(
      projects: List[String] = Nil,
      patterns: List[String] = Nil,
      tests: List[String] = Nil,
      tags: List[String] = Nil,
      ignoreTags: List[String] = Nil,
      mode: TestMode = TestMode.Run,
      color: Boolean = false,
      truecolor: Option[Boolean] = None,
      platform: List[String] = Nil,
      scala: List[String] = Nil,
  ) derives Schema,
        JsonCodec
  final case class PublishArgs(place: PublishPlace, tokens: List[String] = Nil) derives Schema, JsonCodec
  final case class AssemblyArgs(modules: List[String] = Nil) derives Schema, JsonCodec
  final case class CleanArgs(
      projects: List[String] = Nil,
      extent: CleanExtent = CleanExtent.Products,
  ) derives Schema,
        JsonCodec
  final case class ResolveArgs(
      action: ResolveAction = ResolveAction.Verify,
      selectors: List[String] = Nil,
  ) derives Schema,
        JsonCodec
  final case class WorkflowArgs(action: WorkflowAction = WorkflowAction.Check) derives Schema, JsonCodec
  final case class FmtArgs(
      projects: List[String] = Nil,
      files: List[String] = Nil,
      mode: Option[FormatMode] = None,
  ) derives Schema,
        JsonCodec
  final case class CacheCleanArgs(mode: Option[String] = None) derives Schema, JsonCodec
  final case class CacheForgetArgs(target: String) derives Schema, JsonCodec
  final case class CacheVerifyArgs(projects: List[String] = Nil) derives Schema, JsonCodec
  final case class WatchArgs(args: List[String] = Nil) derives Schema, JsonCodec
  final case class PreviewArgs(
      operation: PreviewOp,
      id: String,
      projects: List[String] = Nil,
      patterns: List[String] = Nil,
      mode: TestMode = TestMode.Run,
      extent: CleanExtent = CleanExtent.Products,
      platform: List[String] = Nil,
      scala: List[String] = Nil,
  ) derives Schema,
        JsonCodec
  final case class DoctorArgs(
      area: Option[String] = None,
      json: Boolean = false,
      preflight: Option[String] = None,
      fix: Boolean = false,
      yes: Boolean = false,
      verbose: Boolean = false,
      projects: List[String] = Nil,
      color: Boolean = false,
      truecolor: Option[Boolean] = None,
  ) derives Schema,
        JsonCodec
  final case class DoctorText(text: String) derives Schema, JsonCodec

  enum ServeError:
    case Build(errors: NonEmptyChunk[McpBuildError])
    case Stdio

    def message: String = this match
      case Build(errors) => errors.map(_.message).mkString("\n")
      case Stdio         => "the mcp pipe failed"
  end ServeError

  def serve(
      repo: Path,
      apiJar: Path,
      pipe: LinePipe,
      machine: Machine,
  ): ZIO[Loader & Scope & Resolve & ContentStore & IvyRoot & DoctorSurface.Reads, ServeError, Unit] =
    for
      desk <- Session.desk
      _    <- ZIO.addFinalizer(
        desk.held.get.flatMap {
          case Some(held) => held.scope.close(Exit.unit)
          case None       => ZIO.unit
        }
      )
      mcp <- ZIO
        .fromEither(tools(repo, apiJar, machine, desk))
        .mapError(ServeError.Build(_))
      _ <- mcp.stdio(pipe).mapError(_ => ServeError.Stdio)
    yield ()

  /** Names passed to `tool`. `tools/list` must equal this set. */
  val toolNames: Set[ToolName] = Names.all

  private object Names:
    inline val graph       = "graph"
    inline val run         = "run"
    inline val cancel      = "cancel"
    inline val compile     = "compile"
    inline val test        = "test"
    inline val publish     = "publish"
    inline val version     = "version"
    inline val assembly    = "assembly"
    inline val clean       = "clean"
    inline val resolve     = "resolve"
    inline val workflow    = "workflow"
    inline val fmt         = "fmt"
    inline val reload      = "reload"
    inline val reboot      = "reboot"
    inline val cachePath   = "cachePath"
    inline val cacheList   = "cacheList"
    inline val cacheClean  = "cacheClean"
    inline val cacheForget = "cacheForget"
    inline val cacheVerify = "cacheVerify"
    inline val watch       = "watch"
    inline val preview     = "preview"
    inline val observe     = "observe"
    inline val doctor      = "doctor"

    val all: Set[ToolName] =
      Set(
        ToolName(graph),
        ToolName(run),
        ToolName(cancel),
        ToolName(compile),
        ToolName(test),
        ToolName(publish),
        ToolName(version),
        ToolName(assembly),
        ToolName(clean),
        ToolName(resolve),
        ToolName(workflow),
        ToolName(fmt),
        ToolName(reload),
        ToolName(reboot),
        ToolName(cachePath),
        ToolName(cacheList),
        ToolName(cacheClean),
        ToolName(cacheForget),
        ToolName(cacheVerify),
        ToolName(watch),
        ToolName(preview),
        ToolName(observe),
        ToolName(doctor),
      )
  end Names

  def tools(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Session.Desk,
  ): Either[
    NonEmptyChunk[McpBuildError],
    Mcp[Loader & Scope & Resolve & ContentStore & IvyRoot & DoctorSurface.Reads],
  ] =
    val ask     = (call: Call) => Session.deliver(Session.perform(repo, apiJar, machine, desk, call))
    val refused = (reason: Rejection) => Session.CallProblem.Fault(BuildFault.Rejected(reason))
    for
      base    <- Mcp.from()
      listed  <- base.tool[NoArgs](Names.graph, "Show the project graph")(_ => ask(Call.Graph))
      running <- listed.tool[RunArgs](
        Names.run,
        "Run a main, or inspect, versions, or a plugin task. platform is jvm, js, or native. scala is one series (2.13 or 3) or one full version. A module run starts one process. texts and flags are the plugin arguments. env overlays the server environment and is omitted as empty. directory must already exist inside the repo, and an omitted directory stays under .histos/run. args are the program arguments. stdin is not attached.",
      )(args =>
        val asked = arguments(args.texts, args.flags).left
          .map(BuildFault.Rejected(_))
          .flatMap(parsed => programDirectory(args.directory).map(dir => (parsed, overlay(args.env), dir, args.args)))
        ZIO
          .fromEither(asked)
          .mapError(Session.CallProblem.Fault(_))
          .flatMap { (parsed, env, directory, argv) =>
            directory match
              case None =>
                ZIO
                  .fromEither(Session.runCall(args.id, args.platform, parsed, env, None, argv, args.scala))
                  .mapError(Session.CallProblem.Fault(_))
              case Some(path) =>
                histos.program.RunDirectory
                  .accept(repo, path)
                  .mapError(fault =>
                    Session.CallProblem.Fault(BuildFault.Program(histos.program.ProgramError.Directory(fault)))
                  )
                  .flatMap(real =>
                    ZIO
                      .fromEither(Session.runCall(args.id, args.platform, parsed, env, Some(real), argv, args.scala))
                      .mapError(Session.CallProblem.Fault(_))
                  )
          }
          .flatMap(ask)
      )
      stopped <- running.tool[CancelArgs](Names.cancel, "Cancel a run")(args =>
        ZIO.fromEither(cancelCall(args.id)).mapError(refused).flatMap(ask)
      )
      compiling <- stopped.tool[CompileArgs](
        Names.compile,
        "Compile modules. An empty list compiles every module. An aggregate name compiles its modules. platform is jvm, js, or native. scala is 2.13, 3, or a full version. Empty lists keep every declared platform and version.",
      )(args =>
        depthOf(args.color, args.truecolor).flatMap { depth =>
          ZIO.fromEither(compileCall(args.projects, args.platform, args.scala)).mapError(refused).flatMap { call =>
            Session.deliver(Session.perform(repo, apiJar, machine, desk, call, depth))
          }
        }
      )
      testing <- compiling.tool[TestArgs](
        Names.test,
        "Test modules. An empty list tests every module. An aggregate name tests its modules. patterns are suite name globs. tests are label substrings, tags and ignoreTags are exact tag names. mode is run or quick, and an absent mode is run. platform and scala narrow the cells. Empty lists keep every declared platform and version. A named module refuses a series that matches two versions.",
      )(args =>
        depthOf(args.color, args.truecolor).flatMap { depth =>
          ZIO
            .fromEither(
              testCall(
                args.projects,
                args.patterns,
                args.mode,
                args.tests,
                args.tags,
                args.ignoreTags,
                args.platform,
                args.scala,
              )
            )
            .mapError(refused)
            .flatMap { call =>
              Session.deliver(Session.perform(repo, apiJar, machine, desk, call, depth))
            }
        }
      )
      publishing <- testing.tool[PublishArgs](
        Names.publish,
        "local installs into Coursier's ivy2Local. snapshots publishes the snapshot coordinate. A token is a ship, an aggregate, or a module. An empty list publishes every ship.",
      )(args => ZIO.fromEither(publishCall(args.place, args.tokens)).mapError(refused).flatMap(ask))
      version  <- publishing.tool[NoArgs](Names.version, "Show the histos version")(_ => ask(Call.Version))
      assembly <- version.tool[AssemblyArgs](
        Names.assembly,
        "Write each named assembly module to its asset path, or to a build output path, and print the path.",
      )(args => ask(Call.Assembly(args.modules)))
      cleaning <- assembly.tool[CleanArgs](
        Names.clean,
        "Delete build products. An empty list wipes every product root and does not load the graph. extent full also forgets this repo in the content store and takes no project.",
      )(args => ZIO.fromEither(cleanCall(args.projects, args.extent)).mapError(refused).flatMap(ask))
      resolving <- cleaning.tool[ResolveArgs](
        Names.resolve,
        "verify, update, or check dependencies. update stages plugin cells, loads that generation, then commits once.",
      )(args => ZIO.fromEither(resolveCall(args.action, args.selectors)).mapError(refused).flatMap(ask))
      flowing <- resolving.tool[WorkflowArgs](
        Names.workflow,
        "check or update workflow files. The launcher pin is not advanced.",
      )(args => ask(Call.Workflow(args.action)))
      formatting <- flowing.tool[FmtArgs](
        Names.fmt,
        "Format with the locked scalafmt the build declares. An empty projects and files list is every module and histos.scala. projects names modules. files names repository-relative files. mode is Write or Check, and an absent mode is Write. Check writes no source and rewrites .histos/format/ledger.",
      )(args => ZIO.fromEither(fmtCall(args)).mapError(refused).flatMap(ask))
      reloading <- formatting.tool[NoArgs](
        Names.reload,
        "Reload the build the running server holds.",
      )(_ => ask(Call.Reload))
      rebooting <- reloading.tool[NoArgs](
        Names.reboot,
        "Replace a running server, or start one, and return when a server is listening.",
      )(_ => Successor.here.flatMap(onto => ask(Call.Reboot(onto))))
      cachePath <- rebooting.tool[NoArgs](
        Names.cachePath,
        "Print the content store path.",
      )(_ => ask(Call.CachePath))
      cacheList <- cachePath.tool[NoArgs](
        Names.cacheList,
        "List content store digests and sizes.",
      )(_ => ask(Call.CacheList))
      cacheClean <- cacheList.tool[CacheCleanArgs](
        Names.cacheClean,
        "Remove old content store entries. mode is remove or dry-run. An absent mode removes them. dry-run lists them and deletes nothing.",
      )(args => ZIO.fromEither(cacheCall(args.mode)).mapError(refused).flatMap(ask))
      cacheForget <- cacheClean.tool[CacheForgetArgs](
        Names.cacheForget,
        "Forget a remote namespace, or one module's local compile hit. A namespace is main, pr/<n>, or dev/<name>. Anything else is a module.",
      )(args => ZIO.fromEither(cacheForgetCall(args.target)).mapError(refused).flatMap(ask))
      cacheVerify <- cacheForget.tool[CacheVerifyArgs](
        Names.cacheVerify,
        "Recompile stored compile hits in a scratch checkout and name a module whose classes differ. An empty list verifies every marked module.",
      )(args => ZIO.fromEither(cacheVerifyCall(args.projects)).mapError(refused).flatMap(ask))
      ready <- cacheVerify.tool[WatchArgs](
        Names.watch,
        "Run a command again after a source save. args are the histos command, without the tilde.",
      )(args => Cli.watchCall(args.args, repo, apiJar).mapError(Session.CallProblem.Fault(_)).flatMap(ask))
      previewed <- ready.tool[PreviewArgs](
        Names.preview,
        "Say what compile, test, local, snapshots, clean, or run will do. id is required; run uses it and the others ignore it. An empty project list is the command's default. Writes nothing. Compile and test name stale sources from the last analysis.",
      )(args =>
        ZIO
          .fromEither(previewCall(args))
          .mapError(reason => Session.CallProblem.Fault(BuildFault.Rejected(reason)))
          .flatMap(ask)
      )
      observed <- previewed.tool[NoArgs](
        Names.observe,
        "Read the stored compile tally. Does not compile.",
      )(_ => Stored.read(repo))
      diagnosed <- observed.tool[DoctorArgs](
        Names.doctor,
        "Run doctor. projects are the modules this run launches. An empty list launches nothing, so node.floor, node.shadowed, and clang.source do not probe. The text lists errors and warnings. verbose lists every check. area limits the checks. json prints json. preflight runs the offline checks and does not call GitHub. fix applies local edits and runs histos commands, whose output is part of the text, and never changes GitHub settings.",
      )(args =>
        appearanceOf(args.color, args.truecolor).flatMap { view =>
          ZIO.serviceWithZIO[Loader](_.load(repo, apiJar)).either.flatMap {
            case Left(error)   => ZIO.succeed(DoctorText(error.message))
            case Right(loaded) =>
              launched(loaded.graph, args.projects) match
                case Left(error)    => ZIO.succeed(notLaunched(error))
                case Right(modules) =>
                  DoctorSurface
                    .run(
                      repo,
                      loaded.graph,
                      DoctorSurface.Request(
                        args.area,
                        args.json,
                        args.preflight,
                        args.fix,
                        args.yes,
                        modules,
                        args.verbose,
                      ),
                      machine,
                      view,
                    )
                    .provideSomeLayer[DoctorSurface.Reads](
                      ZLayer.succeed(histos.cli.Stdio.redirected) ++ histos.supply.CommandRunner.captured
                    )
                    .map((_, text) => DoctorText(text))
          }
        }
      )
    yield diagnosed

  /** Absent color stays plain. A set color follows `COLORTERM` unless `truecolor` says otherwise. */
  private def depthOf(color: Boolean, truecolor: Option[Boolean]): UIO[Option[Depth]] =
    if !color then ZIO.succeed(None)
    else
      val choice = truecolor match
        case None        => TruecolorChoice.Default
        case Some(true)  => TruecolorChoice.On
        case Some(false) => TruecolorChoice.Off
      zio.System.env("COLORTERM").orElseSucceed(None).map(term => Some(Paint.depth(choice, term)))

  private def appearanceOf(color: Boolean, truecolor: Option[Boolean]): UIO[Appearance] =
    depthOf(color, truecolor).map:
      case Some(depth) => Appearance(Ink.Colour, depth)
      case None        => Appearance.plain

  /** The modules a doctor run launches. The first bad name or selection is the refusal. */
  private[mcp] def launched(
      graph: histos.ProjectGraph,
      raw: List[String],
  ): Either[ProjectNameError | SelectError, List[histos.Module]] =
    val names = raw.foldLeft[Either[ProjectNameError, List[ProjectName]]](Right(Nil)): (acc, token) =>
      acc.flatMap(parsed => ProjectName.from(token).map(parsed :+ _))
    names.flatMap(DoctorLaunch.modules(graph, _))

  /** A refused launch is the doctor tool's text. A selection fault reads as the command line's doctor prints it. */
  private[mcp] def notLaunched(error: ProjectNameError | SelectError): DoctorText =
    error match
      case found: ProjectNameError => DoctorText(found.message)
      case found: SelectError      => DoctorText(FaultText.show(found.diagnose))

  def compileCall(
      projects: List[String],
      platforms: List[String] = Nil,
      versions: List[String] = Nil,
  ): Either[Rejection, Call] =
    for
      found    <- names(projects)
      selected <- manyPlatforms(platforms)
      asked    <- manyScalas(versions)
    yield Call.Compile(found, selected, asked)

  def testCall(
      projects: List[String],
      patterns: List[String],
      mode: TestMode,
      tests: List[String] = Nil,
      tags: List[String] = Nil,
      ignoreTags: List[String] = Nil,
      platforms: List[String] = Nil,
      versions: List[String] = Nil,
  ): Either[Rejection, Call] =
    for
      found    <- names(projects)
      globs    <- suitePatterns(patterns)
      terms    <- terms(tests)
      kept     <- tagNames(tags)
      ignored  <- tagNames(ignoreTags)
      selected <- manyPlatforms(platforms)
      asked    <- manyScalas(versions)
    yield Call.Test(
      histos.TestSelection.named(found, selected, asked),
      globs,
      mode,
      TestFilter(terms, kept, ignored),
    )

  /** Projects and files are two scopes, so asking for both is refused. No scope is the build. */
  def fmtCall(args: FmtArgs): Either[Rejection, Call] =
    val mode    = Right(args.mode.getOrElse(FormatMode.Write))
    val targets = (args.projects, args.files) match
      case (Nil, Nil) => Right(FormatTargets.Build)
      case (raw, Nil) =>
        names(raw).map(found =>
          NonEmptyChunk.fromIterableOption(found).fold(FormatTargets.Build)(FormatTargets.Projects(_))
        )
      case (Nil, raw)       => paths(raw).map(FormatTargets.Files(_))
      case (_ :: _, _ :: _) => Left(Rejection.FormatTargets)
    for
      chosen <- mode
      scope  <- targets
    yield Call.Format(scope, chosen)

  private def paths(raw: List[String]): Either[Rejection, NonEmptyChunk[Path]] =
    raw
      .foldRight[Either[Rejection, List[Path]]](Right(Nil)): (token, rest) =>
        rest.flatMap: found =>
          scala.util.Try(Path.of(token)).toOption.filterNot(_.isAbsolute) match
            case Some(path) => Right(path :: found)
            case None       => Left(Rejection.FormatPath(token))
      .flatMap(found => NonEmptyChunk.fromIterableOption(found).toRight(Rejection.FormatTargets))

  def publishCall(place: PublishPlace, tokens: List[String]): Either[Rejection, Call] =
    place match
      case PublishPlace.Local =>
        publishTokens(tokens).map(found => Call.PublishLocal(found, histos.publish.ProofScope.All))
      case PublishPlace.Snapshots =>
        Right(Call.PublishSnapshots(tokens))

  def cleanCall(projects: List[String], extent: CleanExtent): Either[Rejection, Call] =
    if extent == CleanExtent.Full && projects.nonEmpty then Left(Rejection.PreviewProjects)
    else names(projects).map(found => Call.Clean(found, extent))

  def cacheCall(mode: Option[String]): Either[Rejection, Call] =
    CacheCleanMode
      .from(mode.getOrElse(CacheCleanMode.Remove.token))
      .left
      .map(Rejection.CacheClean(_))
      .map(Call.CacheClean(_))

  def cacheForgetCall(target: String): Either[Rejection, Call] =
    ForgetTarget.from(target).left.map(forgotten).map(Call.CacheForget(_))

  def cacheVerifyCall(projects: List[String]): Either[Rejection, Call] =
    names(projects).map(Call.CacheVerify(_))

  private def forgotten(fault: ForgetFault): Rejection =
    fault match
      case ForgetFault.Namespace(error) => Rejection.Namespace(error)
      case ForgetFault.Project(error)   => Rejection.ProjectName(error)

  def cancelCall(id: String): Either[Rejection, Call] =
    RunId.from(id).left.map(Rejection.RunId(_)).map(Call.Cancel(_))

  def previewCall(args: PreviewArgs): Either[Rejection, Call] =
    args.operation match
      case PreviewOp.Compile =>
        for
          found    <- names(args.projects)
          selected <- manyPlatforms(args.platform)
          asked    <- manyScalas(args.scala)
        yield Call.PreviewCompile(found, selected, asked)
      case PreviewOp.Test =>
        for
          found    <- names(args.projects)
          globs    <- suitePatterns(args.patterns)
          selected <- manyPlatforms(args.platform)
          asked    <- manyScalas(args.scala)
        yield Call.PreviewTest(histos.TestSelection.named(found, selected, asked), globs, args.mode, TestFilter.empty)
      case PreviewOp.Local =>
        publishTokens(args.projects).map(Call.PreviewPublishLocal(_))
      case PreviewOp.Snapshots =>
        Right(Call.PreviewPublishSnapshots(args.projects))
      case PreviewOp.Clean =>
        if args.extent == CleanExtent.Full && args.projects.nonEmpty then Left(Rejection.PreviewProjects)
        else names(args.projects).map(found => Call.PreviewClean(found, args.extent))
      case PreviewOp.Run =>
        if args.id.isEmpty then Left(Rejection.MissingRunName)
        else
          val task = args.id.contains("/") || TaskId.from(args.id).toOption.exists(_.builtIn)
          if task && args.platform.nonEmpty then Left(Rejection.TaskPlatform(args.id))
          else if task && args.scala.nonEmpty then Left(Rejection.TaskScala(args.id))
          else if task then Right(Call.PreviewRun(args.id, None))
          else
            for
              selected <- onePlatform(args.platform)
              asked    <- oneScala(args.scala)
            yield Call.PreviewRun(args.id, selected, scala = asked)

  private def manyPlatforms(raw: List[String]): Either[Rejection, PlatformSelection] =
    if raw.isEmpty then Right(PlatformSelection.All)
    else PlatformSelection.from(raw).left.map(Rejection.Selection(_))

  private def manyScalas(raw: List[String]): Either[Rejection, ScalaSelection] =
    if raw.isEmpty then Right(ScalaSelection.All)
    else ScalaSelection.from(raw).left.map(Rejection.Selection(_))

  private def onePlatform(raw: List[String]): Either[Rejection, Option[PlatformSelection]] =
    if raw.isEmpty then Right(None)
    else
      PlatformSelection.from(raw).left.map(Rejection.Selection(_)).flatMap {
        case PlatformSelection.Only(values) if values.all.distinct.size != 1 =>
          Left(Rejection.Selection(SelectionError.RunMany(values)))
        case other => Right(Some(other))
      }

  private def oneScala(raw: List[String]): Either[Rejection, Option[ScalaSelection]] =
    if raw.isEmpty then Right(None)
    else
      ScalaSelection.from(raw).left.map(Rejection.Selection(_)).flatMap {
        case ScalaSelection.Only(first, rest) if rest.nonEmpty =>
          Left(Rejection.Selection(SelectionError.RunScalaMany(first :: rest)))
        case other => Right(Some(other))
      }

  /** A word the grammar removed. The tool list does not register it, so a caller asks here. */
  def removed(name: String): Option[Rejection] =
    name match
      case "testOnly"                    => Some(Rejection.Retired("testOnly", "test --only"))
      case "testQuick"                   => Some(Rejection.Retired("testQuick", "test --quick"))
      case "cleanFull"                   => Some(Rejection.Retired("cleanFull", "clean --full"))
      case "publishLocal"                => Some(Rejection.Retired("publishLocal", "publish local"))
      case "client"                      => Some(Rejection.Retired("client", "publish"))
      case "launcher" | "launcherUpdate" => Some(Rejection.Retired(name, "update"))
      case "toolchain"                   => Some(Rejection.Retired("toolchain", "doctor"))
      case _                             => None

  private def publishTokens(raw: List[String]): Either[Rejection, List[PublishToken]] =
    raw.foldLeft[Either[Rejection, List[PublishToken]]](Right(Nil)): (acc, token) =>
      for
        have <- acc
        one  <- PublishToken.from(token).left.map(Rejection.PublishToken(_))
      yield have :+ one

  def resolveCall(action: ResolveAction, selectors: List[String]): Either[Rejection, Call] =
    (action, selectors) match
      case (ResolveAction.Refresh(_), raw) =>
        names(raw).map(found => Call.Resolve(ResolveAction.Refresh(found)))
      case (_, _ :: _) =>
        Left(Rejection.ResolveAction(action.token))
      case (other, Nil) =>
        Right(Call.Resolve(other))

  private def names(raw: List[String]): Either[Rejection, List[ProjectName]] =
    raw.foldLeft[Either[Rejection, List[ProjectName]]](Right(Nil)): (acc, token) =>
      for
        have <- acc
        name <- ProjectName.from(token).left.map(Rejection.ProjectName(_))
      yield have :+ name

  private def suitePatterns(raw: List[String]): Either[Rejection, List[SuitePattern]] =
    raw.foldLeft[Either[Rejection, List[SuitePattern]]](Right(Nil)): (acc, token) =>
      for
        have    <- acc
        pattern <- SuitePattern.from(token).left.map(Rejection.SuitePattern(_))
      yield have :+ pattern

  private def terms(raw: List[String]): Either[Rejection, List[TestTerm]] =
    raw.foldLeft[Either[Rejection, List[TestTerm]]](Right(Nil)): (acc, token) =>
      for
        have <- acc
        term <- TestTerm.from(token).left.map(Rejection.TestTerm(_))
      yield have :+ term

  private def tagNames(raw: List[String]): Either[Rejection, List[TagName]] =
    raw.foldLeft[Either[Rejection, List[TagName]]](Right(Nil)): (acc, token) =>
      for
        have <- acc
        name <- TagName.from(token).left.map(Rejection.TagName(_))
      yield have :+ name

  private def overlay(named: List[NamedArg]): Map[String, String] =
    named.foldLeft(Map.empty[String, String])((acc, item) => acc.updated(item.name, item.value))

  /** A relative directory is resolved against the repo by [[histos.program.RunDirectory.accept]]. */
  private def programDirectory(raw: Option[String]): Either[BuildFault, Option[Path]] =
    raw match
      case None       => Right(None)
      case Some(text) =>
        scala.util.Try(Path.of(text)).toOption match
          case Some(path) => Right(Some(path))
          case None       => Left(BuildFault.Rejected(Rejection.Path(text)))

  private def arguments(
      texts: List[NamedArg],
      flags: List[String],
  ): Either[Rejection, PluginArgs] =
    def named(
        rest: List[NamedArg],
        acc: List[NamedText],
    ): Either[Rejection, List[NamedText]] =
      rest match
        case Nil                           => Right(acc.reverse)
        case NamedArg(name, value) :: tail =>
          ParamName.from(name) match
            case Left(error)  => Left(Rejection.Parameter(error))
            case Right(found) =>
              named(tail, NamedText(found, value) :: acc)
    def marked(
        rest: List[String],
        acc: Set[FlagName],
    ): Either[Rejection, Set[FlagName]] =
      rest match
        case Nil          => Right(acc)
        case name :: tail =>
          FlagName.from(name) match
            case Left(error)  => Left(Rejection.Flag(error))
            case Right(found) => marked(tail, acc + found)
    for
      parsedTexts <- named(texts, Nil)
      parsedFlags <- marked(flags, Set.empty)
    yield PluginArgs(parsedTexts, parsedFlags)

end HistosMcp
