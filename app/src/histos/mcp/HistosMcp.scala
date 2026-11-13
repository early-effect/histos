package histos.mcp

import heddle.LinePipe
import heddle.endpoint.Schema
import heddle.mcp.{Mcp, McpBuildError}
import heddle.mcp.protocol.ToolName
import histos.cache.{CacheCleanMode, ContentStore}
import histos.cli.Cli
import histos.client.{ClientJson, Session}
import histos.format.{FormatMode, FormatTargets}
import histos.report.{BuildFault, Rejection, Stored, WireSchema}
import WireSchema.given
import histos.jdk.Machine
import histos.load.Loader
import histos.plugin.{FlagName, NamedText, ParamName, PluginArgs}
import histos.publish.IvyRoot
import histos.resolve.Resolve
import histos.server.{Call, LauncherAction, ResolveAction, Successor}
import histos.supply.DoctorSurface
import histos.task.RunId
import histos.testrun.{SuitePattern, TestMode}
import histos.workflow.WorkflowAction
import histos.doctor.{DoctorLaunch, FaultText}
import histos.select.SelectError
import histos.{ProjectName, ProjectNameError}

import ClientJson.given

import java.nio.file.Path

import zio.*
import zio.json.JsonCodec

/** MCP tools. Each call reads `.histos/server.json` and sends [[histos.server.Call]] when it matches.
  */
object HistosMcp:
  final case class NoArgs() derives Schema, JsonCodec
  final case class NamedArg(name: String, value: String) derives Schema, JsonCodec
  final case class RunArgs(
      id: String,
      platform: Option[String] = None,
      texts: List[NamedArg] = Nil,
      flags: List[String] = Nil,
  ) derives Schema,
        JsonCodec
  final case class CancelArgs(id: String) derives Schema, JsonCodec
  final case class CompileArgs(projects: List[String] = Nil) derives Schema, JsonCodec
  final case class TestArgs(
      projects: List[String] = Nil,
      patterns: List[String] = Nil,
      mode: Option[String] = None,
  ) derives Schema,
        JsonCodec
  final case class PublishLocalArgs(projects: List[String] = Nil) derives Schema, JsonCodec
  final case class PublishSnapshotsArgs(projects: List[String] = Nil) derives Schema, JsonCodec
  final case class AssemblyArgs(destination: Option[String] = None) derives Schema, JsonCodec
  final case class CleanArgs(projects: List[String] = Nil) derives Schema, JsonCodec
  final case class ResolveArgs(action: String = "verify") derives Schema, JsonCodec
  final case class WorkflowArgs(action: String = "check") derives Schema, JsonCodec
  final case class FmtArgs(
      projects: List[String] = Nil,
      files: List[String] = Nil,
      mode: Option[String] = None,
  ) derives Schema,
        JsonCodec
  final case class LauncherArgs(action: String) derives Schema, JsonCodec
  final case class CacheCleanArgs(mode: Option[String] = None) derives Schema, JsonCodec
  final case class WatchArgs(args: List[String] = Nil) derives Schema, JsonCodec
  final case class PreviewArgs(
      operation: String,
      id: String,
      projects: List[String] = Nil,
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
    inline val graph            = "graph"
    inline val run              = "run"
    inline val cancel           = "cancel"
    inline val compile          = "compile"
    inline val test             = "test"
    inline val publishLocal     = "publishLocal"
    inline val publishSnapshots = "publishSnapshots"
    inline val toolchain        = "toolchain"
    inline val version          = "version"
    inline val assembly         = "assembly"
    inline val clean            = "clean"
    inline val cleanFull        = "cleanFull"
    inline val resolve          = "resolve"
    inline val workflow         = "workflow"
    inline val fmt              = "fmt"
    inline val reload           = "reload"
    inline val reboot           = "reboot"
    inline val cachePath        = "cachePath"
    inline val cacheList        = "cacheList"
    inline val cacheClean       = "cacheClean"
    inline val launcherUpdate   = "launcherUpdate"
    inline val watch            = "watch"
    inline val preview          = "preview"
    inline val observe          = "observe"
    inline val doctor           = "doctor"

    val all: Set[ToolName] =
      Set(
        ToolName(graph),
        ToolName(run),
        ToolName(cancel),
        ToolName(compile),
        ToolName(test),
        ToolName(publishLocal),
        ToolName(publishSnapshots),
        ToolName(toolchain),
        ToolName(version),
        ToolName(assembly),
        ToolName(clean),
        ToolName(cleanFull),
        ToolName(resolve),
        ToolName(workflow),
        ToolName(fmt),
        ToolName(reload),
        ToolName(reboot),
        ToolName(cachePath),
        ToolName(cacheList),
        ToolName(cacheClean),
        ToolName(launcherUpdate),
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
        "Run a main, or inspect, versions, or a plugin task. platform is jvm, js, or native. texts and flags are the plugin arguments.",
      )(args =>
        ZIO
          .fromEither(
            arguments(args.texts, args.flags).left
              .map(BuildFault.Rejected(_))
              .flatMap(parsed => Session.runCall(args.id, args.platform, parsed))
          )
          .mapError(Session.CallProblem.Fault(_))
          .flatMap(ask)
      )
      stopped <- running.tool[CancelArgs](Names.cancel, "Cancel a run")(args =>
        ZIO.fromEither(cancelCall(args.id)).mapError(refused).flatMap(ask)
      )
      compiling <- stopped.tool[CompileArgs](
        Names.compile,
        "Compile modules. An empty list compiles every module. An aggregate name compiles its modules, on every platform each module builds.",
      )(args => ZIO.fromEither(compileCall(args.projects)).mapError(refused).flatMap(ask))
      testing <- compiling.tool[TestArgs](
        Names.test,
        "Test modules. An empty list tests every module. An aggregate name tests its modules. patterns are suite name globs, the same as histos testOnly. mode is run or quick, and an absent mode is run. Each module runs once per Scala version and platform it builds.",
      )(args => ZIO.fromEither(testCall(args.projects, args.patterns, args.mode)).mapError(refused).flatMap(ask))
      publishing <- testing.tool[PublishLocalArgs](
        Names.publishLocal,
        "Install into Coursier's ivy2Local. A token is a ship, an aggregate, or a module. An empty list publishes every ship. Nothing is signed and nothing is uploaded.",
      )(args => ZIO.fromEither(publishLocalCall(args.projects)).mapError(refused).flatMap(ask))
      snapshots <- publishing.tool[PublishSnapshotsArgs](
        Names.publishSnapshots,
        "Publish through the Sonatype layer. A snapshot coordinate is a PUT. A release coordinate is signed and uploaded to the Central Portal. A token is a ship, an aggregate, or a module. An empty list publishes every ship.",
      )(args => ZIO.fromEither(publishSnapshotsCall(args.projects)).mapError(refused).flatMap(ask))
      toolchain <- snapshots.tool[NoArgs](
        Names.toolchain,
        "Show the JDK decision, and the Node or clang decision when the build needs them",
      )(_ => ask(Call.Toolchain))
      version  <- toolchain.tool[NoArgs](Names.version, "Show the histos version")(_ => ask(Call.Version))
      assembly <- version.tool[AssemblyArgs](
        Names.assembly,
        "Build the assembly jar. destination is optional.",
      )(args => ask(Call.Assembly(args.destination)))
      cleaning <- assembly.tool[CleanArgs](
        Names.clean,
        "Delete build products. An empty list wipes every product root and does not load the graph.",
      )(args => ZIO.fromEither(cleanCall(args.projects)).mapError(refused).flatMap(ask))
      cleaned <- cleaning.tool[NoArgs](
        Names.cleanFull,
        "Wipe build products, then forget this repo in the content store.",
      )(_ => ask(Call.CleanFull))
      resolving <- cleaned.tool[ResolveArgs](
        Names.resolve,
        "verify, update, or check dependencies. update stages plugin cells, loads that generation, then commits once.",
      )(args =>
        ZIO
          .fromEither(
            ResolveAction
              .from(args.action)
              .left
              .map(_ => Session.CallProblem.Fault(BuildFault.Rejected(Rejection.ResolveAction(args.action))))
          )
          .flatMap(action => ask(Call.Resolve(action)))
      )
      flowing <- resolving.tool[WorkflowArgs](
        Names.workflow,
        "check or update workflow files and launcher scripts. The launcher pin is not advanced.",
      )(args =>
        ZIO
          .fromEither(
            WorkflowAction
              .from(args.action)
              .left
              .map(_ => Session.CallProblem.Fault(BuildFault.Rejected(Rejection.WorkflowAction(args.action))))
          )
          .flatMap(action => ask(Call.Workflow(action)))
      )
      formatting <- flowing.tool[FmtArgs](
        Names.fmt,
        "Format with the locked scalafmt the build declares. An empty projects and files list is every module and histos.scala. projects names modules. files names repository-relative files. mode is write or check, and an absent mode is write. check writes nothing and fails when a file is not formatted.",
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
      launcher <- cacheClean.tool[LauncherArgs](
        Names.launcherUpdate,
        "update resolves the channel and writes the timestamp. latest writes the snapshot pointer and does not fetch. Does not load the graph.",
      )(args =>
        ZIO
          .fromEither(
            LauncherAction
              .from(args.action)
              .left
              .map(_ => Session.CallProblem.Fault(BuildFault.Rejected(Rejection.LauncherAction(args.action))))
          )
          .flatMap(action => ask(Call.Launcher(action)))
      )
      ready <- launcher.tool[WatchArgs](
        Names.watch,
        "Run a command again after a source save. args are the histos command, without the tilde.",
      )(args => Cli.watchCall(args.args, repo, apiJar).mapError(Session.CallProblem.Fault(_)).flatMap(ask))
      previewed <- ready.tool[PreviewArgs](
        Names.preview,
        "Say what compile, test, publishLocal, publishSnapshots, clean, cleanFull, or run will do. id is required; run uses it and the others ignore it. An empty project list is the command's default. Writes nothing. Compile and test name stale sources from the last analysis.",
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
                  )
                  // The standard streams are the protocol, so no person answers and no fix prints on them.
                  .provideSomeLayer[DoctorSurface.Reads](
                    ZLayer.succeed(histos.cli.Stdio.redirected) ++ histos.supply.CommandRunner.captured
                  )
                  .map((_, text) => DoctorText(text))
        }
      )
    yield diagnosed

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

  def compileCall(projects: List[String]): Either[Rejection, Call] =
    names(projects).map(Call.Compile(_))

  def testCall(
      projects: List[String],
      patterns: List[String],
      mode: Option[String],
  ): Either[Rejection, Call] =
    for
      found  <- names(projects)
      globs  <- suitePatterns(patterns)
      chosen <- TestMode.from(mode.getOrElse(TestMode.Run.token)).left.map(Rejection.TestMode(_))
    yield Call.Test(found, globs, chosen)

  /** Projects and files are two scopes, so asking for both is refused. No scope is the build. */
  def fmtCall(args: FmtArgs): Either[Rejection, Call] =
    val mode = args.mode match
      case None | Some("write") => Right(FormatMode.Write)
      case Some("check")        => Right(FormatMode.Check)
      case Some(other)          => Left(Rejection.FormatMode(other))
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

  def publishLocalCall(projects: List[String]): Either[Rejection, Call] =
    Right(Call.PublishLocal(projects))

  def publishSnapshotsCall(projects: List[String]): Either[Rejection, Call] =
    Right(Call.PublishSnapshots(projects))

  def cleanCall(projects: List[String]): Either[Rejection, Call] =
    names(projects).map(Call.Clean(_))

  def cacheCall(mode: Option[String]): Either[Rejection, Call] =
    CacheCleanMode
      .from(mode.getOrElse(CacheCleanMode.Remove.token))
      .left
      .map(Rejection.CacheClean(_))
      .map(Call.CacheClean(_))

  def cancelCall(id: String): Either[Rejection, Call] =
    RunId.from(id).left.map(Rejection.RunId(_)).map(Call.Cancel(_))

  def previewCall(args: PreviewArgs): Either[Rejection, Call] =
    args.operation match
      case "compile"          => names(args.projects).map(Call.PreviewCompile(_))
      case "test"             => names(args.projects).map(Call.PreviewTest(_))
      case "publishLocal"     => Right(Call.PreviewPublishLocal(args.projects))
      case "publishSnapshots" => Right(Call.PreviewPublishSnapshots(args.projects))
      case "clean"            => names(args.projects).map(Call.PreviewClean(_))
      case "cleanFull"        =>
        if args.projects.nonEmpty then Left(Rejection.PreviewProjects)
        else Right(Call.PreviewCleanFull)
      case "run" =>
        if args.id.isEmpty then Left(Rejection.MissingRunName)
        else Right(Call.PreviewRun(args.id, None))
      case other => Left(Rejection.PreviewOperation(other))

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
