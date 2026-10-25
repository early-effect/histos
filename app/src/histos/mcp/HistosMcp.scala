package histos.mcp

import heddle.LinePipe
import heddle.endpoint.{Schema, SchemaJson}
import heddle.mcp.{Mcp, McpBuildError, ToolError}
import heddle.mcp.protocol.Structured
import histos.cache.ContentStore
import histos.cli.Cli
import histos.client.Session
import histos.report.{Report, Stored}
import histos.jdk.Machine
import histos.load.Loader
import histos.plugin.{FlagName, NamedText, ParamName, PluginArgs}
import histos.resolve.Resolve
import histos.server.Call

import java.nio.file.Path

import zio.*
import zio.json.JsonCodec

/** MCP tools. Each call reads `.histos/server.json` and sends [[Call]] when it matches.
  */
object HistosMcp:
  /** A sentence stays text. Report JSON stays one structured body, not a quoted string.
    */
  given ToolError[String] with
    val shape: Structured =
      Structured.of(SchemaJson.render(summon[Schema[Report]].doc))
    def json(text: String): String = text

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
      quick: Boolean = false,
  ) derives Schema,
        JsonCodec
  final case class PublishArgs(repository: String, projects: List[String] = Nil) derives Schema, JsonCodec
  final case class AssemblyArgs(destination: Option[String] = None) derives Schema, JsonCodec
  final case class CleanArgs(projects: List[String] = Nil) derives Schema, JsonCodec
  final case class ResolveArgs(action: String = "verify") derives Schema, JsonCodec
  final case class WorkflowArgs(action: String = "check") derives Schema, JsonCodec
  final case class CacheCleanArgs(dryRun: Boolean = false) derives Schema, JsonCodec
  final case class WatchArgs(args: List[String] = Nil) derives Schema, JsonCodec
  final case class PreviewArgs(
      operation: String,
      id: String,
      projects: List[String] = Nil,
  ) derives Schema,
        JsonCodec

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
  ): ZIO[Loader & Scope & Resolve & ContentStore, ServeError, Unit] =
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

  def tools(
      repo: Path,
      apiJar: Path,
      machine: Machine,
      desk: Session.Desk,
  ): Either[
    NonEmptyChunk[McpBuildError],
    Mcp[Loader & Scope & Resolve & ContentStore],
  ] =
    val ask = (call: Call) => Session.deliver(Session.perform(repo, apiJar, machine, desk, call))
    for
      base    <- Mcp.from()
      listed  <- base.tool[NoArgs]("graph", "Show the project graph")(_ => ask(Call.Graph))
      running <- listed.tool[RunArgs](
        "run",
        "Run a main, or inspect, versions, or a plugin task. platform is jvm, js, or native. texts and flags are the plugin arguments.",
      )(args =>
        ZIO
          .fromEither(
            arguments(args.texts, args.flags).flatMap(parsed => Session.runCall(args.id, args.platform, parsed))
          )
          .flatMap(ask)
      )
      stopped   <- running.tool[CancelArgs]("cancel", "Cancel a run")(args => ask(Call.Cancel(args.id)))
      compiling <- stopped.tool[CompileArgs](
        "compile",
        "Compile modules. An empty list compiles every module. An aggregate name compiles its modules, on every platform each module builds.",
      )(args => ask(Call.Compile(args.projects)))
      testing <- compiling.tool[TestArgs](
        "test",
        "Test modules. An empty list tests every module. An aggregate name tests its modules. patterns are suite name globs, the same as histos testOnly. Each module runs once per Scala version and platform it builds.",
      )(args => ask(Call.Test(args.projects, args.patterns, args.quick)))
      publishing <- testing.tool[PublishArgs](
        "publish",
        "Publish modules into a local Maven directory. An empty list publishes the ship members.",
      )(args => ask(Call.Publish(args.repository, args.projects)))
      toolchain <- publishing.tool[NoArgs](
        "toolchain",
        "Show the JDK decision, and the Node or clang decision when the build needs them",
      )(_ => ask(Call.Toolchain))
      version  <- toolchain.tool[NoArgs]("version", "Show the histos version")(_ => ask(Call.Version))
      assembly <- version.tool[AssemblyArgs](
        "assembly",
        "Build the assembly jar. destination is optional.",
      )(args => ask(Call.Assembly(args.destination)))
      cleaning <- assembly.tool[CleanArgs](
        "clean",
        "Delete build products. An empty list wipes every product root and does not load the graph.",
      )(args => ask(Call.Clean(args.projects)))
      cleaned <- cleaning.tool[NoArgs](
        "cleanFull",
        "Wipe build products, then forget this repo in the content store.",
      )(_ => ask(Call.CleanFull))
      resolving <- cleaned.tool[ResolveArgs](
        "resolve",
        "verify, update, or check dependencies. update stages plugin cells, loads that generation, then commits once.",
      )(args => ask(Call.Resolve(args.action)))
      flowing <- resolving.tool[WorkflowArgs](
        "workflow",
        "check or update workflow files and launcher scripts. The launcher pin is not advanced.",
      )(args => ask(Call.Workflow(args.action)))
      reloading <- flowing.tool[NoArgs](
        "reload",
        "Reload the build the running server holds.",
      )(_ => ask(Call.Reload))
      rebooting <- reloading.tool[NoArgs](
        "reboot",
        "Replace a running server, or start one.",
      )(_ => ask(Call.Reboot))
      cachePath <- rebooting.tool[NoArgs](
        "cachePath",
        "Print the content store path.",
      )(_ => ask(Call.CachePath))
      cacheList <- cachePath.tool[NoArgs](
        "cacheList",
        "List content store digests and sizes.",
      )(_ => ask(Call.CacheList))
      cacheClean <- cacheList.tool[CacheCleanArgs](
        "cacheClean",
        "Remove old content store entries. dryRun lists them and deletes nothing.",
      )(args => ask(Call.CacheClean(args.dryRun)))
      launcher <- cacheClean.tool[NoArgs](
        "launcherUpdate",
        "Update the launcher pin. Does not load the graph.",
      )(_ => ask(Call.LauncherUpdate))
      ready <- launcher.tool[WatchArgs](
        "watch",
        "Run a command again after a source save. args are the histos command, without the tilde.",
      )(args => Cli.watchCall(args.args, repo, apiJar).flatMap(ask))
      previewed <- ready.tool[PreviewArgs](
        "preview",
        "Say what compile, test, publish, clean, cleanFull, or run will do. id is required; run uses it and the others ignore it. An empty project list is the command's default. Writes nothing. File-level staleness is unknown.",
      )(args => ZIO.fromEither(previewCall(args)).flatMap(ask))
      observed <- previewed.tool[NoArgs](
        "observe",
        "Read the stored compile tally. Does not compile.",
      )(_ => Stored.read(repo))
    yield observed

  def previewCall(args: PreviewArgs): Either[String, Call] =
    args.operation match
      case "compile"   => Right(Call.PreviewCompile(args.projects))
      case "test"      => Right(Call.PreviewTest(args.projects))
      case "publish"   => Right(Call.PreviewPublish(args.projects))
      case "clean"     => Right(Call.PreviewClean(args.projects))
      case "cleanFull" =>
        if args.projects.nonEmpty then Left("cleanFull does not take a project")
        else Right(Call.PreviewCleanFull)
      case "run" =>
        if args.id.isEmpty then Left("run needs one name")
        else Right(Call.PreviewRun(args.id, None))
      case other => Left(s"preview does not run $other")

  private def arguments(
      texts: List[NamedArg],
      flags: List[String],
  ): Either[String, PluginArgs] =
    def named(
        rest: List[NamedArg],
        acc: List[NamedText],
    ): Either[String, List[NamedText]] =
      rest match
        case Nil                           => Right(acc.reverse)
        case NamedArg(name, value) :: tail =>
          ParamName.from(name) match
            case Left(error)  => Left(error.message)
            case Right(found) =>
              named(tail, NamedText(found, value) :: acc)
    def marked(
        rest: List[String],
        acc: Set[FlagName],
    ): Either[String, Set[FlagName]] =
      rest match
        case Nil          => Right(acc)
        case name :: tail =>
          FlagName.from(name) match
            case Left(error)  => Left(error.message)
            case Right(found) => marked(tail, acc + found)
    for
      parsedTexts <- named(texts, Nil)
      parsedFlags <- marked(flags, Set.empty)
    yield PluginArgs(parsedTexts, parsedFlags)

end HistosMcp
