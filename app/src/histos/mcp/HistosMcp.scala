package histos.mcp

import heddle.LinePipe
import heddle.endpoint.Schema
import heddle.mcp.{Mcp, McpBuildError}
import histos.cache.ContentStore
import histos.client.Session
import histos.jdk.Machine
import histos.load.Loader
import histos.resolve.Resolve
import histos.server.Call

import java.nio.file.Path

import zio.*
import zio.json.JsonCodec

/** MCP tools. Each call reads `.histos/server.json` and sends [[Call]] when it
  * matches.
  */
object HistosMcp:
  final case class NoArgs() derives Schema, JsonCodec
  final case class RunArgs(id: String, platform: Option[String] = None)
      derives Schema,
        JsonCodec
  final case class CancelArgs(id: String) derives Schema, JsonCodec
  final case class CompileArgs(projects: List[String] = Nil)
      derives Schema,
        JsonCodec
  final case class TestArgs(
      projects: List[String] = Nil,
      patterns: List[String] = Nil,
      quick: Boolean = false
  ) derives Schema,
        JsonCodec
  final case class PublishArgs(repository: String, projects: List[String] = Nil)
      derives Schema,
        JsonCodec
  final case class PreviewArgs(operation: String, projects: List[String] = Nil)
      derives Schema,
        JsonCodec

  enum ServeError:
    case Build(errors: NonEmptyChunk[McpBuildError])
    case Stdio

    def message: String = this match
      case Build(errors) => errors.map(_.message).mkString("\n")
      case Stdio         => "the mcp pipe failed"

  def serve(
      repo: Path,
      apiJar: Path,
      pipe: LinePipe,
      machine: Machine
  ): ZIO[Loader & Scope & Resolve & ContentStore, ServeError, Unit] =
    for
      desk <- Session.desk
      _ <- ZIO.addFinalizer(
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
      desk: Session.Desk
  ): Either[
    NonEmptyChunk[McpBuildError],
    Mcp[Loader & Resolve & ContentStore]
  ] =
    val ask = Session.perform(repo, apiJar, machine, desk, _)
    for
      base <- Mcp.from()
      listed <- base.tool[NoArgs]("graph", "Show the project graph")(_ =>
        ask(Call.Graph)
      )
      running <- listed.tool[RunArgs](
        "run",
        "Run a main, or inspect, versions, or a plugin task. platform is jvm, js, or native."
      )(args =>
        ZIO.fromEither(Session.runCall(args.id, args.platform)).flatMap(ask)
      )
      stopped <- running.tool[CancelArgs]("cancel", "Cancel a run")(args =>
        ask(Call.Cancel(args.id))
      )
      compiling <- stopped.tool[CompileArgs](
        "compile",
        "Compile modules. An empty list compiles every module. An aggregate name compiles its modules, on every platform each module builds."
      )(args => ask(Call.Compile(args.projects)))
      testing <- compiling.tool[TestArgs](
        "test",
        "Test modules. An empty list tests every module. An aggregate name tests its modules. patterns are suite name globs, the same as histos testOnly. Each module runs once per Scala version and platform it builds."
      )(args => ask(Call.Test(args.projects, args.patterns, args.quick)))
      publishing <- testing.tool[PublishArgs](
        "publish",
        "Publish modules into a local Maven directory. An empty list publishes the ship members."
      )(args => ask(Call.Publish(args.repository, args.projects)))
      ready <- publishing.tool[NoArgs](
        "toolchain",
        "Show the JDK decision, and the Node or clang decision when the build needs them"
      )(_ => ask(Call.Toolchain))
      previewed <- ready.tool[PreviewArgs](
        "preview",
        "Say what a compile will do. operation is compile. An empty project list is every module. Writes nothing."
      )(args =>
        args.operation match
          case "compile" => ask(Call.PreviewCompile(args.projects))
          case other     => ZIO.fail(s"preview does not run $other")
      )
    yield previewed

end HistosMcp
