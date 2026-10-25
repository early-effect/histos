package histos.mcp

import heddle.LinePipe
import heddle.mcp.protocol.{CallToolResult, ClientRequest, ContentBlock, Message, RequestId, ToolName}
import histos.workspace.{IoFailure, IoFault}
import histos.cli.{Cli, CliExit, CliParseError, Command}
import histos.client.Session
import histos.plugin.{FlagName, NamedText, ParamName, PluginArgs, PluginOutput, PluginTaskId}
import histos.server.Call
import histos.cache.ContentStore
import histos.jdk.{Machine, Os}
import histos.native.NativeOrigin
import histos.load.{ApiJar, LoadError, LoadedBuild, Loader, PluginLink, PluginRuntime}
import histos.report.Report
import HistosMcp.{CancelArgs, NoArgs, RunArgs, WatchArgs}
import histos.resolve.{
  Classpath,
  LockedArtifact,
  Repositories,
  Repository,
  Resolve,
  ResolveCell,
  ResolveError,
  ResolvedArtifact,
  RepositoryIo,
}
import histos.{Dep, Jvm, Projects, ScalaVersion, module}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object ProtocolSpec extends ZIOSpecDefault:
  private object IdleResolve extends Resolve:
    def resolveTool(
        request: histos.resolve.ToolRequest
    ): IO[ResolveError, Classpath]                              = ZIO.fail(ResolveError.NoTool(request.id))
    def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def resolvePlugin(
        dependency: Dep,
        repositories: Repositories,
    ): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository,
    ): IO[ResolveError, ResolvedArtifact] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))
  end IdleResolve

  private def loading(dir: Path) =
    ZLayer.make[Loader & histos.resolve.Resolve & histos.cache.ContentStore](
      Loader.live,
      ZLayer.succeed(IdleResolve),
      ZLayer.succeed(ContentStore.in(dir.resolve("cache"))),
    )
  private val source =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph = Projects(module("core", Jvm)(scala"3.9.0"))
      |""".stripMargin

  private val apiJar: ZIO[Scope, LoadError, Path] =
    ApiJar.locate

  private def repo: ZIO[Scope, LoadError, Path] =
    for
      dir <- ZIO
        .attempt(Files.createTempDirectory("histos-mcp"))
        .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
      _ <- ZIO.addFinalizer(delete(dir))
      _ <- ZIO
        .attempt(Files.writeString(dir.resolve("histos.scala"), source))
        .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
    yield dir

  private def delete(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths
          .sortBy(_.getNameCount)
          .reverse
          .foreach(path => Files.deleteIfExists(path))
    }.ignore

  private def machine(
      dir: Path,
      env: Map[String, String] = Map.empty,
  ): Machine =
    Machine(
      env,
      dir.toString,
      Os.Linux,
      histos.toolchain.Architecture.X86_64,
      dir.resolve("cache"),
      dir,
      None,
      NativeOrigin.read(env),
    )

  private def writeJdk(dir: Path): Unit =
    val jdk = dir.resolve("jdk")
    Files.createDirectories(jdk.resolve("bin"))
    Files.createDirectories(dir.resolve(".histos"))
    Files.writeString(jdk.resolve("bin").resolve("javac"), "")
    Files.writeString(jdk.resolve("release"), "JAVA_VERSION=\"21.0.2\"\n")
    Files.writeString(
      dir.resolve(".histos/toolchain.local"),
      jdk.toString + "\n",
    )

  private val protocol = suite("mcp")(
    test("a task call keeps plugin arguments"):
      val args = PluginArgs(
        List(NamedText(ParamName("out"), "docs")),
        Set(FlagName("all")),
      )
      val call = Call.Task("dev.example/echo", args)
      assertTrue(
        call.toJson.fromJson[Call] == Right(call),
        Session.runCall("dev.example/echo", None, args) == Right(call),
        Session.runCall("dev.example/echo", Some("jvm"), args) == Left(
          CliParseError.TaskPlatform("dev.example/echo").message
        ),
        Session.runCall("inspect", None, PluginArgs.empty) == Right(
          Call.Task("inspect", PluginArgs.empty)
        ),
      )
    ,
    test("mcp is a command, and acp is a reserved word with no command"):
      for
        mcp <- histos.cli.Cli.parse(List("mcp"))
        acp <- histos.cli.Cli.parse(List("acp")).flip
      yield assertTrue(mcp == Command.Mcp, acp == CliParseError.UnknownCommand)
    ,
    test("graph and run answer over an in-memory pipe"):
      ZIO.scoped:
        for
          jar              <- apiJar
          dir              <- repo
          (client, server) <- LinePipe.connected
          done             <- HistosMcp
            .serve(dir, jar, server, machine(dir))
            .provideSome[Scope](loading(dir))
            .forkScoped
          _       <- client.writeLine(discover(1))
          graphOn <- invoke(2, ToolName("graph"), NoArgs())
          _       <- client.writeLine(graphOn)
          runOn   <- invoke(3, ToolName("run"), RunArgs("inspect"))
          _       <- client.writeLine(runOn)
          stopOn  <- invoke(4, ToolName("cancel"), CancelArgs("missing"))
          _       <- client.writeLine(stopOn)
          _       <- client.close
          _       <- done.join
          heard   <- readUntil(client, Set(2, 4))
          graph   <- report(heard, 2)
          missing <- report(heard, 4)
        yield assertTrue(
          graph.exit == CliExit.Success.code,
          graph.out.exists(_.contains("core jvm 3.9.0")),
          missing.exit == CliExit.TaskFailed.code,
          missing.err.contains("no run 'missing'"),
        )
    ,
    test("toolchain returns the chosen jdk"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          _   <- ZIO
            .attempt(writeJdk(dir))
            .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          (client, server) <- LinePipe.connected
          done             <- HistosMcp
            .serve(
              dir,
              jar,
              server,
              machine(dir, Map("JAVA_HOME" -> dir.resolve("jdk").toString)),
            )
            .provideSome[Scope](loading(dir))
            .forkScoped
          _     <- client.writeLine(discover(1))
          call  <- invoke(2, ToolName("toolchain"), NoArgs())
          _     <- client.writeLine(call)
          _     <- client.close
          _     <- done.join
          heard <- readUntil(client, Set(2))
          found <- report(heard, 2)
        yield assertTrue(
          found.exit == CliExit.Success.code,
          found.out.exists(_.contains("21.0.2")),
          found.out.exists(_.contains(dir.resolve("jdk").toString)),
        )
    ,
    test("toolchain reports a missing JDK"):
      ZIO.scoped:
        for
          jar              <- apiJar
          dir              <- repo
          (client, server) <- LinePipe.connected
          done             <- HistosMcp
            .serve(dir, jar, server, machine(dir))
            .provideSome[Scope](loading(dir))
            .forkScoped
          _     <- client.writeLine(discover(1))
          call  <- invoke(2, ToolName("toolchain"), NoArgs())
          _     <- client.writeLine(call)
          _     <- client.close
          _     <- done.join
          heard <- readUntil(client, Set(2))
          found <- report(heard, 2)
        yield assertTrue(
          found.exit == CliExit.ToolchainFailed.code,
          found.err.exists(_.contains("21")),
        )
    ,
    test("a js graph without Node keeps the jdk decision"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          _   <- ZIO
            .attempt {
              writeJdk(dir)
              Files.writeString(
                dir.resolve("histos.scala"),
                """import histos.*
                     |object build extends HistosBuild:
                     |  def graph =
                     |    val core = module("core", Js)(scala"3.9.0")
                     |    Projects(core)
                     |""".stripMargin,
              )
            }
            .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          (client, server) <- LinePipe.connected
          done             <- HistosMcp
            .serve(
              dir,
              jar,
              server,
              machine(dir, Map("JAVA_HOME" -> dir.resolve("jdk").toString)),
            )
            .provideSome[Scope](loading(dir))
            .forkScoped
          _     <- client.writeLine(discover(1))
          call  <- invoke(2, ToolName("toolchain"), NoArgs())
          _     <- client.writeLine(call)
          _     <- client.close
          _     <- done.join
          heard <- readUntil(client, Set(2))
          found <- report(heard, 2)
        yield assertTrue(
          found.exit == CliExit.ToolchainFailed.code,
          found.out.exists(_.contains("21.0.2")),
          found.err.exists(_.contains("25")),
        )
    ,
    test("an unknown plugin task says it is not in the graph"):
      ZIO.scoped:
        for
          jar              <- apiJar
          dir              <- repo
          (client, server) <- LinePipe.connected
          done             <- HistosMcp
            .serve(dir, jar, server, machine(dir))
            .provideSome[Scope](loading(dir))
            .forkScoped
          _     <- client.writeLine(discover(1))
          call  <- invoke(2, ToolName("run"), RunArgs("dev.example/echo"))
          _     <- client.writeLine(call)
          _     <- client.close
          _     <- done.join
          heard <- readUntil(client, Set(2))
          found <- report(heard, 2)
        yield assertTrue(
          found.exit == CliExit.TaskFailed.code,
          found.err.contains("plugin task dev.example/echo is not in the graph"),
        )
    ,
    test("version, cache path, and a rejected watch answer on the pipe"):
      ZIO.scoped:
        for
          jar              <- apiJar
          dir              <- repo
          (client, server) <- LinePipe.connected
          done             <- HistosMcp
            .serve(dir, jar, server, machine(dir))
            .provideSome[Scope](loading(dir))
            .forkScoped
          _       <- client.writeLine(discover(1))
          version <- invoke(2, ToolName("version"), NoArgs())
          _       <- client.writeLine(version)
          cache   <- invoke(3, ToolName("cachePath"), NoArgs())
          _       <- client.writeLine(cache)
          empty   <- invoke(4, ToolName("watch"), WatchArgs(Nil))
          _       <- client.writeLine(empty)
          nested  <- invoke(5, ToolName("watch"), WatchArgs(List("mcp")))
          _       <- client.writeLine(nested)
          _       <- client.close
          _       <- done.join
          heard   <- readUntil(client, Set(2, 3, 4, 5))
          printed <- report(heard, 2)
          stored  <- report(heard, 3)
          needs   <- tool(heard, 4)
          usage   <- tool(heard, 5)
        yield assertTrue(
          printed.exit == CliExit.Success.code,
          printed.out == Chunk(Cli.VersionText),
          stored.exit == CliExit.Success.code,
          stored.out == Chunk(dir.resolve("cache").toString),
          needs.failed,
          texts(needs).exists(_.contains("watch needs a command")),
          usage.failed,
          texts(usage).exists(_.contains("usage: histos")),
        )
    ,
    test(
      "a run line is published before the result, and cancel of that id finishes while the call is held"
    ):
      ZIO.scoped:
        for
          jar              <- apiJar
          dir              <- repo
          gates            <- Queue.unbounded[Promise[Nothing, String]]
          (client, server) <- LinePipe.connected
          done             <- HistosMcp
            .serve(dir, jar, server, machine(dir))
            .provideSome[Scope](holding(dir, gates))
            .forkScoped
          _       <- client.writeLine(discover(1))
          holdOn  <- invoke(2, ToolName("run"), RunArgs(heldTask))
          _       <- client.writeLine(holdOn)
          opened  <- untilNote(client)(line => minted(line).isDefined)
          id      <- mintedId(opened)
          note    <- runNote(opened, id)
          stopOn  <- invoke(3, ToolName("cancel"), CancelArgs(id))
          _       <- client.writeLine(stopOn)
          heard   <- readUntil(client, Set(2, 3))
          held    <- report(heard, 2)
          stopped <- report(heard, 3)
          _       <- done.interrupt
        yield assertTrue(
          !hasResult(opened, 2),
          note.level == Notice.Level.Info,
          note.logger == Notice.Logger,
          note.data == s"run $id",
          id != heldTask,
          held.exit == CliExit.Interrupted.code,
          stopped.exit == CliExit.Success.code,
          stopped.out.contains("cancelled"),
        ),
  ) @@ TestAspect.timeout(15.seconds)

  private val heldTask  = "dev.example/hold"
  private val bound     = 40
  private val runPrefix = "run "

  private enum WireFault:
    case Closed
    case TooMany(ids: Set[Long])
    case NotJson(line: String)
    case NotMessage(detail: String)
    case Missing(id: Long)
    case BadResult(id: Long, detail: String)
    case BadReport(detail: String)
    case NotObject(detail: String)
    case BadNotice(detail: String)

  private def discover(id: Long): String =
    Message.stateless(RequestId.Num(id), ClientRequest.Discover).json.toJson

  private def invoke[A: JsonEncoder](id: Long, name: ToolName, args: A): IO[WireFault, String] =
    ZIO
      .fromEither(arguments(args))
      .map: obj =>
        Message.stateless(RequestId.Num(id), ClientRequest.CallTool(name, obj)).json.toJson

  /** `CallTool` arguments are a `Json.Obj`. The value itself stays a codec. */
  private def arguments[A: JsonEncoder](value: A): Either[WireFault, Json.Obj] =
    summon[JsonEncoder[A]].toJsonAST(value) match
      case Right(obj: Json.Obj) => Right(obj)
      case Right(other)         => Left(WireFault.NotObject(other.toJson))
      case Left(detail)         => Left(WireFault.NotObject(detail))

  private def pull(pipe: LinePipe): IO[WireFault, Message] =
    pipe.readLine.orDie.flatMap:
      case None       => ZIO.fail(WireFault.Closed)
      case Some(line) => ZIO.fromEither(decodeLine(line))

  private def decodeLine(line: String): Either[WireFault, Message] =
    line.fromJson[Json] match
      case Left(_)     => Left(WireFault.NotJson(line))
      case Right(json) =>
        Message.decode(json) match
          case Left(rejected) => Left(WireFault.NotMessage(rejected.error.text))
          case Right(message) => Right(message)

  private def readUntil(pipe: LinePipe, ids: Set[Long]): IO[WireFault, List[Message]] =
    def go(acc: List[Message], left: Int): IO[WireFault, List[Message]] =
      val seen = acc.reverse
      if ids.subsetOf(resultIds(seen)) then ZIO.succeed(seen)
      else if left <= 0 then ZIO.fail(WireFault.TooMany(ids))
      else
        pull(pipe).flatMap:
          case Message.Error(Some(RequestId.Num(n)), error) if ids.contains(n) =>
            ZIO.fail(WireFault.NotMessage(s"$n ${error.text}"))
          case message => go(message :: acc, left - 1)
    go(Nil, bound)

  private def untilNote(
      pipe: LinePipe
  )(pred: Notice.LogLine => Boolean): IO[WireFault, List[Message]] =
    def go(acc: List[Message], left: Int): IO[WireFault, List[Message]] =
      if left <= 0 then ZIO.fail(WireFault.TooMany(Set.empty))
      else
        pull(pipe).flatMap: message =>
          val seen = (message :: acc).reverse
          noticeLines(seen) match
            case Left(fault)                        => ZIO.fail(fault)
            case Right(lines) if lines.exists(pred) => ZIO.succeed(seen)
            case Right(_)                           => go(message :: acc, left - 1)
    go(Nil, bound)

  private def resultIds(messages: List[Message]): Set[Long] =
    messages.collect { case Message.Result(RequestId.Num(n), _) => n }.toSet

  private def hasResult(messages: List[Message], id: Long): Boolean =
    messages.exists:
      case Message.Result(RequestId.Num(n), _) => n == id
      case _                                   => false

  private def noticeLines(messages: List[Message]): Either[WireFault, List[Notice.LogLine]] =
    messages.foldLeft(Right(Nil): Either[WireFault, List[Notice.LogLine]]): (acc, message) =>
      (acc, message) match
        case (Left(fault), _)                                                                => Left(fault)
        case (Right(lines), Message.Notification(method, params)) if method == Notice.Method =>
          params.as[Notice.LogLine] match
            case Right(line)  => Right(lines :+ line)
            case Left(detail) => Left(WireFault.BadNotice(detail))
        case (Right(lines), _) => Right(lines)

  private def minted(line: Notice.LogLine): Option[String] =
    val id = line.data.drop(runPrefix.length)
    if line.level == Notice.Level.Info && line.logger == Notice.Logger && line.data.startsWith(runPrefix) && id.nonEmpty
    then Some(id)
    else None

  private def mintedId(messages: List[Message]): IO[WireFault, String] =
    ZIO
      .fromEither(noticeLines(messages))
      .flatMap: lines =>
        ZIO
          .fromOption(lines.flatMap(line => minted(line).toList).lastOption)
          .orElseFail(WireFault.BadNotice("no run id"))

  private def runNote(messages: List[Message], id: String): IO[WireFault, Notice.LogLine] =
    ZIO
      .fromEither(noticeLines(messages))
      .flatMap: lines =>
        ZIO
          .fromOption(lines.find(line => minted(line).contains(id)))
          .orElseFail(WireFault.BadNotice("no run id"))

  private def tool(messages: List[Message], id: Long): IO[WireFault, CallToolResult] =
    ZIO.fromEither(callResult(messages, id))

  private def report(messages: List[Message], id: Long): IO[WireFault, Report] =
    tool(messages, id).flatMap(result => ZIO.fromEither(reportOf(result)))

  private def callResult(messages: List[Message], id: Long): Either[WireFault, CallToolResult] =
    messages.collectFirst { case Message.Result(RequestId.Num(n), body) if n == id => body } match
      case None       => Left(WireFault.Missing(id))
      case Some(body) => body.as[CallToolResult].left.map(WireFault.BadResult(id, _))

  private def reportOf(result: CallToolResult): Either[WireFault, Report] =
    result.structuredContent match
      case Some(obj) => obj.as[Report].left.map(WireFault.BadReport(_))
      case None      =>
        result.content.collectFirst { case ContentBlock.Text(text, _) => text } match
          case None       => Left(WireFault.BadReport("tool result has no report"))
          case Some(text) => text.fromJson[Report].left.map(WireFault.BadReport(_))

  private def texts(result: CallToolResult): Chunk[String] =
    result.content.collect { case ContentBlock.Text(text, _) => text }

  private def holding(dir: Path, gates: Queue[Promise[Nothing, String]]) =
    ZLayer.succeed[Loader](new Loader:
      def scan(repo: Path) = ZIO.fail(LoadError.MissingFile(repo))
      def loadLocked(repo: Path, apiJar: Path, lock: histos.resolve.DependencyLock) =
        load(repo, apiJar)
      def load(repo: Path, apiJar: Path) =
        ZIO
          .fromEither(Projects(module("core", Jvm)(ScalaVersion("3.9.0"))))
          .mapBoth(LoadError.BrokenGraph(_), graph => LoadedBuild(graph, held(gates)))
      def buildClasspath(apiJar: Path)          = ZIO.succeed(Nil)
      def reportBuild(repo: Path, apiJar: Path) = ZIO.fail(LoadError.MissingFile(apiJar))) ++
      ZLayer.succeed[Resolve](IdleResolve) ++
      ZLayer.succeed(ContentStore.in(dir.resolve("cache")))

  private def held(gates: Queue[Promise[Nothing, String]]): PluginRuntime =
    new PluginRuntime:
      def links: List[PluginLink]                 = Nil
      def bind                                    = ZIO.succeed(Map.empty)
      def run(id: PluginTaskId, args: PluginArgs) =
        if id.render != heldTask || args != PluginArgs.empty then ZIO.succeed(PluginOutput(id.render, Nil))
        else
          Promise.make[Nothing, String].flatMap { gate =>
            gates.offer(gate) *> gate.await.map(text => PluginOutput(text, Nil))
          }

  def spec = protocol @@ TestAspect.withLiveSystem
end ProtocolSpec
