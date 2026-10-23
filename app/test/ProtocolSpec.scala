package histos.mcp

import heddle.LinePipe
import heddle.mcp.protocol.{ProtocolVersion, RequestMeta}
import histos.cli.{CliParseError, Command}
import histos.cache.ContentStore
import histos.jdk.{Machine, Os, Settled}
import histos.native.NativeOrigin
import histos.load.{ApiJar, LoadError, LoadIo, Loader}
import histos.resolve.{
  Classpath,
  LockedArtifact,
  Repositories,
  Repository,
  Resolve,
  ResolveCell,
  ResolveError,
  ResolvedArtifact,
  RepositoryIo
}
import histos.Dep

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object ProtocolSpec extends ZIOSpecDefault:
  private object IdleResolve extends Resolve:
    def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def resolvePlugin(
        dependency: Dep,
        repositories: Repositories
    ): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository
    ): IO[ResolveError, ResolvedArtifact] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))

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

  private def obj(fields: (String, Json)*): Json.Obj = Json.Obj(fields*)

  private def req(method: String, params: Json.Obj, id: Int): Json.Obj =
    val meta = obj(
      RequestMeta.VersionKey -> Json.Str(ProtocolVersion.Current.value),
      RequestMeta.ClientCapsKey -> obj()
    )
    val withMeta = obj((params.fields.toList :+ ("_meta" -> meta))*)
    obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id" -> Json.Num(id),
      "method" -> Json.Str(method),
      "params" -> withMeta
    )

  private val apiJar: ZIO[Scope, LoadError, Path] =
    ApiJar.locate

  private def repo: ZIO[Scope, LoadError, Path] =
    for
      dir <- ZIO
        .attempt(Files.createTempDirectory("histos-mcp"))
        .mapError(t => LoadError.Io(LoadIo.Temp))
      _ <- ZIO.addFinalizer(delete(dir))
      _ <- ZIO
        .attempt(Files.writeString(dir.resolve("histos.scala"), source))
        .mapError(t => LoadError.Io(LoadIo.Temp))
    yield dir

  private def delete(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
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
      env: Map[String, String] = Map.empty
  ): Machine =
    Machine(
      env,
      dir.toString,
      Os.Linux,
      dir.resolve("cache"),
      dir,
      None,
      Settled.No,
      NativeOrigin.read(env),
    )

  private def writeJdk(dir: Path): Unit =
    val jdk = dir.resolve("jdk")
    Files.createDirectories(jdk.resolve("bin"))
    Files.createDirectories(dir.resolve(".histos"))
    Files.writeString(jdk.resolve("bin").resolve("javac"), "")
    Files.writeString(jdk.resolve("release"), "JAVA_VERSION=\"21.0.2\"\n")
    Files.writeString(
      dir.resolve(".histos").resolve("jdk.lock"),
      """major 21
        |version 21.0.2
        |url https://example.test/jdk.zip
        |sha256 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
        |""".stripMargin
    )

  private def readLines(pipe: LinePipe, n: Int): UIO[List[String]] =
    if n <= 0 then ZIO.succeed(Nil)
    else
      pipe.readLine.orDie.flatMap:
        case None       => ZIO.succeed(Nil)
        case Some(line) => readLines(pipe, n - 1).map(line :: _)

  private val protocol = suite("mcp")(
    test("mcp is a command, and acp is a reserved word with no command"):
      for
        mcp <- histos.cli.Cli.parse(List("mcp"))
        acp <- histos.cli.Cli.parse(List("acp")).flip
      yield assertTrue(mcp == Command.Mcp, acp == CliParseError.UnknownCommand)
    ,
    test("graph and run answer over an in-memory pipe"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          (client, server) <- LinePipe.connected
          done <- HistosMcp
            .serve(dir, jar, server, machine(dir))
            .provideSome[Scope](loading(dir))
            .forkScoped
          _ <- client.writeLine(req("server/discover", obj(), 1).toJson)
          _ <- client.writeLine(
            req(
              "tools/call",
              obj("name" -> Json.Str("graph"), "arguments" -> obj()),
              2
            ).toJson
          )
          _ <- client.writeLine(
            req(
              "tools/call",
              obj(
                "name" -> Json.Str("run"),
                "arguments" -> obj("id" -> Json.Str("inspect"))
              ),
              3
            ).toJson
          )
          _ <- client.writeLine(
            req(
              "tools/call",
              obj(
                "name" -> Json.Str("cancel"),
                "arguments" -> obj("id" -> Json.Str("missing"))
              ),
              4
            ).toJson
          )
          _ <- client.close
          _ <- done.join
          lines <- readLines(client, 4)
        yield
          val text = lines.mkString("\n")
          assertTrue(text.contains("core jvm 3.9.0"), text.contains("no run"))
    ,
    test("toolchain returns the chosen jdk"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          _ <- ZIO
            .attempt(writeJdk(dir))
            .mapError(t => LoadError.Io(LoadIo.Temp))
          (client, server) <- LinePipe.connected
          done <- HistosMcp
            .serve(
              dir,
              jar,
              server,
              machine(dir, Map("JAVA_HOME" -> dir.resolve("jdk").toString))
            )
            .provideSome[Scope](loading(dir))
            .forkScoped
          _ <- client.writeLine(req("server/discover", obj(), 1).toJson)
          _ <- client.writeLine(
            req(
              "tools/call",
              obj("name" -> Json.Str("toolchain"), "arguments" -> obj()),
              2
            ).toJson
          )
          _ <- client.close
          _ <- done.join
          lines <- readLines(client, 2)
        yield
          val text = lines.mkString("\n")
          assertTrue(
            text.contains("JavaHome"),
            text.contains("21.0.2"),
            !text.contains(""""isError":true""")
          )
    ,
    test("toolchain reports a missing jdk lock"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          (client, server) <- LinePipe.connected
          done <- HistosMcp
            .serve(dir, jar, server, machine(dir))
            .provideSome[Scope](loading(dir))
            .forkScoped
          _ <- client.writeLine(req("server/discover", obj(), 1).toJson)
          _ <- client.writeLine(
            req(
              "tools/call",
              obj("name" -> Json.Str("toolchain"), "arguments" -> obj()),
              2
            ).toJson
          )
          _ <- client.close
          _ <- done.join
          lines <- readLines(client, 2)
        yield
          val text = lines.mkString("\n")
          assertTrue(
            text.contains(""""isError":true"""),
            text.contains("jdk.lock")
          )
    ,
    test("a js graph without a node lock keeps the jdk decision"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          _ <- ZIO
            .attempt {
              writeJdk(dir)
              Files.writeString(
                dir.resolve("histos.scala"),
                """import histos.*
                     |object build extends HistosBuild:
                     |  def graph =
                     |    val core = module("core", Js)(scala"3.9.0")
                     |    Projects(core)
                     |""".stripMargin
              )
            }
            .mapError(t => LoadError.Io(LoadIo.Temp))
          (client, server) <- LinePipe.connected
          done <- HistosMcp
            .serve(
              dir,
              jar,
              server,
              machine(dir, Map("JAVA_HOME" -> dir.resolve("jdk").toString))
            )
            .provideSome[Scope](loading(dir))
            .forkScoped
          _ <- client.writeLine(req("server/discover", obj(), 1).toJson)
          _ <- client.writeLine(
            req(
              "tools/call",
              obj("name" -> Json.Str("toolchain"), "arguments" -> obj()),
              2
            ).toJson
          )
          _ <- client.close
          _ <- done.join
          lines <- readLines(client, 2)
        yield
          val text = lines.mkString("\n")
          assertTrue(
            text.contains(""""isError":true"""),
            text.contains("node.lock"),
            text.contains("JavaHome")
          )
    ,
    test("an unknown plugin task says it is not in the graph"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          (client, server) <- LinePipe.connected
          done <- HistosMcp
            .serve(dir, jar, server, machine(dir))
            .provideSome[Scope](loading(dir))
            .forkScoped
          _ <- client.writeLine(req("server/discover", obj(), 1).toJson)
          _ <- client.writeLine(
            req(
              "tools/call",
              obj(
                "name" -> Json.Str("run"),
                "arguments" -> obj("id" -> Json.Str("dev.example/echo"))
              ),
              2
            ).toJson
          )
          _ <- client.close
          _ <- done.join
          lines <- readLines(client, 2)
        yield assertTrue(
          lines
            .mkString("\n")
            .contains("plugin task dev.example/echo is not in the graph")
        )
  ) @@ TestAspect.timeout(15.seconds)

  def spec = protocol @@ TestAspect.withLiveSystem
