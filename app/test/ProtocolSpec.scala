package histos.mcp

import heddle.LinePipe
import heddle.mcp.protocol.{ProtocolVersion, RequestMeta}
import histos.acp.Acp
import histos.cli.Command
import histos.cache.ContentStore
import histos.jdk.{Machine, Os, Settled}
import histos.load.{LoadError, LoadIo, Loader}
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
    Loader.live ++ ZLayer.succeed(IdleResolve) ++ ZLayer.succeed(
      ContentStore.in(dir.resolve("cache"))
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

  private val apiJar: IO[LoadError, Path] =
    val root = sys.env.get("HISTOS_REPO").filter(_.nonEmpty).map(Path.of(_))
    val candidates = root.toList.map(_.resolve("dist/histos-api.jar")) ++ List(
      Path.of("dist/histos-api.jar"),
      Path.of("../dist/histos-api.jar")
    )
    ZIO
      .attempt(candidates.find(Files.isRegularFile(_)))
      .mapError(t => LoadError.Io(LoadIo.Temp))
      .flatMap:
        case Some(jar) => ZIO.succeed(jar)
        case None      =>
          ZIO.fail(LoadError.MissingFile(Path.of("dist/histos-api.jar")))

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
      Settled.No
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

  private val protocol = suite("mcp and acp")(
    test("mcp and acp are commands"):
      for
        mcp <- histos.cli.Cli.parse(List("mcp"))
        acp <- histos.cli.Cli.parse(List("acp"))
      yield assertTrue(mcp == Command.Mcp && acp == Command.Acp)
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
    ,
    test("acp lists the commands and returns toolchain text"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          checked <- acpTexts(dir, jar).provideSome[Scope](loading(dir))
        yield checked
  ) @@ TestAspect.timeout(60.seconds) @@ TestAspect.withLiveClock

  private def acpTexts(dir: Path, jar: Path) =
    for
      desk <- histos.client.Session.desk
      here = machine(dir)
      init <- Acp.answer(
        dir,
        jar,
        here,
        desk,
        """{"jsonrpc":"2.0","id":7,"method":"initialize","params":{}}"""
      )
      missing <- Acp.answer(
        dir,
        jar,
        here,
        desk,
        """{"jsonrpc":"2.0","id":8,"method":"toolchain","params":{}}"""
      )
      unknown <- Acp.answer(
        dir,
        jar,
        here,
        desk,
        """{"jsonrpc":"2.0","id":9,"method":"nope","params":{}}"""
      )
      bad <- Acp.answer(dir, jar, here, desk, "not json")
      _ <- ZIO.attempt(writeJdk(dir)).mapError(t => LoadError.Io(LoadIo.Temp))
      chosen <- Acp.answer(
        dir,
        jar,
        machine(dir, Map("JAVA_HOME" -> dir.resolve("jdk").toString)),
        desk,
        """{"jsonrpc":"2.0","id":10,"method":"toolchain","params":{}}"""
      )
    yield assertTrue(
      init.contains(""""protocolVersion":1"""),
      init.contains(""""id":7"""),
      init.contains("compile"),
      init.contains("toolchain"),
      resultValue(missing).exists(_.contains("jdk.lock")),
      !missing.contains(""""jdk""""),
      unknown.contains("acp does not handle nope"),
      bad.contains("parse error"),
      resultValue(chosen).exists(_.contains("JavaHome")),
      resultValue(chosen).exists(_.contains("21.0.2"))
    )

  private def resultValue(line: String): Option[String] =
    for
      json <- line.fromJson[Json].toOption
      result <- jsonField(json, "result")
      value <- jsonField(result, "value")
      text <- value match
        case Json.Str(found) => Some(found)
        case _               => None
    yield text

  private def jsonField(json: Json, name: String): Option[Json] =
    json match
      case Json.Obj(fields) =>
        fields.collectFirst { case (`name`, value) => value }
      case _ => None

  def spec = protocol
