package histos.mcp

import heddle.LinePipe
import heddle.mcp.protocol.{ProtocolVersion, RequestMeta}
import histos.acp.Acp
import histos.cli.Command
import histos.load.{LoadError, LoadIo, Loader}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.test.*

object ProtocolSpec extends ZIOSpecDefault:
  private val source =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph = Projects(module("core", Jvm)(scala"3.9.0"))
      |""".stripMargin

  private def obj(fields: (String, Json)*): Json.Obj = Json.Obj(fields*)

  private def req(method: String, params: Json.Obj, id: Int): Json.Obj =
    val meta = obj(
      RequestMeta.VersionKey   -> Json.Str(ProtocolVersion.Current.value),
      RequestMeta.ClientCapsKey -> obj(),
    )
    val withMeta = obj((params.fields.toList :+ ("_meta" -> meta))*)
    obj("jsonrpc" -> Json.Str("2.0"), "id" -> Json.Num(id), "method" -> Json.Str(method), "params" -> withMeta)

  private val apiJar: IO[LoadError, Path] =
    val root = sys.env.get("HISTOS_REPO").filter(_.nonEmpty).map(Path.of(_))
    val candidates = root.toList.map(_.resolve("dist/histos-api.jar")) ++ List(Path.of("dist/histos-api.jar"), Path.of("../dist/histos-api.jar"))
    ZIO.attempt(candidates.find(Files.isRegularFile(_))).mapError(t => LoadError.Io(LoadIo.Temp)).flatMap:
      case Some(jar) => ZIO.succeed(jar)
      case None      => ZIO.fail(LoadError.MissingFile(Path.of("dist/histos-api.jar")))

  private def repo: ZIO[Scope, LoadError, Path] =
    for
      dir <- ZIO.attempt(Files.createTempDirectory("histos-mcp")).mapError(t => LoadError.Io(LoadIo.Temp))
      _   <- ZIO.addFinalizer(delete(dir))
      _   <- ZIO.attempt(Files.writeString(dir.resolve("histos.scala"), source)).mapError(t => LoadError.Io(LoadIo.Temp))
    yield dir

  private def delete(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.ignore

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
      yield assertTrue(mcp == Command.Mcp && acp == Command.Acp),
    test("graph and run answer over an in-memory pipe"):
      ZIO.scoped:
        for
          jar               <- apiJar
          dir               <- repo
          (client, server)  <- LinePipe.connected
          done              <- HistosMcp.serve(dir, jar, server, Map.empty).provideSome[Scope](Loader.live).forkScoped
          _                 <- client.writeLine(req("server/discover", obj(), 1).toJson)
          _                 <- client.writeLine(req("tools/call", obj("name" -> Json.Str("graph"), "arguments" -> obj()), 2).toJson)
          _                 <- client.writeLine(req("tools/call", obj("name" -> Json.Str("run"), "arguments" -> obj("id" -> Json.Str("inspect"))), 3).toJson)
          _                 <- client.writeLine(req("tools/call", obj("name" -> Json.Str("cancel"), "arguments" -> obj("id" -> Json.Str("missing"))), 4).toJson)
          _                 <- client.close
          _                 <- done.join
          lines             <- readLines(client, 4)
        yield
          val text = lines.mkString("\n")
          assertTrue(text.contains("core jvm 3.9.0"), text.contains("no run"))
    ,
    test("acp answers initialize and nothing else"):
      val init = """{"jsonrpc":"2.0","id":7,"method":"initialize","params":{}}"""
      val other = """{"jsonrpc":"2.0","id":8,"method":"run","params":{}}"""
      val initAnswer = Acp.answer(init)
      val otherAnswer = Acp.answer(other)
      assertTrue(
        initAnswer.contains(""""protocolVersion":1""") && initAnswer.contains(""""id":7"""),
        otherAnswer.contains("acp does not handle run") && otherAnswer.contains(""""id":8"""),
        Acp.answer("not json").contains("parse error"),
      ),
  ) @@ TestAspect.timeout(60.seconds) @@ TestAspect.withLiveClock

  def spec = protocol
