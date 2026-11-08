package histos.publish

import com.sun.net.httpserver.{HttpExchange, HttpServer}

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentLinkedQueue

import scala.jdk.CollectionConverters.*

import zio.{Chunk, IO, Scope, ZIO, ZLayer}
import zio.test.*

/** A release host on loopback is the asset client: the tag names the id, the upload host receives the bytes, and a
  * later page is still that asset.
  */
object AssetHostLawSpec extends ZIOSpecDefault:
  private val bytes = Chunk[Byte](1, 2, 9)
  private val token = "Bearer secret"

  def spec = suite("asset host")(
    test("an upload posts the bytes to the upload host after the tag names the release"):
      val api = (hit: Hit) =>
        if hit.auth != token then Reply(401, "")
        else if hit.method == "GET" && hit.path.endsWith("/releases/tags/dist") then Reply(200, release("42"))
        else Reply(500, hit.path)
      val upload = (hit: Hit) =>
        if hit.auth != token then Reply(401, "")
        else if hit.method == "POST" && hit.path.endsWith("/releases/42/assets") then Reply(201, "")
        else Reply(500, hit.path)
      hosted(api, upload)(_.upload("dist", "app.jar", bytes)).map { (result, apiHits, uploadHits) =>
        val posted = uploadHits.filter(_.method == "POST")
        assertTrue(
          result == Right(()),
          apiHits.map(_.method) == List("GET"),
          posted.length == 1,
          posted.forall(hit =>
            hit.auth == token &&
              hit.agent == "histos" &&
              hit.accept == "application/vnd.github+json" &&
              hit.query.contains("name=app.jar") &&
              hit.body.sameElements(bytes.toArray)
          ),
          apiHits.forall(_.path.startsWith("/repos/own/demo/")),
        )
      }
    ,
    test("a missing tag, a refused credential, a foreign status, and a release without an id are those errors"):
      val api = (hit: Hit) =>
        if hit.path.endsWith("/tags/missing") then Reply(404, "")
        else if hit.path.endsWith("/tags/denied") then Reply(401, "")
        else if hit.path.endsWith("/tags/forbidden") then Reply(403, "")
        else if hit.path.endsWith("/tags/down") then Reply(500, "")
        else if hit.path.endsWith("/tags/blank") then Reply(200, """{"blurb":"see \"upload_url\" no"}""")
        else if hit.path.endsWith("/tags/bare") then Reply(200, """{"upload_url":"http://h/releases/42"}""")
        else if hit.path.endsWith("/tags/word") then Reply(200, """{"upload_url":12}""")
        else Reply(500, hit.path)
      hosted(api, _ => Reply(500, "upload")) { assets =>
        for
          missing   <- assets.upload("missing", "app.jar", bytes).either
          denied    <- assets.upload("denied", "app.jar", bytes).either
          forbidden <- assets.delete("forbidden", "app.jar").either
          down      <- assets.upload("down", "app.jar", bytes).either
          blank     <- assets.upload("blank", "app.jar", bytes).either
          bare      <- assets.upload("bare", "app.jar", bytes).either
          word      <- assets.upload("word", "app.jar", bytes).either
        yield (missing, denied, forbidden, down, blank, bare, word)
      }.map { (result, _, uploadHits) =>
        assertTrue(
          result == Right(
            (
              Left(AssetError.Missing("missing")),
              Left(AssetError.Unauthorized),
              Left(AssetError.Unauthorized),
              Left(AssetError.Status(500)),
              Left(AssetError.Report),
              Left(AssetError.Report),
              Left(AssetError.Report),
            )
          ),
          uploadHits == Nil,
        )
      }
    ,
    test("delete removes the asset on a later page, follows a next link, and a missing release is already gone"):
      val api = (hit: Hit) =>
        if hit.auth != token then Reply(401, "")
        else if hit.method == "DELETE" then
          if hit.path.endsWith("/assets/7") || hit.path.endsWith("/assets/8") then Reply(204, "")
          else if hit.path.endsWith("/assets/9") then Reply(404, "")
          else Reply(500, hit.path)
        else if hit.path.endsWith("/tags/missing") then Reply(404, "")
        else if hit.path.endsWith("/tags/dist") then Reply(200, release("42"))
        else if hit.path.endsWith("/tags/linked") then Reply(200, release("43"))
        else if hit.path.endsWith("/tags/plain") then Reply(200, release("44"))
        else if hit.path.endsWith("/tags/gone") then Reply(200, release("45"))
        else if hit.path.contains("/releases/42/assets") then
          page(hit) match
            case Some(1) => Reply(200, fullPage)
            case Some(2) => Reply(200, named("app.jar", 7))
            case _       => Reply(500, hit.query)
        else if hit.path.contains("/releases/43/assets") then
          page(hit) match
            case Some(1) =>
              Reply(200, """[{"name":"other","id":1}]""", List("Link" -> """<http://127.0.0.1/a?page=2>; rel="next""""))
            case Some(2) => Reply(200, named("app.jar", 8))
            case _       => Reply(500, hit.query)
        else if hit.path.contains("/releases/44/assets") then
          page(hit) match
            case Some(1) =>
              Reply(200, """[{"name":"other","id":1}]""", List("Link" -> "<http://127.0.0.1/a?page=2>; rel=next"))
            case Some(2) => Reply(200, named("app.jar", 9))
            case _       => Reply(500, hit.query)
        else if hit.path.contains("/releases/45/assets") then
          Reply(200, """[{"name":"other","id":1}]""", List("Link" -> """<http://127.0.0.1/a>; rel="next""""))
        else Reply(500, hit.path)
      hosted(api, _ => Reply(500, "upload")) { assets =>
        for
          found   <- assets.delete("dist", "app.jar").either
          linked  <- assets.delete("linked", "app.jar").either
          plain   <- assets.delete("plain", "app.jar").either
          missing <- assets.delete("missing", "app.jar").either
          gone    <- assets.delete("gone", "app.jar").either
        yield (found, linked, plain, missing, gone)
      }.map { (result, apiHits, _) =>
        val deleted = apiHits.filter(_.method == "DELETE").map(_.path)
        assertTrue(
          result == Right((Right(()), Right(()), Right(()), Right(()), Right(()))),
          deleted.exists(_.endsWith("/assets/7")),
          deleted.exists(_.endsWith("/assets/8")),
          deleted.exists(_.endsWith("/assets/9")),
          apiHits.exists(hit => hit.path.contains("/releases/42/assets") && hit.query.contains("page=2")),
          apiHits.exists(hit => hit.query.contains("per_page=100")),
          !deleted.exists(_.endsWith("/assets/1000")),
        )
      }
    ,
    test("a hundred full pages are an unreadable list, and one 422 is deleted and posted once more"):
      val posts = new ConcurrentLinkedQueue[Int]()
      val api   = (hit: Hit) =>
        if hit.auth != token then Reply(401, "")
        else if hit.method == "DELETE" && hit.path.endsWith("/assets/7") then Reply(204, "")
        else if hit.path.endsWith("/tags/wide") then Reply(200, release("90"))
        else if hit.path.endsWith("/tags/dist") then Reply(200, release("42"))
        else if hit.path.contains("/releases/90/assets") then
          page(hit) match
            case Some(number) =>
              Reply(200, fullPage, List("Link" -> s"""<http://127.0.0.1/a?page=${number + 1}>; rel="next""""))
            case None => Reply(500, hit.query)
        else if hit.path.contains("/releases/42/assets") then Reply(200, named("app.jar", 7))
        else Reply(500, hit.path)
      val upload = (hit: Hit) =>
        if hit.method != "POST" then Reply(500, hit.path)
        else
          val seen = posts.size
          posts.add(seen)
          if hit.path.contains("/releases/42/") && seen == 0 then Reply(422, "")
          else if hit.path.contains("/releases/42/") then Reply(201, "")
          else Reply(500, hit.path)
      hosted(api, upload) { assets =>
        for
          wide     <- assets.delete("wide", "app.jar").either
          replaced <- assets.replace("dist", "app.jar", bytes).either
        yield (wide, replaced)
      }.map { (result, apiHits, uploadHits) =>
        val wide = apiHits.filter(_.path.contains("/releases/90/assets")).flatMap(hit => page(hit))
        assertTrue(
          result == Right((Left(AssetError.Inventory), Right(()))),
          wide == (1 to 100).toList,
          uploadHits.count(_.method == "POST") == 2,
          uploadHits.forall(_.body.sameElements(bytes.toArray)),
        )
      }
  )

  private def release(id: String): String =
    s"""{"blurb":"see \\\"upload_url\\\" no","upload_url":"http://127.0.0.1/releases/$id/assets{?name}"}"""

  private def named(name: String, id: Int): String =
    s"""[{"note":"a{b} \\\"name\\\": \\\"no\\\"","name":"$name","id":"x","id":$id}]"""

  private def fullPage: String =
    (0 until 100)
      .map(index => s"""{"name":"o$index","id":${1000 + index}}""")
      .mkString("[", ",", "]")

  private def page(hit: Hit): Option[Int] =
    value(hit.query, "page").flatMap(_.toIntOption)

  private def value(query: String, key: String): Option[String] =
    query.split("&", -1).iterator.collectFirst {
      case pair if pair.startsWith(s"$key=") => pair.drop(key.length + 1)
    }

  private def hosted[A](
      api: Hit => Reply,
      upload: Hit => Reply,
  )(
      use: Assets => IO[AssetError, A]
  ): ZIO[Any, AssetError, (Either[AssetError, A], List[Hit], List[Hit])] =
    val apiSeen    = new ConcurrentLinkedQueue[Hit]()
    val uploadSeen = new ConcurrentLinkedQueue[Hit]()
    ZIO.scoped {
      for
        apiPort    <- listen(api, apiSeen)
        uploadPort <- listen(upload, uploadSeen)
        built      <- layer(apiPort, uploadPort)
        result     <- ZIO.serviceWithZIO[Assets](use).either.provideLayer(built)
      yield (
        result,
        apiSeen.iterator.asScala.toList,
        uploadSeen.iterator.asScala.toList,
      )
    }

  private def layer(apiPort: Int, uploadPort: Int): ZIO[Any, Nothing, ZLayer[Any, AssetError, Assets]] =
    for
      api   <- base(apiPort)
      up    <- base(uploadPort)
      named <- ZIO.fromEither(AssetRepo.from("own/demo")).catchAll(error => ZIO.dieMessage(error.message))
    yield Assets.layer(api, up, AssetToken("secret"), named)

  private def base(port: Int): ZIO[Any, Nothing, AssetBase] =
    ZIO.fromEither(AssetBase.from(s"http://127.0.0.1:$port")).catchAll(error => ZIO.dieMessage(error.message))

  private def listen(answer: Hit => Reply, seen: ConcurrentLinkedQueue[Hit]): ZIO[Scope, Nothing, Int] =
    ZIO.acquireRelease(
      ZIO
        .attempt {
          val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
          server.createContext("/", exchange => reply(exchange, answer, seen))
          server.start()
          server
        }
        .orDie
    )(server => ZIO.attempt(server.stop(0)).orDie)
      .map(_.getAddress.getPort)

  private def reply(exchange: HttpExchange, answer: Hit => Reply, seen: ConcurrentLinkedQueue[Hit]): Unit =
    val body = exchange.getRequestBody.readAllBytes()
    val uri  = exchange.getRequestURI
    val hit  = Hit(
      exchange.getRequestMethod,
      uri.getPath,
      Option(uri.getRawQuery).getOrElse(""),
      Option(exchange.getRequestHeaders.getFirst("Authorization")).getOrElse(""),
      Option(exchange.getRequestHeaders.getFirst("User-Agent")).getOrElse(""),
      Option(exchange.getRequestHeaders.getFirst("Accept")).getOrElse(""),
      body,
    )
    seen.add(hit)
    val answered = answer(hit)
    val bytes    = answered.body.getBytes(StandardCharsets.UTF_8)
    answered.headers.foreach((name, value) => exchange.getResponseHeaders.add(name, value))
    if answered.status == 204 || answered.status == 304 then exchange.sendResponseHeaders(answered.status, -1)
    else
      exchange.sendResponseHeaders(answered.status, bytes.length.toLong)
      val out = exchange.getResponseBody
      out.write(bytes)
      out.close()

  private final case class Hit(
      method: String,
      path: String,
      query: String,
      auth: String,
      agent: String,
      accept: String,
      body: Array[Byte],
  )

  private final case class Reply(status: Int, body: String, headers: List[(String, String)] = Nil)
end AssetHostLawSpec
