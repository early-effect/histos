package histos.schedule

import heddle.client.{Authority, Client, ClientError}
import heddle.http.Url
import heddle.route.PathDsl.*
import heddle.server.Port
import heddle.{Body, Method, Request, Response, Routes, Server, Status, string}

import java.nio.charset.StandardCharsets

import zio.{Chunk, Ref, Scope, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

object HttpCacheSpec extends ZIOSpecDefault:
  private val encoded = suite("action result")(
    test("a zero size is omitted and a non-zero size is a varint"):
      val empty  = Digest.ofBytes(Chunk.empty)
      val text   = Chunk.fromArray("hi".getBytes(StandardCharsets.UTF_8))
      val output = Digest.ofBytes(text)
      val wide   = Digest.ofBytes(Chunk.single(9.toByte))
      val zero   = framed(empty.value, Array.empty)
      val two    = framed(output.value, Array(0x10.toByte, 2.toByte))
      val big    = framed(wide.value, Array(0x10.toByte, 0xc8.toByte, 0x01.toByte))
      assertTrue(
        empty.value.length == 64,
        ActionResult.encode(empty, 0) == zero,
        ActionResult.decode(zero) == Some(ActionResult.Pointer(empty, 0L)),
        ActionResult.encode(output, text.length) == two,
        ActionResult.decode(two) == Some(ActionResult.Pointer(output, text.length.toLong)),
        ActionResult.encode(wide, 200) == big,
        ActionResult.decode(big) == Some(ActionResult.Pointer(wide, 200L)),
        ActionResult.decode(Chunk(0x20.toByte, 0x01.toByte) ++ zero) == Some(ActionResult.Pointer(empty, 0L)),
        ActionResult.decode(Chunk(0x4a.toByte, 0x01.toByte, 0x00.toByte) ++ zero) ==
          Some(ActionResult.Pointer(empty, 0L)),
        ActionResult.decode(Chunk.empty) == None,
        ActionResult.decode(Chunk(0xff.toByte)) == None,
        ActionResult.decode(Chunk(0x32.toByte, 0x02.toByte, 0x0a.toByte)) == None,
      )
    ,
    test("an exchange drops the throwable message"):
      val connect = ClientError.Connect(Authority("cache.example", 9), new RuntimeException("s3cret"))
      val trust   = ClientError.InvalidTrust("s3cret")
      val fault   = HttpCache.exchange(connect)
      assertTrue(
        fault == CacheExchange.Connect("cache.example", 9),
        !fault.message.contains("s3cret"),
        HttpCache.exchange(trust) == CacheExchange.Trust,
        !CacheExchange.Trust.message.contains("s3cret"),
      ),
  )

  private val live = suite("loopback")(
    test("put then get returns the same bytes"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Write)
          action = digest("action")
          bytes  = utf8("compile")
          output = Digest.ofBytes(bytes)
          _     <- cache.store(action, bytes)
          found <- cache.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == Some(bytes),
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /ac/${action.value}",
            s"GET /ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("an index entry that names a missing blob is dangling"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read)
          action  = digest("dangling")
          missing = Digest.ofBytes(Chunk.single(7.toByte))
          _     <- stand.plantAc(action.value, ActionResult.encode(missing, 1))
          found <- cache.find(action).either
          calls <- stand.calls
        yield assertTrue(
          found == Left(RemoteError.Dangling(action, missing)),
          calls == List(s"GET /ac/${action.value}", s"GET /cas/${missing.value}"),
        )
    ,
    test("off does not call the store"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Off)
          action = digest("off")
          found   <- cache.find(action)
          refused <- cache.store(action, utf8("nope")).either
          calls   <- stand.calls
        yield assertTrue(found == None, refused == Left(RemoteError.Off), calls.isEmpty)
    ,
    test("read refuses store and still reads"):
      ZIO.scoped:
        for
          stand  <- Stand.open
          writer <- stand.cache(RemoteMode.Write)
          reader <- stand.cache(RemoteMode.Read)
          action = digest("read")
          bytes  = utf8("from-remote")
          output = Digest.ofBytes(bytes)
          _       <- writer.store(action, bytes)
          found   <- reader.find(action)
          refused <- reader.store(action, bytes).either
          calls   <- stand.calls
        yield assertTrue(
          found == Some(bytes),
          refused == Left(RemoteError.ReadOnly),
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /ac/${action.value}",
            s"GET /ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("a missing action is a miss"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read)
          action = digest("missing")
          found <- cache.find(action)
          calls <- stand.calls
        yield assertTrue(found == None, calls == List(s"GET /ac/${action.value}"))
    ,
    test("bytes that do not match the digest are a mismatch"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read)
          action = digest("mismatch")
          bytes  = utf8("right")
          output = Digest.ofBytes(bytes)
          _     <- stand.plantCas(output.value, utf8("nope"))
          _     <- stand.plantAc(action.value, ActionResult.encode(output, bytes.length))
          found <- cache.find(action).either
        yield assertTrue(found == Left(RemoteError.Mismatch(action, output)))
    ,
    test("a body that is not an action result is an index error"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read)
          action = digest("index")
          _     <- stand.plantAc(action.value, utf8("nope"))
          found <- cache.find(action).either
          calls <- stand.calls
        yield assertTrue(
          found == Left(RemoteError.Index(action)),
          calls == List(s"GET /ac/${action.value}"),
        )
    ,
    test("a refused blob put does not write the index"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Write)
          action = digest("status")
          bytes  = utf8("blob")
          output = Digest.ofBytes(bytes)
          _       <- stand.fail(500)
          refused <- cache.store(action, bytes).either
          found   <- cache.find(action)
          calls   <- stand.calls
        yield assertTrue(
          refused == Left(RemoteError.Status(500)),
          found == None,
          calls == List(s"PUT /cas/${output.value}", s"GET /ac/${action.value}"),
        ),
  )

  private def digest(label: String): Digest =
    Digest.ofBytes(utf8(label))

  private def utf8(text: String): Chunk[Byte] =
    Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))

  /** Lengths here stay under 128, so each protobuf length is one byte. */
  private def framed(hash: String, sizeSuffix: Array[Byte]): Chunk[Byte] =
    val hashBytes = hash.getBytes(StandardCharsets.US_ASCII)
    val digest    = Array(0x0a.toByte, hashBytes.length.toByte) ++ hashBytes ++ sizeSuffix
    Chunk.fromArray(Array(0x32.toByte, digest.length.toByte) ++ digest)

  private final case class Note(
      ac: Map[String, Chunk[Byte]],
      cas: Map[String, Chunk[Byte]],
      calls: List[String],
      fail: Option[Int],
  )

  private final class Stand(origin: Url, note: Ref[Note]):
    def cache(mode: RemoteMode): ZIO[Client, Nothing, RemoteCache] =
      HttpCache.make(origin, mode)

    def calls: UIO[List[String]] =
      note.get.map(_.calls)

    def plantAc(hash: String, bytes: Chunk[Byte]): UIO[Unit] =
      note.update(current => current.copy(ac = current.ac + (hash -> bytes)))

    def plantCas(hash: String, bytes: Chunk[Byte]): UIO[Unit] =
      note.update(current => current.copy(cas = current.cas + (hash -> bytes)))

    def fail(code: Int): UIO[Unit] =
      note.update(_.copy(fail = Some(code)))
  end Stand

  private object Stand:
    def open: ZIO[Scope, heddle.error.ServerError, Stand] =
      val http = Server.Config.default.copy(host = "127.0.0.1", port = Port(0))
      for
        note   <- Ref.make(Note(Map.empty, Map.empty, Nil, None))
        server <- Server.install(routes(note), http)
        port   <- server.port
      yield Stand(Url.parse(s"http://127.0.0.1:$port"), note)

    private def routes(note: Ref[Note]): Routes[Any, Nothing] =
      Routes(
        Method.GET / "ac" / string("hash") -> { hash => read(note, "ac", hash) },
        Method.PUT / "ac" / string("hash") handle { (hash, request) => write(note, "ac", hash, request) },
        Method.GET / "cas" / string("hash") -> { hash => read(note, "cas", hash) },
        Method.PUT / "cas" / string("hash") handle { (hash, request) => write(note, "cas", hash, request) },
      )

    private def read(note: Ref[Note], kind: String, hash: String) =
      note.modify: current =>
        val line = s"GET /$kind/$hash"
        forced(current, line).getOrElse:
          val stored   = if kind == "ac" then current.ac else current.cas
          val response = stored.get(hash) match
            case Some(bytes) => Response(Status.Ok, Body.fromBytes(bytes))
            case None        => Response.notFound()
          (response, current.copy(calls = current.calls :+ line))

    private def write(note: Ref[Note], kind: String, hash: String, request: Request) =
      request.body.collect.foldZIO(
        _ => ZIO.succeed(Response.badRequest("the upload had no body")),
        bytes =>
          note.modify: current =>
            val line = s"PUT /$kind/$hash"
            forced(current, line).getOrElse:
              val stored =
                if kind == "ac" then current.copy(ac = current.ac + (hash -> bytes))
                else current.copy(cas = current.cas + (hash -> bytes))
              val status = if kind == "cas" then Status.Ok else Status.NoContent
              (Response.empty(status), stored.copy(calls = stored.calls :+ line)),
      )

    private def forced(current: Note, line: String): Option[(Response, Note)] =
      current.fail.map: code =>
        (Response.empty(Status(code, "forced")), current.copy(calls = current.calls :+ line, fail = None))
  end Stand

  def spec = suite("http remote cache")(
    encoded,
    live.provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      ZLayer.succeed(Client.Config.default) >>> Client.layer
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(1.minute),
  )
end HttpCacheSpec
