package histos.schedule

import heddle.client.{Authority, Client, ClientError}
import heddle.http.Url
import heddle.route.PathDsl.*
import heddle.server.Port
import heddle.{Body, Method, Request, Response, Routes, Server, Status, string}
import histos.version.PullRequestNumber

import java.nio.charset.StandardCharsets

import zio.{Chunk, Ref, Scope, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

object HttpCacheSpec extends ZIOSpecDefault:
  private val pullRequest = CacheNamespace.PullRequest(PullRequestNumber("1"))

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

  private val prefixes = suite("namespace")(
    test("a dev name is one path segment"):
      assertTrue(
        DevName.from("") == Left(DevNameError.Segment("")),
        DevName.from("a/b") == Left(DevNameError.Segment("a/b")),
        DevName.from(".") == Left(DevNameError.Segment(".")),
        DevName.from("..") == Left(DevNameError.Segment("..")),
        DevName.from("laptop").map(_.value) == Right("laptop"),
        DevName.from("a-b_1").map(_.value) == Right("a-b_1"),
      )
    ,
    test("the index prefix is the role, then main"):
      val dev = DevName.from("laptop").map(CacheNamespace.Dev(_))
      assertTrue(
        CacheNamespace.Main.segments == List("main"),
        CacheNamespace.Main.reads == List(CacheNamespace.Main),
        pullRequest.segments == List("pr", "1"),
        pullRequest.reads == List(pullRequest, CacheNamespace.Main),
        dev.map(role => role.segments -> role.reads.map(_.segments)) == Right(
          List("dev", "laptop") -> List(List("dev", "laptop"), List("main"))
        ),
      ),
  )

  private val live = suite("loopback")(
    test("put then get returns the same bytes"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
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
            s"PUT /main/ac/${action.value}",
            s"GET /main/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("an index entry that names a missing blob is dangling"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action  = digest("dangling")
          missing = Digest.ofBytes(Chunk.single(7.toByte))
          _     <- stand.plantAc("main/ac", action.value, ActionResult.encode(missing, 1))
          found <- cache.find(action).either
          calls <- stand.calls
        yield assertTrue(
          found == Left(RemoteError.Dangling(action, missing)),
          calls == List(s"GET /main/ac/${action.value}", s"GET /cas/${missing.value}"),
        )
    ,
    test("off does not call the store"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Off, CacheNamespace.Main)
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
          writer <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
          reader <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
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
            s"PUT /main/ac/${action.value}",
            s"GET /main/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("a missing action is a miss"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action = digest("missing")
          found <- cache.find(action)
          calls <- stand.calls
        yield assertTrue(found == None, calls == List(s"GET /main/ac/${action.value}"))
    ,
    test("bytes that do not match the digest are a mismatch"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action = digest("mismatch")
          bytes  = utf8("right")
          output = Digest.ofBytes(bytes)
          _     <- stand.plantCas(output.value, utf8("nope"))
          _     <- stand.plantAc("main/ac", action.value, ActionResult.encode(output, bytes.length))
          found <- cache.find(action).either
        yield assertTrue(found == Left(RemoteError.Mismatch(action, output)))
    ,
    test("a body that is not an action result is an index error"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action = digest("index")
          _     <- stand.plantAc("main/ac", action.value, utf8("nope"))
          found <- cache.find(action).either
          calls <- stand.calls
        yield assertTrue(
          found == Left(RemoteError.Index(action)),
          calls == List(s"GET /main/ac/${action.value}"),
        )
    ,
    test("a refused blob put does not write the index"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
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
          calls == List(s"PUT /cas/${output.value}", s"GET /main/ac/${action.value}"),
        )
    ,
    test("a write to pr/1 is invisible to main and does not write main"):
      ZIO.scoped:
        for
          stand  <- Stand.open
          writer <- stand.cache(RemoteMode.Write, pullRequest)
          reader <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action = digest("pr-only")
          bytes  = utf8("pull-request")
          output = Digest.ofBytes(bytes)
          _     <- writer.store(action, bytes)
          found <- reader.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == None,
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /pr/1/ac/${action.value}",
            s"GET /main/ac/${action.value}",
          ),
        )
    ,
    test("a read of pr/1 falls through to main on a miss"):
      ZIO.scoped:
        for
          stand  <- Stand.open
          writer <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
          reader <- stand.cache(RemoteMode.Read, pullRequest)
          action = digest("shared")
          bytes  = utf8("from-main")
          output = Digest.ofBytes(bytes)
          _     <- writer.store(action, bytes)
          found <- reader.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == Some(bytes),
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /main/ac/${action.value}",
            s"GET /pr/1/ac/${action.value}",
            s"GET /main/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("a pull request hit does not read main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Write, pullRequest)
          action = digest("pr-hit")
          bytes  = utf8("own")
          output = Digest.ofBytes(bytes)
          _     <- cache.store(action, bytes)
          found <- cache.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == Some(bytes),
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /pr/1/ac/${action.value}",
            s"GET /pr/1/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
        )
    ,
    test("a dangling pull request entry does not read main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, pullRequest)
          action  = digest("pr-dangling")
          missing = Digest.ofBytes(Chunk.single(7.toByte))
          kept    = utf8("on-main")
          output  = Digest.ofBytes(kept)
          _     <- stand.plantCas(output.value, kept)
          _     <- stand.plantAc("main/ac", action.value, ActionResult.encode(output, kept.length))
          _     <- stand.plantAc("pr/1/ac", action.value, ActionResult.encode(missing, 1))
          found <- cache.find(action).either
          calls <- stand.calls
        yield assertTrue(
          found == Left(RemoteError.Dangling(action, missing)),
          calls == List(s"GET /pr/1/ac/${action.value}", s"GET /cas/${missing.value}"),
        )
    ,
    test("a pull request index that is not an action result does not read main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, pullRequest)
          action = digest("pr-index")
          bytes  = utf8("on-main")
          output = Digest.ofBytes(bytes)
          _     <- stand.plantCas(output.value, bytes)
          _     <- stand.plantAc("main/ac", action.value, ActionResult.encode(output, bytes.length))
          _     <- stand.plantAc("pr/1/ac", action.value, utf8("nope"))
          found <- cache.find(action).either
          calls <- stand.calls
        yield assertTrue(
          found == Left(RemoteError.Index(action)),
          calls == List(s"GET /pr/1/ac/${action.value}"),
        )
    ,
    test("a pull request status other than a miss does not read main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          cache <- stand.cache(RemoteMode.Read, pullRequest)
          action = digest("pr-status")
          bytes  = utf8("on-main")
          output = Digest.ofBytes(bytes)
          _     <- stand.plantCas(output.value, bytes)
          _     <- stand.plantAc("main/ac", action.value, ActionResult.encode(output, bytes.length))
          _     <- stand.fail(500)
          found <- cache.find(action).either
          calls <- stand.calls
        yield assertTrue(
          found == Left(RemoteError.Status(500)),
          calls == List(s"GET /pr/1/ac/${action.value}"),
        )
    ,
    test("a dev write is invisible to main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          name  <- ZIO.fromEither(DevName.from("laptop"))
          dev = CacheNamespace.Dev(name)
          writer <- stand.cache(RemoteMode.Write, dev)
          reader <- stand.cache(RemoteMode.Read, CacheNamespace.Main)
          action = digest("dev-only")
          bytes  = utf8("laptop")
          output = Digest.ofBytes(bytes)
          _     <- writer.store(action, bytes)
          found <- reader.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == None,
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /dev/laptop/ac/${action.value}",
            s"GET /main/ac/${action.value}",
          ),
        )
    ,
    test("a dev read falls through to main"):
      ZIO.scoped:
        for
          stand <- Stand.open
          name  <- ZIO.fromEither(DevName.from("laptop"))
          dev = CacheNamespace.Dev(name)
          writer <- stand.cache(RemoteMode.Write, CacheNamespace.Main)
          reader <- stand.cache(RemoteMode.Read, dev)
          action = digest("dev-shared")
          bytes  = utf8("from-main")
          output = Digest.ofBytes(bytes)
          _     <- writer.store(action, bytes)
          found <- reader.find(action)
          calls <- stand.calls
        yield assertTrue(
          found == Some(bytes),
          calls == List(
            s"PUT /cas/${output.value}",
            s"PUT /main/ac/${action.value}",
            s"GET /dev/laptop/ac/${action.value}",
            s"GET /main/ac/${action.value}",
            s"GET /cas/${output.value}",
          ),
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

  private enum Slot:
    case Index(prefix: String)
    case Blob

    def line(method: String, hash: String): String = this match
      case Index(prefix) => s"$method /$prefix/$hash"
      case Blob          => s"$method /cas/$hash"
  end Slot

  private final case class Note(
      ac: Map[String, Chunk[Byte]],
      cas: Map[String, Chunk[Byte]],
      calls: List[String],
      fail: Option[Int],
  )

  private final class Stand(origin: Url, note: Ref[Note]):
    def cache(mode: RemoteMode, namespace: CacheNamespace): ZIO[Client, Nothing, RemoteCache] =
      HttpCache.make(origin, mode, namespace)

    def calls: UIO[List[String]] =
      note.get.map(_.calls)

    def plantAc(prefix: String, hash: String, bytes: Chunk[Byte]): UIO[Unit] =
      note.update(current => current.copy(ac = current.ac + (s"$prefix/$hash" -> bytes)))

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
        Method.GET / "main" / "ac" / string("hash") -> { hash => read(note, Slot.Index("main/ac"), hash) },
        Method.PUT / "main" / "ac" / string("hash") handle { (hash, request) =>
          write(note, Slot.Index("main/ac"), hash, request)
        },
        Method.GET / "pr" / string("number") / "ac" / string("hash") -> { (number, hash) =>
          read(note, Slot.Index(s"pr/$number/ac"), hash)
        },
        Method.PUT / "pr" / string("number") / "ac" / string("hash") handle { (number, hash, request) =>
          write(note, Slot.Index(s"pr/$number/ac"), hash, request)
        },
        Method.GET / "dev" / string("name") / "ac" / string("hash") -> { (name, hash) =>
          read(note, Slot.Index(s"dev/$name/ac"), hash)
        },
        Method.PUT / "dev" / string("name") / "ac" / string("hash") handle { (name, hash, request) =>
          write(note, Slot.Index(s"dev/$name/ac"), hash, request)
        },
        Method.GET / "cas" / string("hash") -> { hash => read(note, Slot.Blob, hash) },
        Method.PUT / "cas" / string("hash") handle { (hash, request) => write(note, Slot.Blob, hash, request) },
      )

    private def read(note: Ref[Note], slot: Slot, hash: String) =
      note.modify: current =>
        val line = slot.line("GET", hash)
        forced(current, line).getOrElse:
          val stored = slot match
            case Slot.Index(prefix) => current.ac.get(s"$prefix/$hash")
            case Slot.Blob          => current.cas.get(hash)
          val response = stored match
            case Some(bytes) => Response(Status.Ok, Body.fromBytes(bytes))
            case None        => Response.notFound()
          (response, current.copy(calls = current.calls :+ line))

    private def write(note: Ref[Note], slot: Slot, hash: String, request: Request) =
      request.body.collect.foldZIO(
        _ => ZIO.succeed(Response.badRequest("the upload had no body")),
        bytes =>
          note.modify: current =>
            val line = slot.line("PUT", hash)
            forced(current, line).getOrElse:
              val stored = slot match
                case Slot.Index(prefix) => current.copy(ac = current.ac + (s"$prefix/$hash" -> bytes))
                case Slot.Blob          => current.copy(cas = current.cas + (hash -> bytes))
              val status = slot match
                case Slot.Blob     => Status.Ok
                case Slot.Index(_) => Status.NoContent
              (Response.empty(status), stored.copy(calls = stored.calls :+ line)),
      )

    private def forced(current: Note, line: String): Option[(Response, Note)] =
      current.fail.map: code =>
        (Response.empty(Status(code, "forced")), current.copy(calls = current.calls :+ line, fail = None))
  end Stand

  def spec = suite("http remote cache")(
    encoded,
    prefixes,
    live.provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      ZLayer.succeed(Client.Config.default) >>> Client.layer
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(1.minute),
  )
end HttpCacheSpec
