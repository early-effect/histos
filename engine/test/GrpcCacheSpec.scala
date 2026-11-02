package histos.schedule

import java.nio.charset.StandardCharsets

import histos.version.PullRequestNumber

import zio.{Chunk, Clock, ZIO, durationInt}
import zio.test.*

object GrpcCacheSpec extends ZIOSpecDefault:
  private val pull = CacheNamespace.PullRequest(PullRequestNumber("1"))

  private val config = suite("configuration")(
    test("an unset uri is off, and a bad mode or namespace is not"):
      val uri = Map(CacheConfig.UriKey -> "grpc://127.0.0.1:9")
      assertTrue(
        CacheConfig.decide(Map.empty) == Right(None),
        CacheConfig.decide(Map(CacheConfig.UriKey -> "  ")) == Right(None),
        CacheConfig.decide(Map(CacheNamespace.Key -> "pr/1")) == Right(None),
        CacheConfig.decide(Map(CacheConfig.ModeKey -> "nope")) == Left(RemoteError.Mode("nope")),
        CacheConfig.decide(Map(CacheNamespace.Key -> "nope")) == Left(
          RemoteError.Namespace(NamespaceError.Shape("nope"))
        ),
        CacheConfig.decide(uri).map(_.map(_.mode)) == Right(Some(RemoteMode.Read)),
        CacheConfig.decide(uri + (CacheConfig.ModeKey -> "write")).map(_.map(_.mode)) == Right(Some(RemoteMode.Write)),
        CacheConfig.decide(uri + (CacheConfig.ModeKey -> "Write")) == Left(RemoteError.Mode("Write")),
        CacheConfig.decide(uri + (CacheNamespace.Key -> "dev/laptop")).map(_.map(_.namespace.token)) == Right(
          Some("dev/laptop")
        ),
      )
    ,
    test("the scheme picks the client and a bad address does not"):
      val grpc    = CacheConfig.decide(Map(CacheConfig.UriKey -> "grpc://127.0.0.1:9092"))
      val grpcs   = CacheConfig.decide(Map(CacheConfig.UriKey -> "grpcs://cache.example:443"))
      val http    = CacheConfig.decide(Map(CacheConfig.UriKey -> "https://cache.example/ac"))
      val decoded = heddle.http.Url.decode("https://cache.example/ac")
      val httpOk  = decoded match
        case Right(url) => http.map(_.map(_.target)) == Right(Some(CacheConfig.Target.Http(url)))
        case Left(_)    => false
      assertTrue(
        grpc == Right(
          Some(
            CacheConfig
              .Selected(CacheConfig.Target.Grpc("127.0.0.1", 9092, false), RemoteMode.Read, CacheNamespace.Main)
          )
        ),
        grpcs == Right(
          Some(
            CacheConfig
              .Selected(CacheConfig.Target.Grpc("cache.example", 443, true), RemoteMode.Read, CacheNamespace.Main)
          )
        ),
        httpOk,
        CacheConfig.decide(Map(CacheConfig.UriKey -> "ftp://cache")) == Left(RemoteError.Scheme("ftp")),
        CacheConfig.decide(Map(CacheConfig.UriKey -> "grpc://127.0.0.1")) == Left(
          RemoteError.Address("grpc://127.0.0.1")
        ),
        CacheConfig.decide(Map(CacheConfig.UriKey -> "grpc://127.0.0.1:9092/ac")) == Left(
          RemoteError.Address("grpc://127.0.0.1:9092/ac")
        ),
        CacheConfig.decide(Map(CacheConfig.UriKey -> "not-a-url")) == Left(RemoteError.Address("not-a-url")),
        RemoteError.Scheme("ftp").message.contains("grpc"),
        RemoteError.HttpOnly("purge").message == "purge is an http cache operation",
        RemoteError.Rpc("INTERNAL").message == "remote cache answered INTERNAL",
      )
    ,
    test("promote on grpc names the limit and does not need a server"):
      val env = Map(CacheConfig.UriKey -> "grpc://127.0.0.1:1")
      for
        off     <- CacheConfig.promote(Map.empty, pull, CacheNamespace.Main).either
        refused <- CacheConfig.promote(env, pull, CacheNamespace.Main).either
      yield assertTrue(
        off == Left(RemoteError.Off),
        refused == Left(RemoteError.HttpOnly("cache promote")),
      ),
  )

  private val codec = suite("messages")(
    test("a request round-trips the instance, the hash, and the action result"):
      val bytes  = Chunk.fromArray("hi".getBytes(StandardCharsets.UTF_8))
      val action = ActionDigest.ofBytes(bytes)
      val output = ActionDigest.ofBytes(bytes)
      val result = ActionResult.encode(output, bytes.length)
      val wide   = Chunk.fill(70 * 1024)(1.toByte)
      assertTrue(
        Reapi.decodeGet(Reapi.getAction("pr/1", action)) == Some(("pr/1", action)),
        Reapi.decodeUpdate(Reapi.updateAction("main", action, result)) == Some(("main", action, result)),
        Reapi.decodeFind(Reapi.findMissing("main", output, bytes.length.toLong)).map(_._2) == Some(List(output)),
        Reapi.missingDigests(Reapi.missingResponse(List(output))) == Some(List(output)),
        Reapi.missingDigests(Chunk.empty) == Some(Nil),
        Reapi.decodeRead(Reapi.readRequest(Reapi.readResource("pr/1", output, bytes.length.toLong))) ==
          Some(Reapi.readResource("pr/1", output, bytes.length.toLong)),
        Reapi.readData(Reapi.readResponse(wide)) == Some(wide),
        Reapi.frames(Reapi.writeResource("main", output, wide.length.toLong), wide).length == 2,
        Reapi.blob(Reapi.writeResource("dev/laptop", output, 3L)) == Some((output.value, 3L)),
        Reapi.blob(Reapi.readResource("pr/1", output, 0L)) == Some((output.value, 0L)),
      )
  )

  private val exchange = suite("exchange")(
    test("put then get returns the same bytes"):
      val bytes  = Chunk.fromArray("same".getBytes(StandardCharsets.UTF_8))
      val action = ActionDigest.ofBytes(bytes)
      for
        port  <- GrpcStand.serve()
        cache <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Write, CacheNamespace.Main)
        _     <- cache.store(action, bytes)
        found <- cache.find(action)
      yield assertTrue(found == Some(bytes))
    ,
    test("a wide output is more than one write frame"):
      val bytes  = Chunk.fill(70 * 1024)(7.toByte)
      val action = ActionDigest.ofBytes(bytes)
      for
        port  <- GrpcStand.serve()
        cache <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Write, CacheNamespace.Main)
        _     <- cache.store(action, bytes)
        found <- cache.find(action)
      yield assertTrue(found == Some(bytes))
    ,
    test("an empty output round-trips"):
      val bytes  = Chunk.empty
      val action = ActionDigest.ofBytes(bytes)
      for
        port  <- GrpcStand.serve()
        cache <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Write, CacheNamespace.Main)
        _     <- cache.store(action, bytes)
        found <- cache.find(action)
      yield assertTrue(found == Some(bytes))
    ,
    test("a pull request write misses on main and hits on pr/1"):
      val bytes  = Chunk.fromArray("namespace".getBytes(StandardCharsets.UTF_8))
      val action = ActionDigest.ofBytes(bytes)
      for
        port      <- GrpcStand.serve()
        pullWrite <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Write, pull)
        _         <- pullWrite.store(action, bytes)
        mainRead  <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Read, CacheNamespace.Main)
        missed    <- mainRead.find(action)
        pullRead  <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Read, pull)
        hit       <- pullRead.find(action)
      yield assertTrue(missed.isEmpty, hit == Some(bytes))
    ,
    test("not found falls through to main"):
      val bytes  = Chunk.fromArray("mainline".getBytes(StandardCharsets.UTF_8))
      val action = ActionDigest.ofBytes(bytes)
      for
        port <- GrpcStand.serve()
        main <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Write, CacheNamespace.Main)
        _    <- main.store(action, bytes)
        read <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Read, pull)
        hit  <- read.find(action)
      yield assertTrue(hit == Some(bytes))
    ,
    test("a bad status does not fall through"):
      val bytes  = Chunk.fromArray("poison".getBytes(StandardCharsets.UTF_8))
      val action = ActionDigest.ofBytes(bytes)
      for
        port  <- GrpcStand.serve(fail = Set(pull.token -> action.value))
        main  <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Write, CacheNamespace.Main)
        _     <- main.store(action, bytes)
        read  <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Read, pull)
        found <- read.find(action).either
      yield assertTrue(found == Left(RemoteError.Rpc("INTERNAL")))
    ,
    test("read refuses store and purge names the http limit"):
      val bytes  = Chunk.fromArray("nope".getBytes(StandardCharsets.UTF_8))
      val action = ActionDigest.ofBytes(bytes)
      for
        port    <- GrpcStand.serve()
        reading <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Read, CacheNamespace.Main)
        refused <- reading.store(action, bytes).either
        purged  <- reading.purge.either
        missed  <- reading.find(action)
        writing <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Write, CacheNamespace.Main)
        named   <- writing.purge.either
      yield assertTrue(
        refused == Left(RemoteError.ReadOnly),
        purged == Left(RemoteError.HttpOnly("purge")),
        missed.isEmpty,
        named == Left(RemoteError.HttpOnly("purge")),
      )
    ,
    test("off is unchanged"):
      val bytes  = Chunk.fromArray("off".getBytes(StandardCharsets.UTF_8))
      val action = ActionDigest.ofBytes(bytes)
      for
        cache  <- CacheConfig.open(Map.empty)
        stored <- cache.store(action, bytes).either
        found  <- cache.find(action)
        purged <- cache.purge.either
      yield assertTrue(
        cache.mode == RemoteMode.Off,
        stored == Left(RemoteError.Off),
        found.isEmpty,
        purged == Left(RemoteError.Off),
      )
    ,
    test("an interrupted lookup cancels the call"):
      val bytes  = Chunk.fromArray("hang".getBytes(StandardCharsets.UTF_8))
      val action = ActionDigest.ofBytes(bytes)
      ZIO.withClock(Clock.ClockLive):
        for
          port  <- GrpcStand.serve(hang = Set(action.value))
          cache <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Read, CacheNamespace.Main)
          fiber <- cache.find(action).fork
          _     <- ZIO.sleep(100.millis)
          done  <- fiber.interrupt.timeout(2.seconds)
        yield assertTrue(done.exists(exit => !exit.isSuccess)),
  )

  def spec = suite("grpc remote cache")(
    config,
    codec,
    exchange @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds),
  )
end GrpcCacheSpec
