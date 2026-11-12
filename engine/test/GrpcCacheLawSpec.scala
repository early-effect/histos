package histos.schedule

import histos.schedule.GrpcLoopback.Answer

import io.grpc.Status

import zio.{Chunk, ZIO, durationInt}
import zio.test.*

/** The cache against a loopback server bound for the law. The law's timeout bounds a call the server never answers. */
object GrpcCacheLawSpec extends ZIOSpecDefault:
  private val action = Digest.trusted("ab" * 32)
  private val body   = Chunk[Byte](1, 2, 9)
  private val dev    = CacheNamespace.Dev(DevName("law"))

  def spec = suite("grpc cache laws")(
    test("a store then a find is the same bytes, and a body wider than one write frame survives"):
      val wide  = Chunk.fill(Reapi.WriteChunk + 1)(7.toByte)
      val wider = ActionDigest.ofBytes(wide)
      ZIO.scoped:
        for
          port   <- GrpcLoopback.serve(Answer.Store)
          writer <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Write, CacheNamespace.Main)
          _      <- writer.store(action, body)
          _      <- writer.store(wider, wide)
          found  <- writer.find(action)
          large  <- writer.find(wider)
        yield assertTrue(found == Some(body), large == Some(wide))
    ,
    test("a miss on the reader's own namespace falls through to main, and a miss everywhere is none"):
      ZIO.scoped:
        for
          port   <- GrpcLoopback.serve(Answer.Store)
          main   <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Write, CacheNamespace.Main)
          reader <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Read, dev)
          before <- reader.find(action)
          _      <- main.store(action, body)
          after  <- reader.find(action)
        yield assertTrue(before == None, after == Some(body))
    ,
    test("off stores nothing, a reader cannot write, and purge is not a remote-execution call"):
      ZIO.scoped:
        for
          port   <- GrpcLoopback.serve(Answer.Store)
          off    <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Off, dev)
          read   <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Read, CacheNamespace.Main)
          found  <- off.find(action)
          kept   <- off.store(action, body).either
          denied <- read.store(action, body).either
          purged <- read.purge.either
        yield assertTrue(
          off.mode == RemoteMode.Off,
          read.mode == RemoteMode.Read,
          found == None,
          kept == Left(RemoteError.Off),
          denied == Left(RemoteError.ReadOnly),
          purged == Left(RemoteError.HttpOnly("purge")),
        )
    ,
    test("an unavailable server stops the lookup and the store, and it is not a missing action"):
      ZIO.scoped:
        for
          port    <- GrpcLoopback.serve(Answer.Fixed(Status.Code.UNAVAILABLE))
          reader  <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Read, dev)
          writer  <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Write, dev)
          missed  <- reader.find(action).either
          refused <- writer.store(action, body).either
        yield assertTrue(
          missed == Left(RemoteError.Rpc("UNAVAILABLE")),
          refused == Left(RemoteError.Rpc("UNAVAILABLE")),
        ),
  ) @@ TestAspect.timeout(30.seconds)
end GrpcCacheLawSpec
