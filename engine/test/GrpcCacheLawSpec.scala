package histos.schedule

import histos.schedule.CacheConfig.Target
import histos.schedule.GrpcLoopback.Answer

import io.grpc.Status

import zio.{Chunk, Ref, Scope, ZIO, ZLayer, durationInt}
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
          writer <- open(port, RemoteMode.Write, CacheNamespace.Main)
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
          main   <- open(port, RemoteMode.Write, CacheNamespace.Main)
          reader <- open(port, RemoteMode.Read, dev)
          before <- reader.find(action)
          _      <- main.store(action, body)
          after  <- reader.find(action)
        yield assertTrue(before == None, after == Some(body))
    ,
    test("off stores nothing, a reader cannot write, and purge is not a remote-execution call"):
      ZIO.scoped:
        for
          port   <- GrpcLoopback.serve(Answer.Store)
          off    <- open(port, RemoteMode.Off, dev)
          read   <- open(port, RemoteMode.Read, CacheNamespace.Main)
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
          reader  <- open(port, RemoteMode.Read, dev)
          writer  <- open(port, RemoteMode.Write, dev)
          missed  <- reader.find(action).either
          refused <- writer.store(action, body).either
        yield assertTrue(
          missed == Left(RemoteError.Rpc("UNAVAILABLE")),
          refused == Left(RemoteError.Rpc("UNAVAILABLE")),
        )
    ,
    test(
      "a server that accepts and never answers is a miss: the step runs its own work and its store does not stop it"
    ):
      val step   = Action(ActionId.trusted("a"), Nil, Nil)
      val digest = ActionDigest.of(step, Map.empty)
      ZIO.scoped:
        for
          port   <- GrpcLoopback.serve(Answer.Silent)
          remote <- GrpcCache.open(loopback(port), GrpcLoopback.Brief, RemoteMode.Write, dev)
          runner <- (ZLayer.succeed(Parallelism(1, 1, 1)) >>> Histos.live).build.map(_.get[Histos])
          runs   <- Ref.make(0)
          value  <- Scheduler(runner, ActionCache.miss, remote).cached(step, digest, runs.updateAndGet(_ + 1).as("ran"))
          called <- runs.get
          found  <- remote.find(digest).either
        yield assertTrue(
          value == "ran",
          called == 1,
          found == Left(RemoteError.Exchange(CacheExchange.ReadTimeout("127.0.0.1", port))),
        ),
  ) @@ TestAspect.timeout(30.seconds)

  private def open(port: Int, mode: RemoteMode, namespace: CacheNamespace): ZIO[Scope, RemoteError, RemoteCache] =
    GrpcCache.open(loopback(port), GrpcChannel.Idle, mode, namespace)

  private def loopback(port: Int): Target.Grpc =
    Target.Grpc("127.0.0.1", port, tls = false)
end GrpcCacheLawSpec
