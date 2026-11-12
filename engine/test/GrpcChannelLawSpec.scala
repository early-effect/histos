package histos.schedule

import histos.schedule.CacheConfig.Target
import histos.schedule.GrpcLoopback.Answer

import io.grpc.Status

import zio.{Chunk, ZIO, durationInt}
import zio.test.*

/** Each call shape against a loopback server bound for the law. The law's timeout bounds a call that never closes. */
object GrpcChannelLawSpec extends ZIOSpecDefault:
  def spec = suite("grpc channel laws")(
    test("an unavailable answer fails every call shape, and so does tls against a plaintext server"):
      ZIO.scoped:
        for
          port  <- GrpcLoopback.serve(Answer.Fixed(Status.Code.UNAVAILABLE))
          plain <- GrpcChannel.open(Target.Grpc("127.0.0.1", port, tls = false), GrpcChannel.Idle)
          unary <- plain.unary(GrpcChannel.getAction, Chunk.single(1.toByte)).either
          many  <- plain.serverStream(GrpcChannel.read, Chunk.single(2.toByte)).either
          sent  <- plain.clientStream(GrpcChannel.write, Chunk(Chunk.single(3.toByte), Chunk.single(4.toByte))).either
          empty <- plain.clientStream(GrpcChannel.write, Chunk.empty).either
          open  <- GrpcLoopback.serve(Answer.Store)
          tls   <- GrpcChannel.open(Target.Grpc("127.0.0.1", open, tls = true), GrpcChannel.Idle)
          coded <- tls.unary(GrpcChannel.updateAction, Chunk.empty).either
        yield assertTrue(
          unary == Left(RemoteError.Rpc("UNAVAILABLE")),
          many == Left(RemoteError.Rpc("UNAVAILABLE")),
          sent == Left(RemoteError.Rpc("UNAVAILABLE")),
          empty == Left(RemoteError.Rpc("UNAVAILABLE")),
          coded == Left(RemoteError.Rpc("UNAVAILABLE")),
        )
    ,
    test("not found is no answer for a lookup and a read, and a failure for a write"):
      ZIO.scoped:
        for
          port  <- GrpcLoopback.serve(Answer.Fixed(Status.Code.NOT_FOUND))
          plain <- GrpcChannel.open(Target.Grpc("127.0.0.1", port, tls = false), GrpcChannel.Idle)
          unary <- plain.unary(GrpcChannel.getAction, Chunk.single(1.toByte))
          many  <- plain.serverStream(GrpcChannel.read, Chunk.single(2.toByte))
          sent  <- plain.clientStream(GrpcChannel.write, Chunk(Chunk.single(3.toByte))).either
        yield assertTrue(
          unary == None,
          many == None,
          sent == Left(RemoteError.Rpc("NOT_FOUND")),
        )
    ,
    test("a server that accepts and never answers ends every call shape, and the end is a timeout"):
      ZIO.scoped:
        for
          port  <- GrpcLoopback.serve(Answer.Silent)
          plain <- GrpcChannel.open(Target.Grpc("127.0.0.1", port, tls = false), GrpcLoopback.Brief)
          unary <- plain.unary(GrpcChannel.getAction, Chunk.single(1.toByte)).either
          many  <- plain.serverStream(GrpcChannel.read, Chunk.single(2.toByte)).either
          sent  <- plain.clientStream(GrpcChannel.write, Chunk(Chunk.single(3.toByte), Chunk.single(4.toByte))).either
        yield
          val stalled = Left(RemoteError.Exchange(CacheExchange.ReadTimeout("127.0.0.1", port)))
          assertTrue(unary == stalled, many == stalled, sent == stalled),
  ) @@ TestAspect.timeout(30.seconds)
