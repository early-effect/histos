package histos.schedule

import java.net.{InetAddress, ServerSocket}

import io.grpc.MethodDescriptor

import zio.{Chunk, ZIO}
import zio.test.*

object GrpcChannelLawSpec extends ZIOSpecDefault:
  def spec = suite("grpc channel laws")(
    test("the four cache methods keep their proto names and their call shapes"):
      assertTrue(
        GrpcChannel.MaxInbound == 64 * 1024 * 1024,
        GrpcChannel.getAction.getFullMethodName == Reapi.GetActionResult,
        GrpcChannel.updateAction.getFullMethodName == Reapi.UpdateActionResult,
        GrpcChannel.findMissing.getFullMethodName == Reapi.FindMissingBlobs,
        GrpcChannel.read.getFullMethodName == Reapi.Read,
        GrpcChannel.write.getFullMethodName == Reapi.Write,
        GrpcChannel.getAction.getType == MethodDescriptor.MethodType.UNARY,
        GrpcChannel.updateAction.getType == MethodDescriptor.MethodType.UNARY,
        GrpcChannel.findMissing.getType == MethodDescriptor.MethodType.UNARY,
        GrpcChannel.read.getType == MethodDescriptor.MethodType.SERVER_STREAMING,
        GrpcChannel.write.getType == MethodDescriptor.MethodType.CLIENT_STREAMING,
      )
    ,
    test("a closed port is a refused call, not a missing action"):
      ZIO.scoped {
        for
          port  <- closed
          plain <- GrpcChannel.open("127.0.0.1", port, tls = false)
          unary <- plain.unary(GrpcChannel.getAction, Chunk.single(1.toByte)).either
          many  <- plain.serverStream(GrpcChannel.read, Chunk.single(2.toByte)).either
          sent  <- plain.clientStream(GrpcChannel.write, Chunk(Chunk.single(3.toByte), Chunk.single(4.toByte))).either
          empty <- plain.clientStream(GrpcChannel.write, Chunk.empty).either
          tls   <- GrpcChannel.open("127.0.0.1", port, tls = true)
          coded <- tls.unary(GrpcChannel.updateAction, Chunk.empty).either
        yield assertTrue(
          unary == Left(RemoteError.Rpc("UNAVAILABLE")),
          many == Left(RemoteError.Rpc("UNAVAILABLE")),
          sent == Left(RemoteError.Rpc("UNAVAILABLE")),
          empty == Left(RemoteError.Rpc("UNAVAILABLE")),
          coded == Left(RemoteError.Rpc("UNAVAILABLE")),
        )
      },
  )

  private def closed: ZIO[Any, Nothing, Int] =
    ZIO
      .attemptBlocking {
        val socket = new ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"))
        val port   = socket.getLocalPort
        socket.close()
        port
      }
      .catchAll(error => ZIO.dieMessage(error.getClass.getName))
end GrpcChannelLawSpec
