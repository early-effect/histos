package histos.schedule

import java.net.{InetAddress, ServerSocket}

import zio.{Chunk, ZIO}
import zio.test.*

object GrpcCacheLawSpec extends ZIOSpecDefault:
  private val action = Digest.trusted("ab" * 32)
  private val body   = Chunk[Byte](1, 2, 9)
  private val dev    = CacheNamespace.Dev(DevName("law"))

  def spec = suite("grpc cache laws")(
    test("off stores nothing, a reader cannot write, and purge is not a remote-execution call"):
      ZIO.scoped {
        for
          port   <- closed
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
      }
    ,
    test("a closed port stops the lookup, and it is not a missing action"):
      ZIO.scoped {
        for
          port    <- closed
          reader  <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Read, dev)
          writer  <- GrpcCache.open("127.0.0.1", port, tls = false, RemoteMode.Write, dev)
          missed  <- reader.find(action).either
          refused <- writer.store(action, body).either
        yield assertTrue(
          missed == Left(RemoteError.Rpc("UNAVAILABLE")),
          refused == Left(RemoteError.Rpc("UNAVAILABLE")),
          writer.mode == RemoteMode.Write,
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
end GrpcCacheLawSpec
