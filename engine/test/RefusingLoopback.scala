package histos.publish

import java.net.{InetAddress, ServerSocket}

import zio.{Ref, Scope, UIO, ZIO}

/** A loopback port the law holds for its scope. Each connection is counted and closed before a byte is read, so a
  * request through [[url]] fails as a transport error and nothing leaves the machine.
  */
final case class RefusingLoopback(url: String, connections: UIO[Int])

object RefusingLoopback:
  val bound: ZIO[Scope, Nothing, RefusingLoopback] =
    for
      seen   <- Ref.make(0)
      socket <- ZIO.acquireRelease(
        ZIO.attemptBlocking(ServerSocket(0, 50, InetAddress.getLoopbackAddress)).orDie
      )(socket => ZIO.attemptBlocking(socket.close()).ignore)
      _ <- refuse(socket, seen).forkScoped
    yield RefusingLoopback(s"http://127.0.0.1:${socket.getLocalPort}", seen.get)

  /** Interrupting the accept closes the socket, which ends the loop. */
  private def refuse(socket: ServerSocket, seen: Ref[Int]): UIO[Unit] =
    ZIO
      .attemptBlockingCancelable(socket.accept())(ZIO.attemptBlocking(socket.close()).ignore)
      .foldZIO(
        _ => ZIO.unit,
        connection => ZIO.attemptBlocking(connection.close()).ignore *> seen.update(_ + 1) *> refuse(socket, seen),
      )
end RefusingLoopback
