package histos.server

import java.nio.file.Path

import zio.{IO, UIO, ZIO, durationInt}

/** Poll until `connect` accepts. A dead child fails with its detail and does not sleep out the remaining attempts. */
object ServerWait:
  def awaitListening(
      connect: IO[ServerError, Unit],
      alive: UIO[Boolean],
      detail: UIO[String],
      socket: Path,
      attempts: Int,
  ): IO[ServerError, Unit] =
    alive.flatMap:
      case false => detail.flatMap(text => ZIO.fail(ServerError.Exited(text)))
      case true  =>
        connect.catchAll:
          case ServerError.Unreachable(_) if attempts > 0 =>
            ZIO.sleep(50.millis) *> awaitListening(connect, alive, detail, socket, attempts - 1)
          case ServerError.Unreachable(_) =>
            ZIO.fail(ServerError.Unreachable(socket))
          case other =>
            ZIO.fail(other)
