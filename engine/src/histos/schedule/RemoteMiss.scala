package histos.schedule

import zio.{IO, ZIO}

/** A connect or read timeout is a miss. Every other remote failure stays a failure. */
private[histos] object RemoteMiss:
  def unanswered[A](call: IO[RemoteError, A])(missed: A): IO[RemoteError, A] =
    call.catchSome:
      case RemoteError.Exchange(CacheExchange.ConnectTimeout(_, _) | CacheExchange.ReadTimeout(_, _)) =>
        ZIO.succeed(missed)
