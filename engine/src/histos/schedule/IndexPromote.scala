package histos.schedule

import histos.Promote

import zio.ZIO

/** `Rebuild` does not run the copy. `CopyIndex` does, and that copy does not compile. */
object IndexPromote:
  def run[R](strategy: Promote, copy: ZIO[R, RemoteError, Unit]): ZIO[R, RemoteError, Unit] =
    strategy match
      case Promote.Rebuild   => ZIO.unit
      case Promote.CopyIndex => copy
