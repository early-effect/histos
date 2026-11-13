package histos.server

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}

import zio.{UIO, ZIO}

/** When `.histos/server.json` was written. A connect that succeeds at bind is earlier than the new server's write, so a
  * stamp from before that write is not the new server.
  */
object ServerStamp:
  def file(repo: Path): Path =
    repo.resolve(".histos").resolve("server.json")

  def note(repo: Path): UIO[Option[FileTime]] =
    ZIO.attemptBlocking(time(file(repo))).orElseSucceed(None)

  /** `now` was written after `before`. No file is not a new server. A file that was already there is not either. */
  def writtenAfter(before: Option[FileTime], now: Option[FileTime]): Boolean =
    now match
      case None        => false
      case Some(found) =>
        before match
          case None        => true
          case Some(prior) => found.compareTo(prior) > 0

  private def time(path: Path): Option[FileTime] =
    if Files.isRegularFile(path) then Some(Files.getLastModifiedTime(path)) else None
end ServerStamp
