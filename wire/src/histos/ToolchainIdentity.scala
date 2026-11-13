package histos

import histos.version.CommitSha

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.json.*
import zio.{UIO, ZIO}

/** The ship coordinate, the full commit, and whether that commit was dirty, packed into the launcher jar. */
final case class ToolchainIdentity(coordinate: String, commit: String, dirty: Boolean) derives JsonCodec

object ToolchainIdentity:
  val entry: String = "META-INF/histos/toolchain.json"

  /** None when this checkout has no toolchain ship, no commit, or git does not answer. */
  def discover(repo: Path): UIO[Option[ToolchainIdentity]] =
    SnapshotId
      .current(repo, SnapshotId.head)
      .flatMap:
        case None             => ZIO.none
        case Some(coordinate) =>
          git(repo, List("rev-parse", "HEAD")).flatMap:
            case None      => ZIO.none
            case Some(raw) =>
              CommitSha.from(raw.trim) match
                case Left(_)    => ZIO.none
                case Right(sha) =>
                  git(repo, List("status", "--porcelain")).map: status =>
                    status.map(text => ToolchainIdentity(coordinate, sha.value, text.trim.nonEmpty))

  def coordinateOf(text: String): Option[String] =
    text.fromJson[ToolchainIdentity] match
      case Right(found) => Some(found.coordinate)
      case Left(_)      => None

  private def git(repo: Path, args: List[String]): UIO[Option[String]] =
    ZIO
      .attemptBlocking:
        val command = "git" :: "-C" :: repo.toString :: args
        val process = new ProcessBuilder(command*)
          .redirectError(ProcessBuilder.Redirect.DISCARD)
          .start()
        val text = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
        if process.waitFor() == 0 then Some(text) else None
      .orElseSucceed(None)
end ToolchainIdentity
