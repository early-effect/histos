package histos

import histos.version.{CommitSha, ShipNumber, SnapshotCoordinate}

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.{UIO, ZIO}

/** The revision a toolchain ship publishes: `<ship>-<12 hex>-SNAPSHOT`. The ship number comes from the loaded catalog.
  * The commit comes from git.
  */
object SnapshotId:
  /** `git rev-parse HEAD` in `repo`. None when git fails or the checkout has no commit. */
  def head(repo: Path): UIO[Option[CommitSha]] =
    ZIO
      .attemptBlocking {
        val process = new ProcessBuilder("git", "-C", repo.toString, "rev-parse", "HEAD")
          .redirectError(ProcessBuilder.Redirect.DISCARD)
          .start()
        val text = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8).trim
        if process.waitFor() == 0 then CommitSha.from(text).toOption else None
      }
      .orElseSucceed(None)

  def render(number: ShipNumber, sha: CommitSha): String =
    SnapshotCoordinate(number, sha.abbreviate).render
end SnapshotId
