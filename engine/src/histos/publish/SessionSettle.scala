package histos.publish

import histos.version.{Branch, Catalog, CommitSha, Session, SessionSource}

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** A development session becomes the snapshot at HEAD before a local or snapshot write. `publish snapshots` then moves
  * the pointer from any checkout. A local write moves it only on the recorded default branch. A pull request stays. A
  * release stays, so a local write of that session still writes the release number. A release write does not settle. An
  * empty catalog is not a publish, so it is not asked for a commit.
  */
object SessionSettle:
  def prepare(
      repo: Path,
      catalog: Catalog,
      env: Map[String, String],
      write: PublishWrite,
  ): IO[LocalPublishError, Map[String, String]] =
    catalog match
      case Catalog.Empty    => ZIO.succeed(env)
      case Catalog.Lines(_) =>
        val settled =
          if settle(source(env), write) then read(repo).map(head => snapshot(env, head))
          else ZIO.succeed(env)
        settled.map(found => if write == PublishWrite.Snapshots then movePointer(found) else found)

  /** The unique coordinate stays. The pointer is the latest snapshot this command wrote. A pull request has none. */
  private def movePointer(env: Map[String, String]): Map[String, String] =
    Session.from(source(env)) match
      case Right(Session.Snapshot(_, _)) => env.updated(Session.BranchKey, "default")
      case _                             => env

  /** Development, and only a local or snapshot write. An incomplete session stays, so its own error is reported. */
  private[publish] def settle(source: SessionSource, write: PublishWrite): Boolean =
    (Session.from(source), write) match
      case (Right(Session.Development), PublishWrite.Local | PublishWrite.Snapshots) => true
      case _                                                                         => false

  private def source(env: Map[String, String]): SessionSource =
    SessionSource(
      env.get(Session.Key),
      env.get(Session.ShaKey),
      env.get(Session.BranchKey),
      env.get(Session.PullRequestKey),
    )

  private def snapshot(env: Map[String, String], head: Head): Map[String, String] =
    env
      .updated(Session.Key, "snapshot")
      .updated(Session.ShaKey, head.sha.value)
      .updated(Session.BranchKey, token(head.branch))

  private def token(branch: Branch): String =
    branch match
      case Branch.Default => "default"
      case Branch.Other   => "other"

  private final case class Head(sha: CommitSha, branch: Branch)

  private def read(repo: Path): IO[LocalPublishError, Head] =
    ZIO
      .attemptBlocking(capture(repo))
      .mapError(_ => LocalPublishError.NoCommit)
      .flatMap:
        case Some(head) => ZIO.succeed(head)
        case None       => ZIO.fail(LocalPublishError.NoCommit)

  private def capture(repo: Path): Option[Head] =
    one(repo, "rev-parse", "HEAD").flatMap: raw =>
      CommitSha.from(raw).toOption.map(sha => Head(sha, place(repo)))

  /** A local write moves the pointer only when this checkout is the recorded default branch. */
  private def place(repo: Path): Branch =
    val current = one(repo, "rev-parse", "--abbrev-ref", "HEAD")
    val main    = one(repo, "rev-parse", "--abbrev-ref", "origin/HEAD").flatMap(drop("origin/"))
    (current, main) match
      case (Some(name), Some(expected)) if name == expected => Branch.Default
      case _                                                => Branch.Other

  private def drop(prefix: String)(ref: String): Option[String] =
    if ref.startsWith(prefix) && ref.length > prefix.length && ref != s"${prefix}HEAD" then
      Some(ref.drop(prefix.length))
    else None

  private def one(repo: Path, args: String*): Option[String] =
    val process = new ProcessBuilder((List("git", "-C", repo.toString) ++ args).asJava)
      .redirectError(ProcessBuilder.Redirect.DISCARD)
      .start()
    val text = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8).trim
    if process.waitFor() == 0 && text.nonEmpty && !text.contains('\n') then Some(text) else None
end SessionSettle
