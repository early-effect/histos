package histos.publish

import histos.version.{CommitSha, VersionTask}
import histos.workspace.Workspace

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.ZIO

/** A commit a publish law can name. The sha is the one git stored. */
object LawGit:
  def commit(repo: Path, branch: String = "topic"): ZIO[Any, Nothing, CommitSha] =
    ZIO.attemptBlocking(prepare(repo, branch)).foldZIO(blow, shaOf)

  def emptyReleases(repo: Path): ZIO[Any, Nothing, Unit] =
    Workspace.writeString(VersionTask.path(repo), "").catchAll(error => ZIO.dieMessage(error.message))

  def ready(repo: Path): ZIO[Any, Nothing, CommitSha] =
    commit(repo) <* emptyReleases(repo)

  /** `origin/HEAD` names `branch`, so a checkout of that branch is the default branch. */
  def recordDefault(repo: Path, branch: String): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking(point(repo, branch)).foldZIO(blow, done)

  private def blow(error: Throwable): ZIO[Any, Nothing, Nothing] =
    ZIO.dieMessage(Option(error.getMessage).getOrElse(error.toString))

  private def shaOf(found: Either[String, String]): ZIO[Any, Nothing, CommitSha] =
    found match
      case Left(text) => ZIO.dieMessage(text)
      case Right(raw) =>
        CommitSha.from(raw) match
          case Left(error) => ZIO.dieMessage(error.message)
          case Right(sha)  => ZIO.succeed(sha)

  private def done(found: Either[String, Unit]): ZIO[Any, Nothing, Unit] =
    found match
      case Left(text) => ZIO.dieMessage(text)
      case Right(_)   => ZIO.unit

  private def prepare(repo: Path, branch: String): Either[String, String] =
    for
      _   <- run(repo, "init", "-b", branch)
      _   <- run(repo, "config", "user.email", "law@example.com")
      _   <- run(repo, "config", "user.name", "Law")
      _   <- writeReady(repo)
      _   <- run(repo, "add", "ready")
      _   <- run(repo, "commit", "--no-gpg-sign", "-m", "ready")
      raw <- run(repo, "rev-parse", "HEAD")
    yield raw.trim

  private def point(repo: Path, branch: String): Either[String, Unit] =
    for
      _ <- run(repo, "update-ref", s"refs/remotes/origin/$branch", "HEAD")
      _ <- run(repo, "symbolic-ref", "refs/remotes/origin/HEAD", s"refs/remotes/origin/$branch")
    yield ()

  private def writeReady(repo: Path): Either[String, Unit] =
    try
      Files.writeString(repo.resolve("ready"), "ready\n")
      Right(())
    catch case error: java.io.IOException => Left(Option(error.getMessage).getOrElse(error.toString))

  private def run(repo: Path, args: String*): Either[String, String] =
    try
      val process = new ProcessBuilder((List("git", "-C", repo.toString) ++ args).asJava)
        .redirectError(ProcessBuilder.Redirect.DISCARD)
        .start()
      val text = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
      val code = process.waitFor()
      if code == 0 then Right(text) else Left(s"git ${args.mkString(" ")} exited $code")
    catch case error: java.io.IOException => Left(Option(error.getMessage).getOrElse(error.toString))
end LawGit
