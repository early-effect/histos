package histos.publish

import histos.version.{CommitSha, VersionTask}
import histos.workspace.Workspace

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.ZIO

/** A commit a law can name. The sha is the one git stored. Git runs without the global or system config, so the
  * developer's signing, hooks, and default branch do not change the result.
  */
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

  /** A bare repository at `root/origin.git` whose HEAD is `branch` and holds one commit. */
  def origin(root: Path, branch: String): ZIO[Any, Nothing, Path] =
    val work = root.resolve("work")
    val bare = root.resolve("origin.git")
    for
      _ <- Workspace.createDirectories(work).catchAll(error => ZIO.dieMessage(error.message))
      _ <- commit(work, branch)
      _ <- git(root, "clone", "--bare", work.toString, bare.toString)
    yield bare

  /** `git` in `dir`. A nonzero exit is a defect of the law's setup. */
  def git(dir: Path, args: String*): ZIO[Any, Nothing, String] =
    ZIO
      .attemptBlocking(run(dir, args*))
      .foldZIO(
        blow,
        {
          case Left(text) => ZIO.dieMessage(text)
          case Right(out) => ZIO.succeed(out)
        },
      )

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
      val builder = new ProcessBuilder((List("git", "-C", repo.toString) ++ args).asJava)
        .redirectError(ProcessBuilder.Redirect.DISCARD)
      builder.environment().put("GIT_CONFIG_GLOBAL", "/dev/null")
      builder.environment().put("GIT_CONFIG_NOSYSTEM", "1")
      val process = builder.start()
      val text    = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
      val code    = process.waitFor()
      if code == 0 then Right(text) else Left(s"git ${args.mkString(" ")} exited $code")
    catch case error: java.io.IOException => Left(Option(error.getMessage).getOrElse(error.toString))
end LawGit
