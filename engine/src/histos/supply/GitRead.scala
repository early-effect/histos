package histos.supply

import histos.version.CommitSha

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.{UIO, ULayer, URIO, ZIO, ZLayer}

/** What the supply commands read from a checkout's git. A read git cannot answer is `None` or empty. */
trait GitRead:
  /** The GitHub repository `origin` names, from an https or an ssh remote. */
  def origin(repo: Path): UIO[Option[RepoName]]

  def head(repo: Path): UIO[Option[CommitSha]]

  def tip(repo: Path, branch: BranchName): UIO[Option[CommitSha]]

  /** The paths `git diff --name-only HEAD` lists. */
  def changed(repo: Path): UIO[List[String]]
end GitRead

object GitRead:
  val live: ULayer[GitRead] = ZLayer.succeed(Live)

  /** `declared`, the `GITHUB_REPOSITORY` a job sets, when it names a repository. Otherwise the one `origin` names. */
  def github(repo: Path, declared: Option[String]): URIO[GitRead, Option[RepoName]] =
    declared.flatMap(raw => RepoName.from(raw).toOption) match
      case Some(name) => ZIO.some(name)
      case None       => ZIO.serviceWithZIO[GitRead](_.origin(repo))

  /** `owner/name` from `https://github.com/owner/name.git` or `git@github.com:owner/name.git`. */
  private[supply] def repository(remote: String): Option[RepoName] =
    val text = remote.trim.stripSuffix(".git")
    val name =
      if text.contains("github.com/") then text.split("github.com/").lastOption.getOrElse("")
      else if text.contains(":") then text.split(":").lastOption.getOrElse("")
      else text
    RepoName.from(name).toOption

  private object Live extends GitRead:
    def origin(repo: Path): UIO[Option[RepoName]] =
      git(repo, "remote", "get-url", "origin").map(_.flatMap(repository))

    def head(repo: Path): UIO[Option[CommitSha]] =
      sha(repo, "HEAD")

    def tip(repo: Path, branch: BranchName): UIO[Option[CommitSha]] =
      sha(repo, branch.value)

    def changed(repo: Path): UIO[List[String]] =
      git(repo, "diff", "--name-only", "HEAD").map(
        _.toList.flatMap(_.split('\n').toList.map(_.trim).filter(_.nonEmpty))
      )

    private def sha(repo: Path, rev: String): UIO[Option[CommitSha]] =
      git(repo, "rev-parse", rev).map(_.flatMap(text => CommitSha.from(text.trim).toOption))

    /** The output of a git that exited zero. */
    private def git(repo: Path, args: String*): UIO[Option[String]] =
      ZIO
        .attemptBlockingInterrupt:
          val process = new ProcessBuilder((List("git", "-C", repo.toString) ++ args)*)
            .redirectError(ProcessBuilder.Redirect.DISCARD)
            .start()
          val text = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
          Option.when(process.waitFor() == 0)(text)
        .orElseSucceed(None)
  end Live
end GitRead
