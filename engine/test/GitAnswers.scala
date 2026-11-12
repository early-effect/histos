package histos.supply

import histos.version.CommitSha

import java.nio.file.Path

import zio.{UIO, ULayer, ZIO, ZLayer}

/** A checkout's git, answered from fixed values. A law names only the answers it reads. */
final case class GitAnswers(
    remote: Option[RepoName] = None,
    commit: Option[CommitSha] = None,
    diff: List[String] = Nil,
) extends GitRead:
  def origin(repo: Path): UIO[Option[RepoName]] = ZIO.succeed(remote)

  def head(repo: Path): UIO[Option[CommitSha]] = ZIO.succeed(commit)

  def tip(repo: Path, branch: BranchName): UIO[Option[CommitSha]] = ZIO.none

  def changed(repo: Path): UIO[List[String]] = ZIO.succeed(diff)

  def layer: ULayer[GitRead] = ZLayer.succeed(this)
end GitAnswers
