package histos.workflow

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/** The short name written into `on.push.branches`. The filter is a static glob, evaluated before `github` exists, so
  * the name is resolved when the workflow is rendered. It is not a `github` expression and not `$default-branch`.
  */
opaque type CiBranch = String

object CiBranch:
  def apply(name: String): CiBranch = name

  extension (branch: CiBranch) def name: String = branch

  /** Local `refs/remotes/origin/HEAD` when the checkout recorded it. A shallow pull-request checkout often has not, so
    * the remote is asked next: `git remote set-head origin --auto`, then `git ls-remote --symref origin HEAD`.
    */
  def read(repo: Path): IO[CiError, CiBranch] =
    ZIO
      .attemptBlocking(resolve(repo))
      .mapError(_ => CiError.NoDefaultBranch)
      .flatMap:
        case Some(name) => ZIO.succeed(CiBranch(name))
        case None       => ZIO.fail(CiError.NoDefaultBranch)

  /** The `ref:` line of `git ls-remote --symref origin HEAD`. */
  private[workflow] def headFromLsRemote(text: String): Option[String] =
    val prefix = "ref: refs/heads/"
    text.linesIterator
      .map(_.trim)
      .collectFirst:
        case line if line.startsWith(prefix) =>
          line.drop(prefix.length).takeWhile(char => char != '\t' && char != ' ')
      .filter(name => name.nonEmpty && name != "HEAD")

  private def resolve(repo: Path): Option[String] =
    local(repo)
      .orElse(asked(repo))
      .orElse(capture(repo, "ls-remote", "--symref", "origin", "HEAD").flatMap(headFromLsRemote))

  private def asked(repo: Path): Option[String] =
    capture(repo, "remote", "set-head", "origin", "--auto").flatMap(_ => local(repo))

  private def local(repo: Path): Option[String] =
    one(repo, "symbolic-ref", "refs/remotes/origin/HEAD")
      .flatMap(drop("refs/remotes/origin/"))
      .orElse(one(repo, "rev-parse", "--abbrev-ref", "origin/HEAD").flatMap(drop("origin/")))

  private def drop(prefix: String)(ref: String): Option[String] =
    if ref.startsWith(prefix) && ref.length > prefix.length && ref != s"${prefix}HEAD" then
      Some(ref.drop(prefix.length))
    else None

  private def one(repo: Path, args: String*): Option[String] =
    capture(repo, args*).flatMap: text =>
      val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
      lines match
        case line :: Nil => Some(line)
        case _           => None

  private def capture(repo: Path, args: String*): Option[String] =
    val process = new ProcessBuilder((List("git", "-C", repo.toString) ++ args).asJava)
      .redirectError(ProcessBuilder.Redirect.DISCARD)
      .start()
    val text = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
    if process.waitFor() == 0 then Some(text) else None
end CiBranch
