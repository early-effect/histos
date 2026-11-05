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

  /** `refs/remotes/origin/HEAD`, then `origin/HEAD`. The short name keeps a slash that belongs to the branch. */
  def read(repo: Path): IO[CiError, CiBranch] =
    ZIO.attemptBlocking(fromGit(repo)).mapError(_ => CiError.NoDefaultBranch).flatMap:
      case Some(name) => ZIO.succeed(CiBranch(name))
      case None       => ZIO.fail(CiError.NoDefaultBranch)

  private def fromGit(repo: Path): Option[String] =
    run(repo, "symbolic-ref", "refs/remotes/origin/HEAD")
      .flatMap(drop("refs/remotes/origin/"))
      .orElse(run(repo, "rev-parse", "--abbrev-ref", "origin/HEAD").flatMap(drop("origin/")))

  private def drop(prefix: String)(ref: String): Option[String] =
    if ref.startsWith(prefix) && ref.length > prefix.length && ref != s"${prefix}HEAD" then
      Some(ref.drop(prefix.length))
    else None

  private def run(repo: Path, args: String*): Option[String] =
    val process = new ProcessBuilder((List("git", "-C", repo.toString) ++ args).asJava)
      .redirectError(ProcessBuilder.Redirect.DISCARD)
      .start()
    val text = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8).trim
    if process.waitFor() == 0 && text.nonEmpty && !text.contains('\n') then Some(text) else None
end CiBranch
