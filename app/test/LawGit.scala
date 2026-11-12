package histos

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import _root_.scala.jdk.CollectionConverters.*

import zio.{UIO, ZIO}

/** Git for a law's own repository. It runs without the global or system config, and a commit is never signed, so the
  * developer's signing, hooks, identity, and default branch do not change the result. A nonzero exit is a defect of the
  * law's setup.
  */
object LawGit:
  def git(dir: Path, args: String*): UIO[String] =
    ZIO
      .attemptBlocking:
        val builder = ProcessBuilder((List("git", "-C", dir.toString) ++ args).asJava).redirectErrorStream(true)
        builder.environment().put("GIT_CONFIG_GLOBAL", "/dev/null")
        builder.environment().put("GIT_CONFIG_NOSYSTEM", "1")
        val process = builder.start()
        val text    = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
        (process.waitFor(), text)
      .orDie
      .flatMap: (code, text) =>
        if code == 0 then ZIO.succeed(text)
        else ZIO.dieMessage(s"git ${args.mkString(" ")} exited $code: $text")

  /** A repository on `branch` whose HEAD commits the files already in `dir` that `paths` name. */
  def commit(dir: Path, branch: String, paths: String*): UIO[Unit] =
    for
      _ <- git(dir, "init", "-b", branch)
      _ <- git(dir, ("add" :: paths.toList)*)
      _ <- git(
        dir,
        "-c",
        "user.email=law@example.com",
        "-c",
        "user.name=Law",
        "commit",
        "--no-gpg-sign",
        "--no-verify",
        "-m",
        "law",
      )
    yield ()
end LawGit
