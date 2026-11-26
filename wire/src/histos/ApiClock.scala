package histos

import histos.server.ServerError

import java.nio.file.Path

import zio.{UIO, ZIO}

/** The running jar against this checkout. Doctor reports a jar packed from a commit this checkout contains when that
  * commit is not HEAD, or the tree was dirty. A command does not print that line. A clean mismatch is
  * [[Admit.Rebuild]]: the JVM command packs this checkout and runs again on that jar.
  */
object ApiClock:
  enum Admit:
    case Go
    case Warn(line: String)

    /** `commit` is HEAD. The pack has to come out stamped with it. `line` is the doctor text. */
    case Rebuild(line: String, commit: String)
  end Admit

  /** `owned` means `identity.commit` is a commit in this checkout. The warning names that commit and HEAD. */
  def decide(owned: Boolean, identity: Option[ToolchainIdentity], head: Option[String]): Admit =
    if !owned then Admit.Go
    else
      (identity, head) match
        case (Some(found), Some(commit)) =>
          ToolchainSkew.warn(found, commit) match
            case Some(line) =>
              ToolchainSkew.rebuild(found, commit) match
                case Some(head) => Admit.Rebuild(line, head)
                case None       => Admit.Warn(line)
            case None => Admit.Go
        case _ => Admit.Go

  def admit(repo: Path): UIO[Admit] =
    val identity = ToolchainIdentity.loaded
    checkout(repo)
      .zip(owns(repo, identity.map(_.commit)))
      .map: (head, owned) =>
        decide(owned, identity, head)

  def versionLines(versionLine: String): String =
    val number = versionLine.stripPrefix("histos ").trim
    s"$versionLine\nserver $number"

  def rebootText(onto: String, was: String, socket: String): String =
    s"rebooting server onto $onto (was $was) at $socket"

  def stopText(was: String, socket: String): String =
    s"stopping server $was at $socket"

  def restart(socket: Path, wanted: String, got: String): ServerError =
    ServerError.Restart(socket, wanted, got)

  def mcpLine: UIO[String] =
    ZIO.succeed(mcpNote(ToolchainIdentity.loaded.map(_.coordinate)))

  def mcpNote(own: Option[String]): String =
    own match
      case Some(here) => s"mcp $here"
      case None       => "mcp has no toolchain id"

  /** True when `commit` is a commit object in `repo`. Another repository does not own a Histos pack. */
  private def owns(repo: Path, commit: Option[String]): UIO[Boolean] =
    commit match
      case None      => ZIO.succeed(false)
      case Some(sha) =>
        ZIO
          .attemptBlocking:
            val process = new ProcessBuilder("git", "-C", repo.toString, "cat-file", "-e", s"$sha^{commit}")
              .redirectError(ProcessBuilder.Redirect.DISCARD)
              .redirectOutput(ProcessBuilder.Redirect.DISCARD)
              .start()
            process.waitFor() == 0
          .orElseSucceed(false)

  private def checkout(repo: Path): UIO[Option[String]] =
    ZIO
      .attemptBlocking:
        val command = List("git", "-C", repo.toString, "rev-parse", "HEAD")
        val process = new ProcessBuilder(command*)
          .redirectError(ProcessBuilder.Redirect.DISCARD)
          .start()
        val text = String(process.getInputStream.readAllBytes, java.nio.charset.StandardCharsets.UTF_8).trim
        if process.waitFor() == 0 && text.nonEmpty then Some(text) else None
      .orElseSucceed(None)
end ApiClock
