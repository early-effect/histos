package histos

import histos.server.ServerError

import java.nio.file.{Files, Path}

import zio.{UIO, ZIO}

/** The local toolchain against this checkout. A pointer at `.histos/toolchain` that was built from another commit, or
  * from a dirty tree, warns. The command runs. There is no stop and no hatch.
  */
object ApiClock:
  val pointerName: String = "toolchain"

  enum Admit:
    case Go
    case Warn(line: String)

  def pointer(repo: Path): Path =
    repo.resolve(".histos").resolve(pointerName)

  /** `pointed` is `.histos/toolchain`. The warning names the packed commit and this checkout. */
  def decide(pointed: Boolean, identity: Option[ToolchainIdentity], head: Option[String]): Admit =
    if !pointed then Admit.Go
    else
      (identity, head) match
        case (Some(found), Some(commit)) =>
          ToolchainSkew.warn(found, commit) match
            case Some(line) => Admit.Warn(line)
            case None       => Admit.Go
        case _ => Admit.Go

  def admit(repo: Path): UIO[Admit] =
    pointed(repo)
      .zip(checkout(repo))
      .map: (marked, head) =>
        decide(marked, ToolchainIdentity.loaded, head)

  def versionLines(versionLine: String): String =
    val number = versionLine.stripPrefix("histos ").trim
    s"$versionLine\nserver $number"

  def rebootText(onto: String, was: String, socket: String): String =
    s"rebooting server onto $onto (was $was) at $socket"

  def restart(socket: Path, wanted: String, got: String): ServerError =
    ServerError.Restart(socket, wanted, got)

  def mcpLine: UIO[String] =
    ZIO.succeed(mcpNote(ToolchainIdentity.loaded.map(_.coordinate)))

  def mcpNote(own: Option[String]): String =
    own match
      case Some(here) => s"mcp $here"
      case None       => "mcp has no toolchain id"

  private def pointed(repo: Path): UIO[Boolean] =
    ZIO.attemptBlocking(Files.isRegularFile(pointer(repo))).orElseSucceed(false)

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
