package histos.cli

import scala.jdk.OptionConverters.*

import zio.{IO, System, ZIO}

/** The shell a completion script is written for. */
enum ShellKind:
  case Bash
  case Zsh

  def token: String = this match
    case Bash => "bash"
    case Zsh  => "zsh"
end ShellKind

object ShellKind:
  def parse(raw: String): Option[ShellKind] =
    raw match
      case "bash" => Some(Bash)
      case "zsh"  => Some(Zsh)
      case _      => None

/** Why `histos completion` cannot pick a shell. The message names both explicit forms. */
enum ShellFault:
  case Unknown
  case Rejected(raw: String)

  def message: String = this match
    case Unknown =>
      "histos completion needs zsh or bash\nhistos completion zsh\nhistos completion bash"
    case Rejected(raw) =>
      s"HISTOS_SHELL '$raw' is not zsh or bash\nhistos completion zsh\nhistos completion bash"
end ShellFault

/** `histos completion` prints a script, or writes it and prints the rc lines. */
enum CompletionRequest:
  case Show(chosen: Option[ShellKind])
  case Install(chosen: Option[ShellKind])

  def shell: Option[ShellKind] = this match
    case Show(chosen)    => chosen
    case Install(chosen) => chosen
end CompletionRequest

object CompletionRequest:
  def parse(args: List[String]): Either[CliParseError, CompletionRequest] =
    args match
      case Nil                        => Right(CompletionRequest.Show(None))
      case "zsh" :: Nil               => Right(CompletionRequest.Show(Some(ShellKind.Zsh)))
      case "bash" :: Nil              => Right(CompletionRequest.Show(Some(ShellKind.Bash)))
      case "install" :: Nil           => Right(CompletionRequest.Install(None))
      case "install" :: "zsh" :: Nil  => Right(CompletionRequest.Install(Some(ShellKind.Zsh)))
      case "install" :: "bash" :: Nil =>
        Right(CompletionRequest.Install(Some(ShellKind.Bash)))
      case _ =>
        Left(CliParseError.Pattern("histos completion [install] [zsh|bash]\nhistos help completion"))

/** Which shell `histos completion` prints for. An explicit argument wins. A set `HISTOS_SHELL` is trusted or rejected.
  * Otherwise the parent command, then `SHELL`.
  */
object ShellChoice:
  def decide(
      explicit: Option[ShellKind],
      histosShell: Option[String],
      parent: Option[String],
      shellEnv: Option[String],
  ): Either[ShellFault, ShellKind] =
    explicit match
      case Some(kind) => Right(kind)
      case None       =>
        named(histosShell) match
          case Some(raw) =>
            ShellKind.parse(raw) match
              case Some(kind) => Right(kind)
              case None       => Left(ShellFault.Rejected(raw))
          case None =>
            basename(parent).flatMap(ShellKind.parse) match
              case Some(kind) => Right(kind)
              case None       =>
                basename(shellEnv).flatMap(ShellKind.parse) match
                  case Some(kind) => Right(kind)
                  case None       => Left(ShellFault.Unknown)

  def live(explicit: Option[ShellKind]): IO[ShellFault, ShellKind] =
    for
      recorded <- System.env("HISTOS_SHELL").orElseSucceed(None)
      shell    <- System.env("SHELL").orElseSucceed(None)
      parent   <- ZIO.succeed(parentCommand)
      kind     <- ZIO.fromEither(decide(explicit, recorded, parent, shell))
    yield kind

  private def parentCommand: Option[String] =
    ProcessHandle.current().parent().toScala.flatMap(_.info().command().toScala)

  private def named(raw: Option[String]): Option[String] =
    raw.map(_.trim).filter(_.nonEmpty)

  private def basename(raw: Option[String]): Option[String] =
    named(raw).map: text =>
      val slash = math.max(text.lastIndexOf('/'), text.lastIndexOf('\\'))
      val file  = if slash < 0 then text else text.substring(slash + 1)
      if file.startsWith("-") then file.drop(1) else file
end ShellChoice
