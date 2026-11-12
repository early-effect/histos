package histos.client

import histos.cli.CliExit

/** How the native client ends after a fatal error, such as a `StackOverflowError`: `line` on stderr, then `exit`. */
final case class Fatal(line: String, exit: CliExit)

object Fatal:
  def of(fatal: Throwable): Fatal =
    Fatal(s"the histos client stopped on a fatal error: $fatal", CliExit.Failed)
