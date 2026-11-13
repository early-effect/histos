package histos.cli

import histos.doctor.{Diagnostic, Fix, Level}

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A parse error is a sentence and a repair. The grammar only builds the case. A case that forwards another error's
  * message, or echoes the text it was built with, has no law here.
  */
object CliParseLawSpec extends ZIOSpecDefault:
  def spec = suite("cli parse error laws")(
    test("every parse error names its sentence and the repair"):
      val helpNote       = CliParseError.Help.diagnose
      val emptyNote      = CliParseError.UnknownCommand("", Nil).diagnose
      val oneNote        = CliParseError.UnknownCommand("compil", List("compile")).diagnose
      val manyNote       = CliParseError.UnknownCommand("comp", List("compile", "coverage")).diagnose
      val rootEmptyNote  = CliParseError.UnknownRoot("").diagnose
      val rootNote       = CliParseError.UnknownRoot("nope").diagnose
      val rejectedNote   = CliParseError.Rejected.diagnose
      val missingNote    = CliParseError.MissingArgument.diagnose
      val unexpectedNote = CliParseError.UnexpectedArguments.diagnose
      assertTrue(
        helpNote.check.value == "cli.help",
        helpNote.level == Level.Error,
        helpNote.summary == "histos help",
        command(helpNote) == Some("histos help"),
        emptyNote.check.value == "cli.unknown-command",
        emptyNote.summary == "unknown command",
        emptyNote.explain == "histos help lists the commands.\nhistos help <command> prints one.",
        command(emptyNote) == Some("histos help"),
        oneNote.summary == "unknown command 'compil'",
        oneNote.explain == "did you mean compile?\nhistos help\nhistos help compile",
        command(oneNote) == Some("histos help compile"),
        manyNote.summary == "unknown command 'comp'",
        manyNote.explain == "did you mean compile or coverage?\nhistos help",
        command(manyNote) == Some("histos help compile"),
        rootEmptyNote.summary == "unknown command",
        command(rootEmptyNote) == Some("histos help"),
        rootNote.check.value == "cli.unknown-command",
        rootNote.summary == "unknown command 'nope'",
        command(rootNote) == Some("histos help"),
        missingNote.check.value == "cli.usage",
        missingNote.summary == "run needs one name",
        missingNote.explain == "run needs one name",
        command(missingNote) == Some("histos help"),
        unexpectedNote.summary == "unexpected arguments\nhistos help",
        command(unexpectedNote) == Some("histos help"),
        CliParseError.BadTaskId.message == "a task id is not empty",
        CliParseError.TaskPlatform("inspect").message == "run inspect does not take a platform",
        CliParseError.Retired("testOnly", "test --only").message == "testOnly was removed\nuse test --only",
        CliParseError.PublishForm.message == "publish is local, snapshots, release, assets, promote, or drop",
        rejectedNote.check.value == "cli.usage",
        rejectedNote.summary == "usage: histos help",
        rejectedNote.explain == "histos help lists the commands.",
        command(rejectedNote) == Some("histos help"),
      )
  )

  /** The first repair, when it is a command. */
  private def command(found: Diagnostic): Option[String] =
    found.fixes match
      case Fix.Command(line) :: _ => Some(line.value)
      case _                      => None
end CliParseLawSpec
