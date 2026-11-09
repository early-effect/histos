package histos.cli

import histos.doctor.{Fix, Level}
import histos.plugin.{FlagNameError, PluginTaskError}
import histos.schedule.NamespaceError
import histos.task.TaskError
import histos.testrun.SuitePatternError
import histos.{PlatformError, ProjectNameError}

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A parse error is a sentence and a repair. The grammar only builds the case. */
object CliParseLawSpec extends ZIOSpecDefault:
  def spec = suite("cli parse error laws")(
    test("every parse error names its sentence and the repair"):
      val help           = CliParseError.Help
      val empty          = CliParseError.UnknownCommand("", Nil)
      val one            = CliParseError.UnknownCommand("compil", List("compile"))
      val many           = CliParseError.UnknownCommand("comp", List("compile", "coverage"))
      val rootEmpty      = CliParseError.UnknownRoot("")
      val root           = CliParseError.UnknownRoot("nope")
      val missing        = CliParseError.MissingArgument
      val unexpected     = CliParseError.UnexpectedArguments
      val badTask        = CliParseError.BadTaskId
      val plugin         = CliParseError.BadPluginTask(PluginTaskError.Shape("nope"))
      val platform       = CliParseError.BadPlatform(PlatformError.Unknown("bogus"))
      val taskPlatform   = CliParseError.TaskPlatform("inspect")
      val missingPattern = CliParseError.MissingPattern
      val pattern        = CliParseError.Pattern("coverage --exclude-file needs a regex")
      val project        = CliParseError.ProjectName(ProjectNameError.Bad("1api"))
      val suitePattern   = CliParseError.SuitePattern(SuitePatternError.Blank(" "))
      val run            = CliParseError.RunId(TaskError.EmptyId)
      val flag           = CliParseError.Flag(FlagNameError.Empty)
      val namespace      = CliParseError.Namespace(NamespaceError.Shape("nope"))
      val watch          = CliParseError.WatchCommand("watch does not nest")
      val form           = CliParseError.PublishForm
      val rejected       = CliParseError.Rejected
      val helpNote       = help.diagnose
      val emptyNote      = empty.diagnose
      val oneNote        = one.diagnose
      val manyNote       = many.diagnose
      val rootEmptyNote  = rootEmpty.diagnose
      val rootNote       = root.diagnose
      val rejectedNote   = rejected.diagnose
      val missingNote    = missing.diagnose
      val unexpectedNote = unexpected.diagnose
      assertTrue(
        help.message == Help.grouped,
        helpNote.check.value == "cli.help",
        helpNote.level == Level.Error,
        helpNote.summary == "histos help",
        helpNote.explain == Help.grouped,
        command(helpNote) == "histos help",
        empty.message == Help.unknown("", Help.names),
        emptyNote.check.value == "cli.unknown-command",
        emptyNote.summary == "unknown command",
        emptyNote.explain == "histos help lists the commands.\nhistos help <command> prints one.",
        command(emptyNote) == "histos help",
        one.message == Help.unknown("compil", List("compile")),
        oneNote.summary == "unknown command 'compil'",
        oneNote.explain == "did you mean compile?\nhistos help\nhistos help compile",
        command(oneNote) == "histos help compile",
        many.message == Help.unknown("comp", List("compile", "coverage")),
        manyNote.summary == "unknown command 'comp'",
        manyNote.explain == "did you mean compile or coverage?\nhistos help",
        command(manyNote) == "histos help compile",
        rootEmpty.message == Help.unknown(""),
        rootEmptyNote.summary == "unknown command",
        rootEmptyNote.explain == Help.unknown(""),
        command(rootEmptyNote) == "histos help",
        root.message == Help.unknown("nope"),
        rootNote.check.value == "cli.unknown-command",
        rootNote.summary == "unknown command 'nope'",
        rootNote.explain == Help.unknown("nope"),
        command(rootNote) == "histos help",
        missing.message == "run needs one name",
        missingNote.check.value == "cli.usage",
        missingNote.summary == missing.message,
        missingNote.explain == missing.message,
        command(missingNote) == "histos help",
        unexpected.message == "unexpected arguments\nhistos help",
        unexpectedNote.summary == unexpected.message,
        unexpectedNote.explain == unexpected.message,
        command(unexpectedNote) == "histos help",
        badTask.message == "a task id is not empty",
        plugin.message == PluginTaskError.Shape("nope").message,
        platform.message == PlatformError.Unknown("bogus").message,
        taskPlatform.message == "run inspect does not take a platform",
        missingPattern.message == "testOnly needs a pattern",
        pattern.message == "coverage --exclude-file needs a regex",
        project.message == ProjectNameError.Bad("1api").message,
        suitePattern.message == SuitePatternError.Blank(" ").message,
        run.message == TaskError.EmptyId.message,
        flag.message == FlagNameError.Empty.message,
        namespace.message == NamespaceError.Shape("nope").message,
        watch.message == "watch does not nest",
        form.message == "publish is snapshots, release, assets, promote, or drop",
        rejected.message == "usage: histos help",
        rejectedNote.check.value == "cli.usage",
        rejectedNote.summary == rejected.message,
        rejectedNote.explain == "histos help lists the commands.",
        command(rejectedNote) == "histos help",
      )
  )

  private def command(found: histos.doctor.Diagnostic): String =
    found.fixes.head match
      case Fix.Command(line)      => line.value
      case Fix.Settings(_, click) => click
      case Fix.Config(_, _)       => ""
      case Fix.Steps(items)       => items.head
end CliParseLawSpec
