package histos.cli

import histos.doctor.Suggest
import histos.plugin.{
  CliParam,
  CommandName,
  CommandSpec,
  FlagName,
  FlagNameError,
  NamedText,
  ParamName,
  PluginArgs,
  PluginDetail,
  PluginId,
  PluginTaskId,
  TaskName,
}

import zio.test.*

object PluginCommandsLawSpec extends ZIOSpecDefault:
  private val bound = BoundCommand(
    PluginId("demo"),
    CommandSpec(
      CommandName("pack"),
      List(CliParam.Text(ParamName("name")), CliParam.Flag(FlagName("yes"))),
      TaskName("pack"),
      PluginDetail.trusted("packs it"),
    ),
  )

  def spec = suite("plugin command laws")(
    test("a known command binds the task, the text, and the flag"):
      val parsed = PluginCommands.parse(List(bound), List("pack", "ada", "--yes"))
      parsed match
        case Right((id, args)) =>
          assertTrue(
            id == PluginTaskId(PluginId("demo"), TaskName("pack")),
            args == PluginArgs(List(NamedText(ParamName("name"), "ada")), Set(FlagName("yes"))),
          )
        case Left(_) => assertTrue(false)
    ,
    test("two optional parameters are rejected before the tokens are read"):
      val open = bound.copy(spec =
        bound.spec
          .copy(params = List(CliParam.OptionalText(ParamName("name")), CliParam.OptionalText(ParamName("extra"))))
      )
      assertTrue(PluginCommands.parse(List(open), List("pack")) == Left(CliParseError.Rejected))
    ,
    test("missing, extra, and unknown tokens are distinct faults"):
      val known = List("pack") ++ Help.names
      assertTrue(
        PluginCommands.parse(Nil, Nil) == Left(CliParseError.UnknownCommand("", Nil)),
        PluginCommands.parse(List(bound), List("pak")) == Left(
          CliParseError.UnknownCommand("pak", Suggest.near("pak", known))
        ),
        PluginCommands.parse(List(bound), List("pack")) == Left(CliParseError.MissingArgument),
        PluginCommands.parse(List(bound), List("pack", "ada", "extra")) == Left(CliParseError.UnexpectedArguments),
        PluginCommands.parse(List(bound), List("pack", "ada", "--ye")) == Left(
          CliParseError.Pattern("--ye is not a flag of pack\ndid you mean --yes?")
        ),
        PluginCommands.parse(List(bound), List("pack", "ada", "--")) == Left(
          CliParseError.Flag(FlagNameError.Empty)
        ),
        PluginCommands.usage(List(bound)).contains("packs it"),
        PluginCommands.usage(Nil) == Help.grouped,
      ),
  )
end PluginCommandsLawSpec
