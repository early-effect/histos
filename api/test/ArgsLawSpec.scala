package histos.plugin

import zio.test.*

object ArgsLawSpec extends ZIOSpecDefault:
  def spec = suite("plugin args laws")(
    test("a flag is present or absent, and require returns the text that was set"):
      val mode    = ParamName("mode")
      val verbose = FlagName("verbose")
      val quiet   = FlagName("quiet")
      val args    = PluginArgs(List(NamedText(mode, "fast")), Set(verbose))
      assertTrue(
        args.flag(verbose),
        !args.flag(quiet),
        args.require(mode) == Right("fast"),
        args.require(ParamName("missing")) == Left(PluginRunError.MissingArg(ParamName("missing"))),
        PluginArgs.empty.flag(verbose) == false,
      )
  )
