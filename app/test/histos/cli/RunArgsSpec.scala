package histos.cli

import histos.Platform

import zio.test.*

object RunArgsSpec extends ZIOSpecDefault:
  def spec = suite("run arguments")(
    test("tokens after -- are the program arguments and jvm stays the platform") {
      Grammar
        .parse(List("run", "app", "jvm", "--", "hello", "world"))
        .map:
          case Command.Run(id, platform, args) =>
            assertTrue(id.value == "app", platform.contains(Platform.Jvm), args == List("hello", "world"))
          case _ => assertTrue(false)
    },
    test("a run without -- has an empty argument list") {
      Grammar
        .parse(List("run", "app", "jvm"))
        .map:
          case Command.Run(id, platform, args) =>
            assertTrue(id.value == "app", platform.contains(Platform.Jvm), args.isEmpty)
          case _ => assertTrue(false)
    },
    test("a token before -- is usage") {
      Grammar
        .parse(List("run", "app", "jvm", "extra"))
        .either
        .map: ran =>
          assertTrue(ran.isLeft)
    },
    test("a built-in task does not take program arguments") {
      Grammar
        .parse(List("run", "inspect", "--", "hello"))
        .either
        .map:
          case Left(CliParseError.Pattern(detail)) => assertTrue(detail.contains("inspect"))
          case _                                   => assertTrue(false)
    },
  )
