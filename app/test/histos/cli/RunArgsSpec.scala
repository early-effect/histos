package histos.cli

import histos.{MainClass, Platform, PlatformSelection, SelectionError}
import histos.suite.HistosSpec

import zio.test.*
import zio.json.*

object RunArgsSpec extends HistosSpec:
  def spec = suite("run arguments")(
    test("platform and main flags keep forwarded arguments unchanged") {
      Grammar
        .parse(
          List(
            "run",
            "--main",
            "bench.HttpBench",
            "app",
            "--platform",
            "jvm",
            "--",
            "hello",
            "--platform",
            "js",
            "a,b",
            "\\",
          )
        )
        .map:
          case Command.Run(id, platform, args, main) =>
            assertTrue(
              id.value == "app",
              platform.contains(PlatformSelection.only(Platform.Jvm)),
              main.contains(MainClass("bench.HttpBench")),
              args == List("hello", "--platform", "js", "a,b", "\\"),
            )
          case _ => assertTrue(false)
    },
    test("the real operation seals typed selectors without changing program arguments") {
      for
        command <- Grammar.parse(
          List("run", "app", "--platform", "jvm", "--main", "bench.HttpBench", "--", "--payload", "a,b")
        )
        call <- Operation.call(command)
      yield call match
        case histos.server.Call.Run(id, platform, _, args, main) =>
          assertTrue(
            id == "app",
            platform.contains(PlatformSelection.only(Platform.Jvm)),
            main.contains(MainClass("bench.HttpBench")),
            args == List("--payload", "a,b"),
          )
        case _ => assertTrue(false)
    },
    test("the wire round trip preserves explicit selection, main, launch and arguments") {
      for
        command <- Grammar.parse(List("run", "app", "--platform", "js", "--main", "bench.WebBench", "--", "a,b"))
        call    <- Operation.call(command)
      yield assertTrue(
        call.toJson.fromJson[histos.server.Call] == Right(call),
        call.attendance == histos.server.Attendance.Program,
      )
    },
    test("omitted platform and main remain omitted, not JVM defaults") {
      Grammar
        .parse(List("run", "app"))
        .map:
          case Command.Run(id, platform, args, main) =>
            assertTrue(id.value == "app", platform.isEmpty, main.isEmpty, args.isEmpty)
          case _ => assertTrue(false)
    },
    test("run option placement is independent of the module") {
      for
        before <- Grammar.parse(List("run", "--platform", "js", "--main", "bench.WebBench", "app")).either
        after  <- Grammar.parse(List("run", "app", "--main", "bench.WebBench", "--platform", "js")).either
      yield assertTrue(before == after, before.isRight)
    },
    test("a positional platform is retired rather than silently consumed") {
      Grammar
        .parse(List("run", "app", "jvm"))
        .either
        .map: parsed =>
          assertTrue(
            parsed == Left(CliParseError.Retired("run <module> <platform>", "run <module> --platform <platform>"))
          )
    },
    test("multi-platform run refuses before execution, repeated identical choices are one platform") {
      for
        many <- Grammar.parse(List("run", "app", "--platform", "jvm,js")).either
        one  <- Grammar.parse(List("run", "app", "--platform", "jvm,jvm", "--platform", "jvm")).either
      yield assertTrue(
        many == Left(
          CliParseError.Selection(SelectionError.RunMany(histos.Platforms(Platform.Jvm, List(Platform.Js))))
        ),
        one.exists:
          case Command.Run(_, selection, _, _) => selection.contains(PlatformSelection.only(Platform.Jvm))
          case _                               => false,
      )
    },
    test("a built-in task takes neither module selectors nor program arguments") {
      for
        args     <- Grammar.parse(List("run", "inspect", "--", "hello")).either
        platform <- Grammar.parse(List("run", "inspect", "--platform", "jvm")).either
        main     <- Grammar.parse(List("run", "inspect", "--main", "bench.Main")).either
      yield assertTrue(args.isLeft, platform.isLeft, main.isLeft)
    },
    test("invalid and repeated mains return typed or usage failures") {
      for
        invalid  <- Grammar.parse(List("run", "app", "--main", "not/main")).either
        repeated <- Grammar.parse(List("run", "app", "--main", "bench.Main", "--main", "bench.Other")).either
      yield assertTrue(
        invalid == Left(CliParseError.MainClass(histos.MainClassError.Bad("not/main"))),
        repeated == Left(CliParseError.Pattern("run --main is already set")),
      )
    },
  )
