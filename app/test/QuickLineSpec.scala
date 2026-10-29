package histos.cli

import histos.{Platform, ProjectName, ScalaVersion}

import zio.ZIO
import zio.test.*

object QuickLineSpec extends histos.SuiteClock:
  def spec = suite("quick line")(
    test("a testQuick hit names the suite and is not skipped"):
      ZIO
        .fromEither(ProjectName.from("app"))
        .map: module =>
          val line =
            Lines.cached(
              module,
              ScalaVersion("3.10.0"),
              Platform.Jvm,
              "demo.Foo",
            )
          assertTrue(
            line.contains("demo.Foo"),
            !line.contains("skipped"),
            line == s"${module.value} 3.10.0 jvm demo.Foo cached",
          )
  )
