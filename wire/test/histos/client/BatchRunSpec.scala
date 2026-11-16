package histos.client

import histos.Platform

import zio.test.*

object BatchRunSpec extends ZIOSpecDefault:
  def spec = suite("run arguments on the native client")(
    test("tokens after -- are the program arguments and jvm stays the platform") {
      assertTrue(
        Batch.parse(List("run", "app", "jvm", "--", "hello", "world")) ==
          Some(Batch.Parsed.Program("app", Some(Platform.Jvm), List("hello", "world")))
      )
    },
    test("a run without -- has an empty argument list") {
      assertTrue(
        Batch.parse(List("run", "app", "jvm")) == Some(Batch.Parsed.Program("app", Some(Platform.Jvm), Nil))
      )
    },
    test("a token before -- is not an argument") {
      assertTrue(Batch.parse(List("run", "app", "jvm", "extra")).isEmpty)
    },
    test("a flag after -- is an argument") {
      assertTrue(
        Batch.parse(List("run", "app", "--", "--verbose")) ==
          Some(Batch.Parsed.Program("app", None, List("--verbose")))
      )
    },
  )
