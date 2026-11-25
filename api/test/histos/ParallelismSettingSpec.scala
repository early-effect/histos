package histos

import histos.suite.HistosSpec

import zio.test.*

object ParallelismSettingSpec extends HistosSpec:
  private val api = module("api", Jvm)(scala"3.10.1")

  def spec = suite("parallelism")(
    test("omission leaves the core count") {
      assertTrue(Projects(api).map(_.parallelism) == Right(None))
    },
    test("a ceiling is stored and a second call replaces it") {
      val once  = Projects.parallelism(2)(api)
      val twice = Projects.parallelism(2).parallelism(4)(api)
      assertTrue(
        once.map(_.parallelism.map(_.value)) == Right(Some(2)),
        twice.map(_.parallelism.map(_.value)) == Right(Some(4)),
      )
    },
    test("a non-positive ceiling is rejected") {
      val zero     = Projects.parallelism(0)(api)
      val negative = Projects.parallelism(-2)(api)
      assertTrue(
        zero == Left(BuildError.Contract(ContractFault.Parallelism(0))),
        negative == Left(BuildError.Contract(ContractFault.Parallelism(-2))),
        BuildError.Contract(ContractFault.Parallelism(0)).message == "a parallelism max is at least 1, not 0",
      )
    },
  )
end ParallelismSettingSpec
