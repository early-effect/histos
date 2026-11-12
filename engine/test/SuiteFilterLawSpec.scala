package histos.testrun

import histos.{ClangMajor, ClangPin, Module, Platform, Platforms, ProjectName, ScalaVersion, ScalaVersions, SourcePos}

import zio.test.*

object SuiteFilterLawSpec extends ZIOSpecDefault:
  private val name    = ProjectName.trusted("core")
  private val version = ScalaVersion.trusted("3.10.0")

  def spec = suite("suite filter laws")(
    test("a clang major is that pin, and an unused pin is a link failure"):
      for
        major <- SuiteLaunch.nativeMajor(module(ClangPin.Major(ClangMajor.trusted(17))))
        none  <- SuiteLaunch.nativeMajor(module(ClangPin.Unused)).either
      yield assertTrue(
        major == ClangMajor.trusted(17),
        none == Left(
          ModuleTestError.LinkFailed(name, version, Platform.Native, "the module does not name a clang major")
        ),
      )
  )

  private def module(clang: ClangPin): Module =
    Module(
      name,
      Platforms(Platform.Native, Nil),
      ScalaVersions(version, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      clang = clang,
    )
end SuiteFilterLawSpec
