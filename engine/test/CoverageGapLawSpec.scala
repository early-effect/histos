package histos.coverage

import histos.{CoverageFloors, Platform, ScalaVersion}

import java.nio.file.Path

import zio.test.*

object CoverageGapLawSpec extends ZIOSpecDefault:
  def spec = suite("coverage gap laws")(
    test("an empty rate meets only a non-positive minimum, and one hit in two is exactly half"):
      assertTrue(
        Rate(0, 0).meets(0),
        !Rate(0, 0).meets(1),
        Rate(1, 2).meets(50),
        !Rate(1, 2).meets(51),
        Rate(2, 2).meets(100),
        !Rate(1, 2).meets(100),
      )
    ,
    test("a file branch floor and a package floor fail on that slice only"):
      val report = Report(
        Rate(2, 2),
        Rate(2, 2),
        List(Slice("A.scala", Rate(2, 2), Rate(0, 2))),
        List(Slice("demo", Rate(1, 2), Rate(0, 2))),
      )
      assertTrue(
        Floor.shortfalls(report, CoverageFloors(fileBranches = Some(1))) == List(
          Miss.FileBranches("A.scala", Rate(0, 2), 1)
        ),
        Floor.shortfalls(report, CoverageFloors(packageStatements = Some(100))) == List(
          Miss.PackageStatements("demo", Rate(1, 2), 100)
        ),
        Floor.shortfalls(report, CoverageFloors(packageBranches = Some(0))) == Nil,
        Floor.shortfalls(report, CoverageFloors(packageBranches = Some(1))) == List(
          Miss.PackageBranches("demo", Rate(0, 2), 1)
        ),
        Floor.shortfalls(report, CoverageFloors(fileStatements = Some(100))) == Nil,
      )
    ,
    test("coverage measures scala 3 and 2.13 on the jvm, and nothing else"):
      val third  = ScalaVersion.trusted("3.10.0")
      val second = ScalaVersion.trusted("2.13.18")
      val older  = ScalaVersion.trusted("2.12.20")
      check(Gen.fromIterable(Platform.values)): platform =>
        assertTrue(
          Cell.measures(third, platform) == (platform == Platform.Jvm),
          Cell.measures(second, platform) == (platform == Platform.Jvm),
          !Cell.measures(older, platform),
        )
    ,
    test("scala 2.12 on the jvm is unmeasured"):
      val older = ScalaVersion.trusted("2.12.20")
      val spec  = CoverageCompile(Excludes(Nil, Nil), histos.ToolRevision.trusted("plugin"), Nil)
      assertTrue(
        Cell.instrumented(Path.of("/repo"), histos.ProjectName.trusted("core"), older, Platform.Jvm, spec) ==
          Left(CellFault.Unmeasured(older, Platform.Jvm))
      )
    ,
    test("empty exclusion lists omit the flag, and the identity moves when an exclusion or the source root moves"):
      val data = Path.of("/data")
      val root = Path.of("/root")
      val plugin = Path.of("/plugin.jar")
      val empty  = Excludes(Nil, Nil)
      assertTrue(
        Flags.scala3(data, empty) == List(s"-coverage-out:${data.toAbsolutePath.normalize}"),
        !Flags.scala2(data, root, plugin, empty).exists(_.contains("excludedPackages")),
        !Flags.scala2(data, root, plugin, empty).exists(_.contains("excludedFiles")),
        Flags.identity("3", data, root, empty, "") != Flags.identity("3", data, Path.of("/other"), empty, ""),
        Flags.identity("3", data, root, Excludes(List("a"), Nil), "") != Flags.identity("3", data, root, empty, ""),
        Flags.identity("3", data, root, Excludes(Nil, List("b")), "") != Flags.identity("3", data, root, empty, ""),
        Flags.identity("2.13", data, root, empty, "2") != Flags.identity("2.13", data, root, empty, ""),
        Cell.revision(ScalaVersion.trusted("3.10.0"), histos.ToolRevision.trusted("2")) == "",
        Cell.revision(ScalaVersion.trusted("2.13.18"), histos.ToolRevision.trusted("2")) == "2",
      )
  )
end CoverageGapLawSpec
