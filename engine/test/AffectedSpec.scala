package histos.workflow

import histos.{
  Module,
  Platform,
  Platforms,
  ProjectDep,
  ProjectGraph,
  ProjectName,
  ProjectPath,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
}

import zio.test.*

object AffectedSpec extends ZIOSpecDefault:
  private val core  = ProjectName("core")
  private val tests = ProjectName("tests")
  private val app   = ProjectName("app")

  private val graph: ProjectGraph =
    ProjectGraph.trusted(
      List(
        module(core, ProjectPath("libs/core"), Nil),
        module(tests, ProjectPath("libs/tests"), List(core)),
        module(app, ProjectPath("app"), List(tests)),
        module(ProjectName("notes"), ProjectPath("notes"), Nil),
      )
    )

  def spec = suite("affected modules")(
    test("the longest directory owns the path, and dependents follow"):
      val selected = Affected.decide(graph, List("libs/core/src/A.scala"))
      assertTrue(selected == Affected.Selection.Modules(List(core, tests, app)))
    ,
    test("a shorter directory does not steal a nested module"):
      val wide = ProjectGraph.trusted(
        List(
          module(ProjectName("libs"), ProjectPath("libs"), Nil),
          module(core, ProjectPath("libs/core"), Nil),
        )
      )
      assertTrue(
        Affected.decide(wide, List("libs/core/A.scala")) == Affected.Selection.Modules(List(core)),
        Affected.decide(wide, List("libs/README.md")) == Affected.Selection.Modules(List(ProjectName("libs"))),
      )
    ,
    test("the build definition selects the whole suite"):
      assertTrue(Affected.decide(graph, List("docs/histos.scala")) == Affected.Selection.Everything)
    ,
    test("an empty diff and an unowned path select nothing"):
      assertTrue(
        Affected.decide(graph, Nil) == Affected.Selection.Modules(Nil),
        Affected.decide(graph, List("README.md")) == Affected.Selection.Modules(Nil),
        Affected.report(graph, "abc", Some(Nil)) == "[]",
        Affected.report(graph, "abc", Some(List("README.md"))) == "[]",
      )
    ,
    test("a failed diff and an unknown base select every module"):
      assertTrue(
        Affected.report(graph, "all", Some(List("README.md"))) == """["all"]""",
        Affected.report(graph, "abc", None) == """["all"]""",
        Affected.report(graph, "abc", Some(List("histos.scala"))) == """["all"]""",
        Affected.report(graph, "abc", Some(List("libs/core/A.scala"))) == """["core","tests","app"]""",
      ),
  )

  private def module(name: ProjectName, directory: ProjectPath, deps: List[ProjectName]): Module =
    Module(
      name,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion("3.10.0"), Nil),
      Nil,
      deps.map(dep => ProjectDep(dep, None)),
      SourcePos("histos.scala", 1, 1),
      directory = Some(directory),
    )
end AffectedSpec
