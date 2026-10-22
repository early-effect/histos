package histos.select

import histos.workflow.CiError
import histos.{Aggregate, Jvm, ProjectGraph, ProjectName, Projects, ScalaVersion, SourcePos, aggregate, module}

import zio.ZIO
import zio.test.*

object SelectionSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")
  private val at = SourcePos("histos.scala", 1, 1)

  private val selection = suite("selection")(
    test("an aggregate names its modules and a repeated member is an error"):
      val core = module("core", Jvm)(scala3)
      val app = module("app", Jvm)(scala3).dependsOn(core)
      val root = aggregate("root", core, app)
      for
        built <- ZIO.fromEither(Projects(root, core, app))
        whole <- ZIO.fromEither(Selection.select(built, Nil))
        named <- ZIO.fromEither(Selection.select(built, List("root")))
        twice <- ZIO.succeed(Selection.select(built, List("root", "core")))
        repeated <- ZIO.fromEither(ProjectName.from("core"))
      yield assertTrue(
        whole.modules.map(_.name.value) == List("core", "app"),
        named.modules.map(_.name.value) == List("core", "app"),
        named.expanded == named.modules.map(_.name).toSet,
        twice == Left(CiError.Duplicate(repeated)),
      )
    ,
    test("a missing aggregate child is an unknown project"):
      for
        child <- ZIO.fromEither(ProjectName.from("child"))
        name <- ZIO.fromEither(ProjectName.from("root"))
        graph = ProjectGraph.trusted(List(Aggregate(name, List(child), at)))
        selected <- ZIO.succeed(Selection.select(graph, List("root")))
      yield assertTrue(selected == Left(CiError.Unknown(child)))
    ,
  )

  def spec = selection
