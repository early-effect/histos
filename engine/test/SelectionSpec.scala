package histos.select

import histos.{
  Aggregate,
  Jvm,
  ProjectGraph,
  ProjectName,
  ProjectNameError,
  Projects,
  ScalaVersion,
  SourcePos,
  aggregate,
  module,
}

import zio.ZIO
import zio.test.*

object SelectionSpec extends histos.SuiteClock:
  private val scala3 = ScalaVersion("3.10.0")
  private val at     = SourcePos("histos.scala", 1, 1)

  private val selection = suite("selection")(
    test("an aggregate names its modules and a repeated member is an error"):
      val core = module("core", Jvm)(scala3)
      val app  = module("app", Jvm)(scala3).dependsOn(core)
      val root = aggregate("root", core, app)
      for
        built    <- ZIO.fromEither(Projects(root, core, app))
        whole    <- ZIO.fromEither(Selection.select(built, Nil))
        named    <- ZIO.fromEither(Selection.select(built, List(ProjectName("root"))))
        twice    <- ZIO.succeed(Selection.select(built, List(ProjectName("root"), ProjectName("core"))))
        repeated <- ZIO.fromEither(ProjectName.from("core"))
      yield assertTrue(
        whole.modules.map(_.name.value) == List("core", "app"),
        named.modules.map(_.name.value) == List("core", "app"),
        named.expanded == named.modules.map(_.name).toSet,
        twice == Left(SelectError.Duplicate(repeated)),
      )
    ,
    test("a missing aggregate child is an unknown project"):
      for
        child <- ZIO.fromEither(ProjectName.from("child"))
        name  <- ZIO.fromEither(ProjectName.from("root"))
        graph = ProjectGraph.trusted(List(Aggregate(name, List(child), at)))
        selected <- ZIO.succeed(Selection.select(graph, List(ProjectName("root"))))
      yield assertTrue(selected == Left(SelectError.Unknown(child)))
    ,
    test("a token that is not a project name fails before selection"):
      assertTrue(
        ProjectName.from("1core") == Left(ProjectNameError.Bad("1core")),
        ProjectName.from("a/b") == Left(ProjectNameError.Bad("a/b")),
      ),
  )

  def spec = selection
end SelectionSpec
