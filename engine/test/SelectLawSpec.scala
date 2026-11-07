package histos.select

import histos.{
  Aggregate,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import zio.test.*

object SelectLawSpec extends ZIOSpecDefault:
  private val core   = module("core")
  private val extra  = module("extra")
  private val all    = Aggregate(ProjectName.trusted("all"), List(core.name), SourcePos("histos.scala", 2, 1))
  private val nested = Aggregate(
    ProjectName.trusted("nested"),
    List(ProjectName.trusted("inner")),
    SourcePos("histos.scala", 3, 1),
  )
  private val inner =
    Aggregate(ProjectName.trusted("inner"), List(core.name), SourcePos("histos.scala", 4, 1))
  private val graph  = ProjectGraph.trusted(List(core, all, extra), Splits.aggregate)
  private val deeper = ProjectGraph.trusted(List(core, nested, inner), Splits.aggregate)

  def spec = suite("select laws")(
    test("empty names are every module in graph order and expand nothing"):
      assertTrue(
        Selection.select(graph, Nil) == Right(Selected(List(core, extra), Set.empty))
      )
    ,
    test("a module name is that module, and an aggregate expands its direct module children"):
      assertTrue(
        Selection.select(graph, List(core.name)) == Right(Selected(List(core), Set.empty)),
        Selection.select(graph, List(all.name)) == Right(Selected(List(core), Set(core.name))),
      )
    ,
    test("a repeated name, or a module reached again through an aggregate, is a duplicate"):
      assertTrue(
        Selection.select(graph, List(core.name, core.name)) == Left(SelectError.Duplicate(core.name)),
        Selection.select(graph, List(all.name, core.name)) == Left(SelectError.Duplicate(core.name)),
        Selection.select(graph, List(core.name, all.name)) == Left(SelectError.Duplicate(core.name)),
      )
    ,
    test("an unknown name, and an aggregate child that is not a module, are unknown"):
      val missing = ProjectName.trusted("missing")
      val unknown = Selection.select(graph, List(missing))
      val child   = Selection.select(deeper, List(nested.name))
      assertTrue(
        unknown match
          case Left(error @ SelectError.Unknown(name, _)) =>
            name == missing && error.message == s"'${missing.value}' is not a project"
          case _ => false
        ,
        child match
          case Left(SelectError.Unknown(name, _)) => name == inner.name
          case _                                  => false,
      ),
  )

  private def module(name: String): Module =
    Module(
      ProjectName.trusted(name),
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion.trusted("3.3.7"), Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )
end SelectLawSpec
