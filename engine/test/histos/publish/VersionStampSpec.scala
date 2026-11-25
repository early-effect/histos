package histos.publish

import histos.plugin.PluginCoordinate
import histos.suite.HistosSpec
import histos.version.Catalog
import histos.{
  Module,
  Platform,
  Platforms,
  PluginPublication,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import zio.test.*

object VersionStampSpec extends HistosSpec:
  private val name = ProjectName("plugin")

  def spec = suite("plugin version stamp")(
    test("a plugin library is stamped when the graph names no coordinate") {
      val built = module(Some(PluginPublication.Rebuildable))
      assertTrue(LocalPublish.versioned(graph(built, Nil), built))
    },
    test("an ordinary library is not stamped") {
      val built = module(None)
      assertTrue(!LocalPublish.versioned(graph(built, Nil), built))
    },
    test("a module the graph names is stamped") {
      val built = module(None)
      assertTrue(LocalPublish.versioned(graph(built, List(PluginCoordinate.Module(name))), built))
    },
  )

  private def module(publication: Option[PluginPublication]): Module =
    Module(
      name,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion("3.10.1"), Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
      pluginLibrary = publication,
    )

  private def graph(built: Module, coordinates: List[PluginCoordinate]): ProjectGraph =
    ProjectGraph.trusted(
      List(built),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      coordinates = coordinates,
    )
end VersionStampSpec
