package histos.publish

import histos.central.Revision
import histos.suite.HistosSpec
import histos.version.{ReleaseVersion, ShipNumber}
import histos.{
  Dep,
  DepKind,
  MainClass,
  Module,
  Organization,
  Platform,
  Platforms,
  ProjectDep,
  ProjectGraph,
  ProjectName,
  Publication,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
}

import zio.test.*

object ThinPublishSpec extends HistosSpec:
  private val version  = ScalaVersion("3.10.1")
  private val revision = Revision.Release(ReleaseVersion(ShipNumber.trusted(0, 5, 0)))
  private val group    = Organization.trusted("rocks.earlyeffect")
  private val where    = SourcePos("histos.scala", 1, 1)

  def spec = suite("a thin launcher")(
    test("the launcher pom names the engine and the engine pom names the compiler") {
      val published = List(ProjectName("engine") -> revision, ProjectName("app") -> revision)
      val launcher  = LocalPublish.dependencies(
        graph,
        group,
        ProjectName("app"),
        version,
        Platform.Jvm,
        revision,
        published,
      )
      val engine = LocalPublish.dependencies(
        graph,
        group,
        ProjectName("engine"),
        version,
        Platform.Jvm,
        revision,
        published,
      )
      assertTrue(
        launcher.exists(_.exists(named("rocks.earlyeffect", "engine_3"))),
        launcher.exists(_.forall(dep => !dep.name.value.contains("scala3-compiler"))),
        engine.exists(_.exists(named("org.scala-lang", "scala3-compiler_3"))),
      )
    }
  )

  private def named(group: String, artifact: String): histos.central.MavenDependency => Boolean =
    dep => dep.group.value == group && dep.name.value == artifact

  private def graph: ProjectGraph =
    ProjectGraph.trusted(
      List(
        Module(
          ProjectName("engine"),
          Platforms(Platform.Jvm, Nil),
          ScalaVersions(version, Nil),
          List(Dep.trusted("org.scala-lang", "scala3-compiler", "3.10.1", DepKind.Binary)),
          Nil,
          where,
        ),
        Module(
          ProjectName("app"),
          Platforms(Platform.Jvm, Nil),
          ScalaVersions(version, Nil),
          Nil,
          List(ProjectDep(ProjectName("engine"), None)),
          where,
          publication = Publication.Assembly(MainClass("histos.cli.Main")),
        ),
      )
    )
end ThinPublishSpec
