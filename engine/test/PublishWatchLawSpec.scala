package histos.publish

import histos.select.SelectError
import histos.version.ShipName
import histos.{Jvm, ProjectName, Projects, Ship, aggregate, module, organization, scala, ships}

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A watch names sources. An aggregate publishes the members a ship owns and skips the rest. */
object PublishWatchLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  def spec = suite("publish watch laws")(
    test("a watch is every named module, and an aggregate skips a module no ship owns"):
      val app   = module("app", Jvm)(scala3)
      val extra = module("extra", Jvm)(scala3)
      val root  = aggregate("root", app, extra)
      val built = Projects.organization(organization"example.com")(
        ships(Ship("demo").next(1, 2, 3)(app)),
        root,
        app,
        extra,
      )
      built match
        case Left(_) => assertTrue(false)
        case Right(graph) =>
          val demo    = ShipName("demo")
          val appName = ProjectName("app")
          val writing = List(ShipWrite(demo, List(appName), false))
          val picked  = PublishSelect.pick(graph, List(PublishName.Aggregate(ProjectName("root"))), writing)
          val held    = PublishSelect.pick(graph, List(PublishName.Ship(demo)), Nil)
          assertTrue(
            PublishSelect.watch(graph, Nil) == None,
            PublishSelect.watch(graph, List("demo")) == Some(List(appName)),
            PublishSelect.watch(graph, List("root")) == Some(List(appName, ProjectName("extra"))),
            PublishSelect.watch(graph, List("extra")) == Some(List(ProjectName("extra"))),
            PublishSelect.watch(graph, List("missing")) == Some(Nil),
            picked.exists { found =>
              found.roots == Set(appName) && found.modules == Set(appName) &&
              found.skipped == List(ProjectName("extra")) && found.refused.isEmpty
            },
            held.exists(found => found.roots.isEmpty && found.modules.isEmpty && found.refused.isEmpty),
            PublishSelect.pick(graph, List(PublishName.Aggregate(ProjectName("gone"))), writing) ==
              Left(LocalPublishError.Target(SelectError.Unknown(ProjectName("gone")))),
          )
  )
end PublishWatchLawSpec
