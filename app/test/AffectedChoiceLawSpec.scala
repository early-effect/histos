package histos.cli

import histos.ProjectName

import zio.test.*

object AffectedChoiceLawSpec extends ZIOSpecDefault:
  def spec = suite("affected choice laws")(
    test("an empty affected list runs nothing and all runs the projects"):
      val api   = ProjectName("api")
      val core  = ProjectName("core")
      val named = List(api, core)
      val rows  = List(
        (named, AffectedChoice.Unset, Some(named)),
        (named, AffectedChoice.NoneSelected, None),
        (named, AffectedChoice.Everything, Some(named)),
        (named, AffectedChoice.Only(List(api)), Some(List(api))),
        (named, AffectedChoice.Only(List(core)), Some(List(core))),
        (named, AffectedChoice.Only(Nil), None),
        (Nil, AffectedChoice.Unset, Some(Nil)),
        (Nil, AffectedChoice.Everything, Some(Nil)),
        (Nil, AffectedChoice.Only(List(api)), Some(List(api))),
        (Nil, AffectedChoice.Only(Nil), None),
        (List(api), AffectedChoice.Only(List(core)), None),
      )
      val bad = rows.filter { (projects, choice, expected) =>
        AffectedChoice.select(projects, choice) != expected
      }
      assertTrue(bad == Nil)
  )
