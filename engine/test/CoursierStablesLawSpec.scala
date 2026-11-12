package histos.version

import zio.test.*

object CoursierStablesLawSpec extends ZIOSpecDefault:
  def spec = suite("coursier stables")(
    test("a maven stable wins, and a snapshot is not a ship number"):
      val result = coursier.Versions.Result(
        Seq(
          coursier.LocalRepositories.ivy2Local -> Right(versions("0.2.9", "0.2.9-SNAPSHOT")),
          coursier.Repositories.central        -> Right(versions("0.2.8")),
        )
      )
      val listed = CoursierStables.fromResult("rocks.earlyeffect:histos-api_3", result)
      assertTrue(
        listed.map(ShipBump.choose) == Right(Some(ListedStable(ShipNumber.trusted(0, 2, 8), StablePlace.Remote)))
      )
    ,
    test("ivy2 local answers when maven did not"):
      val result = coursier.Versions.Result(
        Seq(
          coursier.LocalRepositories.ivy2Local -> Right(versions("0.2.9")),
          coursier.Repositories.central        -> Left("not found"),
        )
      )
      val listed = CoursierStables.fromResult("rocks.earlyeffect:histos-api_3", result)
      assertTrue(
        listed.map(ShipBump.choose) == Right(Some(ListedStable(ShipNumber.trusted(0, 2, 9), StablePlace.Local)))
      )
    ,
    test("no repository answering is the fault"):
      val result = coursier.Versions.Result(
        Seq(coursier.Repositories.central -> Left("not found"))
      )
      assertTrue(
        CoursierStables.fromResult("rocks.earlyeffect:histos-api_3", result) ==
          Left(BumpFault.Unanswered("rocks.earlyeffect:histos-api_3"))
      )
    ,
    test("an empty resolver list still asks ivy2 local and central"):
      val repos = CoursierStables.repositories(Nil)
      assertTrue(
        repos.headOption.contains(coursier.LocalRepositories.ivy2Local),
        repos.contains(coursier.Repositories.central),
      ),
  )

  private def versions(raws: String*): coursier.core.Versions =
    val parsed = raws.toList.map(coursier.version.Version(_))
    parsed match
      case head :: _ =>
        coursier.core.Versions(head, head, parsed, None)
      case Nil =>
        coursier.core.Versions(coursier.version.Version.zero, coursier.version.Version.zero, Nil, None)
end CoursierStablesLawSpec
