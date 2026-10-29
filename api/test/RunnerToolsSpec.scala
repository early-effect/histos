package histos

import _root_.scala.compiletime.testing.typeCheckErrors

import zio.test.*

object RunnerToolsSpec extends histos.SuiteClock:
  private val scala3 = ScalaVersion("3.9.0")
  private val tools  = RunnerTools(
    Some(NodeFloor(25)),
    Some(ToolRevision("21.1.8")),
    Some(ToolRevision("1.62.1")),
  )

  def spec = suite("runner tools")(
    test("a node floor is a positive major"):
      val refused = typeCheckErrors("histos.NodeFloor(0)")
      assertTrue(
        NodeFloor(25).value == 25,
        NodeFloor.from(25).map(_.value) == Right(25),
        NodeFloor.from(0) == Left(NodeFloorError.NotPositive(0)),
        NodeFloor.from(-4) == Left(NodeFloorError.NotPositive(-4)),
        NodeFloorError.NotPositive(0).message == "0 is not a Node major",
        refused match
          case head :: _ => head.message.contains("0 is not a Node major")
          case Nil       => false,
      )
    ,
    test("a build that never names them installs nothing, and the compiler catalog is not a source"):
      val core = module("core", Jvm)(scala3)
      assertTrue(
        Projects(core).map(_.runnerTools) == Right(RunnerTools.empty),
        Projects.tools(ToolCatalog.default)(core).map(_.runnerTools) == Right(RunnerTools.empty),
      )
    ,
    test("organization keeps the tools the build named"):
      val core  = module("core", Jvm)(scala3)
      val named = Projects
        .organization(organization"rocks.earlyeffect")
        .runnerTools(tools)(core)
      val flipped = Projects
        .runnerTools(tools)
        .organization(organization"rocks.earlyeffect")(core)
      val node = RunnerTools(Some(NodeFloor(22)), None, None)
      assertTrue(
        named.map(_.runnerTools) == Right(tools),
        named.map(_.organization.map(_.value)) == Right(Some("rocks.earlyeffect")),
        flipped.map(_.runnerTools) == Right(tools),
        Projects.runnerTools(node)(core).map(_.runnerTools) == Right(node),
      ),
  )
end RunnerToolsSpec
