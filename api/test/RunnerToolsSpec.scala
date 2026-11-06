package histos

import _root_.scala.compiletime.testing.typeCheckErrors

import histos.supply.{PrAuthor, SupplyChain, UpdatePolicy}

import zio.test.*

object RunnerToolsSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.9.0")
  private val tools  = RunnerTools(
    Some(NodeFloor(25)),
    Some(ToolRevision("21.1.8")),
    Some(ToolRevision("1.62.1")),
    None,
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
      val node = RunnerTools(Some(NodeFloor(22)), None, None, None)
      assertTrue(
        named.map(_.runnerTools) == Right(tools),
        named.map(_.organization.map(_.value)) == Right(Some("rocks.earlyeffect")),
        flipped.map(_.runnerTools) == Right(tools),
        Projects.runnerTools(node)(core).map(_.runnerTools) == Right(node),
      )
    ,
    test("test logs stay off until the build asks"):
      val core = module("core", Jvm)(scala3)
      assertTrue(
        Projects(core).map(_.testLog) == Right(false),
        Projects.testLog(core).map(_.testLog) == Right(true),
        Projects
          .organization(organization"rocks.earlyeffect")
          .testLog(core)
          .map(_.testLog) == Right(true),
        Projects.testLog
          .runnerTools(tools)(core)
          .map(graph => graph.testLog && graph.runnerTools == tools) == Right(true),
      )
    ,
    test("the doc check stays off until the build asks"):
      val core = module("core", Jvm)(scala3)
      assertTrue(
        Projects(core).map(_.docCheck) == Right(histos.DocCheck.Off),
        Projects.docCheck(core).map(_.docCheck) == Right(histos.DocCheck.On),
        Projects
          .organization(organization"rocks.earlyeffect")
          .docCheck(core)
          .map(_.docCheck) == Right(histos.DocCheck.On),
      )
    ,
    test("the doc check keeps the supply the build named"):
      val core   = module("core", Jvm)(scala3)
      val policy = SupplyChain(updates = UpdatePolicy(author = PrAuthor.histosDeps))
      val named  = Projects
        .organization(organization"rocks.earlyeffect")
        .supplyChain(policy)
        .docCheck(core)
      assertTrue(
        named.map(_.docCheck) == Right(histos.DocCheck.On),
        named.map(_.supply.updates.author) == Right(PrAuthor.histosDeps),
      )
    ,
    test("a node path is an absolute node binary and stays on the build"):
      val core     = module("core", Jvm)(scala3)
      val path     = NodePath("/usr/local/bin/node")
      val named    = RunnerTools(None, None, None, Some(path))
      val literal  = typeCheckErrors("""histos.NodePath("/usr/bin/node")""")
      val relative = typeCheckErrors("""histos.NodePath("bin/node")""")
      val blank    = typeCheckErrors("""histos.NodePath("")""")
      val other    = typeCheckErrors("""histos.NodePath("/tmp/nodejs")""")
      assertTrue(
        path.value == "/usr/local/bin/node",
        NodePath("/usr/bin/../bin/node").value == "/usr/bin/node",
        NodePath.from("/usr/bin/node").map(_.value) == Right("/usr/bin/node"),
        NodePath.from("  /usr/bin/node  ").map(_.value) == Right("/usr/bin/node"),
        NodePath.from("/usr/bin/node/").map(_.value) == Right("/usr/bin/node"),
        NodePath.from("bin/node") == Left(NodePathError.Relative("bin/node")),
        NodePath.from("") == Left(NodePathError.Empty),
        NodePath.from("   ") == Left(NodePathError.Empty),
        NodePath.from("/tmp/nodejs") == Left(NodePathError.NotBinary("/tmp/nodejs")),
        NodePathError.Relative("bin/node").message == "is not absolute: bin/node",
        NodePathError.Empty.message == "is empty",
        NodePathError.NotBinary("/tmp/nodejs").message == "is not a node binary: /tmp/nodejs",
        Projects.runnerTools(named)(core).map(_.runnerTools) == Right(named),
        literal == Nil,
        relative match
          case head :: _ => head.message.contains("is not absolute")
          case Nil       => false,
        blank match
          case head :: _ => head.message.contains("is empty")
          case Nil       => false,
        other match
          case head :: _ => head.message.contains("is not a node binary")
          case Nil       => false,
      ),
  )
end RunnerToolsSpec
