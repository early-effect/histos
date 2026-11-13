package histos

import histos.version.{ShipName, ShipNumber, ShipToolchain}

import zio.test.*

object ToolchainLawSpec extends ZIOSpecDefault:
  private val number = ShipNumber.trusted(0, 3, 0)
  private val ship   = ShipName.trusted("histos")
  private val here   = SourcePos("law", 4, 1)
  private val scala3 = scala"3.10.0"

  def spec = suite("toolchain load laws")(
    test("a launcher that is not a member of the ship is refused"):
      val api    = module("api", Jvm)(scala3)
      val app    = module("app", Jvm)(scala3).assembly(MainClass("demo.Main"))
      val client = nativeClient
      val row    = ShipDef.line(ship.value, number, ::(api, Nil), here).toolchain(launcher = app, client = client)
      assertTrue(
        fault(Projects(CatalogSpec(row), api, app, client)) == Some(launcher(app, ToolchainLauncherFault.NotMember))
      )
    ,
    test("a launcher the build does not contain is refused"):
      val api    = module("api", Jvm)(scala3)
      val app    = module("app", Jvm)(scala3).assembly(MainClass("demo.Main"))
      val client = nativeClient
      val row    = ShipDef.line(ship.value, number, ::(api, Nil), here).toolchain(launcher = app, client = client)
      assertTrue(fault(Projects(CatalogSpec(row), api, client)) == Some(launcher(app, ToolchainLauncherFault.Missing)))
    ,
    test("a launcher without an assembly is refused"):
      val app    = module("app", Jvm)(scala3)
      val client = nativeClient
      val row    = ShipDef.line(ship.value, number, ::(app, Nil), here).toolchain(launcher = app, client = client)
      assertTrue(
        fault(Projects(CatalogSpec(row), app, client)) == Some(launcher(app, ToolchainLauncherFault.NotAssembly))
      )
    ,
    test("a client that does not build Native is refused"):
      val app    = module("app", Jvm)(scala3).assembly(MainClass("demo.Main"))
      val client = nativeClient
      val jvm    = module("jvm", Jvm)(scala3)
      val row    = ShipDef
        .line(ship.value, number, ::(app, Nil), here)
        .asking(launcher = app, client = jvm)
      assertTrue(
        fault(Projects(CatalogSpec(row), app, client, jvm)) == Some(clientFault(jvm, ToolchainClientFault.NotNative))
      )
    ,
    test("a client the build does not contain is refused"):
      val app    = module("app", Jvm)(scala3).assembly(MainClass("demo.Main"))
      val client = nativeClient
      val row    = ShipDef.line(ship.value, number, ::(app, Nil), here).toolchain(launcher = app, client = client)
      assertTrue(fault(Projects(CatalogSpec(row), app)) == Some(clientFault(client, ToolchainClientFault.Missing)))
    ,
    test("a native client with no main is refused"):
      val app    = module("app", Jvm)(scala3).assembly(MainClass("demo.Main"))
      val client = module("client", Native)(scala3).clang(ClangMajor(21))
      val row    = ShipDef.line(ship.value, number, ::(app, Nil), here).toolchain(launcher = app, client = client)
      assertTrue(
        fault(Projects(CatalogSpec(row), app, client)) == Some(clientFault(client, ToolchainClientFault.NoMain))
      ),
  )

  private def nativeClient: ModuleDef[Platform.Native.type] =
    module("client", Native)(scala3).clang(ClangMajor(21)).main(MainClass("demo.Client"))

  private def fault(loaded: Either[BuildError, ProjectGraph]): Option[BuildError] =
    loaded match
      case Left(error) => Some(error)
      case Right(_)    => None

  private def launcher(module: ModuleDef[?], why: ToolchainLauncherFault): BuildError =
    BuildError.ToolchainLauncher(ship, ProjectName.trusted(module.name), why, here)

  private def clientFault(module: ModuleDef[?], why: ToolchainClientFault): BuildError =
    BuildError.ToolchainClient(ship, ProjectName.trusted(module.name), why, here)
end ToolchainLawSpec
