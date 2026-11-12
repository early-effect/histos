package histos.cli

import histos.LawMachine
import histos.central.ReleaseFeed
import histos.resolve.Resolve
import histos.workspace.Workspace
import slayer.stubbed

import zio.Scope
import zio.test.{TestEnvironment, TestSystem, ZIOSpecDefault, assertTrue, suite, test}

/** `graph` loads the build. The api jar is the one this process packed, not the classes directory. A build with no
  * plugins resolves nothing, so `Resolve` and the release feed are stubs that fail any law that reads them.
  */
object CliReadyLawSpec extends ZIOSpecDefault:
  private val build =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val graphlaw = module("graphlaw", Jvm)(scala"3.10.0")
      |    Projects(graphlaw)
      |""".stripMargin

  def spec = suite("cli ready laws")(
    test("graph, affected, and toolchain run on a build with no plugins"):
      for
        dir      <- Workspace.work("histos-cli-ready")
        _        <- LawMachine.system(dir)
        _        <- TestSystem.putEnv("XDG_CACHE_HOME", "")
        _        <- Workspace.writeString(dir.resolve("histos.scala"), build)
        graph    <- Cli.launch(List("graph"), dir)
        affected <- Cli.launch(List("affected", "--base", "all"), dir)
        tools    <- Cli.launch(List("toolchain"), dir)
      yield assertTrue(
        graph.exit == CliExit.Success,
        graph.err == "",
        graph.out == "graphlaw jvm 3.10.0",
        affected.exit == CliExit.Success,
        affected.err == "",
        affected.out == "[\"all\"]\n",
        tools.exit == CliExit.Success,
        tools.err == "",
        tools.out.contains("jdk "),
        tools.out.contains("node not required"),
        tools.out.contains("clang not required"),
      )
  ).provideSomeLayer[TestEnvironment & Scope](stubbed[ReleaseFeed] ++ stubbed[Resolve] ++ LawMachine.redirected)
end CliReadyLawSpec
