package histos.cli

import histos.LawMachine
import histos.central.{MavenModule, ReleaseFeed}
import histos.publish.LauncherPin
import histos.resolve.Resolve
import histos.version.ShipNumber
import histos.workspace.Workspace
import slayer.{any, stub, stubbed}

import zio.{Scope, ZIO}
import zio.test.{TestConsole, TestEnvironment, TestSystem, ZIOSpecDefault, assertTrue, suite, test}

/** `graph` loads the build. The api jar is the one this process packed, not the classes directory. A build with no
  * plugins resolves nothing, so `Resolve` and the release feed are stubs that fail any law that reads them, unless the
  * law answers the feed itself.
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
    ,
    test("a release past the pin is one line on standard error, and the command still answers"):
      for
        dir  <- Workspace.work("histos-cli-notice")
        _    <- LawMachine.system(dir)
        _    <- TestSystem.putEnv("XDG_CACHE_HOME", "")
        _    <- Workspace.writeString(dir.resolve(LauncherPin.file), "histos.version=0.1.0\n")
        _    <- stub[ReleaseFeed](_.newest(any[MavenModule]))(ZIO.succeed(Some(ShipNumber("0.2.0"))))
        done <- Cli.launch(List("help", "version"), dir)
        err  <- TestConsole.outputErr
      yield assertTrue(
        err == Vector("histos 0.2.0 is released and histos.version pins 0.1.0. Run `histos update`.\n"),
        done.exit == CliExit.Success,
        done.out.startsWith("histos version\n"),
      ),
  ).provideSomeLayer[TestEnvironment & Scope](stubbed[ReleaseFeed] ++ stubbed[Resolve] ++ LawMachine.redirected)
end CliReadyLawSpec
