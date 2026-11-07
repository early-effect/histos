package histos.cli

import histos.workspace.Workspace

import zio.ZIO
import zio.test.{TestSystem, ZIOSpecDefault, assertTrue, suite, test}

/** `graph` loads the build. The api jar is the one this process packed, not the classes directory. */
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
      val jdk  = java.lang.System.getProperty("java.home")
      val os   = java.lang.System.getProperty("os.name")
      val arch = java.lang.System.getProperty("os.arch")
      for
        dir      <- Workspace.work("histos-cli-ready")
        _        <- TestSystem.putProperty("java.home", jdk)
        _        <- TestSystem.putProperty("os.name", os)
        _        <- TestSystem.putProperty("os.arch", arch)
        _        <- TestSystem.putProperty("user.home", dir.toString)
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
  )
end CliReadyLawSpec
