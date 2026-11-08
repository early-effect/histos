package histos.cli

import histos.workspace.Workspace

import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, test}

/** Cancel with no server names the run that is not there. */
object CliCancelLawSpec extends ZIOSpecDefault:
  def spec = suite("cli cancel laws")(
    test("an unknown run is refused"):
      Workspace.work("histos-cli-cancel").flatMap: dir =>
        Cli.launch(List("cancel", "absent"), dir).map: done =>
          assertTrue(
            done.exit != CliExit.Success,
            done.out == "",
            done.err.contains("no run 'absent'"),
          )
  ) @@ TestAspect.withLiveSystem
end CliCancelLawSpec
