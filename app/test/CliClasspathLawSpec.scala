package histos.cli

import histos.LawMachine
import histos.central.ReleaseFeed
import histos.resolve.Resolve
import histos.workspace.Workspace
import slayer.stubbed

import zio.Scope
import zio.test.{TestEnvironment, TestSystem, ZIOSpecDefault, assertTrue, suite, test}

/** `graph` hashes one api jar. A classes directory is packed before that read. */
object CliClasspathLawSpec extends ZIOSpecDefault:
  def spec = suite("cli api classpath")(
    test("graph on an empty repo fails on the build file, not on the api classes directory"):
      for
        dir  <- Workspace.work("histos-cli-classpath")
        _    <- LawMachine.system(dir)
        _    <- TestSystem.putEnv("XDG_CACHE_HOME", "")
        done <- Cli.launch(List("graph"), dir)
      yield assertTrue(
        done.err.contains("no build file"),
        done.err.contains("histos.scala"),
        !done.err.contains(".histos/classes"),
      )
  ).provideSomeLayer[TestEnvironment & Scope](stubbed[ReleaseFeed] ++ stubbed[Resolve] ++ LawMachine.redirected)
