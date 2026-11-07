package histos.cli

import histos.workspace.Workspace

import zio.ZIO
import zio.test.{TestSystem, ZIOSpecDefault, assertTrue, suite, test}

/** `graph` hashes one api jar. A classes directory is packed before that read. */
object CliClasspathLawSpec extends ZIOSpecDefault:
  def spec = suite("cli api classpath")(
    test("graph on an empty repo fails on the build file, not on the api classes directory"):
      val jdk  = java.lang.System.getProperty("java.home")
      val os   = java.lang.System.getProperty("os.name")
      val arch = java.lang.System.getProperty("os.arch")
      for
        dir  <- Workspace.work("histos-cli-classpath")
        _    <- TestSystem.putProperty("java.home", jdk)
        _    <- TestSystem.putProperty("os.name", os)
        _    <- TestSystem.putProperty("os.arch", arch)
        _    <- TestSystem.putProperty("user.home", dir.toString)
        _    <- TestSystem.putEnv("XDG_CACHE_HOME", "")
        done <- Cli.launch(List("graph"), dir)
      yield assertTrue(
        done.err.contains("no build file"),
        done.err.contains("histos.scala"),
        !done.err.contains(".histos/classes"),
      )
  )
end CliClasspathLawSpec
