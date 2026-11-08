package histos.cli

import histos.clean.Products
import histos.workspace.Workspace

import java.nio.file.Files

import zio.ZIO
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, test}

/** Cache, clean, and observe answer on this process. Dry-run does not remove a store entry. */
object CliLocalLawSpec extends ZIOSpecDefault:
  def spec = suite("cli local laws")(
    test("cache, clean, and observe answer without a server, and clean dry-run does not remove"):
      Workspace.work("histos-cli-local").flatMap: dir =>
        val roots = (Products.moduleRoots ++ Products.sharedRoots).map(name => s".histos/$name")
        val tally = Products.observation(dir).toString
        for
          path    <- Cli.launch(List("cache", "path"), dir)
          again   <- Cli.launch(List("cache", "path"), dir)
          listed  <- Cli.launch(List("cache", "list"), dir)
          dry     <- Cli.launch(List("cache", "clean", "--dry-run"), dir)
          cleaned <- Cli.launch(List("clean"), dir)
          preview <- Cli.launch(List("preview", "clean"), dir)
          full    <- Cli.launch(List("preview", "cleanFull"), dir)
          missing <- Cli.launch(List("observe"), dir)
        yield assertTrue(
          path.exit == CliExit.Success && path.err == "" && path.out.nonEmpty && path.out == again.out,
          listed.exit == CliExit.Success && listed.err == "",
          listed.out.isEmpty || listed.out.linesIterator.forall(line => digestLine(line)),
          dry.exit == CliExit.Success && dry.err == "" && dry.out.nonEmpty &&
            dry.out.linesIterator.forall(_.startsWith("would remove")),
          cleaned.exit == CliExit.Success && cleaned.out == "cleaned" && cleaned.err == "",
          preview.exit == CliExit.Success && preview.out == roots.mkString("\n") && preview.err == "",
          full.exit == CliExit.Success &&
            full.out == (roots :+ "content-store entries for this repo would be forgotten").mkString("\n"),
          missing.exit == CliExit.TaskFailed && missing.err.contains(s"no observation at $tally"),
          missing.err.contains("fix  histos compile"),
        )
    ,
    test("an empty affected list runs nothing, a watch of mcp is usage, and latest rewrites a revision"):
      Workspace.work("histos-cli-route").flatMap: dir =>
        val pin = dir.resolve("build.properties")
        for
          none    <- Cli.launch(List("test", "--affected", "[]"), dir)
          bad     <- Cli.launch(List("test", "--affected", "nope"), dir)
          watched <- Cli.launch(List("~", "mcp"), dir)
          bare    <- Cli.launch(List("~"), dir)
          absent  <- Cli.launch(List("launcher", "--latest"), dir)
          _       <- Workspace.writeString(pin, "histos.version=0.1.0-20260101.120000-1\n")
          moved   <- Cli.launch(List("launcher", "--latest"), dir)
          wrote   <- ZIO.attempt(Files.readString(pin)).orDie
          _       <- Workspace.writeString(pin, "histos.version=1.2.3\n")
          held    <- Cli.launch(List("launcher", "--update"), dir)
          stayed  <- ZIO.attempt(Files.readString(pin)).orDie
        yield assertTrue(
          none.exit == CliExit.Success && none.out == "" && none.err == "",
          bad.exit == CliExit.Usage && bad.err.contains("affected is a JSON list of module names"),
          watched.exit == CliExit.Usage && watched.err.contains("usage: histos help"),
          bare.exit == CliExit.Usage && bare.err.contains("watch needs a command"),
          absent.exit == CliExit.ResolveFailed && absent.err.contains("launcher pin is absent"),
          moved.exit == CliExit.Success && moved.out == "histos.version=0.1.0-SNAPSHOT" && moved.err == "",
          wrote == "histos.version=0.1.0-SNAPSHOT\n",
          held.exit == CliExit.ResolveFailed && held.err.contains("a release pin does not move: 1.2.3"),
          stayed == "histos.version=1.2.3\n",
        )
  ) @@ TestAspect.withLiveSystem

  private def digestLine(line: String): Boolean =
    line.split(" ").toList match
      case digest :: bytes :: Nil => digest.nonEmpty && bytes.nonEmpty && bytes.forall(_.isDigit)
      case _                      => false
end CliLocalLawSpec
