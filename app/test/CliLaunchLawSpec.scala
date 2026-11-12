package histos.cli

import histos.workspace.Workspace

import java.nio.file.Files

import zio.ZIO
import zio.test.{TestAspect, TestSystem, ZIOSpecDefault, assertTrue, suite, test}

/** The process entry answers help, version, explain, and setup without a server. */
object CliLaunchLawSpec extends ZIOSpecDefault:
  def spec = suite("cli launch laws")(
    test("help, version, explain, and setup answer on this process, and setup does not write without --fix"):
      Workspace
        .work("histos-cli-launch")
        .flatMap: dir =>
          val build  = dir.resolve("histos.scala")
          val pin    = dir.resolve("build.properties")
          val source = "object Build\n"
          for
            help    <- Cli.launch(List("help"), dir)
            unknown <- Cli.launch(List("help", "no-such-command"), dir)
            absent  <- Cli.launch(List("setup", "supply-chain"), dir)
            missing <- Cli.launch(List("version"), dir)
            _       <- Workspace.writeString(pin, "histos.version=1.2.3\n")
            version <- Cli.launch(List("version"), dir)
            _       <- Workspace.writeString(build, source)
            dry     <- Cli.launch(List("setup", "supply-chain"), dir)
            kept    <- ZIO.attempt(Files.readString(build)).orDie
            fixed   <- Cli.launch(List("setup", "supply-chain", "--fix"), dir)
            wrote   <- ZIO.attempt(Files.readString(build)).orDie
            named   <- Cli.launch(List("--explain", "jdk.source"), dir)
            other   <- Cli.launch(List("--explain", "not-a-check"), dir)
          yield assertTrue(
            help.exit == CliExit.Success && help.out.contains("histos help <command>"),
            unknown.exit == CliExit.Usage && unknown.err.contains("unknown command 'no-such-command'"),
            absent.exit == CliExit.Usage && absent.out == "histos.scala is missing\n",
            missing.exit == CliExit.ResolveFailed && missing.err.contains("launcher pin is absent"),
            version.exit == CliExit.Success && version.out == "histos 1.2.3",
            dry.exit == CliExit.Success && dry.out.contains("would write .supplyChain(SupplyChain())"),
            kept == source,
            fixed.exit == CliExit.Success && fixed.out.contains("wrote histos.scala"),
            wrote.contains(".supplyChain(SupplyChain())"),
            named.exit == CliExit.Success && named.out.contains("JDK 25"),
            other.exit == CliExit.Usage && other.err.contains("no explanation for 'not-a-check'"),
          )
    @@ TestAspect.withLiveSystem,
    test("new --bare writes the build and the sources, and does not write the wrapper"):
      val jdk  = java.lang.System.getProperty("java.home")
      val home = java.lang.System.getProperty("user.home")
      for
        _    <- TestSystem.putEnv("HISTOS_VERSION", "9.9.9")
        _    <- TestSystem.putProperty("java.home", jdk)
        _    <- TestSystem.putProperty("user.home", home)
        _    <- TestSystem.putProperty("os.name", java.lang.System.getProperty("os.name"))
        _    <- TestSystem.putProperty("os.arch", java.lang.System.getProperty("os.arch"))
        dir  <- Workspace.work("histos-cli-new")
        made <- Cli.launch(List("new", "--bare", "sample"), dir)
        root = dir.resolve("sample")
        build <- textAt(root.resolve("histos.scala"))
        pin   <- textAt(root.resolve("build.properties"))
        main = root
          .resolve("app")
          .resolve("src")
          .resolve("main")
          .resolve("scala")
          .resolve("sample")
          .resolve("Main.scala")
        mainText <- textAt(main)
        wrapped  <- ZIO.attempt(Files.exists(root.resolve("histos"))).orDie
        ci       <- ZIO.attempt(Files.exists(root.resolve(".github"))).orDie
      yield assertTrue(
        made.exit == CliExit.Success,
        made.err == "",
        made.out == s"${root.toAbsolutePath.normalize}\nhistos test",
        build.contains("module(\"app\", Jvm)"),
        pin == "histos.version=9.9.9\n",
        mainText.contains("object Main"),
        wrapped == false,
        ci == false,
      ),
  )
  private def textAt(path: java.nio.file.Path): ZIO[Any, Nothing, String] =
    ZIO
      .attempt(if Files.isRegularFile(path) then Files.readString(path) else "")
      .orDie
