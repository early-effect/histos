package histos.cli

import histos.server.CatalogQuery

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.cli.CliConfig
import zio.cli.completion.Completion
import zio.test.*
import zio.{UIO, ZIO}

object CompletionSpec extends ZIOSpecDefault:
  def spec = suite("shell completion")(
    test("an empty command line offers the help names and no retired verb") {
      for words <- offered(List("histos", ""))
      yield assertTrue(
        Help.names.forall(words.contains),
        words.contains("completion"),
        words.contains("~"),
        words.contains("server"),
        !words.contains("testOnly"),
        !words.contains("testQuick"),
        !words.contains("cleanFull"),
        !words.contains("publishLocal"),
        !words.contains("client"),
        !words.contains("launcher"),
        !words.contains("--restart"),
      )
    },
    test("publish, deps, cache, preview, toolchain, and self offer their subcommands") {
      for
        publish    <- offered(List("histos", "publish", ""))
        deps       <- offered(List("histos", "deps", ""))
        cache      <- offered(List("histos", "cache", ""))
        preview    <- offered(List("histos", "preview", ""))
        toolchain  <- offered(List("histos", "toolchain", ""))
        selfUpdate <- offered(List("histos", "self", ""))
      yield assertTrue(
        List("local", "withdraw", "snapshots", "release", "assets", "promote", "drop").forall(publish.contains),
        List("check", "update", "audit", "pr", "alert", "snapshot").forall(deps.contains),
        List("path", "list", "clean", "promote", "forget", "verify").forall(cache.contains),
        List("compile", "test", "publish", "clean", "run").forall(preview.contains),
        List("install", "list", "client").forall(toolchain.contains),
        selfUpdate.contains("update"),
      )
    },
    test("test offers the quick flags and a project slot") {
      for words <- offered(List("histos", "test", ""))
      yield assertTrue(
        words.contains("--quick"),
        words.contains("--platform"),
        words.contains("--only"),
        words.contains("--affected"),
        words.contains("--test"),
        words.contains("--tag"),
        words.contains("--ignore-tag"),
        CompletionSurface.bash.contains("complete projects"),
        CompletionSurface.zsh.contains("complete modules"),
      )
    },
    test("clean offers --full") {
      for words <- offered(List("histos", "clean", ""))
      yield assertTrue(words.contains("--full"))
    },
    test("doctor --area offers the built-in areas") {
      for words <- offered(List("histos", "doctor", "--area", ""))
      yield assertTrue(
        List("toolchain", "build", "workflows", "repo", "supply-chain", "site", "release").forall(words.contains)
      )
    },
    test("init --from offers sbt and zipx") {
      for words <- offered(List("histos", "init", "--from", ""))
      yield assertTrue(words.contains("sbt"), words.contains("zipx"))
    },
    test("run offers a module slot and then the platforms") {
      for platforms <- offered(List("histos", "run", "app", "--platform", ""))
      yield assertTrue(
        platforms.contains("jvm"),
        platforms.contains("js"),
        platforms.contains("native"),
        CompletionSurface.zsh.contains("inspect"),
        CompletionSurface.zsh.contains("versions"),
        CompletionSurface.zsh.contains("doc"),
        CompletionSurface.bash.contains("complete modules"),
      )
    },
    test("a watch offers commands, and --restart offers only run") {
      for
        watched   <- offered(List("histos", "~", ""))
        restarted <- offered(List("histos", "~", "--restart", ""))
      yield assertTrue(
        watched.contains("compile"),
        watched.contains("--restart"),
        restarted.contains("run"),
        !restarted.contains("compile"),
      )
    },
    test("the scripts contain every word the engine returns") {
      val bash = CompletionSurface.bash
      val zsh  = CompletionSurface.zsh
      ZIO.foreach(cursors)(offered).map { lists =>
        val words = lists.flatten.distinct
        assertTrue(words.forall(word => bash.contains(word) && zsh.contains(word)))
      }
    },
    test("zsh describes compile and completes files, and bash parses") {
      for code <- ZIO.attemptBlocking {
          val file = Files.createTempFile("histos-completion", ".bash")
          Files.writeString(file, CompletionSurface.bash)
          val ran = new ProcessBuilder("bash", "-n", file.toString).start().waitFor()
          Files.deleteIfExists(file)
          ran
        }
      yield assertTrue(
        code == 0,
        CompletionSurface.zsh.contains("compile:Compile the named projects"),
        CompletionSurface.zsh.contains("_files"),
        CompletionSurface.bash.contains("compgen -f"),
        CompletionSurface.bash.contains("complete -F _histos histos"),
        CompletionSurface.bash.contains("complete -F _histos ./histos"),
        CompletionSurface.zsh.contains("#compdef histos ./histos"),
        CompletionSurface.zsh.contains("_n0 2 0"),
        CompletionSurface.zsh.contains("_n0()"),
        CompletionSurface.bash.contains("_n0 1 0"),
        !CompletionSurface.zsh.contains("_histos_n0"),
        !CompletionSurface.bash.contains("_histos_n0"),
        CompletionSurface.bash.contains("[ -n \"$cur\" ]"),
      )
    },
    test("the shell is the argument, then HISTOS_SHELL, then the parent, then SHELL") {
      assertTrue(
        ShellChoice.decide(None, None, Some("zsh"), Some("/bin/bash")) == Right(ShellKind.Zsh),
        ShellChoice.decide(None, None, Some("/usr/bin/java"), Some("/bin/zsh")) == Right(ShellKind.Zsh),
        ShellChoice.decide(None, None, Some("histos-client"), Some("/bin/bash")) == Right(ShellKind.Bash),
        ShellChoice.decide(None, None, Some(""), Some("")) == Left(ShellFault.Unknown),
        ShellChoice.decide(None, Some("zsh"), Some("java"), None) == Right(ShellKind.Zsh),
        ShellChoice.decide(None, Some("fish"), Some("zsh"), Some("/bin/bash")) == Left(ShellFault.Rejected("fish")),
        ShellChoice.decide(Some(ShellKind.Bash), Some("zsh"), Some("zsh"), Some("/bin/zsh")) == Right(ShellKind.Bash),
        ShellChoice.decide(None, None, Some("-zsh"), None) == Right(ShellKind.Zsh),
        ShellChoice.decide(None, None, Some("/bin/bash"), None) == Right(ShellKind.Bash),
        ShellChoice.decide(None, Some("  "), Some("zsh"), None) == Right(ShellKind.Zsh),
      )
    },
    test("completion and complete parse, and a bad shell does not") {
      for
        bare     <- Grammar.parse(List("completion")).either
        zsh      <- Grammar.parse(List("completion", "zsh")).either
        install  <- Grammar.parse(List("completion", "install", "bash")).either
        bad      <- Grammar.parse(List("completion", "fish")).either
        projects <- Grammar.parse(List("complete", "projects")).either
        flags    <- Grammar.parse(List("complete", "flags", "ship")).either
        missing  <- Grammar.parse(List("complete")).either
      yield assertTrue(
        bare == Right(Command.Completion(CompletionRequest.Show(None))),
        zsh == Right(Command.Completion(CompletionRequest.Show(Some(ShellKind.Zsh)))),
        install == Right(Command.Completion(CompletionRequest.Install(Some(ShellKind.Bash)))),
        bad.isLeft,
        projects == Right(Command.Complete(CatalogQuery.Projects)),
        flags == Right(Command.Complete(CatalogQuery.Flags("ship"))),
        missing.isLeft,
      )
    },
    test("install writes the script under the given home") {
      ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-completion")).orDie)(remove).flatMap {
        home =>
          CompletionSurface.write(home, ShellKind.Zsh).map { text =>
            val path = CompletionSurface.file(home, ShellKind.Zsh)
            assertTrue(
              Files.isRegularFile(path),
              Files.readString(path).contains("#compdef histos"),
              text.contains("fpath=(~/.histos/completions $fpath)"),
              text.contains(path.toString),
            )
          }
      }
    },
  )

  private val cursors: List[List[String]] = List(
    List("histos", ""),
    List("histos", "publish", ""),
    List("histos", "deps", ""),
    List("histos", "cache", ""),
    List("histos", "preview", ""),
    List("histos", "toolchain", ""),
    List("histos", "self", ""),
    List("histos", "test", ""),
    List("histos", "clean", ""),
    List("histos", "doctor", "--area", ""),
    List("histos", "init", "--from", ""),
    List("histos", "run", "app", ""),
    List("histos", "~", ""),
    List("histos", "~", "--restart", ""),
    List("histos", "completion", ""),
    List("histos", "fmt", ""),
  )

  private def offered(words: List[String]): UIO[List[String]] =
    Completion
      .complete(words, words.length - 1, CompletionSurface.command, CliConfig.default)
      .map(_.map(_.stripSuffix(" ")).filter(_.nonEmpty))

  private def remove(root: Path): UIO[Unit] =
    ZIO
      .attemptBlocking {
        val stream = Files.walk(root)
        try
          val files = List.from(stream.iterator.asScala).reverse
          files.foreach(file => Files.deleteIfExists(file))
        finally stream.close()
      }
      .orDie
      .unit
end CompletionSpec
