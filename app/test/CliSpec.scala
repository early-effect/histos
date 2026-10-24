package histos.cli

import histos.jdk.{Machine, Os, Settled}
import histos.workspace.{IoFailure, IoFault}
import histos.native.NativeOrigin
import histos.load.PluginLink
import histos.plugin.*
import histos.workflow.WorkflowAction
import histos.load.{ApiJar, LoadError}
import histos.*

import java.nio.file.{Files, Path}

import _root_.scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object CliSpec extends ZIOSpecDefault:
  private val source =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph =
      |    val core = module("core", Jvm)(scala"3.9.0")
      |    Projects(core)
      |""".stripMargin

  private val apiJar: ZIO[Scope, LoadError, Path] =
    ApiJar.locate

  private final class Listed extends HistosPlugin:
    val id: PluginId = PluginId("dev.example")

    def contribute: Contribution =
      Contribution(
        List(
          DeclaredTask(
            TaskName("echo"),
            Seal.Open,
            Set(Need.Network),
            List(
              DeclaredInput.Directory(RepoPath("docs")),
              DeclaredInput.Env(EnvName("TOKEN"))
            ),
            Nil,
            Nil
          )
        ),
        List(
          CommandSpec(
            CommandName("list"),
            Nil,
            TaskName("echo"),
            PluginDetail("lists the site")
          )
        ),
        List(CiJob(JobName("nightly"), TaskName("echo"), JobWhen.Always))
      )

    def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
      ZIO.succeed(PluginOutput("", Nil))

  private def bare: ZIO[Scope, LoadError, Path] =
    for
      dir <- ZIO
        .attempt(Files.createTempDirectory("histos-bare"))
        .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
      _ <- ZIO.addFinalizer(delete(dir))
    yield dir

  private def repo: ZIO[Scope, LoadError, Path] =
    for
      dir <- ZIO
        .attempt(Files.createTempDirectory("histos-cli"))
        .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
      _ <- ZIO.addFinalizer(delete(dir))
      _ <- ZIO
        .attempt(Files.writeString(dir.resolve("histos.scala"), source))
        .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
    yield dir

  private def delete(root: Path): UIO[Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths
          .sortBy(_.getNameCount)
          .reverse
          .foreach(path => Files.deleteIfExists(path))
    }.ignore

  private def invoke(args: List[String]): ZIO[Scope, LoadError, Completed] =
    for
      jar <- apiJar
      dir <- repo
      done <- Cli.execute(args, dir, jar).provideSome[Scope](layers(dir))
    yield done

  private val cli = suite("cli")(
    test("version does not touch the build"):
      for parsed <- Cli.parse(List("version"))
      yield assertTrue(
        parsed == Command.Version && Cli.VersionText.startsWith("histos ")
      )
    ,
    test("cache clean takes --dry-run"):
      for
        preview <- Cli.parse(List("cache", "clean", "--dry-run"))
        clean <- Cli.parse(List("cache", "clean"))
      yield assertTrue(
        preview == Command.CacheClean(true) && clean == Command.CacheClean(
          false
        )
      )
    ,
    test("toolchain is a command"):
      for parsed <- Cli.parse(List("toolchain"))
      yield assertTrue(parsed == Command.Toolchain)
    ,
    test("resolve takes update or check, not both"):
      for
        plain <- Cli.parse(List("resolve"))
        update <- Cli.parse(List("resolve", "--update"))
        check <- Cli.parse(List("resolve", "--check"))
        both <- Cli.parse(List("resolve", "--update", "--check")).either
      yield assertTrue(
        plain == Command.Resolve(ResolveAction.Verify),
        update == Command.Resolve(ResolveAction.Update),
        check == Command.Resolve(ResolveAction.Check),
        both == Left(CliParseError.Rejected)
      )
    ,
    test("workflow checks by default and update rewrites"):
      for
        plain <- Cli.parse(List("workflow"))
        check <- Cli.parse(List("workflow", "--check"))
        update <- Cli.parse(List("workflow", "--update"))
        both <- Cli.parse(List("workflow", "--check", "--update")).either
      yield assertTrue(
        plain == Command.Workflow(WorkflowAction.Check),
        check == Command.Workflow(WorkflowAction.Check),
        update == Command.Workflow(WorkflowAction.Update),
        both == Left(CliParseError.Rejected)
      )
    ,
    test("publish takes a repository and project names"):
      for
        named <- Cli.parse(List("publish", "repo", "core"))
        bare <- Cli.parse(List("publish")).either
      yield assertTrue(
        named == Command.Publish("repo", List("core")),
        bare == Left(CliParseError.MissingArgument)
      )
    ,
    test("assembly takes an optional path"):
      for
        plain <- Cli.parse(List("assembly"))
        path <- Cli.parse(List("assembly", "dist/histos.jar"))
      yield assertTrue(
        plain == Command.Assembly(None) && path == Command.Assembly(
          Some("dist/histos.jar")
        )
      )
    ,
    test("compile takes project names"):
      for
        entire <- Cli.parse(List("compile"))
        named <- Cli.parse(List("compile", "core"))
      yield assertTrue(
        entire == Command.Compile(Nil),
        named == Command.Compile(List("core"))
      )
    ,
    test("testQuick and watch parse"):
      for
        all <- Cli.parse(List("testQuick"))
        scoped <- Cli.parse(
          List("testQuick", "--module", "engine", "com.acme.Foo*")
        )
        watched <- Cli.parse(List("~", "testQuick", "com.acme.Foo*"))
        empty <- Cli.parse(List("~")).either
        nested <- Cli.parse(List("~", "~", "test")).either
      yield assertTrue(
        all match
          case Command.TestQuick(Nil, Nil) => true
          case _                           => false
        ,
        scoped match
          case Command.TestQuick(List("engine"), patterns) =>
            patterns.map(_.value) == List("com.acme.Foo*")
          case _ => false
        ,
        watched match
          case Command.Watch(Command.TestQuick(Nil, patterns)) =>
            patterns.map(_.value) == List("com.acme.Foo*")
          case _ => false
        ,
        empty == Left(CliParseError.WatchCommand("watch needs a command")),
        nested == Left(CliParseError.WatchCommand("watch does not nest"))
      )
    ,
    test("testOnly selects suites by name"):
      for
        plain <- Cli.parse(List("testOnly", "com.acme.Foo*"))
        scoped <- Cli.parse(
          List(
            "testOnly",
            "--module",
            "engine",
            "--module",
            "app",
            "com.acme.*",
            "com.acme.Foo?"
          )
        )
        missing <- Cli.parse(List("testOnly")).either
        flag <- Cli.parse(List("testOnly", "--nope", "com.acme.Foo*")).either
      yield assertTrue(
        plain match
          case Command.TestOnly(Nil, patterns) =>
            patterns.map(_.value) == List("com.acme.Foo*")
          case _ => false
        ,
        scoped match
          case Command.TestOnly(modules, patterns) =>
            modules == List("engine", "app") && patterns.map(_.value) == List(
              "com.acme.*",
              "com.acme.Foo?"
            )
          case _ => false
        ,
        missing == Left(CliParseError.MissingPattern),
        flag == Left(CliParseError.Pattern("testOnly does not take --nope"))
      )
    ,
    test("test takes project names"):
      for
        entire <- Cli.parse(List("test"))
        named <- Cli.parse(List("test", "core"))
      yield assertTrue(
        entire == Command.Test(Nil),
        named == Command.Test(List("core"))
      )
    ,
    test("workflow without a file fails"):
      ZIO.scoped:
        for done <- invoke(List("workflow"))
        yield assertTrue(
          done.exit == CliExit.WorkflowFailed && done.exit.code == 6 && done.err
            .contains("ci.yml")
        )
    ,
    test("publish of an empty catalog writes nothing"):
      ZIO.scoped:
        for done <- invoke(List("publish", "repo"))
        yield assertTrue(
          done.exit == CliExit.PublishFailed && done.exit.code == 9 && done.err
            .contains("nothing publishes")
        )
    ,
    test(
      "compile without a lock fails, and an unknown project is not a lock error"
    ):
      ZIO.scoped:
        for
          missing <- invoke(List("compile"))
          unknown <- invoke(List("compile", "missing"))
        yield assertTrue(
          missing.exit == CliExit.CompileFailed && missing.exit.code == 8 && missing.err
            .contains("dependencies.lock"),
          unknown.exit == CliExit.CompileFailed && unknown.err.contains(
            "missing"
          ) && !unknown.err.contains("dependencies.lock")
        )
    ,
    test(
      "a cell that fails to compile fails compile with the compile exit code"
    ):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          _ <- ZIO
            .attempt {
              Files.createDirectories(dir.resolve("core/src"))
              Files.writeString(
                dir.resolve("core/src/Bad.scala"),
                "object Bad:\n  val n: Int = \"text\"\n"
              )
              Files.createDirectories(dir.resolve(".histos"))
              Files.writeString(
                dir.resolve(".histos/dependencies.lock"),
                "cell core 3.9.0 jvm\n"
              )
            }
            .mapError(_ => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          done <- Cli
            .execute(List("compile"), dir, jar)
            .provideSome[Scope](layers(dir))
        yield assertTrue(done.exit == CliExit.CompileFailed)
    ,
    test(
      "test without a lock fails, and an unknown project is not a lock error"
    ):
      ZIO.scoped:
        for
          missing <- invoke(List("test"))
          unknown <- invoke(List("test", "missing"))
        yield assertTrue(
          missing.exit == CliExit.ResolveFailed && missing.err.contains(
            "dependencies.lock"
          ),
          unknown.exit == CliExit.TestFailed && unknown.exit.code == 7 && unknown.err
            .contains("missing")
        )
    ,
    test("resolve without a lock fails"):
      ZIO.scoped:
        for done <- invoke(List("resolve"))
        yield assertTrue(
          done.exit == CliExit.ResolveFailed && done.exit.code == 5 && done.err
            .contains("dependencies.lock")
        )
    ,
    test("toolchain reports a missing lock"):
      ZIO.scoped:
        for done <- invoke(List("toolchain"))
        yield assertTrue(
          done.exit == CliExit.ToolchainFailed && done.exit.code == 4 && done.err
            .contains("jdk.lock")
        )
    ,
    test("toolchain prints the chosen home"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          jdk = dir.resolve("jdk")
          _ <- ZIO
            .attempt {
              Files.createDirectories(jdk.resolve("bin"))
              Files.createDirectories(dir.resolve(".histos"))
              Files.writeString(jdk.resolve("bin").resolve("javac"), "")
              Files.writeString(
                jdk.resolve("release"),
                "JAVA_VERSION=\"21.0.2\"\n"
              )
              Files.writeString(
                dir.resolve(".histos/toolchain.local"),
                jdk.toString + "\n"
              )
              Files.writeString(
                dir.resolve(".histos").resolve("jdk.lock"),
                """major 21
                     |version 21.0.2
                     |archive linux-x86_64 https://example.test/jdk.zip aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                     |""".stripMargin
              )
            }
            .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          done <- Cli
            .execute(List("toolchain"), dir, jar)
            .provideSome[Scope](layers(dir, Map("JAVA_HOME" -> jdk.toString)))
        yield assertTrue(
          done.exit == CliExit.Success && done.out.contains(
            "chosen Override"
          ) && done.out.contains("21.0.2") && done.out.contains("lock 21.0.2"),
          !done.out.contains("node"),
          !done.out.contains("clang")
        )
    ,
    test("a js project asks for a node lock and still prints the jdk"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          jdk = dir.resolve("jdk")
          _ <- ZIO
            .attempt {
              Files.createDirectories(jdk.resolve("bin"))
              Files.createDirectories(dir.resolve(".histos"))
              Files.writeString(jdk.resolve("bin").resolve("javac"), "")
              Files.writeString(
                jdk.resolve("release"),
                "JAVA_VERSION=\"21.0.2\"\n"
              )
              Files.writeString(
                dir.resolve(".histos/toolchain.local"),
                jdk.toString + "\n"
              )
              Files.writeString(
                dir.resolve(".histos").resolve("jdk.lock"),
                """major 21
                     |version 21.0.2
                     |archive linux-x86_64 https://example.test/jdk.zip aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
                     |""".stripMargin
              )
              Files.writeString(
                dir.resolve("histos.scala"),
                """import histos.*
                     |object build extends HistosBuild:
                     |  def graph =
                     |    val core = module("core", Js)(scala"3.9.0")
                     |    Projects(core)
                     |""".stripMargin
              )
            }
            .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          done <- Cli
            .execute(List("toolchain"), dir, jar)
            .provideSome[Scope](layers(dir, Map("JAVA_HOME" -> jdk.toString)))
        yield assertTrue(
          done.exit == CliExit.ToolchainFailed,
          done.out.contains("chosen Override"),
          done.err.contains("node.lock")
        )
    ,
    test("unknown commands and missing ids are usage"):
      for
        empty <- Cli.parse(Nil).either
        run <- Cli.parse(List("run")).either
        nope <- Cli.parse(List("nope")).either
      yield assertTrue(empty.isLeft && run.isLeft && nope.isLeft)
    ,
    test("graph prints the module"):
      ZIO.scoped:
        for done <- invoke(List("graph"))
        yield assertTrue(
          done.exit == CliExit.Success && done.out == "core jvm 3.9.0" && done.err.isEmpty
        )
    ,
    test("run inspect prints the project name"):
      ZIO.scoped:
        for done <- invoke(List("run", "inspect"))
        yield assertTrue(done.exit == CliExit.Success && done.out == "core")
    ,
    test("run versions names an empty catalog"):
      ZIO.scoped:
        for done <- invoke(List("run", "versions"))
        yield assertTrue(
          done.exit == CliExit.Success && done.out == "no catalog" && done.err.isEmpty
        )
    ,
    test("run versions publishes a checked snapshot and refuses a moved ship"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          _ <- ZIO
            .attempt {
              Files.createDirectories(dir.resolve(".histos"))
              Files.writeString(
                dir.resolve("histos.scala"),
                """import histos.*
                     |object build extends HistosBuild:
                     |  def graph =
                     |    val core = module("core", Jvm)(scala"3.9.0")
                     |    Projects(ships(ship("libs", ship"1.4.2")(core)), core)
                     |""".stripMargin
              )
              Files.writeString(
                dir.resolve(".histos").resolve("releases"),
                "libs matches\n"
              )
            }
            .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          env = Map(
            "HISTOS_SESSION" -> "snapshot",
            "HISTOS_SHA" -> "1234567890abcdef1234567890abcdef12345678",
            "HISTOS_BRANCH" -> "default"
          )
          held <- Cli
            .execute(List("run", "versions"), dir, jar)
            .provideSome[Scope](layers(dir, env))
          _ <- ZIO
            .attempt(
              Files.writeString(
                dir.resolve(".histos").resolve("releases"),
                "libs moved\n"
              )
            )
            .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          refused <- Cli
            .execute(List("run", "versions"), dir, jar)
            .provideSome[Scope](layers(dir, env))
        yield assertTrue(
          held.exit == CliExit.Success && held.out.contains(
            "hold already-released"
          ) && held.out.contains("1.4.2-ci"),
          refused.exit == CliExit.TaskFailed && refused.err.contains(
            "has changes since its tag"
          )
        )
    ,
    test("a manual version does not need a commit or a registry file"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          _ <- ZIO
            .attempt(
              Files.writeString(
                dir.resolve("histos.scala"),
                """import histos.*
                |object build extends HistosBuild:
                |  def graph =
                |    val core = module("core", Jvm)(scala"3.9.0")
                |    Projects(ships(manual("libs", manual"1.2.3-SNAPSHOT")(core)), core)
                |""".stripMargin
              )
            )
            .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          done <- Cli
            .execute(List("run", "versions"), dir, jar)
            .provideSome[Scope](layers(dir))
        yield assertTrue(
          done.exit == CliExit.Success,
          done.out.contains("session manual"),
          done.out.contains("compile 1.2.3-SNAPSHOT publish 1.2.3-SNAPSHOT"),
          !done.out.contains("-ci"),
          done.err.isEmpty
        )
    ,
    test("run of an unknown task fails"):
      ZIO.scoped:
        for done <- invoke(List("run", "compile"))
        yield assertTrue(
          done.exit == CliExit.TaskFailed && done.exit.code == 1 && done.err
            .contains("compile")
        )
    ,
    test("run of a module without a main does not link"):
      ZIO.scoped:
        for done <- invoke(List("run", "core"))
        yield assertTrue(
          done.exit == CliExit.TaskFailed && done.err.contains(
            "does not name a main"
          )
        )
    ,
    test("run takes one platform and refuses a token that is not one"):
      for
        plain <- Cli.parse(List("run", "app"))
        jvm <- Cli.parse(List("run", "app", "jvm"))
        bad <- Cli.parse(List("run", "app", "scala")).either
        task <- Cli.parse(List("run", "versions", "jvm")).either
        inspect <- Cli.parse(List("run", "inspect", "native")).either
      yield assertTrue(
        plain match
          case Command.Run(id, None) => id.value == "app"
          case _                     => false
        ,
        jvm match
          case Command.Run(id, Some(histos.Jvm)) => id.value == "app"
          case _                                 => false
        ,
        bad == Left(
          CliParseError.BadPlatform(
            histos.PlatformError.Unknown("scala").message
          )
        ),
        task == Left(CliParseError.TaskPlatform("versions")),
        inspect == Left(CliParseError.TaskPlatform("inspect"))
      )
    ,
    test("a multi-platform module names no platform and does not start"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          _ <- ZIO
            .attempt(
              Files.writeString(
                dir.resolve("histos.scala"),
                """import histos.*
                |object build extends HistosBuild:
                |  def graph =
                |    val app = module("app", Jvm, Js)(scala"3.9.0").main(MainClass("demo.Check"))
                |    Projects(app)
                |""".stripMargin
              )
            )
            .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          done <- Cli
            .execute(List("run", "app"), dir, jar)
            .provideSome[Scope](layers(dir))
        yield assertTrue(
          done.exit == CliExit.TaskFailed,
          done.err.contains("histos run app jvm"),
          done.out.isEmpty
        )
    ,
    test("a platform the module does not build is refused before it starts"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- repo
          _ <- ZIO
            .attempt(
              Files.writeString(
                dir.resolve("histos.scala"),
                """import histos.*
                |object build extends HistosBuild:
                |  def graph =
                |    val app = module("app", Jvm)(scala"3.9.0").main(MainClass("demo.Check"))
                |    Projects(app)
                |""".stripMargin
              )
            )
            .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          done <- Cli
            .execute(List("run", "app", "native"), dir, jar)
            .provideSome[Scope](layers(dir))
        yield assertTrue(
          done.exit == CliExit.TaskFailed && done.err.contains(
            "does not build native"
          ) && done.out.isEmpty
        )
    ,
    test("a missing build is a load failure"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- ZIO
            .attempt(Files.createTempDirectory("histos-none"))
            .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          _ <- ZIO.addFinalizer(delete(dir))
          done <- Cli
            .execute(List("graph"), dir, jar)
            .provideSome[Scope](layers(dir))
        yield assertTrue(
          done.exit == CliExit.LoadFailed && done.exit.code == 3 && done.err
            .contains("histos.scala")
        )
    ,
    test("a slash id is a plugin task and a typo is an unknown root"):
      for
        plain <- Cli.parse(List("run", "dev.example/echo"))
        platform <- Cli.parse(List("run", "dev.example/echo", "jvm")).either
        broken <- Cli.parse(List("run", "a/b/c")).either
        site <- Cli.parse(List("~", "site")).either
        watched <- Cli.parse(List("~", "test"))
        cache <- Cli.parse(List("cache", "nope")).either
        flag <- Cli.parse(List("test", "--nope")).either
        list <- Cli.parse(List("list")).either
      yield assertTrue(
        plain == Command.Plugin(
          PluginTaskId(PluginId("dev.example"), TaskName("echo")),
          PluginArgs.empty
        ),
        platform == Left(CliParseError.TaskPlatform("dev.example/echo")),
        broken match
          case Left(CliParseError.BadPluginTask(_)) => true
          case _                                    => false,
        site == Left(CliParseError.UnknownRoot),
        watched == Command.Watch(Command.Test(Nil)),
        cache == Left(CliParseError.UnknownCommand),
        flag match
          case Left(CliParseError.UnknownRoot) => false
          case Left(_)                         => true
          case Right(_)                        => false,
        list == Left(CliParseError.UnknownRoot)
      )
    ,
    test("built-in failures do not load, and an unknown root does"):
      ZIO.scoped:
        for
          jar <- apiJar
          empty <- bare
          cache <- Cli
            .execute(List("cache", "nope"), empty, jar)
            .provideSome[Scope](layers(empty))
          flag <- Cli
            .execute(List("test", "--nope"), empty, jar)
            .provideSome[Scope](layers(empty))
          typo <- Cli
            .execute(List("tst"), empty, jar)
            .provideSome[Scope](layers(empty))
          dir <- repo
          usage <- Cli
            .execute(List("tst"), dir, jar)
            .provideSome[Scope](layers(dir))
          missing <- Cli
            .execute(List("run", "dev.example/echo"), dir, jar)
            .provideSome[Scope](layers(dir))
          list <- Cli
            .execute(List("list"), dir, jar)
            .provideSome[Scope](layers(dir))
          watched <- Cli
            .execute(List("~", "site"), dir, jar)
            .provideSome[Scope](layers(dir))
        yield assertTrue(
          cache.exit == CliExit.Usage && cache.exit.code == 2,
          flag.exit == CliExit.Usage,
          typo.exit == CliExit.LoadFailed && typo.exit.code == 3,
          usage.exit == CliExit.Usage && usage.err.contains("usage:"),
          missing.exit == CliExit.TaskFailed && missing.err.contains(
            "plugin task"
          ) && !missing.err.contains("is not a module"),
          list.exit == CliExit.Usage && list.err.contains("usage:"),
          watched.exit == CliExit.Usage && watched.err.contains("usage:")
        )
    ,
    test("an api link prints the load error"):
      ZIO.scoped:
        for
          jar <- apiJar
          dir <- bare
          _ <- ZIO
            .attempt(
              Files.writeString(
                dir.resolve("histos.scala"),
                """plugin(dep"dev.example::echo:1.0.0").link(Api)
                |import histos.*
                |object build extends HistosBuild:
                |  def graph = Projects(module("core", Jvm)(scala"3.9.0"))
                |""".stripMargin
              )
            )
            .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          done <- Cli
            .execute(List("graph"), dir, jar)
            .provideSome[Scope](layers(dir))
        yield assertTrue(
          done.exit == CliExit.LoadFailed && done.err.contains(
            "LinkStrategy.Api"
          )
        )
    ,
    test("graph text prints the contribution and not an env value"):
      Projects(use(Listed()), module("core", Jvm)(scala"3.9.0")) match
        case Left(err)    => assertTrue(err.message == "built")
        case Right(built) =>
          val api = ApiDigest.trusted("ab" * 32)
          val other = ApiDigest.trusted("cd" * 32)
          val binary = GraphText.render(
            built,
            List(
              PluginLink(
                PluginId("dev.example"),
                PluginLinked.FromBinary(api),
                Nil
              )
            )
          )
          val source = GraphText.render(
            built,
            List(
              PluginLink(
                PluginId("dev.example"),
                PluginLinked.FromSource(api, other),
                Nil
              )
            )
          )
          assertTrue(
            binary.contains("core jvm 3.9.0"),
            binary.contains("plugin dev.example binary"),
            binary.contains("task dev.example/echo Open Network docs"),
            binary.contains("command list"),
            binary.contains("job nightly"),
            !binary.contains("TOKEN"),
            source.contains("plugin dev.example source")
          )
    ,
    test("a contributed command parses its own argv"):
      val bound = BoundCommand(
        PluginId("dev.example"),
        CommandSpec(
          CommandName("list"),
          List(CliParam.Text(ParamName("out")), CliParam.Flag(FlagName("all"))),
          TaskName("echo"),
          PluginDetail("lists the site")
        )
      )
      val parsed =
        PluginCommands.parse(List(bound), List("list", "docs", "--all"))
      val usage = PluginCommands.usage(List(bound))
      assertTrue(
        parsed == Right(
          (
            PluginTaskId(PluginId("dev.example"), TaskName("echo")),
            PluginArgs(
              List(NamedText(ParamName("out"), "docs")),
              Set(FlagName("all"))
            )
          )
        ),
        usage.contains("list") && usage.contains("lists the site"),
        PluginCommands.parse(List(bound), List("nope")) == Left(
          CliParseError.UnknownCommand
        )
      )
  )

  private def layers(dir: Path, env: Map[String, String] = Map.empty) =
    ZLayer.make[
      histos.load.Loader & histos.cache.ContentStore & histos.jdk.Machine &
        histos.resolve.Resolve
    ](
      histos.load.Loader.live,
      ZLayer.succeed(histos.cache.ContentStore.in(dir.resolve("cache"))),
      ZLayer.fromZIO(
        Machine
          .load(dir)
          .mapError(error =>
            LoadError.Runtime(histos.zinc.GraphCompileError.Runtime(error))
          )
          .map(actual => machine(dir, env).copy(running = actual.running))
      ),
      histos.resolve.Resolve.live(dir.resolve("cache").resolve("resolve"))
    )

  private def machine(dir: Path, env: Map[String, String]): Machine =
    Machine(
      env,
      dir.toString,
      Os.Linux,
      histos.toolchain.Architecture.X86_64,
      dir.resolve("cache"),
      dir,
      None,
      Settled.No,
      NativeOrigin.read(env)
    )

  def spec = cli @@ TestAspect.withLiveSystem
