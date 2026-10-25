package histos.cli

import histos.jdk.{Machine, Os}
import histos.workspace.{IoFailure, IoFault}
import histos.native.NativeOrigin
import histos.load.PluginLink
import histos.plugin.*
import histos.publish.{LauncherPin, LauncherScript}
import histos.workflow.{Workflow, WorkflowAction}
import histos.load.{
  ApiJar,
  BuildNotes,
  LoadError,
  LoadedBuild,
  Loader,
  PluginRuntime,
  ScannedBuild
}
import histos.cache.ContentStore
import histos.resolve.{
  LockedArtifact,
  Repositories,
  Repository,
  RepositoryIo,
  Resolve,
  ResolveCell,
  ResolveError
}
import histos.server.{Calls, Daemon, Message}
import histos.zinc.GraphCompile
import histos.Dep
import histos.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

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

  private final class Listing extends HistosPlugin:
    val id: PluginId = PluginId("dev.example")

    def contribute: Contribution =
      Contribution(
        List(
          DeclaredTask(
            TaskName("echo"),
            Seal.Open,
            Set(Need.Network),
            Nil,
            Nil,
            Nil
          )
        ),
        List(
          CommandSpec(
            CommandName("list"),
            List(
              CliParam.Text(ParamName("out")),
              CliParam.Flag(FlagName("all"))
            ),
            TaskName("echo"),
            PluginDetail("lists the site")
          )
        ),
        Nil
      )

    def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
      ZIO.succeed(PluginOutput("", Nil))

  private final class RecordingRuntime(seen: Ref[Option[PluginArgs]])
      extends PluginRuntime:
    def run(id: PluginTaskId, args: PluginArgs) =
      seen.set(Some(args)).as(PluginOutput("ran docs", Nil))

    def bind =
      ZIO.succeed(
        Map.empty[
          histos.task.TaskId,
          IO[histos.task.TaskError, histos.task.TaskResult]
        ]
      )

    def links: List[PluginLink] = Nil

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
    test("a line reaches the console before the call finishes"):
      for
        release <- Promise.make[Nothing, Unit]
        started <- Promise.make[Nothing, Unit]
        fiber <- Cli.recorded { say =>
          say(Message.Out("compiling core")) *>
            say(Message.Err("unused")) *>
            say(Message.Out("")) *>
            started.succeed(()) *>
            release.await *>
            say(Message.Out("suite passed")) *>
            say(Message.Exit(1, "failed"))
        }.fork
        _ <- started.await
        duringOut <- TestConsole.output.map(_.map(_.stripSuffix("\n")).toList)
        duringErr <- TestConsole.outputErr.map(
          _.map(_.stripSuffix("\n")).toList
        )
        _ <- release.succeed(())
        done <- fiber.join
        afterOut <- TestConsole.output.map(_.map(_.stripSuffix("\n")).toList)
        afterErr <- TestConsole.outputErr.map(_.map(_.stripSuffix("\n")).toList)
      yield assertTrue(
        duringOut == List("compiling core", ""),
        duringErr == List("unused"),
        afterOut == List("compiling core", "", "suite passed"),
        afterErr == List("unused", "failed"),
        done.out == "compiling core\n\nsuite passed",
        done.err == "unused\nfailed",
        done.exit == CliExit.TaskFailed,
        done.playback == Playback.Heard
      )
    ,
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
    test("cancel names the id the server already keys"):
      for parsed <- Cli.parse(List("cancel", "inspect"))
      yield assertTrue(
        parsed == Command.Cancel("inspect"),
        Operation.call(parsed) == Right(histos.server.Call.Cancel("inspect"))
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
    test("clean wipes products and cleanFull takes no names"):
      for
        all <- Cli.parse(List("clean"))
        named <- Cli.parse(List("clean", "engine"))
        full <- Cli.parse(List("cleanFull"))
        watched <- Cli.parse(List("~", "clean", "engine"))
        flag <- Cli.parse(List("clean", "--dry-run")).either
        extra <- Cli.parse(List("cleanFull", "engine")).either
        cache <- Cli.parse(List("cache", "clean", "--dry-run"))
      yield assertTrue(
        all == Command.Clean(Nil),
        named == Command.Clean(List("engine")),
        full == Command.CleanFull,
        watched == Command.Watch(
          Command.Clean(List("engine")),
          List("clean", "engine")
        ),
        flag == Left(CliParseError.Pattern("clean does not take --dry-run")),
        extra == Left(
          CliParseError.Pattern("cleanFull does not take a project")
        ),
        cache == Command.CacheClean(true)
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
          case Command.Watch(Command.TestQuick(Nil, patterns), args) =>
            patterns.map(_.value) == List("com.acme.Foo*") && args == List(
              "testQuick",
              "com.acme.Foo*"
            )
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
    test("toolchain names the running JVM"):
      ZIO.scoped:
        for done <- invoke(List("toolchain"))
        yield assertTrue(
          done.exit == CliExit.Success && done.out.startsWith("jdk "),
          !done.out.contains("node"),
          !done.out.contains("clang")
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
            }
            .mapError(t => LoadError.Io(IoFailure(Path.of("."), IoFault.Temp)))
          done <- Cli
            .execute(List("toolchain"), dir, jar)
            .provideSome[Scope](layers(dir, Map("JAVA_HOME" -> jdk.toString)))
        yield assertTrue(
          done.exit == CliExit.Success && done.out.contains(
            s"jdk 21.0.2 $jdk"
          ),
          !done.out.contains("node"),
          !done.out.contains("clang")
        )
    ,
    test("a js project without Node still prints the jdk"):
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
          done.out.contains(s"jdk 21.0.2 $jdk"),
          done.err.contains("25")
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
        watched == Command.Watch(Command.Test(Nil), List("test")),
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
    ,
    test(
      "resolve --update loads the staged generation with and without a server"
    ):
      ZIO.scoped:
        Projects(module("core", Jvm)(scala"3.9.0")) match
          case Left(error)  => ZIO.succeed(assertTrue(error.message == "built"))
          case Right(graph) =>
            for
              jar <- apiJar.mapError(_.message)
              dir <- repo.mapError(_.message)
              trace <- Ref.make(List.empty[String])
              loader = Recording(trace, graph)
              stack = ZLayer.succeed(loader) ++
                ZLayer.succeed(FetchResolve) ++
                ZLayer.succeed(ContentStore.in(dir.resolve("cache"))) ++
                ZLayer.succeed(machine(dir, Map.empty))
              local <- Cli
                .execute(List("resolve", "--update"), dir, jar)
                .provideSome[Scope](stack)
              localTrace <- trace.getAndSet(Nil)
              jdk <- GraphCompile.jdkHere.mapError(_.message)
              _ <- ZIO
                .attempt {
                  Files.createDirectories(dir.resolve(".histos"))
                  Files.writeString(
                    dir.resolve(".histos").resolve("toolchain.local"),
                    jdk.home.toAbsolutePath.normalize.toString + "\n"
                  )
                }
                .mapError(_.toString)
              _ <- Daemon
                .open(
                  dir,
                  graph,
                  jdk,
                  Cli.VersionText,
                  jar,
                  Nil,
                  ZIO.succeed(BuildNotes(false, Nil)),
                  Calls.quiet,
                  machine(dir, Map.empty)
                )
                .provideSome[Scope](stack)
                .mapError(_.message)
              remote <- Cli
                .execute(List("resolve", "--update"), dir, jar)
                .provideSome[Scope](stack)
              remoteTrace <- trace.get
            yield assertTrue(
              local.exit == CliExit.ResolveFailed,
              remote.exit == CliExit.ResolveFailed,
              local.err.contains("resolve failed while fetch"),
              remote.err.contains("resolve failed while fetch"),
              localTrace == List("scan", "locked:0"),
              remoteTrace == List("scan", "locked:0")
            )
    ,
    test("workflow checks launcher scripts with and without a server"):
      ZIO.scoped:
        for
          jar <- apiJar.mapError(_.message)
          dir <- bare.mapError(_.message)
          _ <- ZIO
            .attempt(Files.writeString(dir.resolve("histos.scala"), ship))
            .mapError(_.toString)
          jdk <- GraphCompile.jdkHere.mapError(_.message)
          _ <- ZIO
            .attempt {
              Files.createDirectories(dir.resolve(".histos"))
              Files.writeString(
                dir.resolve(".histos").resolve("toolchain.local"),
                jdk.home.toAbsolutePath.normalize.toString + "\n"
              )
            }
            .mapError(_.toString)
          stack = layers(dir)
          loaded <- ZIO
            .serviceWithZIO[Loader](_.load(dir, jar))
            .provideSome[Scope](stack)
            .mapError(_.message)
          _ <- Workflow.update(dir, loaded.graph).mapError(_.message)
          local <- Cli
            .execute(List("workflow"), dir, jar)
            .provideSome[Scope](stack)
          _ <- Daemon
            .open(
              dir,
              loaded.graph,
              jdk,
              Cli.VersionText,
              jar,
              Nil,
              ZIO.succeed(BuildNotes(false, Nil)),
              loaded.plugins
            )
            .provideSome[Scope](stack)
            .mapError:
              case error: histos.server.ServerError => error.message
              case error: LoadError                 => error.message
          remote <- Cli
            .execute(List("workflow"), dir, jar)
            .provideSome[Scope](stack)
        yield
          val assembled = loaded.graph.assembly match
            case Some(_) => true
            case None    => false
          assertTrue(
            assembled,
            local.exit == CliExit.WorkflowFailed,
            remote.exit == CliExit.WorkflowFailed,
            local.err.contains("launcher pin is absent"),
            remote.err.contains("launcher pin is absent")
          )
    ,
    test(
      "workflow --update rewrites launcher scripts with and without a server"
    ):
      ZIO.scoped:
        for
          jar <- apiJar.mapError(_.message)
          dir <- bare.mapError(_.message)
          _ <- ZIO
            .attempt(Files.writeString(dir.resolve("histos.scala"), ship))
            .mapError(_.toString)
          jdk <- GraphCompile.jdkHere.mapError(_.message)
          lockPath = dir.resolve(LauncherPin.file)
          _ <- ZIO
            .attempt {
              Files.createDirectories(dir.resolve(".histos"))
              Files.writeString(
                dir.resolve(".histos").resolve("toolchain.local"),
                jdk.home.toAbsolutePath.normalize.toString + "\n"
              )
              Files.writeString(lockPath, launcherLock)
            }
            .mapError(_.toString)
          stack = layers(dir)
          loaded <- ZIO
            .serviceWithZIO[Loader](_.load(dir, jar))
            .provideSome[Scope](stack)
            .mapError(_.message)
          pin <- LauncherPin.read(dir).mapError(_.message)
          lockBytes <- fileBytes(lockPath)
          rendered <- ZIO
            .fromEither(Workflow.render(loaded.graph))
            .mapError(_.message)
          _ <- stageFile(dir.resolve("histos"), Some("stale\n"))
          _ <- stageFile(dir.resolve("histos.bat"), None)
          _ <- stageFile(Workflow.path(dir), Some("stale\n"))
          local <- Cli
            .execute(List("workflow", "--update"), dir, jar)
            .provideSome[Scope](stack)
          localPosix <- fileBytes(dir.resolve("histos"))
          localWindows <- fileBytes(dir.resolve("histos.bat"))
          localWorkflow <- fileBytes(Workflow.path(dir))
          localLock <- fileBytes(lockPath)
          localPin <- LauncherPin.read(dir).mapError(_.message)
          _ <- Daemon
            .open(
              dir,
              loaded.graph,
              jdk,
              Cli.VersionText,
              jar,
              Nil,
              ZIO.succeed(BuildNotes(false, Nil)),
              loaded.plugins
            )
            .provideSome[Scope](stack)
            .mapError:
              case error: histos.server.ServerError => error.message
              case error: LoadError                 => error.message
          _ <- stageFile(dir.resolve("histos"), None)
          _ <- stageFile(dir.resolve("histos.bat"), Some("stale\n"))
          _ <- stageFile(Workflow.path(dir), Some("stale\n"))
          remote <- Cli
            .execute(List("workflow", "--update"), dir, jar)
            .provideSome[Scope](stack)
          remotePosix <- fileBytes(dir.resolve("histos"))
          remoteWindows <- fileBytes(dir.resolve("histos.bat"))
          remoteWorkflow <- fileBytes(Workflow.path(dir))
          remoteLock <- fileBytes(lockPath)
          remotePin <- LauncherPin.read(dir).mapError(_.message)
        yield
          val posix = utf8(LauncherScript.posix(pin.version))
          val windows = utf8(LauncherScript.windows(pin.version))
          val workflow = utf8(rendered)
          val assembled = loaded.graph.assembly match
            case Some(_) => true
            case None    => false
          assertTrue(
            assembled,
            pin.version == "0.0.1-SNAPSHOT",
            pin.revision == "0.0.1-20261024.203501-20",
            local.exit == CliExit.Success,
            local.err.isEmpty,
            localPosix == posix,
            localWindows == windows,
            localWorkflow == workflow,
            localLock == lockBytes,
            localPin.revision == pin.revision,
            localPin.version == pin.version,
            remote.exit == CliExit.Success,
            remote.err.isEmpty,
            remotePosix == posix,
            remoteWindows == windows,
            remoteWorkflow == workflow,
            remoteLock == lockBytes,
            remotePin.revision == pin.revision,
            remotePin.version == pin.version
          )
    @@ TestAspect.withLiveClock @@ TestAspect.timeout(45.seconds),
    test("a watched contributed command runs on the server with its arguments"):
      ZIO.scoped:
        Projects(use(Listing()), module("core", Jvm)(scala"3.9.0")) match
          case Left(error)  => ZIO.succeed(assertTrue(error.message == "built"))
          case Right(graph) =>
            for
              jar <- apiJar.mapError(_.message)
              dir <- repo.mapError(_.message)
              seen <- Ref.make(Option.empty[PluginArgs])
              trace <- Ref.make(List.empty[String])
              loader = Recording(trace, graph)
              stack = ZLayer.succeed(loader) ++
                ZLayer.succeed(FetchResolve) ++
                ZLayer.succeed(ContentStore.in(dir.resolve("cache"))) ++
                ZLayer.succeed(machine(dir, Map.empty))
              jdk <- GraphCompile.jdkHere.mapError(_.message)
              _ <- ZIO
                .attempt {
                  Files.createDirectories(dir.resolve(".histos"))
                  Files.writeString(
                    dir.resolve(".histos").resolve("toolchain.local"),
                    jdk.home.toAbsolutePath.normalize.toString + "\n"
                  )
                }
                .mapError(_.toString)
              _ <- Daemon
                .open(
                  dir,
                  graph,
                  jdk,
                  Cli.VersionText,
                  jar,
                  Nil,
                  ZIO.succeed(BuildNotes(false, Nil)),
                  RecordingRuntime(seen),
                  machine(dir, Map.empty)
                )
                .provideSome[Scope](stack)
                .mapError(_.message)
              fiber <- Cli
                .execute(List("~", "list", "docs", "--all"), dir, jar)
                .provideSome[Scope](stack)
                .forkScoped
              _ <- printed(line => line == "ran docs")
              args <- seen.get
              lines <- TestConsole.output.map(_.map(_.stripSuffix("\n")).toList)
              _ <- fiber.interrupt
            yield assertTrue(
              args == Some(
                PluginArgs(
                  List(NamedText(ParamName("out"), "docs")),
                  Set(FlagName("all"))
                )
              ),
              lines.contains("ran docs"),
              !lines.exists(_.contains("usage:"))
            )
    @@ TestAspect.withLiveClock @@ TestAspect.timeout(45.seconds),
    test("watch of mcp is usage with and without a server"):
      ZIO.scoped:
        Projects(module("core", Jvm)(scala"3.9.0")) match
          case Left(error)  => ZIO.succeed(assertTrue(error.message == "built"))
          case Right(graph) =>
            for
              jar <- apiJar.mapError(_.message)
              dir <- repo.mapError(_.message)
              trace <- Ref.make(List.empty[String])
              loader = Recording(trace, graph)
              stack = ZLayer.succeed(loader) ++
                ZLayer.succeed(FetchResolve) ++
                ZLayer.succeed(ContentStore.in(dir.resolve("cache"))) ++
                ZLayer.succeed(machine(dir, Map.empty))
              localMcp <- Cli
                .execute(List("~", "mcp"), dir, jar)
                .provideSome[Scope](stack)
              localBsp <- Cli
                .execute(List("~", "bsp"), dir, jar)
                .provideSome[Scope](stack)
              localServer <- Cli
                .execute(List("~", "server"), dir, jar)
                .provideSome[Scope](stack)
              localNested <- Cli
                .execute(List("~", "~", "test"), dir, jar)
                .provideSome[Scope](stack)
              jdk <- GraphCompile.jdkHere.mapError(_.message)
              _ <- ZIO
                .attempt {
                  Files.createDirectories(dir.resolve(".histos"))
                  Files.writeString(
                    dir.resolve(".histos").resolve("toolchain.local"),
                    jdk.home.toAbsolutePath.normalize.toString + "\n"
                  )
                }
                .mapError(_.toString)
              _ <- Daemon
                .open(
                  dir,
                  graph,
                  jdk,
                  Cli.VersionText,
                  jar,
                  Nil,
                  ZIO.succeed(BuildNotes(false, Nil)),
                  Calls.quiet,
                  machine(dir, Map.empty)
                )
                .provideSome[Scope](stack)
                .mapError(_.message)
              remoteMcp <- Cli
                .execute(List("~", "mcp"), dir, jar)
                .provideSome[Scope](stack)
              remoteBsp <- Cli
                .execute(List("~", "bsp"), dir, jar)
                .provideSome[Scope](stack)
              remoteServer <- Cli
                .execute(List("~", "server"), dir, jar)
                .provideSome[Scope](stack)
              remoteNested <- Cli
                .execute(List("~", "~", "test"), dir, jar)
                .provideSome[Scope](stack)
            yield assertTrue(
              refused(localMcp),
              refused(localBsp),
              refused(localServer),
              refused(remoteMcp),
              refused(remoteBsp),
              refused(remoteServer),
              localNested.exit == CliExit.Usage && localNested.err.contains(
                "watch does not nest"
              ),
              remoteNested.exit == CliExit.Usage && remoteNested.err.contains(
                "watch does not nest"
              )
            )
    @@ TestAspect.withLiveClock @@ TestAspect.timeout(20.seconds),
    test("watch of compile names the path that changed"):
      ZIO.scoped:
        for
          jar <- apiJar.mapError(_.message)
          dir <- repo.mapError(_.message)
          fiber <- Cli
            .execute(List("~", "compile"), dir, jar)
            .provideSome[Scope](layers(dir))
            .forkScoped
          _ <- printed(line => line == "watching")
          _ <- ZIO.sleep(1.second)
          _ <- ZIO
            .attempt(
              Files.writeString(
                dir.resolve("histos.scala"),
                "\n",
                StandardOpenOption.APPEND
              )
            )
            .mapError(_.toString)
          _ <- printed(line => line == "watching histos.scala")
          lines <- TestConsole.output.map(_.map(_.stripSuffix("\n")).toList)
          watched = lines.filter(_.startsWith("watching"))
          _ <- fiber.interrupt
        yield assertTrue(
          watched.contains("watching"),
          watched.contains("watching histos.scala"),
          watched.forall(line =>
            line == "watching" || line == "watching histos.scala"
          )
        )
    @@ TestAspect.withLiveClock @@ TestAspect.timeout(45.seconds)
  )

  private val launcherLock =
    """repository https://central.sonatype.com/repository/maven-snapshots
      |version 0.0.1-SNAPSHOT
      |revision 0.0.1-20261024.203501-20
      |sha256 73fa77d97aca68cab6aa0ba0ba0ac779193b400bbb1d3b297b81a06bbede827b
      |""".stripMargin

  private def stageFile(path: Path, text: Option[String]): IO[String, Unit] =
    ZIO
      .attempt {
        text match
          case Some(body) =>
            Option(path.getParent) match
              case Some(parent) => Files.createDirectories(parent)
              case None         => ()
            Files.writeString(path, body)
            ()
          case None =>
            Files.deleteIfExists(path)
            ()
      }
      .mapError(_.toString)

  private def fileBytes(path: Path): IO[String, Chunk[Byte]] =
    ZIO
      .attempt(Chunk.fromArray(Files.readAllBytes(path)))
      .mapError(_.toString)

  private def utf8(text: String): Chunk[Byte] =
    Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))

  private val ship =
    """import histos.*
      |object build extends HistosBuild:
      |  def graph =
      |    val api = module("api", Jvm)(scala"3.9.0")
      |    Projects
      |      .organization(organization"rocks.earlyeffect")
      |      .assembly(artifact"histos")(
      |        ships(manual("histos", manual"0.0.1-SNAPSHOT")(api)),
      |        api
      |      )
      |""".stripMargin

  private final class Recording(trace: Ref[List[String]], graph: ProjectGraph)
      extends Loader:
    def scan(repo: Path) =
      trace.update(_ :+ "scan").as(ScannedBuild(Nil, ""))
    def loadLocked(
        repo: Path,
        apiJar: Path,
        lock: histos.resolve.DependencyLock
    ) =
      trace
        .update(_ :+ s"locked:${lock.plugins.length}")
        .as(
          LoadedBuild(graph, Calls.quiet)
        )
    def load(repo: Path, apiJar: Path) =
      trace.update(_ :+ "load").as(LoadedBuild(graph, Calls.quiet))
    def buildClasspath(apiJar: Path) = ZIO.succeed(Nil)
    def reportBuild(repo: Path, apiJar: Path) =
      ZIO.succeed(BuildNotes(false, Nil))

  private object FetchResolve extends Resolve:
    private val failed: IO[ResolveError, Nothing] =
      ZIO.fail(ResolveError.Repository(RepositoryIo.Fetch))
    def resolveTool(request: histos.resolve.ToolRequest) = failed
    def resolve(cell: ResolveCell) = failed
    def fetch(artifact: LockedArtifact) = failed
    def resolvePlugin(dependency: Dep, repositories: Repositories) = failed
    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository
    ) = failed

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
      NativeOrigin.read(env)
    )

  private def refused(done: Completed): Boolean =
    done.exit == CliExit.Usage && done.err.contains("usage:")

  private def printed(pred: String => Boolean): UIO[Unit] =
    TestConsole.output.flatMap { lines =>
      if lines.exists(line => pred(line.stripSuffix("\n"))) then ZIO.unit
      else ZIO.sleep(50.millis) *> printed(pred)
    }

  def spec = cli @@ TestAspect.withLiveSystem
