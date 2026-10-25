package histos.plugin

import _root_.scala.compiletime.testing.typeCheckErrors

import java.nio.file.Path

import histos.*

import zio.{IO, Scope, ZIO}
import zio.test.*

/** In-tree plugin. `Projects` copies [[contribute]] and does not call [[run]].
  */
object Echo extends HistosPlugin:
  val id: PluginId = PluginId("test.echo")

  def contribute: Contribution =
    Contribution(
      List(DeclaredTask(TaskName("echo"), Seal.Open, Set.empty, Nil, Nil, Nil)),
      Nil,
      Nil
    )

  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
    ZIO.succeed(PluginOutput("echo", Nil))

/** `run` throws. A graph that includes this plugin proves `Projects` never
  * called it.
  */
object Quiet extends HistosPlugin:
  val id: PluginId = PluginId("test.quiet")

  def contribute: Contribution =
    Contribution(
      List(
        DeclaredTask(TaskName("quiet"), Seal.Open, Set.empty, Nil, Nil, Nil)
      ),
      Nil,
      Nil
    )

  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
    throw new IllegalStateException("run")

/** `contribute` throws. The message is not the typed error. */
object Boom extends HistosPlugin:
  val id: PluginId = PluginId("test.boom")

  def contribute: Contribution =
    throw new IllegalStateException("nope")

  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
    ZIO.succeed(PluginOutput("boom", Nil))

final class Fixed(val id: PluginId, value: Contribution) extends HistosPlugin:
  def contribute: Contribution = value

  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
    ZIO.succeed(PluginOutput("fixed", Nil))

object PluginSpec extends ZIOSpecDefault:
  private val digest = "ab" * 32
  private val other = "cd" * 32

  private def linkName(link: LinkStrategy): String = link match
    case LinkStrategy.ExactBinary    => "exact"
    case LinkStrategy.Source         => "source"
    case LinkStrategy.BinaryOrSource => "either"
    case LinkStrategy.Api            => "refused"

  private val ids = suite("ids")(
    test("a plugin id is dot-separated segments"):
      val errors = typeCheckErrors("""PluginId("a/b")""")
      assertTrue(
        PluginId(
          "dev.earlyeffect.specular"
        ).value == "dev.earlyeffect.specular",
        PluginId.from("test.echo") == Right(PluginId("test.echo")),
        PluginId.from("") == Left(PluginIdError.Empty),
        PluginId.from("a/b") == Left(PluginIdError.Bad("a/b")),
        PluginId.from(".echo") == Left(PluginIdError.Bad(".echo")),
        PluginId.from("echo.") == Left(PluginIdError.Bad("echo.")),
        PluginId.from("has space") == Left(PluginIdError.Bad("has space")),
        errors.headOption.is(_.some).message.contains("a/b")
      )
    ,
    test("a task name is a token, and inspect still is not a plugin task id"):
      val parsed = PluginTaskId.parse("dev.earlyeffect.specular/versions")
      assertTrue(
        TaskName.from("inspect") == Right(TaskName("inspect")),
        TaskName.from("a/b") == Left(TaskNameError.Bad("a/b")),
        TaskName.from("") == Left(TaskNameError.Empty),
        PluginTaskId.parse("inspect") == Left(PluginTaskError.Shape("inspect")),
        PluginTaskId.parse("versions") == Left(
          PluginTaskError.Shape("versions")
        ),
        PluginTaskId.parse("a/b/c") == Left(PluginTaskError.Shape("a/b/c")),
        parsed == Right(
          PluginTaskId(
            PluginId("dev.earlyeffect.specular"),
            TaskName("versions")
          )
        ),
        parsed.map(_.render) == Right("dev.earlyeffect.specular/versions")
      )
    ,
    test("reserved command and job names are the built-in words"):
      assertTrue(
        CommandName.reserved.map(_.value) == Set(
          "version",
          "graph",
          "run",
          "test",
          "testOnly",
          "testQuick",
          "compile",
          "clean",
          "cleanFull",
          "assembly",
          "publish",
          "mcp",
          "acp",
          "bsp",
          "server",
          "reload",
          "reboot",
          "toolchain",
          "cache",
          "resolve",
          "workflow",
          "launcher",
          "cancel"
        ),
        CommandName("list").isReserved == false,
        CommandName("test").isReserved,
        JobName.reserved
          .map(_.value) == Set("test", "versions", "workflow", "launcher"),
        JobName("docs").isReserved == false
      )
    ,
    test("an entry name is the module class, including the trailing dollar"):
      assertTrue(
        EntryName.fromClass(PluginSpec.getClass) == Right(
          EntryName("histos.plugin.PluginSpec$")
        ),
        EntryName(
          "dev.earlyeffect.specular.SpecularPlugin$"
        ).value == "dev.earlyeffect.specular.SpecularPlugin$",
        EntryName.from("") == Left(EntryNameError.Empty),
        EntryName.from("foo/bar") == Left(EntryNameError.Bad("foo/bar"))
      )
  )

  private val paths = suite("paths and digests")(
    test("a repository path accepts index.html and rejects .."):
      val errors = typeCheckErrors("""RepoPath("docs/../secret")""")
      assertTrue(
        RepoPath("docs/index.html").value == "docs/index.html",
        RepoPath("coverage.xml").value == "coverage.xml",
        RepoPath("README.md").value == "README.md",
        RepoPath.from("docs/../secret") == Left(
          RepoPathError.Escapes("docs/../secret")
        ),
        RepoPath.from("..") == Left(RepoPathError.Escapes("..")),
        RepoPath.from("") == Left(RepoPathError.Empty),
        RepoPath.from("/etc/passwd") == Left(
          RepoPathError.Escapes("/etc/passwd")
        ),
        errors.headOption.is(_.some).message.contains("docs/../secret")
      )
    ,
    test("an api digest is 64 hex digits, stored lowercase"):
      val parsed = ApiDigest.from(digest)
      val upper = ApiDigest.from(digest.toUpperCase)
      val hash = ContentHash.from(digest)
      assertTrue(
        parsed == upper,
        parsed.map(_.value) == Right(digest),
        ApiDigest.resource == "META-INF/histos/version",
        ApiDigest.from("ab") == Left(ApiDigestError.Bad("ab")),
        ApiDigest.from(digest + " ") == Left(ApiDigestError.Bad(digest + " ")),
        hash.map(_.value) == Right(digest)
      )
    ,
    test("a cron expression is five GitHub fields"):
      val daily = Cron.from("0 0 * * *")
      assertTrue(
        daily == Right(Cron("0 0 * * *")),
        Cron.from("0  0  *  *  *") == daily,
        Cron.from("*/15 * * * MON-FRI").map(_.value) == Right(
          "*/15 * * * MON-FRI"
        ),
        Cron.from("0 0 * JAN SUN") == Right(Cron("0 0 * JAN SUN")),
        Cron.from("") == Left(CronError.Empty),
        Cron.from("0 0 * *") == Left(CronError.Fields(4)),
        Cron.from("60 * * * *") == Left(CronError.Field(0, "60")),
        Cron.from("0 0 * * 7") == Left(CronError.Field(4, "7")),
        Cron.from("*/0 * * * *") == Left(CronError.Field(0, "*/0"))
      )
  )

  private val contract = suite("contract")(
    test("a load error names the header until the plugin object exists"):
      val dependency = Dep("dev.earlyeffect::specular:0.2.0")
      val path = ProjectPath("tools/docs-preview")
      for
        pluginApi <- ZIO.fromEither(ApiDigest.from(digest))
        launcher <- ZIO.fromEither(ApiDigest.from(other))
      yield
        val mismatch = PluginLoadError
          .VersionMismatch(dependency, pluginApi, launcher)
          .message
        val missing = PluginLoadError
          .NoSource(PluginCoordinate.Published(dependency))
          .message
        val pathMissing =
          PluginLoadError.NoSource(PluginCoordinate.Path(path)).message
        assertTrue(
          linkName(LinkStrategy.Api) == "refused",
          mismatch.contains("dev.earlyeffect::specular:0.2.0"),
          mismatch.contains(pluginApi.value),
          mismatch.contains(launcher.value),
          !mismatch.contains("test.echo"),
          PluginLoadError
            .ApiLink(dependency)
            .message
            .contains("dev.earlyeffect::specular:0.2.0"),
          missing.contains("dev.earlyeffect::specular:0.2.0"),
          !missing.contains(pluginApi.value),
          pathMissing.contains("tools/docs-preview"),
          !pathMissing.contains(pluginApi.value),
          PluginLoadError
            .EngineLeak(
              PluginCoordinate.Published(dependency),
              "histos.schedule.Sendable"
            )
            .message
            .contains("histos.schedule.Sendable"),
          PluginLoadError
            .Defect(
              PluginCoordinate.Path(path),
              "java.lang.ExceptionInInitializerError"
            )
            .message
            .contains("tools/docs-preview"),
          PluginRunError
            .Defect("java.lang.RuntimeException")
            .message
            .contains("java.lang.RuntimeException")
        )
    ,
    test("a fixture request is the task id and nothing else"):
      val repo = Path.of("repo")
      val request =
        PluginFixture.request(PluginId("test.echo"), TaskName("echo"), repo)
      assertTrue(
        request.id.render == "test.echo/echo",
        request.repo == repo,
        request.modules.isEmpty,
        request.args == PluginArgs.empty,
        request.inputs.isEmpty,
        PluginArgs.empty.require(ParamName("out")) == Left(
          PluginRunError.MissingArg(ParamName("out"))
        ),
        HistosPlugin.entryResource == "META-INF/histos/plugin"
      )
  )

  private val detail = PluginDetail("runs the task")

  private def projectName(raw: String): IO[ProjectNameError, ProjectName] =
    ZIO.fromEither(ProjectName.from(raw))

  private def jvm(name: String): ModuleDef =
    module(name, Jvm)(scala"3.9.0")

  private def task(
      name: TaskName,
      inputs: List[DeclaredInput] = Nil,
      dependsOn: List[TaskDep] = Nil
  ): DeclaredTask =
    DeclaredTask(name, Seal.Open, Set.empty, inputs, Nil, dependsOn)

  private def fixed(
      id: PluginId,
      tasks: List[DeclaredTask],
      commands: List[CommandSpec] = Nil,
      jobs: List[CiJob] = Nil
  ): Fixed =
    Fixed(id, Contribution(tasks, commands, jobs))

  private def command(name: CommandName, taskName: TaskName): CommandSpec =
    CommandSpec(name, Nil, taskName, detail)

  private def job(name: JobName, taskName: TaskName): CiJob =
    CiJob(name, taskName, JobWhen.Always)

  private val registration = suite("registration")(
    test("use copies the contribution and does not call run"):
      val built = Projects(use(Echo, Quiet), jvm("core"))
      val echo = Contribution(
        List(
          DeclaredTask(TaskName("echo"), Seal.Open, Set.empty, Nil, Nil, Nil)
        ),
        Nil,
        Nil
      )
      val quiet = Contribution(
        List(
          DeclaredTask(TaskName("quiet"), Seal.Open, Set.empty, Nil, Nil, Nil)
        ),
        Nil,
        Nil
      )
      assertTrue:
        built.map(_.plugins) == Right(
          List(
            RegisteredPlugin(Echo.id, EntryName("histos.plugin.Echo$"), echo),
            RegisteredPlugin(Quiet.id, EntryName("histos.plugin.Quiet$"), quiet)
          )
        )
    ,
    test("a build that does not call use stores no plugins"):
      assertTrue(Projects(jvm("core")).map(_.plugins) == Right(Nil))
    ,
    test("pipe applies a module function and keeps the note"):
      val docs = jvm("docs").pipe: item =>
        item.note(Echo.id, NoteKey("role"), NoteText("site"))
      val built = Projects(use(Echo), docs)
      assertTrue(
        built.map(_.projects.collect { case found: Module => found.notes }) ==
          Right(
            List(List(ModuleNote(Echo.id, NoteKey("role"), NoteText("site"))))
          ),
        built.map(_.plugins.map(_.id)) == Right(List(Echo.id))
      )
    ,
    test("use is the first argument"):
      val bad = typeCheckErrors(
        """Projects(module("docs", Jvm)(scala"3.9.0"), use(Echo))"""
      )
      val good = typeCheckErrors(
        """Projects(use(Echo), module("docs", Jvm)(scala"3.9.0"))"""
      )
      assertTrue(bad.nonEmpty, good.isEmpty)
    ,
    test("an organized build keeps the registered plugin"):
      val built = Projects.resolvers(central)(use(Echo), jvm("core"))
      assertTrue(
        built.map(_.plugins.map(_.id)) == Right(List(Echo.id)),
        built.map(_.resolvers) == Right(List(central))
      )
    ,
    test("the same plugin id cannot be registered twice"):
      val built = Projects(use(Echo, Echo), jvm("core"))
      assertTrue(built == Left(BuildError.DuplicatePlugin(Echo.id)))
    ,
    test(
      "one plugin cannot declare a task twice, and two plugins can share the local name"
    ):
      val twice = fixed(
        PluginId("test.echo"),
        List(task(TaskName("echo")), task(TaskName("echo")))
      )
      val left = fixed(PluginId("test.one"), List(task(TaskName("echo"))))
      val right = fixed(PluginId("test.two"), List(task(TaskName("echo"))))
      val repeated = Projects(use(twice), jvm("core"))
      val shared = Projects(use(left, right), jvm("core"))
      assertTrue(
        repeated == Left(
          BuildError
            .DuplicatePluginTask(PluginId("test.echo"), TaskName("echo"))
        ),
        shared.map(_.plugins.map(_.id)) == Right(
          List(PluginId("test.one"), PluginId("test.two"))
        )
      )
    ,
    test("a command name is one flat namespace"):
      val left = fixed(
        PluginId("test.one"),
        List(task(TaskName("echo"))),
        List(command(CommandName("site"), TaskName("echo")))
      )
      val right = fixed(
        PluginId("test.two"),
        List(task(TaskName("echo"))),
        List(command(CommandName("site"), TaskName("echo")))
      )
      val built = Projects(use(left, right), jvm("core"))
      assertTrue(
        built == Left(BuildError.DuplicateCommand(CommandName("site")))
      )
    ,
    test(
      "a command cannot shadow a built-in, and it must name a declared task"
    ):
      val shadow = fixed(
        PluginId("test.echo"),
        List(task(TaskName("echo"))),
        List(command(CommandName("test"), TaskName("echo")))
      )
      val missing = fixed(
        PluginId("test.echo"),
        List(task(TaskName("echo"))),
        List(command(CommandName("site"), TaskName("absent")))
      )
      val shadowed = Projects(use(shadow), jvm("core"))
      val unknown = Projects(use(missing), jvm("core"))
      assertTrue(
        shadowed == Left(BuildError.CommandShadow(CommandName("test"))),
        unknown == Left(
          BuildError.UnknownPluginTask(
            PluginId("test.echo"),
            TaskName("absent")
          )
        )
      )
    ,
    test(
      "a job name is one flat namespace, and the built-in names stay reserved"
    ):
      val left = fixed(
        PluginId("test.one"),
        List(task(TaskName("echo"))),
        jobs = List(job(JobName("nightly"), TaskName("echo")))
      )
      val right = fixed(
        PluginId("test.two"),
        List(task(TaskName("echo"))),
        jobs = List(job(JobName("nightly"), TaskName("echo")))
      )
      val reserved = fixed(
        PluginId("test.echo"),
        List(task(TaskName("echo"))),
        jobs = List(job(JobName("test"), TaskName("echo")))
      )
      val missing = fixed(
        PluginId("test.echo"),
        List(task(TaskName("echo"))),
        jobs = List(job(JobName("nightly"), TaskName("absent")))
      )
      val duplicated = Projects(use(left, right), jvm("core"))
      val refused = Projects(use(reserved), jvm("core"))
      val unknown = Projects(use(missing), jvm("core"))
      assertTrue(
        duplicated == Left(BuildError.DuplicateJob(JobName("nightly"))),
        refused == Left(BuildError.ReservedJob(JobName("test"))),
        unknown == Left(
          BuildError.UnknownPluginTask(
            PluginId("test.echo"),
            TaskName("absent")
          )
        )
      )
    ,
    test("a sibling task and an open cell are accepted"):
      projectName("docs").map: docs =>
        val plugin = fixed(
          PluginId("test.echo"),
          List(
            task(
              TaskName("echo"),
              inputs = List(
                DeclaredInput.Text(InputKey("title"), "site"),
                DeclaredInput.Env(EnvName("PATH"))
              )
            ),
            task(
              TaskName("later"),
              inputs = List(
                DeclaredInput.Note(docs, NoteKey("role")),
                DeclaredInput.Module(docs, None, None)
              ),
              dependsOn = List(
                TaskDep.Same(TaskName("echo")),
                TaskDep.Compile(docs, None, None)
              )
            )
          )
        )
        val docsModule = jvm("docs")
          .note(PluginId("test.echo"), NoteKey("role"), NoteText("site"))
        val built = Projects(use(plugin), docsModule)
        assertTrue:
          built.map(
            _.plugins.flatMap(_.contribution.tasks.map(_.name))
          ) == Right(List(TaskName("echo"), TaskName("later")))
    ,
    test("a missing sibling task is refused"):
      val plugin = fixed(
        PluginId("test.echo"),
        List(
          task(
            TaskName("echo"),
            dependsOn = List(TaskDep.Same(TaskName("absent")))
          )
        )
      )
      val built = Projects(use(plugin), jvm("core"))
      assertTrue(
        built == Left(
          BuildError.UnknownPluginTask(
            PluginId("test.echo"),
            TaskName("absent")
          )
        )
      )
    ,
    test(
      "a repeated note is refused, and another plugin's note does not count"
    ):
      val id = PluginId("test.echo")
      val repeated = jvm("docs")
        .note(id, NoteKey("role"), NoteText("site"))
        .note(id, NoteKey("role"), NoteText("other"))
      projectName("docs").map: docs =>
        val plugin = fixed(
          id,
          List(
            task(
              TaskName("echo"),
              inputs = List(DeclaredInput.Note(docs, NoteKey("role")))
            )
          )
        )
        val docsModule = jvm("docs")
          .note(PluginId("test.other"), NoteKey("role"), NoteText("site"))
        assertTrue(
          Projects(repeated) == Left(
            BuildError.DuplicateNote(docs, id, NoteKey("role"))
          ),
          Projects(use(plugin), docsModule) == Left(
            BuildError.MissingNote(docs, id, NoteKey("role"))
          )
        )
    ,
    test("a note input that names a missing module is refused"):
      projectName("missing").map: missing =>
        val plugin = fixed(
          PluginId("test.echo"),
          List(
            task(
              TaskName("echo"),
              inputs = List(DeclaredInput.Note(missing, NoteKey("role")))
            )
          )
        )
        assertTrue(
          Projects(use(plugin), jvm("core")) == Left(
            BuildError.PluginMissingModule(PluginId("test.echo"), missing)
          )
        )
    ,
    test("an aggregate is not a module a plugin can compile"):
      projectName("root").map: root =>
        val core = jvm("core")
        val plugin = fixed(
          PluginId("test.echo"),
          List(
            task(
              TaskName("echo"),
              inputs = List(DeclaredInput.Module(root, None, None))
            )
          )
        )
        assertTrue:
          Projects(use(plugin), aggregate("root", core), core) == Left(
            BuildError.PluginMissingModule(PluginId("test.echo"), root)
          )
    ,
    test("a named cell has to be one the module builds"):
      val scala2 = ScalaVersion("2.13.16")
      projectName("core").map: core =>
        def plugin(dep: TaskDep) = fixed(
          PluginId("test.echo"),
          List(task(TaskName("echo"), dependsOn = List(dep)))
        )
        val version = Projects(
          use(plugin(TaskDep.Compile(core, Some(scala2), None))),
          jvm("core")
        )
        val platform = Projects(
          use(plugin(TaskDep.Compile(core, None, Some(Js)))),
          jvm("core")
        )
        assertTrue(
          version == Left(
            BuildError.PluginMissingCell(
              PluginId("test.echo"),
              core,
              Some(scala2),
              None
            )
          ),
          platform == Left(
            BuildError.PluginMissingCell(
              PluginId("test.echo"),
              core,
              None,
              Some(Js)
            )
          )
        )
    ,
    test(
      "a throw from contribute becomes the exception class, not the message"
    ):
      val built = Projects(use(Boom), jvm("core"))
      assertTrue(
        built == Left(
          BuildError.PluginDefect(Boom.id, "java.lang.IllegalStateException")
        ),
        built.is(_.left).message.contains("java.lang.IllegalStateException"),
        !built.is(_.left).message.contains("nope")
      )
  )

  def spec = suite("plugins")(ids, paths, contract, registration)
