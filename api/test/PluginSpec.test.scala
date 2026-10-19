package histos.plugin

import _root_.scala.compiletime.testing.typeCheckErrors

import java.nio.file.Path

import zio.test.*

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
        PluginId("dev.earlyeffect.specular").value == "dev.earlyeffect.specular",
        PluginId.from("test.echo") == Right(PluginId("test.echo")),
        PluginId.from("") == Left(PluginIdError.Empty),
        PluginId.from("a/b") == Left(PluginIdError.Bad("a/b")),
        PluginId.from(".echo") == Left(PluginIdError.Bad(".echo")),
        PluginId.from("echo.") == Left(PluginIdError.Bad("echo.")),
        PluginId.from("has space") == Left(PluginIdError.Bad("has space")),
        errors match
          case e :: _ => e.message.contains("a/b")
          case Nil    => false,
      )
    ,
    test("a task name is a token, and inspect still is not a plugin task id"):
      val parsed = PluginTaskId.parse("dev.earlyeffect.specular/versions")
      assertTrue(
        TaskName.from("inspect") == Right(TaskName("inspect")),
        TaskName.from("a/b") == Left(TaskNameError.Bad("a/b")),
        TaskName.from("") == Left(TaskNameError.Empty),
        PluginTaskId.parse("inspect") == Left(PluginTaskError.Shape("inspect")),
        PluginTaskId.parse("versions") == Left(PluginTaskError.Shape("versions")),
        PluginTaskId.parse("a/b/c") == Left(PluginTaskError.Shape("a/b/c")),
        parsed == Right(PluginTaskId(PluginId("dev.earlyeffect.specular"), TaskName("versions"))),
        parsed.exists(_.render == "dev.earlyeffect.specular/versions"),
      )
    ,
    test("reserved command and job names are the built-in words"):
      assertTrue(
        CommandName.reserved.map(_.value) == Set(
          "version", "graph", "run", "test", "testOnly", "testQuick", "compile",
          "assembly", "publish", "mcp", "acp", "bsp", "server", "toolchain",
          "cache", "resolve", "workflow",
        ),
        CommandName("list").isReserved == false,
        CommandName("test").isReserved,
        JobName.reserved.map(_.value) == Set("test", "versions", "workflow"),
        JobName("docs").isReserved == false,
      )
    ,
    test("an entry name is the module class, including the trailing dollar"):
      assertTrue(
        EntryName.fromClass(PluginSpec.getClass) == Right(EntryName("histos.plugin.PluginSpec$")),
        EntryName("dev.earlyeffect.specular.SpecularPlugin$").value == "dev.earlyeffect.specular.SpecularPlugin$",
        EntryName.from("") == Left(EntryNameError.Empty),
        EntryName.from("foo/bar") == Left(EntryNameError.Bad("foo/bar")),
      )
    ,
  )

  private val paths = suite("paths and digests")(
    test("a repository path accepts index.html and rejects .."):
      val errors = typeCheckErrors("""RepoPath("docs/../secret")""")
      assertTrue(
        RepoPath("docs/index.html").value == "docs/index.html",
        RepoPath("coverage.xml").value == "coverage.xml",
        RepoPath("README.md").value == "README.md",
        RepoPath.from("docs/../secret") == Left(RepoPathError.Escapes("docs/../secret")),
        RepoPath.from("..") == Left(RepoPathError.Escapes("..")),
        RepoPath.from("") == Left(RepoPathError.Empty),
        RepoPath.from("/etc/passwd") == Left(RepoPathError.Escapes("/etc/passwd")),
        errors match
          case e :: _ => e.message.contains("docs/../secret")
          case Nil    => false,
      )
    ,
    test("an api digest is 64 hex digits, stored lowercase"):
      val parsed = ApiDigest.from(digest)
      val upper = ApiDigest.from(digest.toUpperCase)
      val hash = ContentHash.from(digest)
      assertTrue(
        parsed == upper,
        parsed.exists(_.value == digest),
        ApiDigest.resource == "META-INF/histos/version",
        ApiDigest.from("ab") == Left(ApiDigestError.Bad("ab")),
        ApiDigest.from(digest + " ") == Left(ApiDigestError.Bad(digest + " ")),
        hash.exists(_.value == digest),
      )
    ,
    test("a cron expression is five GitHub fields"):
      val daily = Cron.from("0 0 * * *")
      assertTrue(
        daily == Right(Cron("0 0 * * *")),
        Cron.from("0  0  *  *  *") == daily,
        Cron.from("*/15 * * * MON-FRI").exists(_.value == "*/15 * * * MON-FRI"),
        Cron.from("0 0 * JAN SUN") == Right(Cron("0 0 * JAN SUN")),
        Cron.from("") == Left(CronError.Empty),
        Cron.from("0 0 * *") == Left(CronError.Fields(4)),
        Cron.from("60 * * * *") == Left(CronError.Field(0, "60")),
        Cron.from("0 0 * * 7") == Left(CronError.Field(4, "7")),
        Cron.from("*/0 * * * *") == Left(CronError.Field(0, "*/0")),
      )
    ,
  )

  private val contract = suite("contract")(
    test("LinkStrategy.Api is a case, and a mismatch names both digests"):
      val plugin = PluginId("test.echo")
      val built = ApiDigest.from(digest)
      val loaded = ApiDigest.from(other)
      (built, loaded) match
        case (Right(pluginApi), Right(launcher)) =>
          val mismatch = PluginLoadError.VersionMismatch(plugin, pluginApi, launcher).message
          assertTrue(
            linkName(LinkStrategy.Api) == "refused",
            mismatch.contains(plugin.value),
            mismatch.contains(pluginApi.value),
            mismatch.contains(launcher.value),
            PluginLoadError.ApiLink(plugin).message.contains(plugin.value),
            PluginRunError.Defect("java.lang.RuntimeException").message.contains("java.lang.RuntimeException"),
          )
        case _ => assertTrue(false)
    ,
    test("a fixture request is the task id and nothing else"):
      val repo = Path.of("repo")
      val request = PluginFixture.request(PluginId("test.echo"), TaskName("echo"), repo)
      assertTrue(
        request.id.render == "test.echo/echo",
        request.repo == repo,
        request.modules.isEmpty,
        request.args == PluginArgs.empty,
        request.inputs.isEmpty,
        PluginArgs.empty.require(ParamName("out")) == Left(PluginRunError.MissingArg(ParamName("out"))),
        HistosPlugin.entryResource == "META-INF/histos/plugin",
      )
  )

  def spec = suite("plugins")(ids, paths, contract)
