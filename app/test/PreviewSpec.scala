package histos.cli

import histos.build.BuildEvent
import histos.mcp.HistosMcp
import histos.plugin.{PluginId, PluginTaskId, TaskName}
import histos.preview.{Plan, PreviewError, RunMiss}
import histos.report.{BuildFault, CallResult, Rejection}
import histos.server.{Call, Calls, Feed, Say}
import histos.task.TaskId
import histos.version.ManualVersion
import histos.{Jvm, MainClass, Organization, ProjectName, Projects, ScalaVersion, module}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Ref, Scope, ZIO}
import zio.test.*

object PreviewSpec extends histos.SuiteClock:
  private val scala3 = ScalaVersion("3.10.0")

  private val commands = suite("preview commands")(
    test("preview compile is one command"):
      for
        named <- Cli.parse(List("preview", "compile", "app"))
        all   <- Cli.parse(List("preview", "compile"))
        bare  <- Cli.parse(List("preview")).either
        other <- Cli.parse(List("preview", "nope")).either
      yield assertTrue(
        named == Command.PreviewCompile(List(ProjectName("app"))),
        all == Command.PreviewCompile(Nil),
        bare == Left(CliParseError.Help),
        other == Left(CliParseError.UnknownCommand),
      )
    ,
    test("preview test, publish, clean, cleanFull, and run parse"):
      for
        tests   <- Cli.parse(List("preview", "test", "app"))
        publish <- Cli.parse(List("preview", "publishLocal"))
        snaps   <- Cli.parse(List("preview", "publish", "snapshots", "app"))
        clean   <- Cli.parse(List("preview", "clean", "app"))
        full    <- Cli.parse(List("preview", "cleanFull"))
        extra   <- Cli.parse(List("preview", "cleanFull", "app")).either
        run     <- Cli.parse(List("preview", "run", "app"))
        jvm     <- Cli.parse(List("preview", "run", "app", "jvm"))
        inspect <- Cli.parse(List("preview", "run", "inspect"))
        plugin  <- Cli.parse(List("preview", "run", "dev.example/echo"))
      yield assertTrue(
        tests == Command.PreviewTest(List(ProjectName("app"))),
        publish == Command.PreviewPublishLocal(None, Nil),
        snaps == Command.PreviewPublishSnapshots(List("app")),
        clean == Command.PreviewClean(List(ProjectName("app"))),
        full == Command.PreviewCleanFull,
        extra == Left(
          CliParseError.Pattern("cleanFull does not take a project")
        ),
        run match
          case Command.PreviewRun(id, None) => id.value == "app"
          case _                            => false
        ,
        jvm match
          case Command.PreviewRun(id, Some(Jvm)) => id.value == "app"
          case _                                 => false
        ,
        inspect == Command.PreviewRun(TaskId.Inspect, None),
        plugin == Command.PreviewPlugin(
          PluginTaskId(PluginId("dev.example"), TaskName("echo"))
        ),
      )
    ,
    test("the preview tool accepts the five operations and requires a run id"):
      val args = HistosMcp.PreviewArgs
      assertTrue(
        HistosMcp.previewCall(args("compile", "", Nil)) == Right(
          Call.PreviewCompile(Nil)
        ),
        HistosMcp.previewCall(args("test", "ignored", List("app"))) == Right(
          Call.PreviewTest(List(ProjectName("app")))
        ),
        HistosMcp.previewCall(args("publishLocal", "", List("app"), Some("repo"))) == Right(
          Call.PreviewPublishLocal(Some("repo"), List("app"))
        ),
        HistosMcp.previewCall(args("publishSnapshots", "", List("app"))) == Right(
          Call.PreviewPublishSnapshots(List("app"))
        ),
        HistosMcp.previewCall(args("clean", "", Nil)) == Right(
          Call.PreviewClean(Nil)
        ),
        HistosMcp.previewCall(args("cleanFull", "x")) == Right(
          Call.PreviewCleanFull
        ),
        HistosMcp.previewCall(args("cleanFull", "x", List("app"))) == Left(
          Rejection.PreviewProjects
        ),
        HistosMcp.previewCall(args("run", "")) == Left(Rejection.MissingRunName),
        HistosMcp.previewCall(args("run", "app")) == Right(
          Call.PreviewRun("app", None)
        ),
        HistosMcp.previewCall(args("nope", "x")) == Left(
          Rejection.PreviewOperation("nope")
        ),
      ),
  )

  private val handlers = suite("preview handlers")(
    test("preview test prints test sources and leaves the class file"):
      ZIO.scoped {
        for
          repo    <- keep
          _       <- write(repo, "core/src/main/scala/Core.scala", "object Core")
          _       <- write(repo, "core/src/test/scala/CoreSpec.scala", "object CoreSpec")
          _       <- write(repo, "app/src/main/scala/App.scala", "object App")
          _       <- write(repo, "app/src/test/scala/AppSpec.scala", "object AppSpec")
          planted <- plant(repo)
          core = module("core", Jvm)(scala3)
          app  = module("app", Jvm)(scala3).dependsOn(core)
          graph <- ZIO.fromEither(Projects(core, app))
          text  <- Plan.test(repo, graph, List(ProjectName("app"))).map(Plan.renderTest(repo, _))
          heard <- ask(repo)(Calls.previewTest(repo, graph, List(ProjectName("app")), _))
          kept  <- same(planted)
        yield assertTrue(
          heard.outs == Chunk(text),
          heard.done == Chunk(CallResult.reported()),
          text.startsWith("core 3.10.0 jvm upstream"),
          text.contains("no previous compile"),
          text.contains("stale core/src/test/scala/CoreSpec.scala"),
          text.contains("quick: no suite would be skipped"),
          text.contains("sources core/src/test/scala/CoreSpec.scala"),
          text.contains("app 3.10.0 jvm selected"),
          text.contains("sources app/src/test/scala/AppSpec.scala"),
          !text.contains("file-level staleness: unknown"),
          !text.contains("src/main"),
          !text.contains("testQuick"),
          !text.contains("would take"),
          kept,
          !Files.exists(repo.resolve(".histos").resolve("test-classes")),
        )
      }
    ,
    test("preview publish prints the ship lines and writes no jar"):
      ZIO.scoped {
        for
          repo    <- keep
          planted <- plant(repo)
          core = module("core", Jvm)(scala3)
          app  = module("app", Jvm)(scala3).dependsOn(core)
          graph <- ZIO.fromEither(
            Projects.organization(Organization("com.example"))(
              histos.ships(
                histos.manual("libs", ManualVersion("1.0.0"))(core, app)
              ),
              core,
              app,
            )
          )
          text <- Plan
            .publishLocal(repo, graph, List("app"), Map.empty, Some("target/repo"), ZIO.succeed(repo))
            .map(Plan.renderPublish)
          heard <- ask(repo)(
            Calls.previewPublishLocal(repo, graph, Some("target/repo"), List("app"), Map.empty, _)
          )
          kept <- same(planted)
        yield assertTrue(
          heard.outs == Chunk(text),
          heard.done == Chunk(CallResult.reported()),
          text ==
            """target/repo
              |core libs 1.0.0 upstream
              |app libs 1.0.0 selected""".stripMargin,
          kept,
          !Files.exists(repo.resolve(".histos").resolve("jars")),
        )
      }
    ,
    test("preview clean does not delete a planted class"):
      ZIO.scoped {
        for
          repo    <- keep
          planted <- plant(repo)
          _       <- write(repo, ".histos/server.json", "{}")
          _       <- write(repo, ".histos/dependencies.lock", "lock")
          app = module("app", Jvm)(scala3)
          graph <- ZIO.fromEither(Projects(app))
          server = repo.resolve(".histos").resolve("server.json")
          lock   = repo.resolve(".histos").resolve("dependencies.lock")
          named <- Plan.clean(repo, graph, List(ProjectName("app"))).map(Plan.renderClean(repo, _))
          heard <- ask(repo)(Calls.previewClean(repo, graph, List(ProjectName("app")), _))
          all = Plan.renderClean(repo, Plan.cleanAll(repo))
          wiped      <- ask(repo)(Calls.previewCleanAll(repo, _))
          kept       <- same(planted)
          serverLeft <- ZIO.attempt(Files.isRegularFile(server))
          lockLeft   <- ZIO.attempt(Files.isRegularFile(lock))
        yield assertTrue(
          heard.outs == Chunk(named),
          heard.done == Chunk(CallResult.reported()),
          named.contains(".histos/classes/app"),
          !named.contains("provider"),
          !named.contains("server.json"),
          wiped.outs == Chunk(all),
          all.contains(".histos/classes"),
          all.contains(".histos/provider"),
          !all.contains("server.json"),
          !all.contains("dependencies.lock"),
          kept,
          serverLeft,
          lockLeft,
        )
      }
    ,
    test("preview cleanFull names the forgotten store and deletes nothing"):
      ZIO.scoped {
        for
          repo    <- keep
          planted <- plant(repo)
          text = Plan.renderClean(repo, Plan.cleanFull(repo))
          heard <- ask(repo)(Calls.previewCleanFull(repo, _))
          kept  <- same(planted)
          sentence = "content-store entries for this repo would be forgotten"
        yield assertTrue(
          heard.outs == Chunk(text),
          heard.done == Chunk(CallResult.reported()),
          text.startsWith(".histos/classes"),
          !text.contains("staleness"),
          text.endsWith(sentence),
          !text.contains("byte"),
          kept,
        )
      }
    ,
    test("preview run prints the module and an unknown id is a typed error"):
      ZIO.scoped {
        for
          repo    <- keep
          planted <- plant(repo)
          core = module("core", Jvm)(scala3)
          app  = module("app", Jvm)(scala3)
            .dependsOn(core)
            .main(MainClass("demo.App"))
          graph   <- ZIO.fromEither(Projects(core, app))
          planned <- ZIO.fromEither(Plan.run(graph, "app", Some(Jvm)))
          text = Plan.renderRun(planned)
          heard   <- ask(repo)(Calls.previewRun(graph, "app", Some(Jvm), _))
          missing <- ask(repo)(Calls.previewRun(graph, "missing", None, _))
          kept    <- same(planted)
          missed = CallResult.Fault(BuildFault.Preview(PreviewError.Run(RunMiss.Absent("missing"))))
        yield assertTrue(
          heard.outs == Chunk(text),
          heard.done == Chunk(CallResult.reported()),
          text ==
            """app jvm
              |upstreams core""".stripMargin,
          missing.outs.isEmpty,
          missing.done == Chunk(missed),
          CliExit.from(Call.PreviewRun("missing", None), missed) == CliExit.TaskFailed,
          missed.message == "'missing' is not a module",
          kept,
        )
      },
  )

  private final case class Heard(outs: Chunk[String], done: Chunk[CallResult])

  private def ask[E](
      repo: Path
  )(
      call: Say => ZIO[Any, E, Unit]
  ): ZIO[Any, E, Heard] =
    for
      feeds <- Ref.make(Chunk.empty[Feed])
      _     <- call(Say.record(repo, feeds))
      heard <- feeds.get
    yield Heard(
      heard.collect { case Feed.Event(BuildEvent.Out(line)) => line },
      heard.collect { case Feed.Done(result) => result },
    )

  private final case class Planted(
      path: Path,
      time: FileTime,
      bytes: Array[Byte],
  )

  private def plant(repo: Path): ZIO[Any, Throwable, Planted] =
    ZIO.attempt {
      val path = repo
        .resolve(".histos")
        .resolve("classes")
        .resolve("app")
        .resolve("Demo.class")
      Files.createDirectories(path.getParent)
      val bytes = Array[Byte](1, 2, 3)
      Files.write(path, bytes)
      Files.setLastModifiedTime(path, FileTime.fromMillis(1700000000000L))
      Planted(path, Files.getLastModifiedTime(path), bytes)
    }

  private def same(planted: Planted): ZIO[Any, Throwable, Boolean] =
    ZIO.attempt {
      Files.isRegularFile(planted.path) &&
      Files.getLastModifiedTime(planted.path) == planted.time &&
      java.util.Arrays.equals(Files.readAllBytes(planted.path), planted.bytes)
    }

  private def keep: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(
      ZIO.attempt(Files.createTempDirectory("histos-preview-cli"))
    )(delete)

  private def write(
      root: Path,
      name: String,
      text: String,
  ): ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      val path = root.resolve(name)
      Files.createDirectories(path.getParent)
      Files.writeString(path, text)
    }

  private def delete(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths
          .sortBy(_.getNameCount)
          .reverse
          .foreach(path => Files.deleteIfExists(path))
    }.ignore

  def spec = suite("preview")(commands, handlers)
end PreviewSpec
