package histos.cli

import histos.build.BuildEvent
import histos.client.{Remote, Session}
import histos.report.{CallResult, Diagnostic, ModuleTally, Observation, Report, Severity}
import histos.server.{Calls, Feed, Message, Say}
import histos.workspace.Workspace
import histos.zinc.{
  CompileError,
  CompileFacts,
  CompileFeed,
  CompileProblem,
  CompileSeverity,
  CompiledCell,
  GraphCompileError,
  PositionBase,
  SourceCount,
}
import histos.{Platform, ProjectName, ProjectNameError, ScalaVersion}

import java.nio.file.Path

import zio.*
import zio.json.*
import zio.test.*

object CompileConsoleSpec extends ZIOSpecDefault:
  private val repo    = Path.of("/repo")
  private val version = ScalaVersion("3.10.0")

  def spec = suite("compile console")(
    test("a start names the module and a finished cell says what it did"):
      named("core", "app").flatMap { (core, app) =>
        val warn = CompileProblem(
          "unused",
          Some(repo.resolve("core/src/Core.scala")),
          CompileSeverity.Warn,
          Some(4),
          Some(2),
          None,
          None,
          PositionBase.Zinc,
        )
        val dotty = CompileProblem(
          "missing",
          Some(repo.resolve("core/src/Core.scala")),
          CompileSeverity.Error,
          Some(3),
          Some(0),
          None,
          None,
          PositionBase.Dotty,
        )
        val appCell = cell(
          app,
          changed = false,
          sources = Some(SourceCount(0, 3)),
          classesWritten = Some(0),
        )
        val coreCell = cell(
          core,
          changed = true,
          sources = Some(SourceCount(2, 1)),
          classesWritten = Some(4),
        )
        for
          console <- CompileConsole.open(repo)
          lines   <- play(
            console,
            CompileFeed.Began(core, version, Platform.Jvm),
            CompileFeed.Began(app, version, Platform.Jvm),
            CompileFeed.Done(appCell),
            CompileFeed.Diagnostic(core, version, Platform.Jvm, warn),
            CompileFeed.Diagnostic(core, version, Platform.Jvm, dotty),
            CompileFeed.Done(coreCell),
          )
          tally <- console.tally
          again <- console.tally
          found <- console.diagnostics
        yield assertTrue(
          lines == Chunk(
            CompileConsole.Line.Out(
              Lines.compiling(core, version, Platform.Jvm)
            ),
            CompileConsole.Line.Out(
              Lines.compiling(app, version, Platform.Jvm)
            ),
            CompileConsole.Line.Out(
              "app 3.10.0 jvm skipped unchanged files compiled: 0 skipped: 3 classes: 0"
            ),
            CompileConsole.Line.Err(Lines.diagnostic(repo, warn)),
            CompileConsole.Line.Err(Lines.diagnostic(repo, dotty)),
            CompileConsole.Line.Out(
              "core 3.10.0 jvm files compiled: 2 skipped: 1 classes: 4"
            ),
          ),
          Lines.diagnostic(repo, warn) == "core/src/Core.scala:4:2: warn: unused",
          Lines.diagnostic(repo, dotty) == "core/src/Core.scala:4:1: error: missing",
          found == Chunk(
            Diagnostic(
              Some("core/src/Core.scala"),
              Some(4),
              Some(2),
              None,
              None,
              Severity.Warn,
              "unused",
            ),
            Diagnostic(
              Some("core/src/Core.scala"),
              Some(4),
              Some(1),
              None,
              None,
              Severity.Error,
              "missing",
            ),
          ),
          tally.contains(
            "files compiled: 2 skipped: 4; modules: 2; classes: 4"
          ),
          again == tally,
        )
      }
    ,
    test("unknown file counts are omitted and a census is not printed"):
      name("core").flatMap { core =>
        val bare = cell(core, changed = true, classesWritten = Some(9))
        for
          console <- CompileConsole.open(repo)
          lines   <- play(
            console,
            CompileFeed.Began(core, version, Platform.Jvm),
            CompileFeed.Done(bare),
          )
          tally <- console.tally
          found <- console.observation
        yield assertTrue(
          lines == Chunk(
            CompileConsole.Line.Out(
              Lines.compiling(core, version, Platform.Jvm)
            )
          ),
          tally.contains("modules: 1"),
          !tally.exists(_.contains("files")),
          !tally.exists(_.contains("classes:")),
          found.exists(unknownOmitsClasses),
          !lines.exists {
            case CompileConsole.Line.Out(text) =>
              text.contains("classes:") || text.contains("4")
            case CompileConsole.Line.Err(_) => false
          },
        )
      }
    ,
    test("four unchanged cells spell files and modules"):
      ZIO
        .foreach(List("api", "central", "engine", "app"))(name)
        .flatMap { names =>
          for
            console <- CompileConsole.open(repo)
            _       <- ZIO.foreachDiscard(names) { module =>
              console.hear(
                CompileFeed.Done(
                  cell(
                    module,
                    changed = false,
                    sources = Some(SourceCount(0, 2)),
                    classesWritten = Some(0),
                  )
                )
              )
            }
            tally <- console.tally
          yield assertTrue(
            tally.contains(
              "files compiled: 0 skipped: 8; modules: 4; classes: 0"
            )
          )
        }
    ,
    test("a failed cell drops the file total"):
      name("core").flatMap { core =>
        val error = GraphCompileError.NoRoot(core)
        for
          console <- CompileConsole.open(repo)
          lines   <- play(
            console,
            CompileFeed.Began(core, version, Platform.Jvm),
            CompileFeed.Done(
              cell(
                core,
                changed = true,
                sources = Some(SourceCount(1, 0)),
                classesWritten = Some(1),
              )
            ),
            CompileFeed.Failed(error),
          )
          tally <- console.tally
          found <- console.observation
        yield assertTrue(
          lines.exists {
            case CompileConsole.Line.Out(text) =>
              text == Lines.compiling(core, version, Platform.Jvm)
            case CompileConsole.Line.Err(_) => false
          },
          lines.exists {
            case CompileConsole.Line.Err(text) => text == error.message
            case CompileConsole.Line.Out(_)    => false
          },
          tally.contains("modules: 2 failed: 1"),
          !tally.exists(line => line.contains("files") || line.contains("classes")),
          found == Some(
            Observation(
              Chunk(
                ModuleTally(
                  core.value,
                  version.value,
                  Platform.Jvm.token,
                  Some(1),
                  Some(0),
                  Some(1),
                )
              ),
              None,
              None,
              None,
              2,
            )
          ),
        )
      }
    ,
    test("a failed cell with a module records no counts"):
      named("core", "app").flatMap { (core, app) =>
        val failed = GraphCompileError.Failed(
          core,
          version,
          Platform.Jvm,
          CompileError.NoSources,
        )
        val missing = GraphCompileError.MissingUpstream(
          app,
          core,
          version,
          Platform.Jvm,
        )
        for
          console <- CompileConsole.open(repo)
          _       <- play(
            console,
            CompileFeed.Failed(failed),
            CompileFeed.Failed(missing),
          )
          found <- console.observation
        yield assertTrue(
          found.exists { observation =>
            observation.moduleCount == 2 &&
            observation.compiled.isEmpty &&
            observation.skipped.isEmpty &&
            observation.classes.isEmpty &&
            observation.modules == Chunk(
              ModuleTally("core", "3.10.0", "jvm", None, None, None),
              ModuleTally("app", "3.10.0", "jvm", None, None, None),
            )
          }
        )
      }
    ,
    test("an unchanged test compile is not tests skipped"):
      named("api", "engine").flatMap { (api, engine) =>
        val unchanged = CompileFacts(
          changed = false,
          sources = Some(SourceCount(0, 5)),
          classesWritten = Some(0),
        )
        val changed = CompileFacts(
          changed = true,
          sources = Some(SourceCount(1, 0)),
          classesWritten = Some(2),
        )
        for
          notes    <- CompileConsole.notes
          skipped  <- notes(api, version, Platform.Jvm, unchanged)
          compiled <- notes(engine, version, Platform.Jvm, changed)
          tally    <- notes.tally
          again    <- notes.tally
        yield assertTrue(
          skipped ==
            "api 3.10.0 jvm test compile unchanged files compiled: 0 skipped: 5 classes: 0",
          compiled ==
            "engine 3.10.0 jvm test files compiled: 1 skipped: 0 classes: 2",
          !skipped.contains("tests skipped"),
          !compiled.contains("tests skipped"),
          tally.contains(
            "test files compiled: 1 skipped: 5; modules: 2; classes: 2"
          ),
          again == tally,
        )
      }
    ,
    test("run and assembly speak the same compile lines"):
      named("core", "app").flatMap { (core, app) =>
        val unchanged = cell(
          core,
          changed = false,
          sources = Some(SourceCount(0, 3)),
          classesWritten = Some(0),
        )
        val written = cell(
          app,
          changed = true,
          sources = Some(SourceCount(2, 1)),
          classesWritten = Some(7),
        )
        for
          console <- CompileConsole.open(repo)
          lines   <- play(
            console,
            CompileFeed.Began(core, version, Platform.Jvm),
            CompileFeed.Done(unchanged),
            CompileFeed.Began(app, version, Platform.Jvm),
            CompileFeed.Done(written),
          )
        yield assertTrue(
          lines == Chunk(
            CompileConsole.Line.Out("compiling core 3.10.0 jvm"),
            CompileConsole.Line.Out(
              "core 3.10.0 jvm skipped unchanged files compiled: 0 skipped: 3 classes: 0"
            ),
            CompileConsole.Line.Out("compiling app 3.10.0 jvm"),
            CompileConsole.Line.Out(
              "app 3.10.0 jvm files compiled: 2 skipped: 1 classes: 7"
            ),
          )
        )
      }
    ,
    test("an empty run spells nothing"):
      CompileConsole.open(repo).flatMap { console =>
        for
          tally <- console.tally
          found <- console.observation
          noted <- console.diagnostics
        yield assertTrue(tally == None, found == None, noted == Chunk.empty)
      }
    ,
    test("a diagnostic is the stderr line and the report fact"):
      name("core").flatMap { core =>
        val warn = CompileProblem(
          "unused",
          Some(repo.resolve("core/src/Core.scala")),
          CompileSeverity.Warn,
          Some(4),
          Some(2),
          Some(4),
          Some(8),
          PositionBase.Zinc,
        )
        val dotty = CompileProblem(
          "missing",
          Some(repo.resolve("core/src/Core.scala")),
          CompileSeverity.Error,
          Some(3),
          Some(0),
          Some(3),
          Some(5),
          PositionBase.Dotty,
        )
        val bare = CompileProblem(
          "no file",
          None,
          CompileSeverity.Info,
          None,
          None,
          None,
          None,
          PositionBase.Zinc,
        )
        val expected = Chunk(
          Diagnostic(
            Some("core/src/Core.scala"),
            Some(4),
            Some(2),
            Some(4),
            Some(8),
            Severity.Warn,
            "unused",
          ),
          Diagnostic(
            Some("core/src/Core.scala"),
            Some(4),
            Some(1),
            Some(4),
            Some(6),
            Severity.Error,
            "missing",
          ),
          Diagnostic(None, None, None, None, None, Severity.Info, "no file"),
        )
        val problems = List(warn, dotty, bare)
        for
          console <- CompileConsole.open(repo)
          feeds   <- Ref.make(Chunk.empty[Feed])
          say = Say.record(repo, feeds)
          lines <- ZIO.foldLeft(problems)(Chunk.empty[CompileConsole.Line]) { (acc, problem) =>
            speak(console, say, CompileFeed.Diagnostic(core, version, Platform.Jvm, problem)).map(acc ++ _)
          }
          found   <- console.diagnostics
          _       <- say.note(found)
          _       <- say(Message.Done(CallResult.reported()))
          chunk   <- feeds.get
          carried <- say.diagnostics
          local = Session.spoken(repo, chunk.toList)
          wired = chunk.reverse.collectFirst { case Feed.Done(result) => result } match
            case Some(result) => Say.wire(Message.Done(result))
            case None         => ""
          remote = Remote.event(wired) match
            case Right(Remote.Event.Stop(done)) =>
              val events = chunk.collect { case feed @ Feed.Event(_) => feed }
              Session.spoken(repo, (events :+ done).toList)
            case _ => Left(Session.SessionError.DidNotFinish)
          parallel <- CompileConsole.open(repo)
          _        <- ZIO.foreachParDiscard(problems) { problem =>
            parallel.hear(CompileFeed.Diagnostic(core, version, Platform.Jvm, problem))
          }
          kept <- parallel.diagnostics
        yield
          val report = Report(
            Chunk.empty,
            Chunk(
              Lines.diagnostic(repo, warn),
              Lines.diagnostic(repo, dotty),
              Lines.diagnostic(repo, bare),
            ),
            None,
            None,
            expected,
            Chunk.empty,
            None,
          )
          assertTrue(
            lines == Chunk(
              CompileConsole.Line.Err(Lines.diagnostic(repo, warn)),
              CompileConsole.Line.Err(Lines.diagnostic(repo, dotty)),
              CompileConsole.Line.Err(Lines.diagnostic(repo, bare)),
            ),
            Lines.diagnostic(repo, warn) == "core/src/Core.scala:4:2: warn: unused",
            Lines.diagnostic(repo, dotty) == "core/src/Core.scala:4:1: error: missing",
            Lines.diagnostic(repo, bare) == "info: no file",
            found == expected,
            carried == expected,
            local == Right(CallResult.Reported(report)),
            remote == Right(CallResult.Reported(report)),
            kept.length == 3,
            kept.exists(_.message == "unused"),
            kept.exists(_.message == "missing"),
            kept.exists(diag =>
              diag.message == "no file" && diag.path == None && diag.line == None && diag.column == None
            ),
          )
      }
    ,
    test("run, assembly, and publish keep the diagnostic"):
      name("core").flatMap { core =>
        ZIO.scoped {
          for
            home <- Workspace.work("histos-run-diagnostics")
            warn = CompileProblem(
              "unused",
              Some(home.resolve("core/src/Core.scala")),
              CompileSeverity.Warn,
              Some(4),
              Some(2),
              Some(4),
              Some(8),
              PositionBase.Zinc,
            )
            bare = CompileProblem(
              "no file",
              None,
              CompileSeverity.Info,
              None,
              None,
              None,
              None,
              PositionBase.Zinc,
            )
            console <- CompileConsole.open(home)
            feeds   <- Ref.make(Chunk.empty[Feed])
            say = Say.record(home, feeds)
            _     <- speak(console, say, CompileFeed.Diagnostic(core, version, Platform.Jvm, warn))
            _     <- speak(console, say, CompileFeed.Diagnostic(core, version, Platform.Jvm, bare))
            _     <- Calls.finishNoted(home, say, console, List(console.observation), CallResult.reported())
            chunk <- feeds.get
            found = Session.spoken(home, chunk.toList)
          yield
            val zinc    = Lines.diagnostic(home, warn)
            val missing = Lines.diagnostic(home, bare)
            val heard   = found match
              case Right(CallResult.Reported(report)) => report.diagnostics
              case _                                  => Chunk.empty
            val expected = Chunk(
              Diagnostic(
                Some("core/src/Core.scala"),
                Some(4),
                Some(2),
                Some(4),
                Some(8),
                Severity.Warn,
                "unused",
              ),
              Diagnostic(None, None, None, None, None, Severity.Info, "no file"),
            )
            assertTrue(
              found == Right(
                CallResult.Reported(
                  Report(Chunk.empty, Chunk(zinc, missing), None, None, expected, Chunk.empty, None)
                )
              ),
              zinc == "core/src/Core.scala:4:2: warn: unused",
              missing == "info: no file",
              heard.exists(diag =>
                diag.message == "unused" && diag.path == Some("core/src/Core.scala") &&
                  diag.line == Some(4) && diag.column == Some(2) &&
                  diag.endLine == Some(4) && diag.endColumn == Some(8)
              ),
              heard.exists(diag =>
                diag.message == "no file" && diag.path == None && diag.line == None &&
                  diag.column == None && diag.endLine == None && diag.endColumn == None
              ),
              !heard.exists(diag =>
                diag.line.contains(0) || diag.column.contains(0) || diag.endLine.contains(0) ||
                  diag.endColumn.contains(0) || diag.path.contains("0")
              ),
            )
        }
      },
  )

  private def unknownOmitsClasses(observation: Observation): Boolean =
    observation.compiled.isEmpty && observation.skipped.isEmpty &&
      observation.classes.isEmpty && observation.moduleCount == 1 &&
      (observation.modules match
        case Chunk(tally) =>
          tally.module == "core" && tally.version == "3.10.0" &&
          tally.platform == "jvm" && tally.compiled.isEmpty &&
          tally.skipped.isEmpty && tally.classes.isEmpty
        case _ => false) && !observation.toJson.contains(""""classes":4""") &&
      !observation.toJson.contains(""""classes":9""")

  private def speak(
      console: CompileConsole.Session,
      say: Say,
      feed: CompileFeed,
  ): IO[histos.server.ServerError, Chunk[CompileConsole.Line]] =
    console.hear(feed).flatMap { lines =>
      say(Message.Event(BuildEvent.Compile(feed))).as(lines)
    }

  private def play(
      console: CompileConsole.Session,
      feeds: CompileFeed*
  ): UIO[Chunk[CompileConsole.Line]] =
    ZIO.foldLeft(feeds.toList)(Chunk.empty[CompileConsole.Line]): (acc, feed) =>
      console.hear(feed).map(acc ++ _)

  private def cell(
      module: ProjectName,
      changed: Boolean,
      sources: Option[SourceCount] = None,
      classesWritten: Option[Int] = None,
  ): CompiledCell =
    CompiledCell(
      module,
      version,
      Platform.Jvm,
      repo.resolve(module.value),
      4,
      key = None,
      changed = changed,
      sources = sources,
      classesWritten = classesWritten,
    )

  private def name(raw: String): IO[ProjectNameError, ProjectName] =
    ZIO.fromEither(ProjectName.from(raw))

  private def named(
      left: String,
      right: String,
  ): IO[ProjectNameError, (ProjectName, ProjectName)] =
    name(left).zip(name(right))
end CompileConsoleSpec
