package histos.cli

import histos.zinc.{
  CompileFacts,
  CompileFeed,
  CompileProblem,
  CompileSeverity,
  CompiledCell,
  GraphCompileError,
  PositionBase,
  SourceCount
}
import histos.{Platform, ProjectName, ProjectNameError, ScalaVersion}

import java.nio.file.Path

import zio.*
import zio.test.*

object CompileConsoleSpec extends ZIOSpecDefault:
  private val repo = Path.of("/repo")
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
          PositionBase.Zinc
        )
        val dotty = CompileProblem(
          "missing",
          Some(repo.resolve("core/src/Core.scala")),
          CompileSeverity.Error,
          Some(3),
          Some(0),
          None,
          None,
          PositionBase.Dotty
        )
        val appCell = cell(
          app,
          changed = false,
          sources = Some(SourceCount(0, 3)),
          classesWritten = Some(0)
        )
        val coreCell = cell(
          core,
          changed = true,
          sources = Some(SourceCount(2, 1)),
          classesWritten = Some(4)
        )
        for
          console <- CompileConsole.open(repo)
          lines <- play(
            console,
            CompileFeed.Began(core, version, Platform.Jvm),
            CompileFeed.Began(app, version, Platform.Jvm),
            CompileFeed.Done(appCell),
            CompileFeed.Diagnostic(core, version, Platform.Jvm, warn),
            CompileFeed.Diagnostic(core, version, Platform.Jvm, dotty),
            CompileFeed.Done(coreCell)
          )
          tally <- console.tally
          again <- console.tally
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
            CompileConsole.Line.Err(
              "core/src/Core.scala:4:2: warn: unused"
            ),
            CompileConsole.Line.Err(
              "core/src/Core.scala:4:1: error: missing"
            ),
            CompileConsole.Line.Out(
              "core 3.10.0 jvm files compiled: 2 skipped: 1 classes: 4"
            )
          ),
          tally.contains(
            "files compiled: 2 skipped: 4; modules: 2; classes: 4"
          ),
          again == tally
        )
      }
    ,
    test("unknown file counts are omitted and a census is not printed"):
      name("core").flatMap { core =>
        val bare = cell(core, changed = true, classesWritten = Some(9))
        for
          console <- CompileConsole.open(repo)
          lines <- play(
            console,
            CompileFeed.Began(core, version, Platform.Jvm),
            CompileFeed.Done(bare)
          )
          tally <- console.tally
        yield assertTrue(
          lines == Chunk(
            CompileConsole.Line.Out(
              Lines.compiling(core, version, Platform.Jvm)
            )
          ),
          tally.contains("modules: 1"),
          !tally.exists(_.contains("files")),
          !tally.exists(_.contains("classes:")),
          !lines.exists {
            case CompileConsole.Line.Out(text) =>
              text.contains("classes:") || text.contains("4")
            case CompileConsole.Line.Err(_) => false
          }
        )
      }
    ,
    test("four unchanged cells spell files and modules"):
      ZIO
        .foreach(List("api", "central", "engine", "app"))(name)
        .flatMap { names =>
          for
            console <- CompileConsole.open(repo)
            _ <- ZIO.foreachDiscard(names) { module =>
              console.hear(
                CompileFeed.Done(
                  cell(
                    module,
                    changed = false,
                    sources = Some(SourceCount(0, 2)),
                    classesWritten = Some(0)
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
          lines <- play(
            console,
            CompileFeed.Began(core, version, Platform.Jvm),
            CompileFeed.Done(
              cell(
                core,
                changed = true,
                sources = Some(SourceCount(1, 0)),
                classesWritten = Some(1)
              )
            ),
            CompileFeed.Failed(error)
          )
          tally <- console.tally
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
          !tally.exists(line =>
            line.contains("files") || line.contains("classes")
          )
        )
      }
    ,
    test("an unchanged test compile is not tests skipped"):
      named("api", "engine").flatMap { (api, engine) =>
        val unchanged = CompileFacts(
          changed = false,
          sources = Some(SourceCount(0, 5)),
          classesWritten = Some(0)
        )
        val changed = CompileFacts(
          changed = true,
          sources = Some(SourceCount(1, 0)),
          classesWritten = Some(2)
        )
        for
          notes <- CompileConsole.notes
          skipped <- notes(api, version, Platform.Jvm, unchanged)
          compiled <- notes(engine, version, Platform.Jvm, changed)
          tally <- notes.tally
          again <- notes.tally
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
          again == tally
        )
      }
    ,
    test("run and assembly speak the same compile lines"):
      named("core", "app").flatMap { (core, app) =>
        val unchanged = cell(
          core,
          changed = false,
          sources = Some(SourceCount(0, 3)),
          classesWritten = Some(0)
        )
        val written = cell(
          app,
          changed = true,
          sources = Some(SourceCount(2, 1)),
          classesWritten = Some(7)
        )
        for
          console <- CompileConsole.open(repo)
          lines <- play(
            console,
            CompileFeed.Began(core, version, Platform.Jvm),
            CompileFeed.Done(unchanged),
            CompileFeed.Began(app, version, Platform.Jvm),
            CompileFeed.Done(written)
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
            )
          )
        )
      }
    ,
    test("an empty run spells nothing"):
      CompileConsole.open(repo).flatMap { console =>
        console.tally.map(tally => assertTrue(tally.isEmpty))
      }
  )

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
      classesWritten: Option[Int] = None
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
      classesWritten = classesWritten
    )

  private def name(raw: String): IO[ProjectNameError, ProjectName] =
    ZIO.fromEither(ProjectName.from(raw))

  private def named(
      left: String,
      right: String
  ): IO[ProjectNameError, (ProjectName, ProjectName)] =
    name(left).zip(name(right))
end CompileConsoleSpec
