package histos.cli

import histos.zinc.{
  CompileFeed,
  CompileProblem,
  CompiledCell,
  GraphCompileError
}
import histos.{Platform, ProjectName, ProjectNameError, ScalaVersion}

import java.nio.file.Path

import zio.*
import zio.test.*

object CompileConsoleSpec extends ZIOSpecDefault:
  private val repo = Path.of("/repo")
  private val version = ScalaVersion("3.10.0")

  def spec = suite("compile console")(
    test("a start is silent and each finished cell says what happened"):
      named("core", "app").flatMap { (core, app) =>
        for
          console <- CompileConsole.open(repo)
          lines <- play(
            console,
            CompileFeed.Began(core, version, Platform.Jvm),
            CompileFeed.Began(app, version, Platform.Jvm),
            CompileFeed.Done(cell(app, changed = false)),
            CompileFeed.Diagnostic(
              core,
              version,
              Platform.Jvm,
              CompileProblem.text("warn")
            ),
            CompileFeed.Done(cell(core, changed = true))
          )
          tally <- console.tally
          again <- console.tally
        yield assertTrue(
          lines == Chunk(
            CompileConsole.Line.Out(
              Lines.skipped(app, version, Platform.Jvm)
            ),
            CompileConsole.Line.Err("warn"),
            CompileConsole.Line.Out(
              Lines.compiling(core, version, Platform.Jvm)
            ),
            CompileConsole.Line.Out(
              Lines.compiled(repo, cell(core, changed = true))
            )
          ),
          tally.contains("total: 2 skipped: 1 compiled: 1"),
          again.isEmpty
        )
      }
    ,
    test("four unchanged cells spell the tally"):
      ZIO
        .foreach(List("api", "central", "engine", "app"))(name)
        .flatMap { names =>
          for
            console <- CompileConsole.open(repo)
            _ <- ZIO.foreachDiscard(names) { module =>
              console.hear(CompileFeed.Done(cell(module, changed = false)))
            }
            tally <- console.tally
          yield assertTrue(tally.contains("total: 4 skipped: 4 compiled: 0"))
        }
    ,
    test("a failed cell is counted on the tally"):
      name("core").flatMap { core =>
        val error = GraphCompileError.NoRoot(core)
        for
          console <- CompileConsole.open(repo)
          lines <- play(
            console,
            CompileFeed.Began(core, version, Platform.Jvm),
            CompileFeed.Failed(error)
          )
          tally <- console.tally
        yield assertTrue(
          lines == Chunk(CompileConsole.Line.Err(error.message)),
          tally.contains("total: 1 skipped: 0 compiled: 0 failed: 1")
        )
      }
    ,
    test("test compiles keep a separate tally"):
      named("api", "engine").flatMap { (api, engine) =>
        for
          notes <- CompileConsole.notes
          skipped <- notes(api, version, Platform.Jvm, false)
          compiled <- notes(engine, version, Platform.Jvm, true)
          tally <- notes.tally
          again <- notes.tally
        yield assertTrue(
          skipped == Lines.skippedTests(api, version, Platform.Jvm),
          compiled == Lines.compilingTests(engine, version, Platform.Jvm),
          tally.contains("test total: 2 skipped: 1 compiled: 1"),
          again.isEmpty
        )
      }
  )

  private def play(
      console: CompileConsole.Session,
      feeds: CompileFeed*
  ): UIO[Chunk[CompileConsole.Line]] =
    ZIO.foldLeft(feeds.toList)(Chunk.empty[CompileConsole.Line]): (acc, feed) =>
      console.hear(feed).map(acc ++ _)

  private def cell(module: ProjectName, changed: Boolean): CompiledCell =
    CompiledCell(
      module,
      version,
      Platform.Jvm,
      repo.resolve(module.value),
      4,
      changed = changed
    )

  private def name(raw: String): IO[ProjectNameError, ProjectName] =
    ZIO.fromEither(ProjectName.from(raw))

  private def named(
      left: String,
      right: String
  ): IO[ProjectNameError, (ProjectName, ProjectName)] =
    name(left).zip(name(right))
end CompileConsoleSpec
