package histos.cli

import histos.build.BuildEvent
import histos.theme.Role
import histos.zinc.{CompileFeed, CompilePulse}
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

import zio.Chunk
import zio.test.*

object ColumnSpec extends ZIOSpecDefault:
  private val api     = ProjectName("api")
  private val engine  = ProjectName("engine")
  private val version = ScalaVersion("3.10.1")
  private val column  = 6
  private val repo    = Path.of("/repo")

  def spec = suite("module columns")(
    test("names pad to the column and a zero column does not") {
      val apiLine    = Lines.begin(api, version, Platform.Jvm, column)
      val engineLine = Lines.begin(engine, version, Platform.Jvm, column)
      val compiling  = Lines.compiling(api, version, Platform.Jvm, column)
      val skipped    = Lines.outcome(api, version, Platform.Jvm, false, None, None, column)
      assertTrue(
        apiLine == "api    3.10.1 jvm",
        engineLine == "engine 3.10.1 jvm",
        apiLine.indexOf("3.10.1") == engineLine.indexOf("3.10.1"),
        Lines.begin(api, version, Platform.Jvm) == "api 3.10.1 jvm",
        compiling == "api    3.10.1 jvm compiling",
        skipped == Some("api    3.10.1 jvm skipped unchanged"),
        compiling.indexOf("3.10.1") == apiLine.indexOf("3.10.1"),
      )
    },
    test("a pulse uses the same column") {
      val line = CompileConsole.lines(
        repo,
        CompileFeed.Pulse(
          engine,
          version,
          Platform.Jvm,
          CompilePulse.Phase("typer", Some("zinc/GraphCompile.scala")),
        ),
        column,
        CompileLog.Phases,
      )
      assertTrue(
        line.map(_.text) == Chunk("engine 3.10.1 jvm typer  zinc/GraphCompile.scala"),
        line.flatMap(_.spans).exists {
          case Part.Mark(text, Role.Pulse) => text == "typer  zinc/GraphCompile.scala"
          case _                           => false
        },
      )
    },
    test("a finished source is one line and a phase line waits for verbose") {
      val phase = CompileFeed.Pulse(
        engine,
        version,
        Platform.Jvm,
        CompilePulse.Phase("typer", Some("zinc/GraphCompile.scala")),
      )
      val done = CompileFeed.Pulse(
        engine,
        version,
        Platform.Jvm,
        CompilePulse.Finished("zinc/GraphCompile.scala"),
      )
      val java = CompileFeed.Pulse(
        engine,
        version,
        Platform.Jvm,
        CompilePulse.Phase("Java compilation", None),
      )
      assertTrue(
        CompileConsole.lines(repo, phase, column).isEmpty,
        CompileConsole.lines(repo, done, column).map(_.text) == Chunk(
          "engine 3.10.1 jvm zinc/GraphCompile.scala"
        ),
        CompileConsole.lines(repo, phase, column, CompileLog.Phases).map(_.text) == Chunk(
          "engine 3.10.1 jvm typer  zinc/GraphCompile.scala"
        ),
        CompileConsole.lines(repo, java, column).map(_.text) == Chunk("engine 3.10.1 jvm Java compilation"),
      )
    },
    test("a layout in the transcript pads the lines") {
      val events = Chunk(
        BuildEvent.Layout(column),
        BuildEvent.Compile(CompileFeed.Began(api, version, Platform.Jvm)),
      )
      val (out, err) = Show.transcript(repo, events)
      assertTrue(out == Chunk("api    3.10.1 jvm compiling"), err.isEmpty)
    },
  )
end ColumnSpec
