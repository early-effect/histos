package histos.cli

import histos.build.BuildEvent
import histos.testrun.TestFeed

import java.nio.file.Path

import zio.Chunk

/** Console lines for one [[BuildEvent]]. The event is not rewritten into this text.
  */
object Show:
  def apply(repo: Path, event: BuildEvent): Chunk[CompileConsole.Line] =
    event match
      case BuildEvent.Compile(feed) => CompileConsole.lines(repo, feed)
      case BuildEvent.Test(feed)    => tested(feed)
      case BuildEvent.Out(text)     => Chunk(CompileConsole.Line.Out(text))
      case BuildEvent.Err(text)     => Chunk(CompileConsole.Line.Err(text))

  def transcript(repo: Path, events: Chunk[BuildEvent]): (Chunk[String], Chunk[String]) =
    val lines = events.flatMap(apply(repo, _))
    (
      lines.collect { case CompileConsole.Line.Out(text) => text },
      lines.collect { case CompileConsole.Line.Err(text) => text },
    )

  private def tested(feed: TestFeed): Chunk[CompileConsole.Line] =
    feed match
      case TestFeed.Begin(module, version, platform) =>
        Chunk(CompileConsole.Line.Out(Lines.begin(module, version, platform)))
      case TestFeed.Out(text) =>
        Chunk(CompileConsole.Line.Out(text))
      case TestFeed.Err(text) =>
        Chunk(CompileConsole.Line.Err(text))
      case TestFeed.Cached(module, version, platform, suite) =>
        Chunk(CompileConsole.Line.Out(Lines.cached(module, version, platform, suite)))
      case TestFeed.Done(cell) =>
        Chunk(CompileConsole.Line.Out(Lines.tested(cell)), CompileConsole.Line.Out(""))
