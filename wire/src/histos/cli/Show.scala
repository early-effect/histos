package histos.cli

import histos.build.BuildEvent
import histos.testrun.TestFeed

import java.nio.file.Path

import zio.{Chunk, Ref, ZIO}

/** Console lines for one [[BuildEvent]]. The event is not rewritten into this text.
  */
object Show:
  def apply(repo: Path, event: BuildEvent, column: Int = 0): Chunk[CompileConsole.Line] =
    event match
      case BuildEvent.Layout(_)     => Chunk.empty
      case BuildEvent.Compile(feed) => CompileConsole.lines(repo, feed, column)
      case BuildEvent.Test(feed)    => tested(feed, column)
      case BuildEvent.Out(text)     => Chunk(CompileConsole.Line.Out(text))
      case BuildEvent.Err(text)     => Chunk(CompileConsole.Line.Err(text))

  /** `Layout` in the chunk is the column. A chunk without one leaves names unpadded. */
  def transcript(repo: Path, events: Chunk[BuildEvent]): (Chunk[String], Chunk[String]) =
    val column = events.collectFirst { case BuildEvent.Layout(width) => width }.getOrElse(0)
    val lines  = events.flatMap(apply(repo, _, column))
    (
      lines.collect { case CompileConsole.Line.Out(text) => text },
      lines.collect { case CompileConsole.Line.Err(text) => text },
    )

  /** Remember a layout, then print every other event at that width. */
  def follow[R, E](
      repo: Path,
      column: Ref[Int],
      event: BuildEvent,
      out: String => ZIO[R, E, Unit],
      err: String => ZIO[R, E, Unit],
  ): ZIO[R, E, Unit] =
    event match
      case BuildEvent.Layout(width) => column.set(width)
      case other                    =>
        column.get.flatMap(width => CompileConsole.show(apply(repo, other, width), out, err))

  private def tested(feed: TestFeed, column: Int): Chunk[CompileConsole.Line] =
    feed match
      case TestFeed.Begin(module, version, platform) =>
        Chunk(CompileConsole.Line.Out(Lines.begin(module, version, platform, column)))
      case TestFeed.Out(module, version, platform, text) =>
        Chunk(CompileConsole.Line.Out(Lines.cell(module, version, platform, text, column)))
      case TestFeed.Err(module, version, platform, text) =>
        Chunk(CompileConsole.Line.Err(Lines.cell(module, version, platform, text, column)))
      case TestFeed.Cached(module, version, platform, suite) =>
        Chunk(CompileConsole.Line.Out(Lines.cached(module, version, platform, suite, column)))
      case TestFeed.Done(cell) =>
        Chunk(CompileConsole.Line.Out(Lines.tested(cell, column)), CompileConsole.Line.Out(""))
end Show
