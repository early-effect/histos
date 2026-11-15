package histos.cli

import histos.build.BuildEvent
import histos.testrun.TestFeed
import histos.theme.Theme

import java.nio.file.Path

import zio.{Chunk, Ref, ZIO}

/** Console lines for one [[BuildEvent]]. The event is not rewritten into this text.
  */
object Show:
  /** The column and the theme remembered from silent events. */
  final case class Shown(column: Int, theme: Theme)

  val start: Shown = Shown(0, Theme.standard)

  def apply(repo: Path, event: BuildEvent, column: Int = 0): Chunk[CompileConsole.Line] =
    event match
      case BuildEvent.Layout(_)          => Chunk.empty
      case BuildEvent.Theme(_)           => Chunk.empty
      case BuildEvent.Compile(feed)      => CompileConsole.lines(repo, feed, column)
      case BuildEvent.Test(feed)         => tested(feed, column)
      case BuildEvent.Out(text)          => Chunk(CompileConsole.Line.out(text))
      case BuildEvent.Err(text)          => Chunk(CompileConsole.Line.err(text))
      case BuildEvent.Status(text, role) =>
        Chunk(CompileConsole.Line.Out(Chunk(Part.Mark(text, role))))

  /** `Layout` in the chunk is the column. A chunk without one leaves names unpadded. Escapes are not added. */
  def transcript(repo: Path, events: Chunk[BuildEvent]): (Chunk[String], Chunk[String]) =
    val column = events.collectFirst { case BuildEvent.Layout(width) => width }.getOrElse(0)
    val lines  = events.flatMap(apply(repo, _, column))
    (
      lines.collect { case line: CompileConsole.Line.Out => line.text },
      lines.collect { case line: CompileConsole.Line.Err => line.text },
    )

  /** The same transcript, painted with the theme events in the chunk. */
  def colored(repo: Path, events: Chunk[BuildEvent], depth: Depth): (Chunk[String], Chunk[String]) =
    val appearance    = Appearance(Ink.Colour, depth)
    val (_, out, err) =
      events.foldLeft((start, Chunk.empty[String], Chunk.empty[String])):
        case ((shown, out, err), event) =>
          event match
            case BuildEvent.Layout(width) => (shown.copy(column = width), out, err)
            case BuildEvent.Theme(value)  => (shown.copy(theme = value.orElse(Theme.standard)), out, err)
            case other                    =>
              val lines = apply(repo, other, shown.column)
              val next  = lines.foldLeft((out, err)):
                case ((outLines, errLines), line: CompileConsole.Line.Out) =>
                  (outLines :+ Paint.paint(line.spans, shown.theme, appearance), errLines)
                case ((outLines, errLines), line: CompileConsole.Line.Err) =>
                  (outLines, errLines :+ Paint.paint(line.spans, shown.theme, appearance))
              (shown, next._1, next._2)
    (out, err)

  /** Remember a layout and a theme, then print every other event. */
  def follow[R, E](
      repo: Path,
      shown: Ref[Shown],
      event: BuildEvent,
      appearance: Appearance,
      out: String => ZIO[R, E, Unit],
      err: String => ZIO[R, E, Unit],
  ): ZIO[R, E, Unit] =
    event match
      case BuildEvent.Layout(width) => shown.update(_.copy(column = width))
      case BuildEvent.Theme(value)  => shown.update(_.copy(theme = value.orElse(Theme.standard)))
      case other                    =>
        shown.get.flatMap: frame =>
          CompileConsole.show(apply(repo, other, frame.column), frame.theme, appearance, out, err)

  private def tested(feed: TestFeed, column: Int): Chunk[CompileConsole.Line] =
    feed match
      case TestFeed.Begin(module, version, platform) =>
        Chunk(CompileConsole.Line.out(Lines.begin(module, version, platform, column)))
      case TestFeed.Out(module, version, platform, text) =>
        Chunk(CompileConsole.Line.out(Lines.cell(module, version, platform, text, column)))
      case TestFeed.Err(module, version, platform, text) =>
        Chunk(CompileConsole.Line.err(Lines.cell(module, version, platform, text, column)))
      case TestFeed.Cached(module, version, platform, suite) =>
        Chunk(CompileConsole.Line.Out(Lines.cachedParts(module, version, platform, suite, column)))
      case TestFeed.Done(cell) =>
        Chunk(CompileConsole.Line.Out(Lines.testedParts(cell, column)), CompileConsole.Line.out(""))
end Show
