package histos.cli

import histos.zinc.{CompileFacts, CompileFeed, SourceCount}
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

import zio.{Chunk, Ref, UIO, ZIO}

/** One compile's console. Parallel cells share the ref. The tally stays after
  * it is read.
  */
object CompileConsole:
  enum Line:
    case Out(text: String)
    case Err(text: String)

  def open(repo: Path): UIO[Session] =
    Ref.make(Counts.empty).map(counts => new Session(repo, counts))

  def notes: UIO[Notes] =
    Ref.make(Counts.empty).map(counts => new Notes(counts))

  def show[R, E](
      lines: Chunk[Line],
      out: String => ZIO[R, E, Unit],
      err: String => ZIO[R, E, Unit]
  ): ZIO[R, E, Unit] =
    ZIO.foreachDiscard(lines):
      case Line.Out(text) => out(text)
      case Line.Err(text) => err(text)

  final class Session private[CompileConsole] (repo: Path, counts: Ref[Counts]):
    def hear(feed: CompileFeed): UIO[Chunk[Line]] =
      feed match
        case CompileFeed.Began(module, version, platform) =>
          ZIO.succeed(
            Chunk(Line.Out(Lines.compiling(module, version, platform)))
          )
        case CompileFeed.Diagnostic(_, _, _, problem) =>
          ZIO.succeed(Chunk(Line.Err(Lines.diagnostic(repo, problem))))
        case CompileFeed.Done(cell) =>
          counts.update(_.finish(cell.sources, cell.classesWritten)).as {
            Lines
              .outcome(
                cell.module,
                cell.version,
                cell.platform,
                cell.changed,
                cell.sources,
                cell.classesWritten
              )
              .fold(Chunk.empty)(text => Chunk(Line.Out(text)))
          }
        case CompileFeed.Failed(error) =>
          counts.update(_.fail).as(Chunk(Line.Err(error.message)))

    /** The spelled tally. A later read is the same line. */
    def tally: UIO[Option[String]] =
      counts.get.map(_.line(Lines.tally))

    def announce[R, E](out: String => ZIO[R, E, Unit]): ZIO[R, E, Unit] =
      tally.flatMap:
        case Some(line) => out(line)
        case None       => ZIO.unit

  /** Test compiles. The same ref, read when the test run ends. */
  final class Notes private[CompileConsole] (counts: Ref[Counts]):
    def apply(
        module: ProjectName,
        version: ScalaVersion,
        platform: Platform,
        facts: CompileFacts
    ): UIO[String] =
      counts
        .update(_.finish(facts.sources, facts.classesWritten))
        .as(
          Lines.testOutcome(
            module,
            version,
            platform,
            facts.changed,
            facts.sources,
            facts.classesWritten
          )
        )

    def tally: UIO[Option[String]] =
      counts.get.map(_.line(Lines.testTally))

    def announce[R, E](out: String => ZIO[R, E, Unit]): ZIO[R, E, Unit] =
      tally.flatMap:
        case Some(line) => out(line)
        case None       => ZIO.unit

  private enum Files:
    case Empty
    case Known(count: SourceCount)
    case Unknown

  private enum Classes:
    case Empty
    case Known(written: Int)
    case Unknown

  private final case class Counts(
      modules: Int,
      failed: Int,
      files: Files,
      classes: Classes
  ):
    def finish(
        sources: Option[SourceCount],
        classesWritten: Option[Int]
    ): Counts =
      copy(
        modules = modules + 1,
        files = addFiles(files, sources),
        classes = addClasses(classes, classesWritten)
      )

    /** A failed cell has no honest file or class total. */
    def fail: Counts =
      copy(
        modules = modules + 1,
        failed = failed + 1,
        files = Files.Unknown,
        classes = Classes.Unknown
      )

    def line(
        spell: (
            Int,
            Int,
            Option[SourceCount],
            Option[Int]
        ) => Option[String]
    ): Option[String] =
      spell(modules, failed, knownFiles, knownClasses)

    private def knownFiles: Option[SourceCount] =
      files match
        case Files.Known(count)          => Some(count)
        case Files.Empty | Files.Unknown => None

    private def knownClasses: Option[Int] =
      classes match
        case Classes.Known(written)          => Some(written)
        case Classes.Empty | Classes.Unknown => None

  private def addFiles(sum: Files, sources: Option[SourceCount]): Files =
    (sum, sources) match
      case (Files.Unknown, _)               => Files.Unknown
      case (_, None)                        => Files.Unknown
      case (Files.Empty, Some(count))       => Files.Known(count)
      case (Files.Known(have), Some(count)) =>
        Files.Known(
          SourceCount(
            have.compiled + count.compiled,
            have.skipped + count.skipped
          )
        )

  private def addClasses(sum: Classes, written: Option[Int]): Classes =
    (sum, written) match
      case (Classes.Unknown, _)               => Classes.Unknown
      case (_, None)                          => Classes.Unknown
      case (Classes.Empty, Some(count))       => Classes.Known(count)
      case (Classes.Known(have), Some(count)) =>
        Classes.Known(have + count)

  private object Counts:
    val empty: Counts = Counts(0, 0, Files.Empty, Classes.Empty)
end CompileConsole
