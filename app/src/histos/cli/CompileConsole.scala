package histos.cli

import histos.report.{Diagnostic, ModuleTally, Observation}
import histos.zinc.{CompileFacts, CompileFeed, KeyMiss, SourceCount}
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

import zio.{Chunk, Ref, UIO, ZIO}

/** One compile's console. Parallel cells share the ref. The tally and the diagnostics stay after they are read.
  */
object CompileConsole:
  enum Line:
    case Out(text: String)
    case Err(text: String)

  def lines(repo: Path, feed: CompileFeed): Chunk[Line] =
    feed match
      case CompileFeed.Began(module, version, platform) =>
        Chunk(Line.Out(Lines.compiling(module, version, platform)))
      case CompileFeed.Diagnostic(_, _, _, problem) =>
        Chunk(Line.Err(Lines.diagnostic(repo, problem)))
      case CompileFeed.Miss(module, version, platform, change) =>
        Chunk(Line.Out(KeyMiss.line(module, version, platform, change)))
      case CompileFeed.Done(cell) =>
        Lines
          .outcome(
            cell.module,
            cell.version,
            cell.platform,
            cell.changed,
            cell.sources,
            cell.classesWritten,
          )
          .fold(Chunk.empty)(text => Chunk(Line.Out(text)))
      case CompileFeed.Failed(error) =>
        Chunk(Line.Err(error.message))

  def open(repo: Path): UIO[Session] =
    Ref.make(Counts.empty).map(counts => new Session(repo, counts))

  def notes: UIO[Notes] =
    Ref.make(Counts.empty).map(counts => new Notes(counts))

  def show[R, E](
      lines: Chunk[Line],
      out: String => ZIO[R, E, Unit],
      err: String => ZIO[R, E, Unit],
  ): ZIO[R, E, Unit] =
    ZIO.foreachDiscard(lines):
      case Line.Out(text) => out(text)
      case Line.Err(text) => err(text)

  final class Session private[CompileConsole] (repo: Path, counts: Ref[Counts]):
    def hear(feed: CompileFeed): UIO[Chunk[Line]] =
      val shown = lines(repo, feed)
      feed match
        case CompileFeed.Began(_, _, _) =>
          ZIO.succeed(shown)
        case CompileFeed.Miss(_, _, _, _) =>
          ZIO.succeed(shown)
        case CompileFeed.Diagnostic(_, _, _, problem) =>
          val (found, _) = Lines.heard(repo, problem)
          counts.update(_.add(found)).as(shown)
        case CompileFeed.Done(cell) =>
          counts
            .update(
              _.finish(
                cell.module,
                cell.version,
                cell.platform,
                cell.sources,
                cell.classesWritten,
              )
            )
            .as(shown)
        case CompileFeed.Failed(error) =>
          counts.update(_.fail(Observation.failed(error))).as(shown)

    /** The spelled tally. A later read is the same line. */
    def tally: UIO[Option[String]] =
      counts.get.map(_.line(Lines.tally))

    def announce[R, E](out: String => ZIO[R, E, Unit]): ZIO[R, E, Unit] =
      tally.flatMap:
        case Some(line) => out(line)
        case None       => ZIO.unit

    def observation: UIO[Option[Observation]] =
      counts.get.map(_.observed)

    /** Every diagnostic this console heard. A later read is the same chunk. */
    def diagnostics: UIO[Chunk[Diagnostic]] =
      counts.get.map(_.diagnostics)
  end Session

  /** Test compiles. The same ref, read when the test run ends. */
  final class Notes private[CompileConsole] (counts: Ref[Counts]):
    def apply(
        module: ProjectName,
        version: ScalaVersion,
        platform: Platform,
        facts: CompileFacts,
    ): UIO[String] =
      counts
        .update(
          _.finish(
            module,
            version,
            platform,
            facts.sources,
            facts.classesWritten,
          )
        )
        .as(
          Lines.testOutcome(
            module,
            version,
            platform,
            facts.changed,
            facts.sources,
            facts.classesWritten,
          )
        )

    def tally: UIO[Option[String]] =
      counts.get.map(_.line(Lines.testTally))

    def announce[R, E](out: String => ZIO[R, E, Unit]): ZIO[R, E, Unit] =
      tally.flatMap:
        case Some(line) => out(line)
        case None       => ZIO.unit

    def observation: UIO[Option[Observation]] =
      counts.get.map(_.observed)
  end Notes

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
      classes: Classes,
      entered: Chunk[ModuleTally],
      diagnostics: Chunk[Diagnostic],
  ):
    def add(found: Diagnostic): Counts =
      copy(diagnostics = diagnostics :+ found)
    def finish(
        module: ProjectName,
        version: ScalaVersion,
        platform: Platform,
        sources: Option[SourceCount],
        classesWritten: Option[Int],
    ): Counts =
      val nextFiles = addFiles(files, sources)
      val record    = Observation.recorded(
        module,
        version,
        platform,
        sources,
        classesWritten,
      )
      copy(
        modules = modules + 1,
        files = nextFiles,
        classes = nextFiles match
          case Files.Unknown                => Classes.Unknown
          case Files.Empty | Files.Known(_) =>
            addClasses(classes, classesWritten),
        entered = entered :+ record,
      )

    /** A failed cell has no honest file or class total. */
    def fail(record: Option[ModuleTally]): Counts =
      copy(
        modules = modules + 1,
        failed = failed + 1,
        files = Files.Unknown,
        classes = Classes.Unknown,
        entered = record.fold(entered)(entered :+ _),
      )

    def line(
        spell: (
            Int,
            Int,
            Option[SourceCount],
            Option[Int],
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

    def observed: Option[Observation] =
      if modules == 0 then None
      else
        Some(
          Observation(
            entered,
            knownFiles.map(_.compiled),
            knownFiles.map(_.skipped),
            knownClasses,
            modules,
          )
        )
  end Counts

  private def addFiles(sum: Files, sources: Option[SourceCount]): Files =
    (sum, sources) match
      case (Files.Unknown, _)               => Files.Unknown
      case (_, None)                        => Files.Unknown
      case (Files.Empty, Some(count))       => Files.Known(count)
      case (Files.Known(have), Some(count)) =>
        Files.Known(
          SourceCount(
            have.compiled + count.compiled,
            have.skipped + count.skipped,
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
    val empty: Counts =
      Counts(0, 0, Files.Empty, Classes.Empty, Chunk.empty, Chunk.empty)
end CompileConsole
