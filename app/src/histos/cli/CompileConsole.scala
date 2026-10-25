package histos.cli

import histos.zinc.CompileFeed
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

import zio.{Chunk, Ref, UIO, ZIO}

/** One compile's console. Parallel cells share the ref: a start is not a
  * compile, and the tally is whatever finished, including when the run fails.
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
        case CompileFeed.Began(_, _, _) =>
          ZIO.succeed(Chunk.empty)
        case CompileFeed.Diagnostic(_, _, _, problem) =>
          ZIO.succeed(Chunk(Line.Err(problem.text)))
        case CompileFeed.Done(cell) if !cell.changed =>
          counts
            .update(_.skip)
            .as(
              Chunk(
                Line
                  .Out(Lines.skipped(cell.module, cell.version, cell.platform))
              )
            )
        case CompileFeed.Done(cell) =>
          counts
            .update(_.compile)
            .as(
              Chunk(
                Line.Out(
                  Lines.compiling(cell.module, cell.version, cell.platform)
                ),
                Line.Out(Lines.compiled(repo, cell))
              )
            )
        case CompileFeed.Failed(error) =>
          counts.update(_.fail).as(Chunk(Line.Err(error.message)))

    /** The spelled tally, once. A second read is empty. */
    def tally: UIO[Option[String]] =
      counts.modify: current =>
        (
          Lines.tally(
            current.total,
            current.skipped,
            current.compiled,
            current.failed
          ),
          Counts.empty
        )

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
        changed: Boolean
    ): UIO[String] =
      val counted =
        if changed then counts.update(_.compile) else counts.update(_.skip)
      counted.as:
        if changed then Lines.compilingTests(module, version, platform)
        else Lines.skippedTests(module, version, platform)

    def tally: UIO[Option[String]] =
      counts.modify: current =>
        (
          Lines.testTally(
            current.total,
            current.skipped,
            current.compiled,
            current.failed
          ),
          Counts.empty
        )

    def announce[R, E](out: String => ZIO[R, E, Unit]): ZIO[R, E, Unit] =
      tally.flatMap:
        case Some(line) => out(line)
        case None       => ZIO.unit

  private final case class Counts(skipped: Int, compiled: Int, failed: Int):
    def total: Int = skipped + compiled + failed
    def skip: Counts = copy(skipped = skipped + 1)
    def compile: Counts = copy(compiled = compiled + 1)
    def fail: Counts = copy(failed = failed + 1)

  private object Counts:
    val empty: Counts = Counts(0, 0, 0)
end CompileConsole
