package histos.zinc

import histos.cli.Lines

import java.nio.file.Path

import scala.collection.mutable
import scala.util.Try

/** One cell, called by the compiler on its own thread. Phase pulses stay here because the callback is not an effect. A
  * file finishes when the next phase is the end of the run.
  */
private[zinc] final class CellPulse(checkout: Path, emit: CompilePulse => Unit):
  private var phase: String                 = ""
  private var pending: Option[String]       = None
  private val files: mutable.Set[String]    = mutable.Set.empty
  private val finished: mutable.Set[String] = mutable.Set.empty

  def start(next: String, raw: String): Unit =
    val path = shown(raw)
    pending = path
    if next != phase then
      phase = next
      files.clear()
      path.foreach(files.add)
      emit(CompilePulse.Phase(next, path))
    else if typechecks(next) then
      path match
        case Some(file) if files.add(file) => emit(CompilePulse.Source(next, file))
        case _                             => ()

  /** `nextPhase` is the phase after the one that just ran this file. A phase change with no new file leaves nothing
    * pending, so it does not finish the previous file.
    */
  def finish(nextPhase: String): Unit =
    val ready = pending
    pending = None
    if nextPhase == CellPulse.End then
      ready match
        case Some(file) if finished.add(file) => emit(CompilePulse.Finished(file))
        case _                                => ()

  private def shown(raw: String): Option[String] =
    Option(raw)
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap: value =>
        Try(Lines.relative(checkout, Path.of(value))).toOption

  private def typechecks(name: String): Boolean =
    name == "typer" || name.contains("typechecking")
end CellPulse

object CellPulse:
  /** Scala 3's name for the phase after the last one. */
  private[zinc] val End: String = "<end>"
