package histos.zinc

import histos.cli.Lines

import java.nio.file.Path

import scala.collection.mutable
import scala.util.Try

/** One cell, called by the compiler on its own thread. The phase stays here because that callback is not an effect. A
  * phase change is one pulse. A later file is a pulse only while typechecking.
  */
private[zinc] final class CellPulse(checkout: Path, emit: CompilePulse => Unit):
  private var phase: String              = ""
  private val files: mutable.Set[String] = mutable.Set.empty

  def start(next: String, raw: String): Unit =
    val path = shown(raw)
    if next != phase then
      phase = next
      files.clear()
      path.foreach(files.add)
      emit(CompilePulse.Phase(next, path))
    else if typechecks(next) then
      path match
        case Some(file) if files.add(file) => emit(CompilePulse.Source(next, file))
        case _                             => ()

  private def shown(raw: String): Option[String] =
    Option(raw)
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap: value =>
        Try(Lines.relative(checkout, Path.of(value))).toOption

  private def typechecks(name: String): Boolean =
    name == "typer" || name.contains("typechecking")
end CellPulse
