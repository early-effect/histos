package histos.testrun

import histos.process.ProcessOutput

import zio.Chunk

/** The lines a process wrote, per stream, in the order they arrived. */
final case class Transcript(out: Chunk[String], err: Chunk[String]):
  def withOut(line: String): Transcript = copy(out = out :+ line)
  def withErr(line: String): Transcript = copy(err = err :+ line)
  def outText: ProcessOutput = Transcript.text(out)
  def errText: ProcessOutput = Transcript.text(err)

object Transcript:
  val empty: Transcript = Transcript(Chunk.empty, Chunk.empty)

  /** `splitLines` drops each newline, so every line gets one back. */
  private def text(lines: Chunk[String]): ProcessOutput =
    ProcessOutput.from(lines.mkString("", "\n", if lines.isEmpty then "" else "\n"))
