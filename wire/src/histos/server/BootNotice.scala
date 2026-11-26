package histos.server

import java.io.RandomAccessFile
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.annotation.tailrec

import zio.{UIO, ZIO, durationInt}

/** `compiling <file>` lines a booting server appends to its log.
  *
  * The client prints these and leaves the rest of the log alone.
  *
  * {{{
  * BootNotice.take("", "compiling histos.scala\nloaded\n")
  * // (List("compiling histos.scala"), "")
  * }}}
  */
object BootNotice:
  private val Prefix  = "compiling "
  private val MaxRead = 1024 * 1024

  /** Complete notices in `pending + chunk`, and the unfinished tail. */
  def take(pending: String, chunk: String): (List[String], String) =
    val text         = pending + chunk
    val parts        = text.split("\n", -1).toList
    val (done, rest) =
      if text.endsWith("\n") then (parts.dropRight(1), "")
      else (parts.dropRight(1), parts.lastOption.getOrElse(""))
    (done.map(_.stripSuffix("\r")).filter(notice), rest.stripSuffix("\r"))

  /** Bytes already in `log`. A missing file is zero. */
  def mark(log: Path): UIO[Long] =
    ZIO.attemptBlocking(if Files.isRegularFile(log) then Files.size(log) else 0L).orElseSucceed(0L)

  /** Print notices appended after `from`. One last read runs when this fiber is interrupted. */
  def follow(log: Path, from: Long): UIO[Unit] =
    zio.Ref
      .make((from, ""))
      .flatMap: place =>
        val step =
          place.get.flatMap: (offset, pending) =>
            ZIO
              .attemptBlocking(readNew(log, offset))
              .orElseSucceed((offset, ""))
              .flatMap: (next, text) =>
                val (lines, rest) = take(pending, text)
                emit(lines) *> place.set((next, rest)) *> ZIO.sleep(50.millis)
        step.forever.ensuring:
          ZIO.uninterruptible:
            place.get.flatMap: (offset, pending) =>
              ZIO
                .attemptBlocking(readNew(log, offset))
                .orElseSucceed((offset, ""))
                .flatMap: (_, text) =>
                  val (lines, _) = take(pending, text)
                  emit(lines)

  private def notice(line: String): Boolean =
    line.startsWith(Prefix) && line.drop(Prefix.length).trim.nonEmpty

  private def emit(lines: List[String]): UIO[Unit] =
    ZIO.foreachDiscard(lines): line =>
      zio.Console.printLineError(line).ignore *> ZIO.attempt(java.lang.System.err.flush()).ignore

  private def readNew(log: Path, offset: Long): (Long, String) =
    if !Files.isRegularFile(log) then (offset, "")
    else
      val file = new RandomAccessFile(log.toFile, "r")
      try
        val size = file.length()
        if size <= offset then (offset, "")
        else
          file.seek(offset)
          val count = math.min(size - offset, MaxRead.toLong).toInt
          val bytes = new Array[Byte](count)
          val got   = readInto(file, bytes, 0)
          (offset + got, new String(bytes, 0, got, StandardCharsets.UTF_8))
      finally file.close()

  @tailrec
  private def readInto(file: RandomAccessFile, bytes: Array[Byte], at: Int): Int =
    if at >= bytes.length then at
    else
      val n = file.read(bytes, at, bytes.length - at)
      if n < 0 then at
      else readInto(file, bytes, at + n)
end BootNotice
