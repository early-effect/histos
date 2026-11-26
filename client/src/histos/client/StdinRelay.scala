package histos.client

import histos.bsp.FrameQueue
import histos.server.{RunBytes, RunInput}

import scala.scalanative.libc.stdlib
import scala.scalanative.posix.errno
import scala.scalanative.posix.unistd
import scala.scalanative.unsafe.{CInt, Zone, alloc, stackalloc}
import scala.scalanative.unsigned.*

import zio.json.*
import zio.{Chunk, Queue, Scope, UIO, ZIO}

/** Copy this process's stdin to the server as [[RunInput]] frames. Frames are offered to the connection writer. */
private[client] object StdinRelay:
  private val Interrupt: Byte = 'I'.toByte
  private val Stop: Byte      = 'S'.toByte

  def attach(sink: FrameQueue): ZIO[Scope, Nothing, Unit] =
    openPipe match
      case None                    => copyStdin(sink)
      case Some(readEnd, writeEnd) =>
        for
          queue <- Queue.unbounded[RunInput]
          _     <- ZIO.succeed(Main.arm(writeEnd))
          _     <- ZIO.addFinalizer(ZIO.succeed {
            Main.disarm()
            poke(writeEnd, Stop)
            unistd.close(readEnd)
            unistd.close(writeEnd)
          })
          _ <- readStdin(queue).forkScoped
          _ <- readNotes(readEnd, queue).forkScoped
          _ <- writeAll(sink, queue).forkScoped
        yield ()

  /** A pipe still copies bytes when the signal pipe cannot be opened. Interrupt then ends this process. */
  private def copyStdin(sink: FrameQueue): ZIO[Scope, Nothing, Unit] =
    drain(sink).forkScoped.unit

  private def drain(sink: FrameQueue): UIO[Unit] =
    readChunks.flatMap {
      case None        => put(sink, RunInput.StdinEnd)
      case Some(bytes) => put(sink, RunInput.Stdin(RunBytes.encode(bytes))) *> drain(sink)
    }

  private def put(sink: FrameQueue, frame: RunInput): UIO[Unit] =
    sink.offer(frame.toJson).ignore

  private def readStdin(queue: Queue[RunInput]): UIO[Unit] =
    def loop: UIO[Unit] =
      readChunks.flatMap {
        case None        => queue.offer(RunInput.StdinEnd).unit
        case Some(bytes) => queue.offer(RunInput.Stdin(RunBytes.encode(bytes))) *> loop
      }
    loop

  private def readNotes(fd: CInt, queue: Queue[RunInput]): UIO[Unit] =
    def loop: UIO[Unit] =
      ZIO.attemptBlocking(note(fd)).orDie.flatMap {
        case Some(Interrupt) => queue.offer(RunInput.Interrupt) *> loop
        case _               => ZIO.unit
      }
    loop

  private def writeAll(sink: FrameQueue, queue: Queue[RunInput]): UIO[Unit] =
    def loop: UIO[Unit] =
      queue.take.flatMap(frame => put(sink, frame)) *> loop
    loop

  private def readChunks: UIO[Option[Chunk[Byte]]] =
    ZIO
      .attemptBlocking {
        val buf = new Array[Byte](4096)
        val n   = System.in.read(buf)
        if n < 0 then None
        else if n == 0 then Some(Chunk.empty[Byte])
        else Some(Chunk.fromArray(java.util.Arrays.copyOf(buf, n)))
      }
      .orDie
      .flatMap {
        case Some(bytes) if bytes.isEmpty => readChunks
        case other                        => ZIO.succeed(other)
      }

  private def openPipe: Option[(CInt, CInt)] =
    val ends = stackalloc[CInt](2)
    if unistd.pipe(ends) != 0 then None
    else Some((ends(0), ends(1)))

  private def poke(fd: CInt, byte: Byte): Unit =
    val note = stdlib.malloc(1.toCSize)
    if note != null then
      note(0) = byte
      unistd.write(fd, note, 1.toCSize)
      stdlib.free(note)

  /** `None` ends the notes fiber. `EINTR` retries. */
  private def note(fd: CInt): Option[Byte] =
    Zone {
      val buf = alloc[Byte](1)
      var n   = unistd.read(fd, buf, 1.toCSize)
      while n < 0 && errno.errno == errno.EINTR do n = unistd.read(fd, buf, 1.toCSize)
      if n == 1 then Some(buf(0)) else None
    }
end StdinRelay
