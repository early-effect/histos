package histos.bsp

import histos.suite.HistosSpec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import java.util.concurrent.ConcurrentLinkedQueue

import zio.ZIO
import zio.test.*

object FrameSpec extends HistosSpec:
  private val workers = 8
  private val each    = 20

  def spec = suite("a frame is one write")(
    test("parallel offers on one stream leave whole frames") {
      val chunks = new ConcurrentLinkedQueue[Array[Byte]]
      val out    = yielding(chunks)
      ZIO.scoped {
        FrameQueue.open(out).flatMap { sink =>
          ZIO.foreachParDiscard(payloads)(sink.offer).map { _ =>
            val summary = readAll(new ByteArrayInputStream(bytes(chunks)), Nil) match
              case Left(error)  => Left(error.message)
              case Right(found) => Right((found.length, found.toSet == payloads.toSet))
            assertTrue(summary == Right((payloads.length, true)))
          }
        }
      }
    },
    test("a broken stream fails the frame in hand and the one behind it") {
      val out = new OutputStream:
        override def write(b: Int): Unit = throw new java.io.IOException("closed")
      ZIO.scoped {
        FrameQueue.open(out).flatMap { sink =>
          for
            first  <- sink.offer(payload(1, 1)).either
            second <- sink.offer(payload(1, 2)).either
          yield assertTrue(first == Left(Frame.FrameError.Io), second == Left(Frame.FrameError.Io))
        }
      }
    },
  )

  /** Each `write` records its bytes, then yields. Two callers would split a frame. Offers share one writer. */
  private def yielding(chunks: ConcurrentLinkedQueue[Array[Byte]]): OutputStream =
    new OutputStream:
      override def write(b: Int): Unit = write(Array(b.toByte))

      override def write(buf: Array[Byte], off: Int, len: Int): Unit =
        val copy = new Array[Byte](len)
        System.arraycopy(buf, off, copy, 0, len)
        chunks.add(copy)
        spin(32)

  private def spin(left: Int): Unit =
    if left > 0 then
      Thread.`yield`()
      spin(left - 1)

  private def bytes(chunks: ConcurrentLinkedQueue[Array[Byte]]): Array[Byte] =
    val raw = new ByteArrayOutputStream
    chunks.forEach(raw.write(_))
    raw.toByteArray

  private def readAll(in: ByteArrayInputStream, seen: List[String]): Either[Frame.FrameError, List[String]] =
    Frame.take(in) match
      case Left(error)       => Left(error)
      case Right(None)       => Right(seen.reverse)
      case Right(Some(text)) => readAll(in, text :: seen)

  private def payloads: List[String] =
    (1 to workers).toList.flatMap(worker => (1 to each).toList.map(n => payload(worker, n)))

  private def payload(worker: Int, n: Int): String =
    val mark = s"worker-$worker-$n"
    s"""{"Event":{"Compile":{"Pulse":{"module":"api","version":"3.10.1","platform":"Native","pulse":{"Finished":{"path":"$mark"}}}}}}"""
end FrameSpec
