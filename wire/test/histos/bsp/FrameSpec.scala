package histos.bsp

import histos.suite.HistosSpec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference

import zio.ZIO
import zio.test.*

object FrameSpec extends HistosSpec:
  private val workers = 8
  private val each    = 20

  def spec = suite("a frame is one write")(
    test("two cells writing one stream leave whole frames") {
      ZIO.attemptBlocking(together).map { found =>
        val summary = found match
          case Left(error)   => Left(error.message)
          case Right(bodies) =>
            Right((bodies.length, bodies.toSet == payloads.toSet))
        assertTrue(summary == Right((payloads.length, true)))
      }
    }
  )

  /** Each worker writes its own payloads. The result is every body. */
  private def together: Either[Frame.FrameError, List[String]] =
    val chunks  = new ConcurrentLinkedQueue[Array[Byte]]
    val refused = new AtomicReference[Option[Frame.FrameError]](None)
    val out     = new OutputStream:
      override def write(b: Int): Unit = write(Array(b.toByte))

      override def write(buf: Array[Byte], off: Int, len: Int): Unit =
        val copy = new Array[Byte](len)
        System.arraycopy(buf, off, copy, 0, len)
        chunks.add(copy)
        var spins = 0
        while spins < 32 do
          Thread.`yield`()
          spins += 1

    val threads = (1 to workers).toList.map { worker =>
      new Thread(() =>
        var n = 1
        while n <= each && refused.get.isEmpty do
          Frame.put(out, payload(worker, n)) match
            case Left(error) => refused.set(Some(error))
            case Right(_)    => ()
          n += 1
      )
    }
    threads.foreach(_.start())
    threads.foreach(_.join())
    refused.get match
      case Some(error) => Left(error)
      case None        =>
        val raw = new ByteArrayOutputStream
        chunks.forEach(raw.write(_))
        readAll(new ByteArrayInputStream(raw.toByteArray), Nil)
  end together

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
