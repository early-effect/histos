package histos.client

import histos.server.ServerError

import java.io.{InputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Path

import scala.scalanative.posix.sys.socket.{connect as unixConnect, *}
import scala.scalanative.posix.sys.un.*
import scala.scalanative.posix.sys.unOps.*
import scala.scalanative.posix.unistd.{close as unixClose, read as unixRead, write as unixWrite}
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

import zio.{IO, ZIO}

/** One connected `AF_UNIX` stream. Scala Native has no `UnixDomainSocketAddress`. The descriptor stays in this file. */
object UnixPipe:
  final class Session private[UnixPipe] (
      val input: InputStream,
      val output: OutputStream,
      release: () => Unit,
  ):
    def close(): Unit = release()

  def connect(path: Path): IO[ServerError, Session] =
    ZIO
      .attemptBlocking(open(path))
      .mapError(_ => ServerError.Unreachable(path))
      .flatMap(ZIO.fromEither)

  /** macOS `sun_path` is 104 bytes, including the terminator. */
  private val PathLimit = 103

  private def open(path: Path): Either[ServerError, Session] =
    val raw   = path.toString
    val bytes = raw.getBytes(StandardCharsets.UTF_8)
    if bytes.length > PathLimit then Left(ServerError.Address(path))
    else
      val fd = socket(AF_UNIX, SOCK_STREAM, 0)
      if fd < 0 then Left(ServerError.Unreachable(path))
      else
        val connected = Zone {
          val addr = alloc[sockaddr_un]()
          addr.sun_family = AF_UNIX.toUShort
          val chars = toCString(raw)
          copyPath(addr, chars, 0)
          unixConnect(fd, addr.asInstanceOf[Ptr[sockaddr]], sizeof[sockaddr_un].toUInt) == 0
        }
        if !connected then
          unixClose(fd)
          Left(ServerError.Unreachable(path))
        else Right(session(fd))

  private def copyPath(addr: Ptr[sockaddr_un], chars: CString, index: Int)(using Zone): Unit =
    val char = chars(index)
    addr.sun_path(index) = char
    if char != 0.toByte && index < PathLimit then copyPath(addr, chars, index + 1)

  private def session(fd: CInt): Session =
    new Session(new FdIn(fd), new FdOut(fd), () => unixClose(fd))

  private final class FdIn(fd: CInt) extends InputStream:
    override def read(): Int =
      val one = new Array[Byte](1)
      if read(one, 0, 1) <= 0 then -1 else one(0) & 0xff

    override def read(b: Array[Byte], off: Int, len: Int): Int =
      if len == 0 then 0
      else
        Zone {
          val buf = alloc[Byte](len.toCSize)
          val n   = unixRead(fd, buf, len.toCSize)
          if n <= 0 then -1
          else
            copyIn(b, off, buf, n, 0)
            n
        }

    private def copyIn(dest: Array[Byte], off: Int, buf: Ptr[Byte], n: CInt, index: Int)(using Zone): Unit =
      if index < n then
        dest(off + index) = buf(index)
        copyIn(dest, off, buf, n, index + 1)
  end FdIn

  private final class FdOut(fd: CInt) extends OutputStream:
    override def write(b: Int): Unit =
      write(Array((b & 0xff).toByte), 0, 1)

    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      send(b, off, len)

    private def send(b: Array[Byte], off: Int, len: Int): Unit =
      if len > 0 then
        val n = Zone {
          val buf = alloc[Byte](len.toCSize)
          copyOut(b, off, buf, len, 0)
          unixWrite(fd, buf, len.toCSize)
        }
        if n <= 0 then throw new java.io.IOException("unix socket write failed")
        else send(b, off + n, len - n)

    private def copyOut(src: Array[Byte], off: Int, buf: Ptr[Byte], len: Int, index: Int)(using Zone): Unit =
      if index < len then
        buf(index) = src(off + index)
        copyOut(src, off, buf, len, index + 1)
  end FdOut
end UnixPipe
