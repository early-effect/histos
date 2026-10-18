package histos.bsp

import java.io.InputStream
import java.io.OutputStream
import java.nio.ByteBuffer
import java.nio.charset.{CharacterCodingException, CodingErrorAction, StandardCharsets}

import zio.{Chunk, IO, ZIO}

/** LSP base-protocol frames. BSP rides these, not newline-delimited JSON. */
object Frame:
  private val HeaderLimit: Int = 8192
  private val BodyLimit: Int = 8 * 1024 * 1024

  enum FrameError:
    case Header, MissingLength, BadLength, Truncated, TooLarge, Encoding, Io

    def message: String = this match
      case Header         => "bsp header is not a content-length frame"
      case MissingLength  => "bsp frame has no content-length"
      case BadLength      => "bsp content-length is not a byte count"
      case Truncated      => "bsp frame ended early"
      case TooLarge       => "bsp frame is too large"
      case Encoding       => "bsp frame is not utf-8"
      case Io             => "bsp frame could not be read"

  def read(in: InputStream): IO[FrameError, Option[String]] =
    ZIO.attemptBlocking(readMessage(in)).mapError(_ => FrameError.Io).flatMap(ZIO.fromEither)

  def write(out: OutputStream, payload: String): IO[FrameError, Unit] =
    val bytes = payload.getBytes(StandardCharsets.UTF_8)
    if bytes.length > BodyLimit then ZIO.fail(FrameError.TooLarge)
    else
      ZIO.attemptBlocking {
        val header = s"Content-Length: ${bytes.length}\r\n\r\n".getBytes(StandardCharsets.US_ASCII)
        out.write(header)
        out.write(bytes)
        out.flush()
      }.mapError(_ => FrameError.Io)

  private def readMessage(in: InputStream): Either[FrameError, Option[String]] =
    try
      readHeader(in, Chunk.empty).flatMap:
        case None => Right(None)
        case Some(length) =>
          val body = in.readNBytes(length)
          if body.length != length then Left(FrameError.Truncated)
          else decode(body).map(Some(_))
    catch case _: java.io.IOException => Left(FrameError.Io)

  private def readHeader(in: InputStream, seen: Chunk[Byte]): Either[FrameError, Option[Int]] =
    if seen.length >= HeaderLimit then Left(FrameError.TooLarge)
    else if marker(seen) then lengthOf(seen.dropRight(4)).map(Some(_))
    else
      val next = in.read()
      if next < 0 then
        if seen.isEmpty then Right(None) else Left(FrameError.Truncated)
      else readHeader(in, seen :+ next.toByte)

  private def marker(seen: Chunk[Byte]): Boolean =
    seen.length >= 4 && seen.drop(seen.length - 4).sameElements(Chunk[Byte]('\r'.toByte, '\n'.toByte, '\r'.toByte, '\n'.toByte))

  private def lengthOf(bytes: Chunk[Byte]): Either[FrameError, Int] =
    ascii(bytes).flatMap: text =>
      fold(text.split("\r\n", -1).toList.filter(_.nonEmpty), None)

  private def fold(lines: List[String], found: Option[Int]): Either[FrameError, Int] =
    lines match
      case Nil =>
        found match
          case Some(n) => Right(n)
          case None    => Left(FrameError.MissingLength)
      case line :: rest =>
        classify(line) match
          case HeaderLine.Ignore => fold(rest, found)
          case HeaderLine.Bad(error) => Left(error)
          case HeaderLine.Length(n) =>
            found match
              case Some(_) => Left(FrameError.BadLength)
              case None    => fold(rest, Some(n))

  private enum HeaderLine:
    case Ignore
    case Bad(error: FrameError)
    case Length(n: Int)

  private def classify(line: String): HeaderLine =
    val colon = line.indexOf(':')
    if colon <= 0 then HeaderLine.Bad(FrameError.Header)
    else
      val name = line.take(colon).trim
      val value = line.drop(colon + 1).trim
      if name.equalsIgnoreCase("Content-Length") then
        digits(value) match
          case Left(error) => HeaderLine.Bad(error)
          case Right(n)    => HeaderLine.Length(n)
      else HeaderLine.Ignore

  private def digits(raw: String): Either[FrameError, Int] =
    if raw.nonEmpty && raw.forall(_.isDigit) then
      raw.toIntOption match
        case Some(n) if n <= BodyLimit => Right(n)
        case _                         => Left(FrameError.TooLarge)
    else Left(FrameError.BadLength)

  private def ascii(bytes: Chunk[Byte]): Either[FrameError, String] =
    if bytes.exists(_ < 0) then Left(FrameError.Header)
    else Right(new String(bytes.toArray, StandardCharsets.US_ASCII))

  private def decode(bytes: Array[Byte]): Either[FrameError, String] =
    val decoder = StandardCharsets.UTF_8.newDecoder
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    try Right(decoder.decode(ByteBuffer.wrap(bytes)).toString)
    catch case _: CharacterCodingException => Left(FrameError.Encoding)
end Frame
