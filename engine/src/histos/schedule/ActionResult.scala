package histos.schedule

import java.nio.charset.StandardCharsets

import scala.annotation.tailrec

import zio.Chunk

/** Action-cache body. bazel-remote stores an ActionResult and rejects any other bytes unless validation is disabled.
  * Field 6 is the stdout digest and names the blob. A zero size is omitted: that is the proto3 encoding of the default.
  */
private[schedule] object ActionResult:
  final case class Pointer(output: Digest, size: Long)

  def encode(output: Digest, size: Int): Chunk[Byte] =
    val hash   = output.value.getBytes(StandardCharsets.US_ASCII)
    val digest = Array.newBuilder[Byte]
    digest += HashTag
    digest += hash.length.toByte
    digest ++= hash
    if size != 0 then
      digest += SizeTag
      digest ++= varint(size.toLong)
    val body = digest.result()
    val out  = Array.newBuilder[Byte]
    out += StdoutTag
    out ++= varint(body.length.toLong)
    out ++= body
    Chunk.fromArray(out.result())

  def decode(bytes: Chunk[Byte]): Option[Pointer] =
    stdout(bytes, 0, None).flatMap(decodeDigest)

  private def decodeDigest(bytes: Chunk[Byte]): Option[Pointer] =
    fields(bytes, 0, None, 0L).flatMap: (hash, size) =>
      if size < 0 then None
      else hash.flatMap(text => Digest.from(text).map(Pointer(_, size)))

  @tailrec
  private def stdout(bytes: Chunk[Byte], at: Int, found: Option[Chunk[Byte]]): Option[Chunk[Byte]] =
    if at == bytes.length then found
    else
      tag(bytes, at) match
        case None                         => None
        case Some(Tag(field, wire, next)) =>
          skip(bytes, next, wire) match
            case None                             => None
            case Some(Skipped(payload, _, after)) =>
              val chosen = if field == StdoutDigest && wire == LengthDelimited then Some(payload) else found
              stdout(bytes, after, chosen)

  @tailrec
  private def fields(
      bytes: Chunk[Byte],
      at: Int,
      hash: Option[String],
      size: Long,
  ): Option[(Option[String], Long)] =
    if at == bytes.length then Some((hash, size))
    else
      tag(bytes, at) match
        case None                         => None
        case Some(Tag(field, wire, next)) =>
          skip(bytes, next, wire) match
            case None                                  => None
            case Some(Skipped(payload, number, after)) =>
              val hashed =
                if field == Hash && wire == LengthDelimited then ascii(payload).map(Some(_))
                else Some(hash)
              hashed match
                case None       => None
                case Some(text) =>
                  val nextSize = if field == SizeBytes && wire == Varint then number else size
                  fields(bytes, after, text, nextSize)

  private def ascii(bytes: Chunk[Byte]): Option[String] =
    if bytes.exists(_ < 0) then None
    else Some(new String(bytes.toArray, StandardCharsets.US_ASCII))

  private def tag(bytes: Chunk[Byte], at: Int): Option[Tag] =
    readVarint(bytes, at).flatMap: (value, next) =>
      if value <= 0 || value > Int.MaxValue then None
      else
        val raw   = value.toInt
        val wire  = raw & 7
        val field = raw >>> 3
        if field == 0 then None else Some(Tag(field, wire, next))

  private def skip(bytes: Chunk[Byte], at: Int, wire: Int): Option[Skipped] =
    wire match
      case Varint =>
        readVarint(bytes, at).map((value, next) => Skipped(Chunk.empty, value, next))
      case Fixed64 =>
        if at + 8 <= bytes.length then Some(Skipped(Chunk.empty, 0L, at + 8)) else None
      case LengthDelimited =>
        readVarint(bytes, at).flatMap: (length, next) =>
          if length < 0 || length > Int.MaxValue then None
          else
            val size = length.toInt
            if next + size <= bytes.length then Some(Skipped(bytes.slice(next, next + size), 0L, next + size))
            else None
      case Fixed32 =>
        if at + 4 <= bytes.length then Some(Skipped(Chunk.empty, 0L, at + 4)) else None
      case _ => None

  private def readVarint(bytes: Chunk[Byte], at: Int): Option[(Long, Int)] =
    def step(index: Int, shift: Int, acc: Long): Option[(Long, Int)] =
      if shift > 63 || index >= bytes.length then None
      else
        val bits  = bytes(index) & 0xff
        val value = acc | ((bits & 0x7f).toLong << shift)
        if (bits & 0x80) == 0 then Some((value, index + 1))
        else step(index + 1, shift + 7, value)
    step(at, 0, 0L)

  private def varint(value: Long): Array[Byte] =
    val out                     = Array.newBuilder[Byte]
    def write(rest: Long): Unit =
      val bits = (rest & 0x7f).toInt
      val next = rest >>> 7
      if next == 0L then out += bits.toByte
      else
        out += (bits | 0x80).toByte
        write(next)
    write(value)
    out.result()

  private final case class Tag(field: Int, wire: Int, next: Int)
  private final case class Skipped(payload: Chunk[Byte], number: Long, after: Int)

  private val StdoutDigest    = 6
  private val Hash            = 1
  private val SizeBytes       = 2
  private val Varint          = 0
  private val Fixed64         = 1
  private val LengthDelimited = 2
  private val Fixed32         = 5
  private val StdoutTag       = ((StdoutDigest << 3) | LengthDelimited).toByte
  private val HashTag         = ((Hash << 3) | LengthDelimited).toByte
  private val SizeTag         = ((SizeBytes << 3) | Varint).toByte
end ActionResult
