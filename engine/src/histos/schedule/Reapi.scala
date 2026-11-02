package histos.schedule

import java.nio.charset.StandardCharsets

import scala.annotation.tailrec

import zio.Chunk

/** Hand-written remote-execution messages. The published jars do not contain `ActionCache` or `ByteStream`. Field
  * numbers match those protos. ByteStream `data` is field 10 on both `ReadResponse` and `WriteRequest`. An action-cache
  * digest uses [[ActionKeySize]]: bazel-remote rejects a zero size and does not use the size as the lookup key. The key
  * is the hash, the same key the HTTP path uses. A blob digest uses the real byte length. Zero is legal only for the
  * empty SHA-256, which proto3 already omits.
  */
private[schedule] object Reapi:
  val ActionKeySize: Long = 1L

  val ActionCache: String        = "build.bazel.remote.execution.v2.ActionCache"
  val ContentAddressable: String = "build.bazel.remote.execution.v2.ContentAddressableStorage"
  val ByteStream: String         = "google.bytestream.ByteStream"

  val GetActionResult: String    = s"$ActionCache/GetActionResult"
  val UpdateActionResult: String = s"$ActionCache/UpdateActionResult"
  val FindMissingBlobs: String   = s"$ContentAddressable/FindMissingBlobs"
  val Read: String               = s"$ByteStream/Read"
  val Write: String              = s"$ByteStream/Write"

  val WriteChunk: Int = 64 * 1024

  /** `data` on `ReadResponse` and `WriteRequest`. The bytestream proto numbers that field 10. */
  private val Data: Int = 10

  /** `blob_digests` on the request and `missing_blob_digests` on the response. Both are field 2. */
  private val BlobDigest: Int = 2

  final case class WriteFrame(resource: Option[String], offset: Long, finish: Boolean, data: Chunk[Byte])

  def getAction(instance: String, action: Digest): Chunk[Byte] =
    stringField(1, instance) ++ messageField(2, digestMessage(action.value, ActionKeySize))

  def updateAction(instance: String, action: Digest, result: Chunk[Byte]): Chunk[Byte] =
    stringField(1, instance) ++ messageField(2, digestMessage(action.value, ActionKeySize)) ++ messageField(3, result)

  def findMissing(instance: String, output: Digest, size: Long): Chunk[Byte] =
    stringField(1, instance) ++ messageField(BlobDigest, digestMessage(output.value, size))

  def missingResponse(digests: List[Digest]): Chunk[Byte] =
    digests.foldLeft(Chunk.empty[Byte])((acc, digest) =>
      acc ++ messageField(BlobDigest, digestMessage(digest.value, 0L))
    )

  def missingDigests(bytes: Chunk[Byte]): Option[List[Digest]] =
    read(bytes).flatMap: fields =>
      foldDigests(repeated(fields, BlobDigest))

  def readRequest(resource: String): Chunk[Byte] =
    stringField(1, resource)

  def readData(bytes: Chunk[Byte]): Option[Chunk[Byte]] =
    read(bytes).map: fields =>
      repeated(fields, Data).foldLeft(Chunk.empty[Byte])(_ ++ _)

  def readResponse(data: Chunk[Byte]): Chunk[Byte] =
    bytesField(Data, data)

  def writeResponse(committed: Long): Chunk[Byte] =
    numberField(1, committed)

  def frames(resource: String, bytes: Chunk[Byte]): Chunk[Chunk[Byte]] =
    if bytes.isEmpty then Chunk(writeFrame(Some(resource), 0L, finish = true, Chunk.empty))
    else
      val parts = bytes.grouped(WriteChunk).toList
      Chunk.fromIterable:
        parts.zipWithIndex.map: (part, index) =>
          val name = if index == 0 then Some(resource) else None
          writeFrame(name, index.toLong * WriteChunk, index == parts.length - 1, part)

  def writeFrame(resource: Option[String], offset: Long, finish: Boolean, data: Chunk[Byte]): Chunk[Byte] =
    resource.fold(Chunk.empty[Byte])(stringField(1, _)) ++
      numberField(2, offset) ++
      boolField(3, finish) ++
      bytesField(Data, data)

  def readResource(instance: String, output: Digest, size: Long): String =
    s"$instance/blobs/${output.value}/$size"

  def writeResource(instance: String, output: Digest, size: Long): String =
    s"$instance/uploads/${output.value}/blobs/${output.value}/$size"

  def decodeGet(bytes: Chunk[Byte]): Option[(String, Digest)] =
    read(bytes).flatMap: fields =>
      for
        instance <- fieldText(fields, 1)
        body     <- fieldBytes(fields, 2)
        digest   <- digestOf(body)
      yield (instance, digest)

  def decodeUpdate(bytes: Chunk[Byte]): Option[(String, Digest, Chunk[Byte])] =
    read(bytes).flatMap: fields =>
      for
        instance <- fieldText(fields, 1)
        body     <- fieldBytes(fields, 2)
        digest   <- digestOf(body)
        result   <- fieldBytes(fields, 3)
      yield (instance, digest, result)

  def decodeFind(bytes: Chunk[Byte]): Option[(String, List[Digest])] =
    read(bytes).flatMap: fields =>
      fieldText(fields, 1).flatMap: instance =>
        foldDigests(repeated(fields, BlobDigest)).map(instance -> _)

  def decodeRead(bytes: Chunk[Byte]): Option[String] =
    read(bytes).flatMap(fields => fieldText(fields, 1))

  def decodeWrite(bytes: Chunk[Byte]): Option[WriteFrame] =
    read(bytes).map: fields =>
      WriteFrame(
        fieldText(fields, 1),
        fieldLong(fields, 2),
        fieldLong(fields, 3) != 0L,
        fieldBytes(fields, Data).getOrElse(Chunk.empty),
      )

  /** Hash and size after the `blobs` segment. The instance and the upload id sit in front of that segment. */
  def blob(resource: String): Option[(String, Long)] =
    val fields = resource.split("/", -1).toList
    val at     = fields.indexOf("blobs")
    if at < 0 then None
    else fields.lift(at + 1).zip(fields.lift(at + 2).flatMap(_.toLongOption.filter(_ >= 0)))

  private def digestMessage(hash: String, size: Long): Chunk[Byte] =
    stringField(1, hash) ++ numberField(2, size)

  private def digestOf(bytes: Chunk[Byte]): Option[Digest] =
    read(bytes).flatMap(fields => fieldText(fields, 1).flatMap(Digest.from))

  private def foldDigests(bodies: List[Chunk[Byte]]): Option[List[Digest]] =
    bodies.foldLeft[Option[List[Digest]]](Some(Nil)): (acc, body) =>
      acc.flatMap(rest => digestOf(body).map(rest :+ _))

  private def fieldText(fields: List[Field], number: Int): Option[String] =
    fieldBytes(fields, number).map(utf8)

  private def fieldBytes(fields: List[Field], number: Int): Option[Chunk[Byte]] =
    fields.collectFirst { case Field.Bytes(`number`, value) => value }

  private def fieldLong(fields: List[Field], number: Int): Long =
    fields.collectFirst { case Field.Number(`number`, value) => value }.getOrElse(0L)

  private def repeated(fields: List[Field], number: Int): List[Chunk[Byte]] =
    fields.collect { case Field.Bytes(`number`, value) => value }

  private def utf8(bytes: Chunk[Byte]): String =
    new String(bytes.toArray, StandardCharsets.UTF_8)

  private def stringField(field: Int, text: String): Chunk[Byte] =
    if text.isEmpty then Chunk.empty
    else bytesField(field, Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8)))

  private def bytesField(field: Int, payload: Chunk[Byte]): Chunk[Byte] =
    varint(((field << 3) | 2).toLong) ++ varint(payload.length.toLong) ++ payload

  private def messageField(field: Int, payload: Chunk[Byte]): Chunk[Byte] =
    bytesField(field, payload)

  private def numberField(field: Int, value: Long): Chunk[Byte] =
    if value == 0L then Chunk.empty
    else varint((field << 3).toLong) ++ varint(value)

  private def boolField(field: Int, value: Boolean): Chunk[Byte] =
    if value then numberField(field, 1L) else Chunk.empty

  private def varint(value: Long): Chunk[Byte] =
    val out                     = Array.newBuilder[Byte]
    def write(rest: Long): Unit =
      val bits = (rest & 0x7f).toInt
      val next = rest >>> 7
      if next == 0L then out += bits.toByte
      else
        out += (bits | 0x80).toByte
        write(next)
    write(value)
    Chunk.fromArray(out.result())

  private enum Field:
    case Number(field: Int, value: Long)
    case Bytes(field: Int, value: Chunk[Byte])

  private final case class Tag(field: Int, wire: Int, next: Int)
  private final case class Skipped(payload: Chunk[Byte], number: Long, after: Int)

  private def read(bytes: Chunk[Byte]): Option[List[Field]] =
    @tailrec
    def loop(at: Int, acc: List[Field]): Option[List[Field]] =
      if at == bytes.length then Some(acc.reverse)
      else
        tag(bytes, at) match
          case None                         => None
          case Some(Tag(field, wire, next)) =>
            skip(bytes, next, wire) match
              case None                                  => None
              case Some(Skipped(payload, number, after)) =>
                val kept = wire match
                  case 0 => Field.Number(field, number) :: acc
                  case 2 => Field.Bytes(field, payload) :: acc
                  case _ => acc
                loop(after, kept)
    loop(0, Nil)

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
      case 0 =>
        readVarint(bytes, at).map((value, next) => Skipped(Chunk.empty, value, next))
      case 1 =>
        if at + 8 <= bytes.length then Some(Skipped(Chunk.empty, 0L, at + 8)) else None
      case 2 =>
        readVarint(bytes, at).flatMap: (length, next) =>
          if length < 0 || length > Int.MaxValue then None
          else
            val size = length.toInt
            if next + size <= bytes.length then Some(Skipped(bytes.slice(next, next + size), 0L, next + size))
            else None
      case 5 =>
        if at + 4 <= bytes.length then Some(Skipped(Chunk.empty, 0L, at + 4)) else None
      case _ => None

  private def readVarint(bytes: Chunk[Byte], at: Int): Option[(Long, Int)] =
    @tailrec
    def step(index: Int, shift: Int, acc: Long): Option[(Long, Int)] =
      if shift > 63 || index >= bytes.length then None
      else
        val bits  = bytes(index) & 0xff
        val value = acc | ((bits & 0x7f).toLong << shift)
        if (bits & 0x80) == 0 then Some((value, index + 1))
        else step(index + 1, shift + 7, value)
    step(at, 0, 0L)
end Reapi
