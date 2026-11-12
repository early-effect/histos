package histos.schedule

import zio.Chunk
import zio.test.{Gen, ZIOSpecDefault, assertTrue, check, suite, test}

object ReapiLawSpec extends ZIOSpecDefault:
  private val action = Digest.trusted("ab" * 32)
  private val other  = Digest.trusted("cd" * 32)
  private val bytes  = Chunk[Byte](1, 2, 3)
  private val wrong  = Chunk[Byte](9, 8)

  private val instances = Gen.stringBounded(1, 300)(Gen.unicodeChar)
  private val digests   = Gen.stringN(64)(Gen.elements("0123456789abcdef".toSeq*)).map(Digest.trusted)
  private val payloads  = Gen.chunkOfBounded(1, 300)(Gen.byte)

  def spec = suite("remote execution messages")(
    test("every request and response round-trips for any instance, digest, size, and payload"):
      check(instances, digests, payloads, Gen.long(0L, Long.MaxValue), Gen.listOf(digests)):
        (instance, digest, payload, size, listed) =>
          assertTrue(
            Reapi.decodeGet(Reapi.getAction(instance, digest)) == Some((instance, digest)),
            Reapi.decodeUpdate(Reapi.updateAction(instance, digest, payload)) == Some((instance, digest, payload)),
            Reapi.decodeFind(Reapi.findMissing(instance, digest, size)) == Some((instance, List(digest))),
            Reapi.missingDigests(Reapi.missingResponse(listed)) == Some(listed),
            Reapi.decodeRead(Reapi.readRequest(instance)) == Some(instance),
            Reapi.readData(Reapi.readResponse(payload) ++ Reapi.readResponse(payload)) == Some(payload ++ payload),
            Reapi.decodeWrite(Reapi.writeFrame(Some(instance), size, true, payload)) ==
              Some(Reapi.WriteFrame(Some(instance), size, true, payload)),
          )
    ,
    test("frames carry the resource once, count the offset, finish last, and join back to the payload"):
      val sizes = Gen.int(0, 3 * Reapi.WriteChunk + 1)
      check(instances, sizes, Gen.byte): (resource, length, byte) =>
        val payload = Chunk.fill(length)(byte)
        val frames  = Reapi.frames(resource, payload).map(Reapi.decodeWrite).flatten
        val offsets = frames.map(_.data.length.toLong).scanLeft(0L)(_ + _).dropRight(1)
        assertTrue(
          frames.length == math.max(1, (length + Reapi.WriteChunk - 1) / Reapi.WriteChunk),
          frames.map(_.resource) == Chunk(Some(resource)) ++ Chunk.fill(frames.length - 1)(None),
          frames.map(_.offset) == offsets,
          frames.map(_.finish) == Chunk.fill(frames.length - 1)(false) :+ true,
          frames.flatMap(_.data) == payload,
          frames.forall(_.data.length <= Reapi.WriteChunk),
        )
    ,
    test("the service names are the wire contract, and an empty field is omitted"):
      val resource = "main/uploads/job"
      val empty    = Digest.trusted("zz")
      assertTrue(
        Reapi.ActionKeySize == 1L,
        Reapi.WriteChunk == 64 * 1024,
        Reapi.ActionCache == "build.bazel.remote.execution.v2.ActionCache",
        Reapi.ContentAddressable == "build.bazel.remote.execution.v2.ContentAddressableStorage",
        Reapi.ByteStream == "google.bytestream.ByteStream",
        Reapi.GetActionResult == "build.bazel.remote.execution.v2.ActionCache/GetActionResult",
        Reapi.UpdateActionResult == "build.bazel.remote.execution.v2.ActionCache/UpdateActionResult",
        Reapi.FindMissingBlobs == "build.bazel.remote.execution.v2.ContentAddressableStorage/FindMissingBlobs",
        Reapi.Read == "google.bytestream.ByteStream/Read",
        Reapi.Write == "google.bytestream.ByteStream/Write",
        Reapi.decodeGet(Reapi.getAction("", action)) == None,
        Reapi.decodeGet(Reapi.getAction("main", empty)) == None,

        Reapi.findMissing("main", action, 0L).length < Reapi.findMissing("main", action, 4L).length,
        Reapi.decodeFind(Reapi.findMissing("main", empty, 4L)) == None,
        Reapi.missingDigests(Reapi.missingResponse(List(empty))) == None,
        Reapi.decodeRead(Reapi.readRequest("")) == None,
        Reapi.readData(Reapi.readResponse(bytes) ++ Reapi.readResponse(wrong)) == Some(bytes ++ wrong),
        Reapi.readData(Chunk.empty) == Some(Chunk.empty),
        Reapi.writeResponse(0L) == Chunk.empty,
        Reapi.writeResponse(3L) == Chunk(8.toByte, 3.toByte),
        Reapi.writeResponse(128L) == Chunk(8.toByte, 0x80.toByte, 1.toByte),
        Reapi.decodeWrite(Reapi.writeFrame(Some(resource), 0L, false, Chunk.empty)) ==
          Some(Reapi.WriteFrame(Some(resource), 0L, false, Chunk.empty)),
        Reapi.decodeWrite(Reapi.writeFrame(None, 5L, true, bytes)) ==
          Some(Reapi.WriteFrame(None, 5L, true, bytes)),
        Reapi.decodeWrite(Chunk.empty) == Some(Reapi.WriteFrame(None, 0L, false, Chunk.empty)),
      )
    ,
    test("a blob path is the hash and size after the first blobs segment"):
      val read  = s"main/blobs/${action.value}/3"
      val write = s"main/uploads/${action.value}/blobs/${action.value}/0"
      assertTrue(
        Reapi.readResource("main", action, 3L) == read,
        Reapi.writeResource("main", action, 0L) == write,
        Reapi.blob(read) == Some((action.value, 3L)),
        Reapi.blob(write) == Some((action.value, 0L)),
        Reapi.blob(s"blobs/${action.value}/1/blobs/${other.value}/2") == Some((action.value, 1L)),
        Reapi.blob("main/blobs") == None,
        Reapi.blob(s"blobs/${action.value}/-1") == None,
        Reapi.blob(s"blobs/${action.value}/n") == None,
        Reapi.blob("no-blobs") == None,
      )
    ,
    test("a fixed-width field is skipped, and a truncated, overlong, or illegal frame is refused"):
      val valid = Reapi.getAction("main", action)
      val kept  = Some(("main", action))
      assertTrue(
        Reapi.decodeGet(flat(valid ++ fixed(0x21, 8))) == kept,
        Reapi.decodeGet(flat(valid ++ fixed(0x25, 4))) == kept,
        Reapi.decodeGet(valid ++ Chunk(0x27.toByte)) == None,
        Reapi.decodeGet(Chunk(0x21.toByte, 0.toByte, 0.toByte)) == None,
        Reapi.decodeGet(Chunk(0x25.toByte, 0.toByte)) == None,
        Reapi.decodeGet(Chunk(0x0a.toByte, 0x05.toByte, 0x01.toByte)) == None,
        Reapi.decodeGet(Chunk(0.toByte)) == None,
        Reapi.decodeGet(Chunk(0x02.toByte)) == None,
        Reapi.decodeGet(Chunk.fill(10)(0x80.toByte)) == None,
        Reapi.decodeGet(Chunk(0x80.toByte)) == None,
        Reapi.decodeGet(negativeLength) == None,
        Reapi.decodeGet(oversizedLength) == None,
        Reapi.decodeGet(oversizedTag) == None,
      ),
  )

  private def fixed(tag: Int, width: Int): Chunk[Byte] =
    Chunk(tag.toByte) ++ Chunk.fill(width)(0.toByte)

  /** One buffer. A nested `++` makes `Chunk.toArray` throw, and a network frame is one buffer. */
  private def flat(bytes: Chunk[Byte]): Chunk[Byte] =
    val out = new Array[Byte](bytes.length)
    var i   = 0
    bytes.foreach: byte =>
      out(i) = byte
      i += 1
    Chunk.fromArray(out)

  private val negativeLength: Chunk[Byte] =
    Chunk(0x0a.toByte) ++ Chunk.fill(9)(0x80.toByte) ++ Chunk(0x01.toByte)

  private val oversizedLength: Chunk[Byte] =
    Chunk(0x0a.toByte, 0x80.toByte, 0x80.toByte, 0x80.toByte, 0x80.toByte, 0x08.toByte)

  private val oversizedTag: Chunk[Byte] =
    Chunk(0x80.toByte, 0x80.toByte, 0x80.toByte, 0x80.toByte, 0x08.toByte)
end ReapiLawSpec
