package histos.jdk

import zio.Chunk
import zio.test.*

object Sha256DigestLawSpec extends ZIOSpecDefault:
  def spec = suite("sha256 digest laws")(
    test("the empty digest is the published constant, and every digest is a sha256 value"):
      check(Gen.chunkOf(Gen.byte)): bytes =>
        val digest = Sha256Digest.of(bytes)
        assertTrue(
          Sha256Digest.of(Chunk.empty).value == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
          Sha256.from(digest.value) == Right(digest),
          Sha256Digest.of(bytes :+ 0.toByte) != digest,
        )
  )
