package histos.jdk

import zio.test.*

object Sha256DigestLawSpec extends ZIOSpecDefault:
  def spec = suite("sha256 digest laws")(
    test("every digest's text parses back to that digest"):
      check(Gen.chunkOf(Gen.byte)): bytes =>
        val digest = Sha256Digest.of(bytes)
        assertTrue(Sha256.from(digest.value) == Right(digest))
  )
