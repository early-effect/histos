package publication.consumer

import histos.*

import _root_.scala.compiletime.testing.typeChecks

import zio.test.*

object MetadataUriSpec extends ZIOSpecDefault:
  def spec = suite("MetadataUri")(
    test("metadata literals preserve HTTP and HTTPS without granting a network HttpUrl") {
      val http  = MetadataUri("http://www.apache.org/licenses/LICENSE-2.0.txt")
      val https = MetadataUri("https://www.earlyeffect.rocks")
      assertTrue(
        http.value == "http://www.apache.org/licenses/LICENSE-2.0.txt",
        https.value == "https://www.earlyeffect.rocks",
        !typeChecks("histos.MetadataUri(\"ftp://example.com/license\")"),
        !typeChecks("histos.MetadataUri(\"https:///missing-host\")"),
        !typeChecks("histos.MetadataUri(\"https://example.com/%GG\")"),
        !typeChecks("histos.MetadataUri(\"relative/path\")"),
        !typeChecks("histos.HttpUrl(\"http://www.apache.org/licenses/LICENSE-2.0.txt\")"),
        !typeChecks("val url: histos.HttpUrl = histos.MetadataUri(\"https://example.com\")"),
      )
    },
    test("runtime parsing returns the typed error for malformed or non-HTTP metadata") {
      val invalid = List(
        "",
        "relative/path",
        "file:///tmp/license",
        "ftp://example.com/license",
        "https://",
        "https:///path",
        "https://example.com/%GG",
        "https://example.com:bad/path",
        "https://example.com:65536/path",
        "https://example.com/space here",
        "https://example.com/\u0000",
        "https://[broken/path",
      )
      assertTrue(
        invalid.forall(raw => MetadataUri.from(raw) == Left(MetadataUriError.Bad(raw))),
        MetadataUri.from("http://www.apache.org/licenses/LICENSE-2.0.txt").map(_.value) == Right(
          "http://www.apache.org/licenses/LICENSE-2.0.txt"
        ),
        MetadataUri.from("https://example.com/path?q=a%20b#fragment").map(_.value) == Right(
          "https://example.com/path?q=a%20b#fragment"
        ),
      )
    },
  )
