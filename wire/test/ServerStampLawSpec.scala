package histos.server

import java.nio.file.attribute.FileTime

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A record written at the stamp noted before spawn is the previous server. */
object ServerStampLawSpec extends ZIOSpecDefault:
  def spec = suite("server stamp laws")(
    test("a stamp that is not strictly newer is not a new server"):
      val prior = FileTime.fromMillis(1_000)
      val same  = FileTime.fromMillis(1_000)
      val later = FileTime.fromMillis(1_001)
      assertTrue(
        !ServerStamp.writtenAfter(Some(prior), None),
        !ServerStamp.writtenAfter(None, None),
        ServerStamp.writtenAfter(None, Some(later)),
        !ServerStamp.writtenAfter(Some(prior), Some(same)),
        ServerStamp.writtenAfter(Some(prior), Some(later)),
      )
  )
