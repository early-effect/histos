package histos.publish

import zio.Duration
import zio.test.*

object ChannelLawSpec extends ZIOSpecDefault:
  def spec = suite("channel refresh")(
    test("a ttl is fresh strictly inside its window, and every call is never fresh"):
      check(Gen.long(-1_000L, 3_700_000L)): millis =>
        val age = Duration.fromMillis(millis)
        assertTrue(
          ChannelRefresh.Default.fresh(age) == (millis >= 0 && millis < 3_600_000L),
          !ChannelRefresh.EveryCall.fresh(age),
          ChannelRefresh.seconds(ChannelRefresh.Default) == 3600L,
          ChannelRefresh.seconds(ChannelRefresh.EveryCall) == 0L,
        )
  )
