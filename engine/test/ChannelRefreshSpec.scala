package histos.publish

import java.nio.file.{Files, Path}

import zio.durationInt
import zio.test.{ZIOSpecDefault, assertTrue}

object ChannelRefreshSpec extends ZIOSpecDefault:
  def spec = suite("channel refresh")(
    test("the default ttl is one hour and every call is never fresh"):
      val root = sys.env.get("HISTOS_REPO").fold(Path.of(""))(Path.of(_))
      val shim = Files.readString(root.resolve("install/histos"))
      ChannelRefresh.Default match
        case ChannelRefresh.Ttl(window) =>
          assertTrue(
            window == 1.hour,
            ChannelRefresh.seconds(ChannelRefresh.Default) == 3600,
            ChannelRefresh.Default.fresh(3599.seconds),
            !ChannelRefresh.Default.fresh(3600.seconds),
            !ChannelRefresh.Default.fresh(-1.seconds),
            !ChannelRefresh.EveryCall.fresh(0.seconds),
            shim.contains("channel_ttl=3600"),
            shim.contains("every-call"),
          )
        case ChannelRefresh.EveryCall => assertTrue(false)
  )
