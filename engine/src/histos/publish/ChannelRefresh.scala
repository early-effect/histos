package histos.publish

import zio.Duration
import zio.durationInt

/** How often a snapshot pin reads `channel-<version>` from the dist base. `EveryCall` is `histos update` and
  * `histos toolchain install`. A normal boot uses [[ChannelRefresh.Default]].
  */
enum ChannelRefresh:
  case EveryCall
  case Ttl(window: Duration)

  def fresh(age: Duration): Boolean = this match
    case EveryCall     => false
    case Ttl(window)   =>
      val millis = age.toMillis
      millis >= 0 && millis < window.toMillis
end ChannelRefresh

object ChannelRefresh:
  val Default: ChannelRefresh = ChannelRefresh.Ttl(1.hour)

  /** Seconds the shim stores as `channel_ttl`. The installer script and this value stay paired. */
  def seconds(strategy: ChannelRefresh): Long = strategy match
    case ChannelRefresh.EveryCall   => 0
    case ChannelRefresh.Ttl(window) => window.toSeconds
end ChannelRefresh
