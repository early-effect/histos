package histos.resolve

import histos.Platform

/** Which classpath a lock cell is. Compile is published. Test is not. */
enum ClasspathScope:
  case Compile
  case Test

  def header: String = this match
    case Compile => "cell"
    case Test    => "test"
end ClasspathScope

object PlatformToken:
  def render(platform: Platform): String = platform.token

  def parse(raw: String): Option[Platform] = Platform.from(raw).toOption
end PlatformToken
