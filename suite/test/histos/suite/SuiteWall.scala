package histos.suite

import zio.*
import zio.test.*

import java.time.Instant

object SuiteWall:
  def apply(label: String): TestAspectPoly =
    TestAspect.aroundAllWith(now)(start => report(label, start))

  def line(label: String, elapsed: Duration): String =
    s"suite $label ${elapsed.render}"

  def report(label: String, start: Instant): UIO[Unit] =
    now.flatMap: end =>
      Console
        .printLine(line(label, Duration.fromInterval(start, end)))
        .withConsole(Console.ConsoleLive)
        .orDie

  private def now: UIO[Instant] =
    Clock.instant.withClock(Clock.ClockLive)
end SuiteWall
