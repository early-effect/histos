package histos.build

import histos.testrun.TestFeed
import histos.theme.Role
import histos.zinc.CompileFeed

/** One step of a running call. Compile and test feeds stay typed. `Out` and `Err` are lines that are not those feeds.
  */
enum BuildEvent:
  /** The module column for this call. Printers pad names to it. The event itself prints nothing. */
  case Layout(column: Int)

  /** The theme for this call. Printers remember it. The event itself prints nothing. */
  case Theme(value: histos.theme.Theme)

  case Compile(feed: CompileFeed)
  case Test(feed: TestFeed)
  case Out(text: String)
  case Err(text: String)

  /** A tally or another line whose whole text has one role. */
  case Status(text: String, role: Role)

  /** A lock fact on the build feed. The text is the summary. It is not a prompt. */
  case Lock(event: histos.lock.LockEvent)
end BuildEvent
