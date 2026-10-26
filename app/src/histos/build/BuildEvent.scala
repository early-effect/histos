package histos.build

import histos.testrun.TestFeed
import histos.zinc.CompileFeed

/** One step of a running call. Compile and test feeds stay typed. `Out` and `Err` are lines that are not those feeds.
  */
enum BuildEvent:
  case Compile(feed: CompileFeed)
  case Test(feed: TestFeed)
  case Out(text: String)
  case Err(text: String)
