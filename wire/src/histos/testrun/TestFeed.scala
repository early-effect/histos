package histos.testrun

import histos.{Platform, ProjectName, ScalaVersion}

final case class TestedCell(
    module: ProjectName,
    version: ScalaVersion,
    platform: Platform,
    status: TestStatus,
)

/** What one cell did. `NoTests` means there was nothing to run. It is not a passing suite. The output is not here: the
  * console streams it once.
  */
enum TestStatus:
  case NoTests
  case Unmatched
  case Passed
  case Failed(exit: Int)

  def label: String = this match
    case NoTests      => "no tests"
    case Unmatched    => "unmatched"
    case Passed       => "passed"
    case Failed(exit) => s"failed $exit"
end TestStatus

/** What the test console is doing. A cell's lines carry that cell, so two modules can share the screen.
  */
enum TestFeed:
  case Begin(module: ProjectName, version: ScalaVersion, platform: Platform)
  case Out(module: ProjectName, version: ScalaVersion, platform: Platform, text: String)
  case Err(module: ProjectName, version: ScalaVersion, platform: Platform, text: String)
  case Done(cell: TestedCell)
  case Cached(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      suite: String,
  )
