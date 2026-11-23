package histos

/** Where a module's suites run. `Suite` is `histos test`. `Verify` is a named run and a verify job, so a browser
  * suite does not start inside the everyday test command.
  */
enum TestLane:
  case Suite
  case Verify

  def token: String = this match
    case Suite  => "suite"
    case Verify => "verify"
end TestLane
