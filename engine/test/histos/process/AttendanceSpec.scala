package histos.process

import zio.Exit
import zio.test.*

object AttendanceSpec extends ZIOSpecDefault:
  def spec = suite("attendance")(
    test("a descendant that still holds the pipe is an orphan") {
      for outcome <- Attendance.settle(ProcessExit(0)).exit
      yield assertTrue(outcome == Exit.fail(ChildError.Orphan(0)))
    }.provide(Attendance.held),
    test("a closed pipe is a clean exit") {
      for event <- Attendance.settle(ProcessExit(0))
      yield assertTrue(event == ChildEvent.Exited(ProcessExit(0)))
    }.provide(Attendance.clear),
  )
