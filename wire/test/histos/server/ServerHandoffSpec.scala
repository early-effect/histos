package histos.server

import zio.test.*

object ServerHandoffSpec extends ZIOSpecDefault:
  def spec = suite("a server record moves with the process")(
    test("a listening server at this version is kept") {
      assertTrue(ServerHandoff.step(recorded = true, listening = true, same = true) == ServerHandoff.Step.Use)
    },
    test("a listening server at another version is stopped before the next one starts") {
      assertTrue(ServerHandoff.step(recorded = true, listening = true, same = false) == ServerHandoff.Step.Stop)
    },
    test("a record with nobody listening is dropped") {
      assertTrue(
        ServerHandoff.step(recorded = true, listening = false, same = true) == ServerHandoff.Step.Start,
        ServerHandoff.step(recorded = true, listening = false, same = false) == ServerHandoff.Step.Start,
      )
    },
    test("no record starts a server") {
      assertTrue(ServerHandoff.step(recorded = false, listening = false, same = true) == ServerHandoff.Step.Start)
    },
  )
