package histos.client

import histos.Platform
import histos.plugin.PluginArgs
import histos.server.{Call, RunLaunch}

import zio.test.*

object RunCallSpec extends ZIOSpecDefault:
  def spec = suite("an mcp run")(
    test("omitted arguments are an empty tool overlay and an empty argv") {
      val call = Session.runCall("app", None, PluginArgs.empty)
      assertTrue(call == Right(Call.Run("app", None, RunLaunch.Tool(Map.empty, None), Nil)))
    },
    test("a named variable is the overlay and the directory stays unset") {
      val call = Session.runCall(
        "app",
        Some("jvm"),
        PluginArgs.empty,
        env = Map("TERRET_POLICY" -> "Clamp"),
        argv = List("hello"),
      )
      assertTrue(
        call == Right(
          Call.Run(
            "app",
            Some(Platform.Jvm),
            RunLaunch.Tool(Map("TERRET_POLICY" -> "Clamp"), None),
            List("hello"),
          )
        )
      )
    },
  )
