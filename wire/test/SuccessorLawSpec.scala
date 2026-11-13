package histos.server

import zio.test.*

/** A reboot's successor starts as its requester is, and drops an inherited assembly variable. */
object SuccessorLawSpec extends ZIOSpecDefault:
  private val replaced =
    Map("HISTOS_VERSION" -> "histos 0.2.10", "HISTOS_ASSEMBLY" -> "/old/histos.jar", "PATH" -> "/usr/bin")

  def spec = suite("successor laws")(
    test("a successor's environment names the requester's version, drops an inherited assembly, and keeps the rest"):
      assertTrue(
        Successor(Some("histos 0.3.0")).environment(replaced) ==
          Map("HISTOS_VERSION" -> "histos 0.3.0", "PATH" -> "/usr/bin"),
        Successor(None).environment(replaced) == Map("PATH" -> "/usr/bin"),
      )
    ,
    test("this process's version is HISTOS_VERSION, and a blank one is none"):
      for
        absent <- Successor.here
        _      <- TestSystem.putEnv("HISTOS_VERSION", "histos 0.3.0")
        _      <- TestSystem.putEnv("HISTOS_ASSEMBLY", "/next/histos.jar")
        named  <- Successor.here
        _      <- TestSystem.putEnv("HISTOS_VERSION", " ")
        blank  <- Successor.here
      yield assertTrue(
        absent == Successor(None),
        named == Successor(Some("histos 0.3.0")),
        blank == Successor(None),
      ),
  )
end SuccessorLawSpec
