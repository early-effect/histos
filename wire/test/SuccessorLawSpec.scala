package histos.server

import java.nio.file.Path

import zio.test.*

/** A reboot's successor starts as its requester is, and never keeps the identity of the server it replaces. */
object SuccessorLawSpec extends ZIOSpecDefault:
  private val replaced =
    Map("HISTOS_VERSION" -> "histos 0.2.10", "HISTOS_ASSEMBLY" -> "/old/histos.jar", "PATH" -> "/usr/bin")

  def spec = suite("successor laws")(
    test("a successor's environment names the requester's version and jar, and keeps no identity it was not given"):
      assertTrue(
        Successor(Some("histos 0.3.0"), Some(Path.of("/next/histos.jar"))).environment(replaced) ==
          Map("HISTOS_VERSION" -> "histos 0.3.0", "HISTOS_ASSEMBLY" -> "/next/histos.jar", "PATH" -> "/usr/bin"),
        Successor(Some("histos 0.3.0"), None).environment(replaced) ==
          Map("HISTOS_VERSION" -> "histos 0.3.0", "PATH" -> "/usr/bin"),
        Successor(None, None).environment(replaced) == Map("PATH" -> "/usr/bin"),
      )
    ,
    test("this process's identity is its two variables, and a blank one is none"):
      for
        absent <- Successor.here
        _      <- TestSystem.putEnv("HISTOS_VERSION", "histos 0.3.0")
        _      <- TestSystem.putEnv("HISTOS_ASSEMBLY", "/next/histos.jar")
        named  <- Successor.here
        _      <- TestSystem.putEnv("HISTOS_VERSION", " ")
        _      <- TestSystem.putEnv("HISTOS_ASSEMBLY", "")
        blank  <- Successor.here
      yield assertTrue(
        absent == Successor(None, None),
        named == Successor(Some("histos 0.3.0"), Some(Path.of("/next/histos.jar"))),
        blank == Successor(None, None),
      ),
  )
end SuccessorLawSpec
