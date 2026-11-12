package histos.server

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.test.*

/** A checkout's socket lives under the Histos home, named for the checkout, and fits a Unix socket path at any depth.
  */
object SocketPathLawSpec extends ZIOSpecDefault:
  private val home = Path.of("/Users/somebody/.histos")
  private val deep = Path.of("/work/" + ("d" * 60 + "/") * 4 + "repo")

  def spec = suite("socket path laws")(
    test("any checkout depth fits the 104 bytes macOS allows, and the name is the checkout's"):
      val socket = SocketPath.of(deep, home)
      assertTrue(
        deep.toString.length > 200,
        socket.toString.getBytes(StandardCharsets.UTF_8).length < 104,
        socket.getParent == home.resolve("run"),
        socket.getFileName.toString.matches("[0-9a-f]{16}\\.sock"),
        SocketPath.of(deep, home) == socket,
        SocketPath.of(deep.resolve("x").resolve(".."), home) == socket,
        SocketPath.of(deep.resolveSibling("other"), home) != socket,
      )
    ,
    test("the JVM server and the native client name a checkout's socket the same, FNV-1a 64 of its path"):
      assertTrue(
        SocketPath.of(Path.of("/work/repo"), home) == home.resolve("run").resolve("1cd4fc81c8742440.sock")
      )
    ,
    test("the home is HISTOS_HOME, or .histos under the user home"):
      for
        _      <- TestSystem.putProperty("user.home", "/Users/somebody")
        absent <- SocketPath.home
        _      <- TestSystem.putEnv("HISTOS_HOME", "/opt/histos")
        set    <- SocketPath.home
        _      <- TestSystem.putEnv("HISTOS_HOME", "  ")
        blank  <- SocketPath.home
      yield assertTrue(absent == home, set == Path.of("/opt/histos"), blank == home),
  )
end SocketPathLawSpec
