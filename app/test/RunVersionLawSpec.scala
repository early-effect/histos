package histos.server

import histos.LawDir
import histos.cli.Cli
import histos.publish.LauncherError

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

/** The run version is the toolchain the shim resolved, so a server started under a `-SNAPSHOT` pin and the next command
  * under that pin agree. Without the shim it is the pin line.
  */
object RunVersionLawSpec extends ZIOSpecDefault:
  private val resolved = "histos 0.3.0-20261112.042525-1"

  def spec = suite("run version laws")(
    test("a server started under a pointer pin records the resolved toolchain, and the next command attaches"):
      for
        root   <- ZIO.service[Path]
        repo   <- LawDir.child(root, "pointer")
        _      <- pin(repo, "0.3.0-SNAPSHOT")
        _      <- TestSystem.putEnv("HISTOS_VERSION", resolved)
        served <- Cli.versionText(repo)
        socket = repo.resolve(".histos").resolve("server.sock")
        _       <- Record.write(repo, served, Fingerprint.trusted("abc"), socket)
        next    <- Cli.versionText(repo)
        decided <- Attach.decide(repo, next)
      yield assertTrue(
        served == resolved,
        next == served,
        decided == Decision.Open(socket.toAbsolutePath.normalize),
      )
    ,
    test("without the shim's text, or with a blank one, the run version is the pin line"):
      for
        root   <- ZIO.service[Path]
        repo   <- LawDir.child(root, "line")
        _      <- pin(repo, "0.3.0-SNAPSHOT")
        absent <- Cli.versionText(repo)
        _      <- TestSystem.putEnv("HISTOS_VERSION", "  ")
        blank  <- Cli.versionText(repo)
      yield assertTrue(absent == "histos 0.3.0-SNAPSHOT", blank == absent)
    ,
    test("a pin that does not parse fails, whatever the shim passed"):
      for
        root   <- ZIO.service[Path]
        repo   <- LawDir.child(root, "bad")
        _      <- pin(repo, "nope")
        _      <- TestSystem.putEnv("HISTOS_VERSION", resolved)
        failed <- Cli.versionText(repo).flip
      yield assertTrue(failed == LauncherError.Version("nope")),
  ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer)

  private def pin(repo: Path, raw: String): UIO[Unit] =
    ZIO.attemptBlocking(Files.writeString(repo.resolve("build.properties"), s"histos.version=$raw\n")).unit.orDie
end RunVersionLawSpec
