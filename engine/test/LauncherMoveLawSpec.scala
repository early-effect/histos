package histos.publish

import histos.workspace.Workspace

import java.nio.file.Files

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}
import zio.ZIO

/** A snapshot pin moves to its channel. A release pin stays, and a missing file is absent. */
object LauncherMoveLawSpec extends ZIOSpecDefault:
  def spec = suite("launcher pin laws")(
    test("latest rewrites a timestamp as its snapshot channel, and a release pin does not move"):
      Workspace.work("histos-launcher-pin").flatMap: dir =>
        val path = dir.resolve(LauncherPin.file)
        val revision = "0.1.0-20260101.120000-1"
        for
          absent <- LauncherPin.read(dir).either
          _      <- Workspace.writeString(path, s"histos.version=$revision\n")
          moved  <- LauncherPin.latest(dir)
          text   <- ZIO.attempt(Files.readString(path)).orDie
          _      <- Workspace.writeString(path, "histos.version=1.2.3\n")
          held   <- LauncherPin.update(dir).either
          kept   <- ZIO.attempt(Files.readString(path)).orDie
        yield assertTrue(
          absent == Left(LauncherError.Absent(path)),
          moved.version == "0.1.0-SNAPSHOT",
          moved.channel == "0.1.0-SNAPSHOT",
          text == "histos.version=0.1.0-SNAPSHOT\n",
          held == Left(LauncherError.Release("1.2.3")),
          kept == "histos.version=1.2.3\n",
        )
  )
end LauncherMoveLawSpec
