package histos.publish

import histos.server.ServerFile
import histos.version.ShipNumber
import histos.workspace.Workspace

import java.nio.file.Files

import zio.ZIO
import zio.test.*

import ShimRig.*

/** `histos update` retires a server record an older driver left behind, and the socket file that record names, before
  * it runs the new driver. The client the rig serves records the exec and does not touch either file, and the shim
  * execs that client, so a file that survives is one the new driver was started against.
  */
object ShimRetireLawSpec extends ZIOSpecDefault:
  def spec = suite("shim retire laws")(
    test("update drops a leftover server record before the new driver runs"):
      ZIO.scoped:
        val older = Release(ShipNumber("0.2.0"), clients = true)
        val newer = Release(ShipNumber("0.2.1"), clients = true)
        for
          rig <- loopback
          _   <- rig.publish(Published(List(older, newer), Nil))
          _   <- rig.pin(older.number.render)
          home   = rig.project.resolve(".histos")
          record = home.resolve("server.json")
          socket = home.resolve("server.sock")
          body   = ServerFile.render(
            ServerFile(
              s"histos ${older.number.render}",
              "leftover",
              socket.toString,
              Some("leftover"),
            )
          )
          _      <- Workspace.writeString(record, body).mapError(ShimFault.Io(_))
          _      <- Workspace.writeString(socket, "").mapError(ShimFault.Io(_))
          ran    <- rig.update()
          choice <- rig.choice
          exec   <- rig.executed
          left   <- Workspace.readIfPresent(record).mapError(ShimFault.Io(_))
          dead   <- ZIO.attemptBlocking(!Files.exists(socket)).orElseFail(ShimFault.Stat)
          jar = rig.toolchain(newer.number.render).resolve("histos.jar").toString
        yield assertTrue(
          ran.exit == 0,
          choice == LauncherChoice.Release(newer.number),
          exec.exists: hit =>
            hit.program.startsWith("client-") &&
              hit.version == s"histos ${newer.number.render}" &&
              hit.jar == jar &&
              hit.args == List("version"),
          dead,
          left.isEmpty,
        )
  ) @@ TestAspect.withLiveSystem
