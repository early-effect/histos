package histos.publish

import histos.version.ShipNumber

import zio.ZIO
import zio.test.*

import ShimRig.*

/** A command through the installed shim runs the pin's toolchain. The client the manifest names runs batch commands,
  * also when the jar was already cached. `server`, `bsp`, `mcp`, and no arguments stay on `java -jar`.
  */
object ShimLaunchLawSpec extends ZIOSpecDefault:
  def spec = suite("shim launch laws")(
    test("a cached jar fetches the client the manifest names, and that client runs with the toolchain jar"):
      ZIO.scoped:
        for
          rig    <- loopback
          _      <- rig.publish(Published(List(Release(release, clients = true)), Nil))
          _      <- rig.pin(release.render)
          _      <- rig.install(release.render)
          _      <- rig.dropClient(release.render)
          ran    <- rig.launch("compile", "api")
          ran2   <- rig.executed
          origin <- rig.origin(release.render)
          toolJar = rig.toolchain(release.render).resolve("histos.jar").toString
        yield assertTrue(
          ran.exit == 0,
          ran2.exists(_.program.startsWith("client-")),
          ran2.exists(_.jar == toolJar),
          ran2.exists(_.args == List("compile", "api")),
          origin.contains("prebuilt"),
        )
    ,
    test("a manifest that names no client runs java -jar on the toolchain jar"):
      ZIO.scoped:
        for
          rig  <- loopback
          _    <- rig.publish(Published(List(Release(release, clients = false)), Nil))
          _    <- rig.pin(release.render)
          _    <- rig.install(release.render)
          ran  <- rig.launch("compile")
          ran2 <- rig.executed
          toolJar = rig.toolchain(release.render).resolve("histos.jar").toString
        yield assertTrue(
          ran.exit == 0,
          ran2.exists(_.program == "java"),
          ran2.exists(_.args == List("--enable-native-access=ALL-UNNAMED", "-jar", toolJar, "compile")),
        )
    ,
    test("server, bsp, mcp, and no arguments stay on java -jar with a client installed"):
      ZIO.scoped:
        for
          rig <- loopback
          _   <- rig.publish(Published(List(Release(release, clients = true)), Nil))
          _   <- rig.pin(release.render)
          _   <- rig.install(release.render)
          ran <- ZIO.foreach(List(List("server"), List("bsp"), List("mcp"), Nil)): args =>
            rig.launch(args*).zip(rig.executed).map((done, seen) => (args, done.exit, seen.map(_.program)))
        yield assertTrue(ran.forall((_, exit, program) => exit == 0 && program.contains("java")))
    ,
    test("a JDK older than 25 is refused with the floor and the install guide"):
      ZIO.scoped:
        for
          rig <- loopback
          _   <- rig.publish(Published(List(Release(release, clients = false)), Nil))
          _   <- rig.pin(release.render)
          _   <- rig.install(release.render)
          _   <- rig.jdk("21.0.1")
          ran <- rig.launch("compile")
        yield assertTrue(
          ran.exit != 0,
          ran.err == List(
            "histos: java 21.0.1 is older than 25",
            "A JDK 25 or newer is required. Histos does not install it.",
            "macOS: brew install --cask temurin@25",
            "Linux: https://adoptium.net/temurin/releases?version=25",
          ),
        ),
  ) @@ TestAspect.withLiveSystem

  private val release = ShipNumber("0.3.0")
end ShimLaunchLawSpec
