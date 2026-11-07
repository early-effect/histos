package histos.bsp

import histos.LawDir
import histos.publish.LauncherPin

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

object ConnectionLawSpec extends ZIOSpecDefault:
  def spec = suite("connection laws")(
    test("here fills the bsp name, protocol version, and scala"):
      val made = Connection.here(List("./histos", "bsp"), "1.0.0-SNAPSHOT", "graph-token")
      assertTrue(
        made.name == "histos",
        made.name == Bsp.ServerName,
        made.argv == List("./histos", "bsp"),
        made.version == "1.0.0-SNAPSHOT",
        made.bspVersion == "2.2.0",
        made.bspVersion == Bsp.BspVersion,
        made.languages == List("scala"),
        made.graph == "graph-token",
      )
    ,
    suite("pin file")(
      test("a missing or bad pin is absent and a snapshot pointer is kept"):
        for
          root      <- ZIO.service[Path]
          missing   <- LawDir.child(root, "missing")
          bad       <- LawDir.child(root, "bad")
          wrong     <- LawDir.child(root, "wrong")
          twice     <- LawDir.child(root, "twice")
          pinned    <- LawDir.child(root, "pinned")
          _         <- write(bad, "not a field\n")
          _         <- write(wrong, "other.version=1.0.0-SNAPSHOT\n")
          _         <- write(twice, "histos.version=1.0.0-SNAPSHOT\nhistos.version=1.0.0-SNAPSHOT\n")
          _         <- write(pinned, "# note\nhistos.version=1.0.0-SNAPSHOT\n")
          absent    <- Connection.version(missing)
          broken    <- Connection.version(bad)
          unknown   <- Connection.version(wrong)
          duplicate <- Connection.version(twice)
          found     <- Connection.version(pinned)
        yield assertTrue(
          absent == None,
          broken == None,
          unknown == None,
          duplicate == None,
          found == Some("1.0.0-SNAPSHOT"),
        )
      ,
      test("launch uses the project wrapper when that file is present"):
        for
          root      <- ZIO.service[Path]
          plain     <- LawDir.child(root, "plain")
          wrapped   <- LawDir.child(root, "wrapped")
          directory <- LawDir.child(root, "directory")
          _         <- ZIO.attemptBlocking {
            Files.writeString(wrapped.resolve("histos"), "")
            Files.createDirectories(directory.resolve("histos"))
          }.orDie
        yield assertTrue(
          Connection.launch(plain) == List("histos", "bsp"),
          Connection.launch(wrapped) == List("./histos", "bsp"),
          Connection.launch(directory) == List("histos", "bsp"),
        ),
    ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer),
  )

  private def write(repo: Path, text: String): UIO[Unit] =
    ZIO.attemptBlocking {
      Files.createDirectories(repo)
      Files.writeString(repo.resolve(LauncherPin.file), text)
      ()
    }.orDie
end ConnectionLawSpec
