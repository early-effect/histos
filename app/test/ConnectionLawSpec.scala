package histos.bsp

import histos.{LawDir, Resolution}
import histos.publish.LauncherPin
import histos.server.Fingerprint

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path}

import zio.*
import zio.json.*
import zio.test.*

/** `ensure` writes the `.bsp/histos.json` Metals reads, from the pin and the build fingerprint. The pin's own grammar
  * is `LauncherPinTextLawSpec`'s.
  */
object ConnectionLawSpec extends ZIOSpecDefault:
  def spec = suite("connection laws")(
    test("ensure writes the connection file, writes nothing without a pin, and leaves an equal file untouched"):
      for
        root     <- ZIO.service[Path]
        pinned   <- LawDir.child(root, "pinned")
        unpinned <- LawDir.child(root, "unpinned")
        _        <- write(pinned.resolve(LauncherPin.file), "histos.version=1.0.0-SNAPSHOT\n")
        _        <- write(pinned.resolve("histos.scala"), "// law\n")
        _        <- write(unpinned.resolve("histos.scala"), "// law\n")
        file = pinned.resolve(".bsp").resolve("histos.json")
        _           <- Connection.ensure(pinned)
        fingerprint <- Fingerprint.read(pinned, Resolution.Solve)
        written     <- ZIO.attemptBlocking(Files.readString(file)).orDie
        _           <- ZIO.attemptBlocking(Files.setLastModifiedTime(file, FileTime.fromMillis(0))).orDie
        _           <- Connection.ensure(pinned)
        stamped     <- ZIO.attemptBlocking(Files.getLastModifiedTime(file).toMillis).orDie
        _           <- Connection.ensure(unpinned)
        bare        <- ZIO.attemptBlocking(Files.exists(unpinned.resolve(".bsp"))).orDie
      yield assertTrue(
        written.fromJson[Connection] == Right(
          Connection("histos", List("histos", "bsp"), "1.0.0-SNAPSHOT", "2.2.0", List("scala"), fingerprint.value)
        ),
        stamped == 0L,
        !bare,
      )
    ,
    test("a missing or bad pin is absent and a pin line is its version"):
      for
        root    <- ZIO.service[Path]
        missing <- LawDir.child(root, "missing")
        bad     <- LawDir.child(root, "bad")
        pinned  <- LawDir.child(root, "pinned")
        _       <- write(bad.resolve(LauncherPin.file), "not a field\n")
        _       <- write(pinned.resolve(LauncherPin.file), "# note\nhistos.version=1.0.0-SNAPSHOT\n")
        absent  <- Connection.version(missing)
        broken  <- Connection.version(bad)
        found   <- Connection.version(pinned)
      yield assertTrue(absent == None, broken == None, found == Some("1.0.0-SNAPSHOT"))
    ,
    test("launch uses the project wrapper when that file is present"):
      for
        root      <- ZIO.service[Path]
        plain     <- LawDir.child(root, "plain")
        wrapped   <- LawDir.child(root, "wrapped")
        directory <- LawDir.child(root, "directory")
        _         <- write(wrapped.resolve("histos"), "")
        _         <- ZIO.attemptBlocking(Files.createDirectories(directory.resolve("histos"))).orDie
      yield assertTrue(
        Connection.launch(plain) == List("histos", "bsp"),
        Connection.launch(wrapped) == List("./histos", "bsp"),
        Connection.launch(directory) == List("histos", "bsp"),
      ),
  ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer)

  private def write(path: Path, text: String): UIO[Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(Files.createDirectories(_))
      Files.writeString(path, text)
      ()
    }.orDie
end ConnectionLawSpec
