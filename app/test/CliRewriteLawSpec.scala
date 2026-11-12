package histos.cli

import histos.LawDir
import histos.version.{BumpFault, ShipBumpRun, ShipName}

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

/** `bump` reads the build, edits its text, and writes what the edit returns. A read and a write name their own fault.
  */
object CliRewriteLawSpec extends ZIOSpecDefault:
  def spec = suite("cli rewrite laws")(
    test("an edit that moves a ship writes the text, and one that moves nothing leaves the file"):
      for
        root <- ZIO.service[Path]
        repo <- LawDir.child(root, "moved")
        source = repo.resolve("histos.scala")
        _     <- write(source, "before\n")
        moved <- Cli.rewrite(source)(text => ZIO.succeed(ShipBumpRun.Report("api moved", Some(text + "after\n"))))
        wrote <- read(source)
        kept  <- Cli.rewrite(source)(_ => ZIO.succeed(ShipBumpRun.Report("api is current", None)))
        same  <- read(source)
      yield assertTrue(
        moved == Completed(CliExit.Success, "api moved", "", Playback.Held),
        wrote == "before\nafter\n",
        kept == Completed(CliExit.Success, "api is current", "", Playback.Held),
        same == "before\nafter\n",
      )
    ,
    test("a build it cannot read, a text it cannot write, and an unknown ship each name their own fault"):
      for
        root <- ZIO.service[Path]
        repo <- LawDir.child(root, "faults")
        source = repo.resolve("histos.scala")
        unread    <- Cli.rewrite(source)(_ => ZIO.dieMessage("the edit never runs on a build it cannot read"))
        _         <- write(source, "before\n")
        unwritten <- Cli.rewrite(source)(_ => ZIO.succeed(ShipBumpRun.Report("api moved", Some("\uD800"))))
        kept      <- read(source)
        unknown   <- Cli.rewrite(source)(_ => ZIO.fail(BumpFault.MissingShip(ShipName("nope"))))
      yield assertTrue(
        unread == Completed(CliExit.Failed, "", s"could not read $source", Playback.Held),
        unwritten == Completed(CliExit.Failed, "", s"could not write $source", Playback.Held),
        kept == "before\n",
        unknown == Completed(CliExit.Usage, "", "nope is not a ship in this catalog", Playback.Held),
      ),
  ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer)

  private def write(path: Path, text: String): UIO[Unit] =
    ZIO.attemptBlocking(Files.writeString(path, text)).orDie.unit

  private def read(path: Path): UIO[String] =
    ZIO.attemptBlocking(Files.readString(path)).orDie
end CliRewriteLawSpec
