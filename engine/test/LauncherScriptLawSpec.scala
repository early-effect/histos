package histos.publish

import histos.version.{Catalog, Ship, ShipName, ShipNumber}
import histos.workspace.Workspace
import histos.{
  MainClass,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Publication,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.*

object LauncherScriptLawSpec extends ZIOSpecDefault:
  private val version               = "9.8.7"
  private val app                   = ProjectName("app")
  private val core: Ship.Line       = Ship.Line(ShipName("core"), ShipNumber("9.8.7"), ::(app, Nil))
  private val extra: Ship.Line      = Ship.Line(ShipName("extra"), ShipNumber("9.8.8"), ::(app, Nil))
  private val sameNumber: Ship.Line = Ship.Line(ShipName("extra"), ShipNumber("9.8.7"), ::(app, Nil))
  private val assembly              = Publication.Assembly(MainClass("app.Main"))

  def spec = suite("launcher script laws")(
    test("the script is the posix body with the ship number written once"):
      val script = LauncherScript.posix(version)
      assertTrue(
        script.startsWith("#!/bin/sh\n"),
        occurrences(script, version) == 1,
        !script.contains("__VERSION__"),
        script.contains("Histos runs on macOS and Linux. On Windows, use WSL."),
        script.contains("home=${HISTOS_HOME:-$HOME/.histos}"),
        script.contains("exec \"$shim\" \"$@\""),
        !script.contains("HISTOS_ASSEMBLY"),
        !script.contains("assembly.sha256"),
        !script.contains("java_major"),
      )
    ,
    test("no assembly is a no-op, a split catalog is a revision, and the pin is read before the script"):
      val program =
        Workspace
          .work("histos-launcher")
          .flatMap: repo =>
            val script  = repo.resolve("histos")
            val pin     = repo.resolve(LauncherPin.file)
            val library = graph(Publication.Library, List(core, extra))
            val empty   = graph(assembly, Nil)
            val split   = graph(assembly, List(core, extra))
            val same    = graph(assembly, List(core, sameNumber))
            val one     = graph(assembly, List(core))
            val body    = LauncherScript.posix(version)
            for
              skipped <- LauncherScript.check(repo, library)
              _       <- LauncherScript.update(repo, library)
              absent  <- ZIO.attemptBlocking(Files.exists(script))
              revised <- LauncherScript.check(repo, empty).either
              _       <- ZIO.attemptBlocking:
                Files.writeString(pin, s"histos.version=$version\n")
                Files.writeString(script, body)
              splitCheck <- LauncherScript.check(repo, split).either
              _          <- ZIO.attemptBlocking(Files.deleteIfExists(script))
              duplicated <- LauncherScript.check(repo, same).either
              _          <- ZIO.attemptBlocking(Files.deleteIfExists(pin))
              _          <- ZIO.attemptBlocking(Files.writeString(script, body))
              noPin      <- LauncherScript.check(repo, one).either
              _          <- ZIO.attemptBlocking(Files.writeString(pin, "\n"))
              noKey      <- LauncherScript.check(repo, one).either
              _          <- ZIO.attemptBlocking(Files.writeString(pin, s"histos.version=$version\n"))
              matched    <- LauncherScript.check(repo, one).either
              _          <- ZIO.attemptBlocking(Files.writeString(script, "drift\n"))
              drifted    <- LauncherScript.check(repo, one).either
              _          <- ZIO.attemptBlocking(Files.deleteIfExists(script))
              missing    <- LauncherScript.check(repo, one).either
              _          <- LauncherScript.update(repo, one)
              checked    <- LauncherScript.check(repo, one).either
              first      <- ZIO.attemptBlocking(Files.readString(script))
              _          <- LauncherScript.update(repo, one)
              second     <- ZIO.attemptBlocking(Files.readString(script))
              executable <- ZIO.attemptBlocking(Files.isExecutable(script))
            yield assertTrue(
              skipped == (),
              !absent,
              revised == Left(ScriptError.Revision),
              splitCheck == Left(ScriptError.Revision),
              duplicated == Left(ScriptError.Missing(script)),
              noPin == Left(ScriptError.Pin(LauncherError.Absent(pin))),
              noKey == Left(ScriptError.Pin(LauncherError.Missing)),
              matched == Right(()),
              drifted == Left(ScriptError.Drift(script)),
              missing == Left(ScriptError.Missing(script)),
              checked == Right(()),
              first == body,
              second == body,
              executable,
            )
      program,
  )

  private def graph(publication: Publication, ships: List[Ship.Line]): ProjectGraph =
    val catalog = ships match
      case Nil          => Catalog.Empty
      case head :: tail => Catalog.Lines(::(head, tail))
    ProjectGraph.trusted(List(module(publication)), Splits.aggregate, catalog)

  private def module(publication: Publication): Module =
    Module(
      app,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion("3.7.0"), Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
      publication = publication,
    )

  private def occurrences(text: String, needle: String): Int =
    LazyList
      .unfold(0): from =>
        val at = text.indexOf(needle, from)
        if at < 0 then None else Some(at -> (at + needle.length))
      .size
end LauncherScriptLawSpec
