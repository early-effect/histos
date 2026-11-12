package histos.report

import histos.LawDir
import histos.build.{Beat, Build, BuildEvent}
import histos.clean.Products
import histos.server.Feed

import java.nio.file.{Files, Path}

import zio.*
import zio.json.*
import zio.test.*

import WireSchema.given

object ReportLawSpec extends ZIOSpecDefault:
  def spec = suite("report laws")(
    test("a diagnostic without a path is the two required fields"):
      val note = Diagnostic(None, None, None, None, None, Severity.Error, "boom")
      val text = """{"severity":"error","message":"boom"}"""
      assertTrue(text.fromJson[Diagnostic] == Right(note), note.toJson == text)
    ,
    test("the last finished beat is the result and a heard beat is an event"):
      val first = CallResult.reported("first")
      val last  = CallResult.reported("last")
      val beats = Chunk(
        Beat.Finished(first),
        Beat.Heard(BuildEvent.Out("x")),
        Beat.Finished(last),
      )
      assertTrue(
        Build.result(beats) == Some(last),
        Build.result(Chunk(Beat.Heard(BuildEvent.Out("x")))) == None,
        Build.feed(Beat.Heard(BuildEvent.Out("x"))) == Feed.Event(BuildEvent.Out("x")),
        Build.feed(Beat.Finished(last)) == Feed.Done(last),
      )
    ,
    suite("stored tally")(
      test("a tally round-trips and a missing or malformed file is that fault"):
        val tally = Observation(
          Chunk(ModuleTally("api", "3.3.4", "jvm", Some(2), None, Some(4))),
          Some(2),
          None,
          Some(4),
          1,
        )
        for
          root <- ZIO.service[Path]
          repo <- LawDir.child(root, "tally")
          path = Products.observation(repo)
          missing   <- Stored.read(repo).either
          _         <- Stored.keep(repo, Some(tally))
          read      <- Stored.read(repo)
          text      <- ZIO.attemptBlocking(Files.readString(path)).orDie
          _         <- ZIO.attemptBlocking(Files.writeString(path, "{}")).orDie
          malformed <- Stored.read(repo).either
          _         <- Stored.keep(repo, None)
          gone      <- Stored.read(repo).either
        yield assertTrue(
          missing == Left(ObserveError.Missing(path.toString)),
          read == tally,
          text.fromJson[Observation] == Right(tally),
          malformed == Left(ObserveError.Malformed(path.toString)),
          gone == Left(ObserveError.Missing(path.toString)),
        )
      ,
      test("a directory where the tally should be is missing"):
        for
          root <- ZIO.service[Path]
          repo <- LawDir.child(root, "dir")
          path = Products.observation(repo)
          _    <- ZIO.attemptBlocking(Files.createDirectories(path)).orDie
          read <- Stored.read(repo).either
        yield assertTrue(read == Left(ObserveError.Missing(path.toString))),
    ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer),
  )
