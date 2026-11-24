package histos.client

import histos.build.BuildEvent
import histos.cli.{Appearance, Show}
import histos.server.Attendance
import histos.theme.Role
import histos.zinc.{CompileFeed, CompiledCell}
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path

import scala.scalanative.posix.unistd

import zio.json.*
import zio.json.ast.Json
import zio.{Console, IO, UIO, ZIO, ZIOAppDefault}

/** Runs the actual Native frontend renderer without a daemon or compiler launch. */
object RunStreamsFixture extends ZIOAppDefault:
  private enum FixtureError:
    case Input, JsonInput, Output

  def run = program.foldZIO(
    error => Console.printLineError(error.toString).ignore *> exit(1),
    _ => exit(0),
  )

  private def program: IO[FixtureError, Unit] =
    val repo    = Path.of(".")
    val module  = ProjectName("fixture")
    val version = ScalaVersion("3.10.1")
    val events  = List(
      BuildEvent.Out("run 12345678-1234-1234-1234-123456789abc"),
      BuildEvent.Compile(CompileFeed.Began(module, version, Platform.Jvm)),
      BuildEvent.Compile(
        CompileFeed.Done(CompiledCell(module, version, Platform.Jvm, repo, 1, None, false, None, None))
      ),
      BuildEvent.Status("fixture progress", Role.Pulse),
      BuildEvent.Err("fixture stderr"),
    )
    for
      line <- Console.readLine.mapError(_ => FixtureError.Input)
      _    <- ZIO.fromEither(line.fromJson[Json]).mapError(_ => FixtureError.JsonInput)
      _    <- ZIO.foreachDiscard(events)(event =>
        ZIO.succeed(Main.show(repo, event, Show.start, Appearance.plain, Attendance.Program))
      )
      _ <- ZIO
        .attemptBlocking {
          System.out.write((line + "\n").getBytes(UTF_8))
          System.out.flush()
        }
        .mapError(_ => FixtureError.Output)
    yield ()

  private def exit(code: Int): UIO[Unit] =
    ZIO.succeed {
      System.out.flush()
      System.err.flush()
      unistd._exit(code)
    }
end RunStreamsFixture
