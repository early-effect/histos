package histos.client

import histos.{LawDir, LawMachine, LawPlugins}
import histos.build.BuildEvent
import histos.cache.ContentStore
import histos.load.{LoadedBuild, Loader}
import histos.publish.{IvyRoot, LauncherError, LauncherPin}
import histos.report.{BuildFault, CallResult, Report}
import histos.resolve.Resolve
import histos.server.{Call, Feed, Fingerprint, Record, ServerError}
import histos.{HistosBuild, ProjectGraph, Resolution}
import slayer.stubbed

import java.nio.file.{Files, Path}

import zio.*
import zio.test.*

object SessionFrameLawSpec extends ZIOSpecDefault:
  private val jar = Path.of("api.jar")

  def spec = suite("session frame laws")(
    test("the last done frame is the result and an earlier fault is not"):
      val fault  = CallResult.Fault(BuildFault.Interrupted)
      val report = CallResult.reported()
      val feeds  = List(
        Feed.Done(fault),
        Feed.Event(BuildEvent.Out("later")),
        Feed.Event(BuildEvent.Err("boom")),
        Feed.Done(report),
      )
      assertTrue(
        Session.spoken(Path.of("repo"), feeds) == Right(
          CallResult.Reported(Report.empty.copy(out = Chunk("later"), err = Chunk("boom")))
        )
      )
    ,
    suite("refused before the call")(
      test("a missing pin makes version a launcher fault"):
        for
          root   <- ZIO.service[Path]
          repo   <- LawDir.child(root, "absent")
          desk   <- Session.desk
          result <- Session.perform(repo, jar, LawMachine.idle, desk, Call.Version)
        yield assertTrue(
          result == CallResult.Fault(BuildFault.Launcher(LauncherError.Absent(repo.resolve(LauncherPin.file))))
        )
      ,
      test("a bad pin line refuses compile and keeps the held graph"):
        for
          root   <- ZIO.service[Path]
          repo   <- LawDir.child(root, "bad-pin")
          scope  <- Scope.make
          _      <- ZIO.attemptBlocking(Files.writeString(repo.resolve(LauncherPin.file), "nope\n")).orDie
          desk   <- Session.desk
          _      <- hold(repo, desk, scope)
          before <- desk.held.get
          result <- Session.perform(repo, jar, LawMachine.idle, desk, Call.Compile(Nil))
          after  <- desk.held.get
          _      <- scope.close(Exit.unit)
        yield assertTrue(
          result == CallResult.Fault(BuildFault.Launcher(LauncherError.Line("nope"))),
          before.nonEmpty,
          after == before,
        )
      ,
      // Attach compares the printed `histos <pin>` line with the record, not the pin alone.
      test("a different server version drops the held graph"):
        for
          root   <- ZIO.service[Path]
          repo   <- LawDir.child(root, "skew")
          scope  <- Scope.make
          closed <- Ref.make(false)
          _      <- scope.addFinalizer(closed.set(true))
          _      <- ZIO
            .attemptBlocking(Files.writeString(repo.resolve(LauncherPin.file), "histos.version=9.8.7\n"))
            .orDie
          fingerprint <- ready(Fingerprint.read(repo, Resolution.Solve))
          _           <- ready(Record.write(repo, "9.8.7", fingerprint, repo.resolve("server.sock"), None))
          desk        <- Session.desk
          _           <- hold(repo, desk, scope)
          result      <- Session.perform(repo, jar, LawMachine.idle, desk, Call.Version).either
          held        <- desk.held.get
          gone        <- closed.get
          said        <- TestConsole.outputErr
        yield assertTrue(
          result == Left(
            Session.TransportError.Remote(ServerError.Restart(repo.resolve("server.sock"), None, None))
          ),
          held.isEmpty,
          gone,
          said == Vector(s"rebooting server onto none (was none) at ${repo.resolve("server.sock")}\n"),
        ),
    ).provideSomeLayerShared[TestEnvironment & Scope](
      LawDir.layer ++ stubbed[Loader] ++ stubbed[Resolve] ++ stubbed[ContentStore] ++
        ZLayer.succeed(IvyRoot.at(Path.of("ivy-never-written")))
    ),
  )

  private def hold(repo: Path, desk: Session.Desk, scope: Scope.Closeable): UIO[Unit] =
    val empty = ProjectGraph.trusted(Nil)
    ready(Fingerprint.read(repo, Resolution.Solve)).flatMap { fingerprint =>
      desk.held.set(
        Some(Session.Held(fingerprint, LoadedBuild(empty, LawPlugins.none, HistosBuild.fixed(empty)), scope))
      )
    }

  /** A fixture that cannot be written is a defect. The law's error channel stays the call. */
  private def ready[A](effect: IO[ServerError, A]): UIO[A] =
    effect.foldZIO(error => ZIO.die(new IllegalStateException(error.message)), ZIO.succeed)
end SessionFrameLawSpec
