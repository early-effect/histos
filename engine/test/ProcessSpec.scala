package histos.program

import histos.pack.MainClass
import histos.process.{
  ChildClasspath,
  ChildError,
  ChildEvent,
  ChildRequest,
  GroupOp,
  Process,
  ProcessExit,
  ProcessOutput,
}
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.CompiledCell
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

import slayer.{Stubbed, any, stub, stubbedTraced}

import zio.{Chunk, Promise, URIO, ZIO}
import zio.stream.ZStream
import zio.test.*

object ProcessSpec extends ZIOSpecDefault:
  private val cell =
    CompiledCell(
      ProjectName("core"),
      ScalaVersion("3.0.0"),
      Platform.Jvm,
      Path.of("classes"),
      0,
      None,
      false,
      None,
      None,
    )

  private val request =
    ChildRequest(
      Platform.Jvm,
      Path.of("java"),
      ChildClasspath(cell.classes, Nil),
      MainClass.trusted("demo.Main"),
      Path.of("work"),
    )

  def spec =
    suite("process")(
      ran,
      linked,
      cancelled,
      failed,
    )

  private def ran =
    test("stdout and the exit come from the process"):
      reports(
        spoken => stub[Process](_.events(any[ChildRequest]))(spoken),
        Program.events(cell, request),
        "events",
      )

  private def linked =
    test("a linked program reports the process stdout and exit"):
      reports(
        spoken => stub[Process](_.command(any[List[String]], any[Path], any[Map[String, String]]))(spoken),
        Program.command(cell, List("program"), request.work, request.environment),
        "command",
      )

  private def cancelled =
    test("cancelling a run records the process call"):
      traced {
        for
          started <- Promise.make[Nothing, Unit]
          _       <- stub[Process](_.events(any[ChildRequest]))(
            ZStream.fromZIO(started.succeed(()).unit *> ZIO.never)
          )
          fiber <- Program.events(cell, request).runDrain.fork
          _     <- started.await
          exit  <- fiber.interrupt
          calls <- ZIO.serviceWith[Stubbed[Process]](_.calls)
        yield assertTrue(
          exit.isInterrupted,
          calls.map(_.id.jvmName).toList == List("events"),
        )
      }

  private def failed =
    faults match
      case head :: tail =>
        (test("a process error is the run failure"):
          check(Gen.elements((head :: tail)*)): error =>
            traced(
              for
                _      <- stub[Process](_.events(any[ChildRequest]))(ZStream.fail(error))
                result <- Program.events(cell, request).runCollect.either
              yield assertTrue(surfaces(result, error))
            )
        ) @@ TestAspect.samples(faults.size)
      case Nil =>
        test("process errors have a case"):
          assertTrue(faults.nonEmpty)

  private def reports(
      install: ZStream[Any, Nothing, ChildEvent] => URIO[Stubbed[Process], Unit],
      open: ZStream[Process, ProgramError, ProgramFeed],
      method: String,
  ) =
    check(Gen.string, Gen.string, Gen.int): (out, err, code) =>
      traced(
        for
          _ <- install(
            ZStream(
              ChildEvent.Out(out),
              ChildEvent.Err(err),
              ChildEvent.Exited(ProcessExit(code)),
            )
          )
          feed  <- open.runCollect
          calls <- ZIO.serviceWith[Stubbed[Process]](_.calls)
        yield assertTrue(
          feed.toList == List(
            ProgramFeed.Out(out),
            ProgramFeed.Err(err),
            ProgramFeed.Exited(cell.module, cell.version, cell.platform, code),
          ),
          calls.map(_.id.jvmName).toList == List(method),
        )
      )

  private def traced[E, A](effect: ZIO[Process & Stubbed[Process], E, A]) =
    effect.provide(stubbedTraced[Process])

  private def surfaces(result: Either[ProgramError, Chunk[ProgramFeed]], error: ChildError) =
    result match
      case Left(ProgramError.Process(module, version, detail)) =>
        module == cell.module && version == cell.version && detail == error.message
      case _ => false

  /** One inhabitant per `ChildError` constructor. */
  private val faults: List[ChildError] =
    val path = Path.of("absent")
    List(
      ChildError.UnsupportedPlatform(Platform.Js),
      ChildError.Java(path),
      ChildError.Classes(path),
      ChildError.Artifact(path),
      ChildError.Work(path),
      ChildError.Group(GroupOp.Lookup),
      ChildError.Bootstrap(ProcessOutput.from("absent")),
      ChildError.NoProgram,
      ChildError.Start("absent"),
      ChildError.Property("absent"),
      ChildError.Status,
      ChildError.Console,
      ChildError.Io(IoFailure(path, IoFault.Read)),
    )
end ProcessSpec
