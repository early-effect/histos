package histos.task

import histos.*

import zio.*
import zio.test.*

object TaskSpec extends ZIOSpecDefault:
  private val graph: Either[BuildError, ProjectGraph] =
    Projects(module("core", Jvm)(scala"3.9.0"))

  private def withGraph[R, E](
      use: ProjectGraph => ZIO[R, E, TestResult]
  ): ZIO[R, E, TestResult] =
    graph match
      case Right(built) => use(built)
      case Left(err)    => ZIO.succeed(assertTrue(err.message == "built"))

  private val tasks = suite("tasks")(
    test("inspect names the projects"):
      withGraph: built =>
        ZIO
          .scoped:
            for
              run <- ZIO.serviceWithZIO[Tasks](_.run(TaskId.Inspect))
              result <- run.await
            yield assertTrue:
              result match
                case TaskResult.Inspect(projects) =>
                  projects.map(_.value) == List("core")
                case _ => false
          .provide(Tasks.live(built))
    ,
    test("an unknown task has no runner"):
      withGraph: built =>
        ZIO
          .scoped:
            for
              id <- ZIO.fromEither(TaskId.from("compile"))
              out <- ZIO.serviceWithZIO[Tasks](_.run(id)).either
            yield assertTrue:
              out match
                case Left(TaskError.NoRunner(missing)) =>
                  missing.value == "compile"
                case _ => false
          .provide(Tasks.live(built))
    ,
    test("cancel interrupts a slow run"):
      withGraph: built =>
        for
          slow <- ZIO.fromEither(TaskId.from("slow"))
          result <-
            ZIO
              .scoped:
                for
                  run <- ZIO.serviceWithZIO[Tasks](_.run(slow))
                  _ <- ZIO.serviceWithZIO[Tasks](_.cancel(run.id))
                  exit <- run.await.exit
                yield assertTrue(exit.isInterrupted)
              .provide(
                Tasks.live(
                  built,
                  Map(slow -> ZIO.sleep(1.hour).as(TaskResult.Inspect(Nil)))
                )
              )
        yield result
    ,
    test("cancel of an unknown run fails"):
      withGraph: built =>
        ZIO
          .scoped:
            for
              id <- ZIO.fromEither(RunId.from("missing"))
              out <- ZIO.serviceWithZIO[Tasks](_.cancel(id)).either
            yield assertTrue:
              out match
                case Left(TaskError.UnknownRun(missing)) =>
                  missing.value == "missing"
                case _ => false
          .provide(Tasks.live(built))
  )

  def spec = tasks
