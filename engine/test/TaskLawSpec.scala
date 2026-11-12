package histos.task

import histos.version.Decision
import histos.{
  Aggregate,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
}

import zio.{Promise, Ref, ZIO}
import zio.test.*

object TaskLawSpec extends ZIOSpecDefault:
  private val core  = module("core")
  private val all   = Aggregate(ProjectName.trusted("all"), List(core.name), SourcePos("histos.scala", 2, 1))
  private val graph = ProjectGraph.trusted(List(core, all))

  def spec = suite("task laws")(
    test("an empty id is rejected, and the built-in ids are inspect, versions, and doc"):
      val inspect  = TaskId.from("inspect")
      val versions = TaskId.from("versions")
      val doc      = TaskId.from("doc")
      val slow     = TaskId.from("slow")
      assertTrue(
        TaskId.from("") == Left(TaskError.EmptyId),
        inspect.map(_.builtIn) == Right(true),
        versions.map(_.builtIn) == Right(true),
        doc.map(_.builtIn) == Right(true),
        slow.map(_.builtIn) == Right(false),
        inspect.map(_.value) == Right(TaskId.Inspect.value),
        TaskResult.Doc.render == "scaladoc",
        TaskResult.Inspect(List(core.name, all.name)).render == "core,all",
        TaskResult.Versions(Decision.NoCatalog).render == "no catalog",
      )
    ,
    test("inspect names every project, and versions and doc have no runner until one is added"):
      ZIO
        .scoped:
          ZIO.serviceWithZIO[Tasks]: tasks =>
            for
              run      <- tasks.run(TaskId.Inspect)
              result   <- run.await
              versions <- tasks.run(TaskId.Versions).exit
              doc      <- tasks.run(TaskId.Doc).exit
              id       <- ZIO.fromEither(TaskId.from("missing"))
              missing  <- tasks.run(id).exit
            yield assertTrue(
              result == TaskResult.Inspect(List(core.name, all.name)),
              result.render == "core,all",
              versions.causeOption.flatMap(_.failureOption) == Some(TaskError.NoRunner(TaskId.Versions)),
              doc.causeOption.flatMap(_.failureOption) == Some(TaskError.NoRunner(TaskId.Doc)),
              missing.causeOption.flatMap(_.failureOption) == Some(TaskError.NoRunner(id)),
            )
        .provide(Tasks.live(graph))
    ,
    test("cancel interrupts the run, an unknown run is typed, and closing the scope interrupts a leftover"):
      for
        slow    <- ZIO.fromEither(TaskId.from("slow"))
        probe   <- Ref.make(false)
        entered <- Promise.make[Nothing, Unit]
        exit    <- ZIO
          .scoped:
            ZIO.serviceWithZIO[Tasks]: tasks =>
              for
                run    <- tasks.run(slow)
                fiber  <- run.await.fork
                _      <- tasks.cancel(run.id)
                done   <- fiber.await
                absent <- tasks.cancel(RunId.trusted("absent")).exit
              yield assertTrue(
                done.isInterrupted,
                absent.causeOption.flatMap(_.failureOption) ==
                  Some(TaskError.UnknownRun(RunId.trusted("absent"))),
              )
          .provide(Tasks.live(graph, Map(slow -> ZIO.never.as(TaskResult.Doc))))
        _ <- ZIO
          .scoped(ZIO.serviceWithZIO[Tasks](_.run(slow) *> entered.await))
          .provide(
            Tasks.live(
              graph,
              Map(
                slow -> (entered.succeed(()) *> ZIO.never).onInterrupt(probe.set(true)).as(TaskResult.Doc)
              ),
            )
          )
        flagged <- probe.get
      yield exit && assertTrue(flagged),
  )

  private def module(name: String): Module =
    Module(
      ProjectName.trusted(name),
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion.trusted("3.3.7"), Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )
end TaskLawSpec
