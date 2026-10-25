package histos.publish

import histos.resolve.IdleResolve
import histos.version.{
  Decision,
  PublishAction,
  Session,
  ShipNumber,
  VersionTask
}
import histos.zinc.GraphCompile
import histos.{Organization, Projects, ScalaVersion, module}

import java.nio.file.{Files, Path}

import _root_.scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO, durationInt}
import zio.test.*

object PublishHoldSpec extends ZIOSpecDefault:
  private val line = "libs compile 1.0.0-ci hold already-released"
  private val env = Map(
    Session.Key -> "snapshot",
    Session.ShaKey -> "0123456789abcdef0123456789abcdef01234567",
    Session.BranchKey -> "default"
  )

  def spec = suite("publish hold")(
    test("a held ship is named and not published"):
      ZIO.scoped:
        for
          jdk <- GraphCompile.jdkHere
          root <- temp
          repo <- temp
          _ <- ZIO.attempt {
            Files.createDirectories(root.resolve(".histos"))
            Files.writeString(
              root.resolve(".histos").resolve("releases"),
              "libs matches\n"
            )
          }
          graph <- ZIO.fromEither(held)
          report <- PublishRun
            .run(root, graph, Nil, jdk, env, repo.toString, _ => ZIO.unit)
            .provide(IdleResolve.layer)
          decision <- VersionTask.decide(graph, root, env)
          wrote <- ZIO.attempt(
            Option(repo.toFile.list()).map(_.toList).getOrElse(Nil)
          )
        yield
          val expected = decision match
            case Decision.Publish(_, lines) =>
              lines.toList.flatMap: row =>
                row.action match
                  case PublishAction.Hold(_, _) =>
                    List(Decision.rendered(row))
                  case _ => Nil
            case _ => Nil
          assertTrue(
            expected == List(line),
            report.lines == expected,
            report.output.modules.isEmpty,
            report.lines.exists(text =>
              text.contains("libs") && text.contains("already-released")
            ),
            wrote.isEmpty
          )
  ) @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock @@ TestAspect
    .timeout(30.seconds)

  private def held =
    val core = module("core", histos.Jvm)(ScalaVersion("3.10.0"))
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.ship("libs", ShipNumber("1.0.0"))(core)),
      core
    )

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(
      ZIO.attempt(Files.createTempDirectory("histos-hold"))
    ): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths
            .sortBy(_.getNameCount)
            .reverse
            .foreach(path => Files.deleteIfExists(path))
      }.ignore
end PublishHoldSpec
