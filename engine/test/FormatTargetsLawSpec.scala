package histos.format

import histos.select.SelectError
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{Module, Platform, Platforms, ProjectGraph, ProjectName, ScalaVersion, ScalaVersions, SourcePos, Splits}

import java.nio.file.{Files, Path}

import zio.{NonEmptyChunk, ZIO}
import zio.test.*

object FormatTargetsLawSpec extends ZIOSpecDefault:
  private val core = ProjectName.trusted("core")
  private val app  = ProjectName.trusted("app")

  private val tree = List(
    "core/src/histos/A.scala",
    "core/src/scala-2.13/B.scala",
    "core/test/ASpec.scala",
    "core/src/notes.md",
    "app/src/Main.scala",
    "app/test/MainSpec.scala",
    "tools/Script.scala",
    "histos.scala",
  )

  def spec = suite("format targets")(
    test("the build is every module's src and test trees plus histos.scala, and nothing outside them"):
      within: repo =>
        FormatScope
          .select(repo, graph(List(core, app)), FormatTargets.Build)
          .map(selection =>
            assertTrue(
              selection.git == GitFilter.Tracked,
              selection.files.map(_.toString).toSet == Set(
                "core/src/histos/A.scala",
                "core/src/scala-2.13/B.scala",
                "core/test/ASpec.scala",
                "app/src/Main.scala",
                "app/test/MainSpec.scala",
                "histos.scala",
              ),
            )
          )
    ,
    test("a named module is its own trees, without histos.scala or another module"):
      within: repo =>
        for
          named   <- FormatScope.select(repo, graph(List(core, app)), FormatTargets.Projects(NonEmptyChunk(app)))
          unknown <- FormatScope
            .select(repo, graph(List(core, app)), FormatTargets.Projects(NonEmptyChunk(ProjectName.trusted("nope"))))
            .either
        yield assertTrue(
          named.files.map(_.toString) == List("app/src/Main.scala", "app/test/MainSpec.scala"),
          unknown.left.exists:
            case FormatError.Selected(_: SelectError) => true
            case _                                    => false,
        )
    ,
    test("named files are exactly those files, normalized, and git does not narrow them"):
      within: repo =>
        val asked = NonEmptyChunk(
          Path.of("tools/./Script.scala"),
          Path.of("core/src/histos/A.scala"),
          Path.of("tools/Script.scala"),
        )
        FormatScope
          .select(repo, graph(List(core)), FormatTargets.Files(asked))
          .map(selection =>
            assertTrue(
              selection.git == GitFilter.Named,
              selection.files == List(Path.of("tools/Script.scala"), Path.of("core/src/histos/A.scala")),
            )
          ),
  )

  private def within[E](body: Path => ZIO[Any, E, TestResult]) =
    Workspace
      .work("histos-format-targets")
      .flatMap: repo =>
        ZIO
          .foreachDiscard(tree): file =>
            ZIO.attempt {
              Files.createDirectories(repo.resolve(file).getParent)
              Files.writeString(repo.resolve(file), "object X\n")
            }.orDie
          .zipRight(body(repo))

  private def graph(names: List[ProjectName]): ProjectGraph =
    ProjectGraph.trusted(
      names.map(name =>
        Module(
          name,
          Platforms(Platform.Jvm, Nil),
          ScalaVersions(ScalaVersion.trusted("3.10.0"), Nil),
          Nil,
          Nil,
          SourcePos("histos.scala", 1, 1),
        )
      ),
      Splits.aggregate,
      Catalog.empty,
    )
end FormatTargetsLawSpec
