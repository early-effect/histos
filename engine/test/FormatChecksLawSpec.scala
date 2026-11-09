package histos.resolve

import histos.doctor.{FormatChecks, Outcome}
import histos.format.{ScalafmtTools, ScalafmtVersion}
import histos.jdk.Sha256
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
  Formatting,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalafmtConf,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
  ToolCatalog,
}

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.*

object FormatChecksLawSpec extends ZIOSpecDefault:
  private val tools = ToolCatalog.default.format
  private val sha   = Sha256.trusted("ab" * 32)

  def spec = suite("format doctor checks")(
    test("a build that does not format passes both checks without reading a file"):
      Workspace
        .work("histos-format-doctor-off")
        .flatMap: repo =>
          for
            config <- FormatChecks.config(repo, graph(Formatting.Off, Resolution.Lock))
            lock   <- FormatChecks.lock(repo, graph(Formatting.Off, Resolution.Lock))
          yield assertTrue(ok(config), ok(lock))
    ,
    test("a missing configuration is the config finding, and the lock check leaves it to that finding"):
      Workspace
        .work("histos-format-doctor-missing")
        .flatMap: repo =>
          val formats = graph(Formatting.Scalafmt(ScalafmtConf.Root), Resolution.Lock)
          for
            config <- FormatChecks.config(repo, formats)
            lock   <- FormatChecks.lock(repo, formats)
          yield assertTrue(ids(config) == List("format.config"), ok(lock))
    ,
    test("a locked build passes while the lock pins the configured core, and a moved version is drift"):
      Workspace
        .work("histos-format-doctor-lock")
        .flatMap: repo =>
          val formats = graph(Formatting.Scalafmt(ScalafmtConf.Root), Resolution.Lock)
          for
            _       <- conf(repo, "3.11.5")
            version <- ZIO.fromEither(ScalafmtVersion.from(Path.of(".scalafmt.conf"), "3.11.5"))
            held = List(
              ScalafmtTools.dynamic(tools, Repositories.central),
              ScalafmtTools.core(version, tools, Repositories.central),
            )
            _       <- Resolve.write(Resolve.lockPath(repo), DependencyLock(Nil, Nil, held.map(pin)))
            pinned  <- FormatChecks.lock(repo, formats)
            _       <- conf(repo, "3.11.4")
            moved   <- FormatChecks.lock(repo, formats)
            solving <- FormatChecks.lock(repo, graph(Formatting.Scalafmt(ScalafmtConf.Root), Resolution.Solve))
            config  <- FormatChecks.config(repo, formats)
          yield assertTrue(
            ok(pinned),
            ids(moved) == List("resolve.tool-drift"),
            ok(solving),
            ok(config),
          ),
  )

  private def ok(outcomes: List[Outcome]): Boolean =
    outcomes.nonEmpty && outcomes.forall:
      case Outcome.Ok(_, _) => true
      case _                => false

  private def ids(outcomes: List[Outcome]): List[String] =
    outcomes.collect { case Outcome.Found(found) => found.check.value }

  private def conf(repo: Path, version: String) =
    ZIO.attempt(Files.writeString(repo.resolve(".scalafmt.conf"), s"version = \"$version\"\n")).orDie

  private def graph(formatting: Formatting, resolution: Resolution): ProjectGraph =
    ProjectGraph.trusted(
      List(
        Module(
          ProjectName.trusted("core"),
          Platforms(Platform.Jvm, Nil),
          ScalaVersions(ScalaVersion.trusted("3.10.0"), Nil),
          Nil,
          Nil,
          SourcePos("histos.scala", 1, 1),
        )
      ),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = resolution,
      formatting = formatting,
    )

  private def pin(request: ToolRequest): LockedTool =
    LockedTool(
      request.id,
      request.roots,
      Repositories.ordered(request.repositories),
      request.roots.map(root => LockedArtifact(root.organization, root.name, root.revision, sha, Repository.Central)),
      Nil,
    )
end FormatChecksLawSpec
