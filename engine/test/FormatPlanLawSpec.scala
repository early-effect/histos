package histos.resolve

import histos.format.{ScalafmtConfError, ScalafmtTools, ScalafmtVersion}
import histos.jdk.Sha256
import histos.native.NativeOrigin
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
  Formatting,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalafmtConf,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
  ToolCatalog,
}

import java.nio.file.{Files, Path}

import slayer.{any, stub, stubbed}

import zio.{Scope, ZIO}
import zio.test.*

object FormatPlanLawSpec extends ZIOSpecDefault:
  private val tools = ToolCatalog.default.format
  private val sha   = Sha256.trusted("ab" * 32)
  private val jar   = Path.of("tool.jar")

  def spec = suite("formatter lock plan")(
    test("a build that formats plans the dynamic cell and the core cell its configuration names, and nothing else"):
      Workspace
        .work("histos-format-plan")
        .flatMap: repo =>
          for
            _       <- conf(repo, "3.11.5")
            off     <- LockedTools.plan(repo, graph(Formatting.Off), NativeOrigin.Central)
            on      <- LockedTools.plan(repo, graph(Formatting.Scalafmt(ScalafmtConf.Root)), NativeOrigin.Central)
            version <- ZIO.fromEither(ScalafmtVersion.from(Path.of(".scalafmt.conf"), "3.11.5"))
          yield
            val added = on.filterNot(off.contains)
            assertTrue(
              added == List(
                ScalafmtTools.dynamic(tools, Repositories.central),
                ScalafmtTools.core(version, tools, Repositories.central),
              ),
              added.map(_.roots.map(root => s"${root.organization}:${root.name}:${root.revision}")) == List(
                List("org.scalameta:scalafmt-dynamic-core_2.13:3.11.5"),
                List("org.scalameta:scalafmt-core_2.13:3.11.5", "org.scala-lang:scala-reflect:2.13.18"),
              ),
              added.forall(_.roots.forall(_.transitivity == Transitivity.Closure)),
            )
    ,
    test("a declared configuration that is missing stops the plan with that error"):
      Workspace
        .work("histos-format-missing")
        .flatMap: repo =>
          LockedTools
            .plan(repo, graph(Formatting.Scalafmt(ScalafmtConf.Root)), NativeOrigin.Central)
            .either
            .map(planned =>
              assertTrue(planned == Left(ResolveError.Scalafmt(ScalafmtConfError.Missing(Path.of(".scalafmt.conf")))))
            )
    ,
    test("a configuration that moved past the lock is drift on the core cell"):
      Workspace
        .work("histos-format-drift")
        .flatMap: repo =>
          val formats = graph(Formatting.Scalafmt(ScalafmtConf.Root))
          val run     =
            for
              _ <- stub[Resolve](_.fetch(any[LockedArtifact]))(
                ZIO.succeed(ResolvedArtifact("o", "n", "r", jar, sha, Repository.Central))
              )
              _       <- conf(repo, "3.11.5")
              planned <- LockedTools.plan(repo, formats, NativeOrigin.Central)
              _       <- Resolve.write(Resolve.lockPath(repo), DependencyLock(Nil, Nil, planned.map(pin)))
              agreed  <- Resolve.verifyTools(repo, formats).either
              _       <- conf(repo, "3.11.4")
              drifted <- Resolve.verifyTools(repo, formats).either
            yield assertTrue(
              agreed == Right(()),
              drifted == Left(ResolveError.ToolDrift(ToolId(ToolRole.ScalafmtCore, tools.scala, ToolVariant.Jvm))),
            )
          run.provideSomeLayer[Scope](stubbed[Resolve]),
  )

  private def conf(repo: Path, version: String) =
    ZIO.attempt(Files.writeString(repo.resolve(".scalafmt.conf"), s"version = \"$version\"\n")).orDie

  private def graph(formatting: Formatting): ProjectGraph =
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
end FormatPlanLawSpec
