package histos.resolve

import histos.jdk.Sha256
import histos.{
  Aggregate,
  Dep,
  DepKind,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Resolver,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  SbtSeries,
}

import zio.*
import zio.test.*

object ResolveLawSpec extends ZIOSpecDefault:
  private val name    = ProjectName.trusted("core")
  private val version = ScalaVersion.trusted("3.3.7")
  private val sha     = Sha256.trusted("ab" * 32)
  private val other   = Sha256.trusted("cd" * 32)
  private val direct  = LockedDirect("org.example", "demo", "1.0.0", DepKind.Java)
  private val dep     = Dep.trusted("org.example", "demo", "1.0.0", DepKind.Java)

  def spec = suite("resolve laws")(
    test("a published name follows the platform and the Scala binary suffix"):
      val scala2 = ScalaVersion.trusted("2.13.14")
      assertTrue(
        Platform.values.forall { platform =>
          val suffix = platform match
            case Platform.Jvm    => ""
            case Platform.Js     => "_sjs1"
            case Platform.Native => "_native0.5"
          ArtifactName.published("core", version, platform) == s"core${suffix}_3" &&
          ArtifactName
            .of(Dep.trusted("org.example", "core", "1.0.0", DepKind.Binary), version, platform) == s"core${suffix}_3" &&
          ArtifactName.of(
            Dep.trusted("org.example", "core", "1.0.0", DepKind.Full),
            version,
            platform,
          ) == s"core${suffix}_3.3.7" &&
          ArtifactName.of(Dep.trusted("org.example", "core", "1.0.0", DepKind.Java), version, platform) == "core"
        },
        ArtifactName.published("core", scala2, Platform.Jvm) == "core_2.13",
        ArtifactName.plugin("core", version, Platform.Jvm, SbtSeries.trusted("2")) == "core_sbt2_3",
      )
    ,
    test("an empty resolver list is central, and a listed central moves last"):
      val url   = histos.RepositoryUrl.trusted("file:/tmp/m2")
      val repos = BuildResolvers(List(Resolver.Central, Resolver.Maven(url)))
      assertTrue(
        BuildResolvers(Nil) == Repositories.central,
        Repositories.ordered(repos) == List(Repository.Maven(RepoUrl.trusted(url.value)), Repository.Central),
        Repositories.ordered(Repositories.central) == List(Repository.Central),
      )
    ,
    test("a ranged revision is refused before a solve, and a fixed revision is not"):
      val ranged = Dep.trusted("org.example", "demo", "1.+", DepKind.Java)
      val fixed  = cell(List(dep))
      for
        refused <- Resolve.prepare(cell(List(ranged))).exit
        ready   <- Resolve.prepare(fixed).exit
      yield assertTrue(
        refused.causeOption.flatMap(_.failureOption).contains(ResolveError.Range(ranged)),
        ready match
          case Exit.Success(_) => true
          case Exit.Failure(_) => false,
      )
    ,
    test("locate misses a cell or a direct, and a changed checksum is still that cell"):
      val locked      = lock(sha)
      val moved       = cell(List(Dep.trusted("org.example", "demo", "1.1.0", DepKind.Java)))
      val same        = Resolve.locate(cell(List(dep)), locked)
      val drifted     = Resolve.locate(cell(List(dep)), lock(other))
      val directMoved = Resolve.locate(moved, locked)
      val absent      = Resolve.locate(cell(List(dep)), DependencyLock(Nil))
      same match
        case Right(found) =>
          assertTrue(
            found.artifacts.map(_.sha256) == List(sha),
            drifted == Right(found.copy(artifacts = found.artifacts.map(_.copy(sha256 = other)))),
            directMoved match
              case Left(ResolveError.LockDrift(module, scalaVersion, platform, scope, changed)) =>
                module == name && scalaVersion == version && platform == Platform.Jvm &&
                scope == ClasspathScope.Compile && changed.nonEmpty
              case _ => false
            ,
            absent == Left(ResolveError.LockDrift(name, version, Platform.Jvm, ClasspathScope.Compile, Nil)),
          )
        case Left(_) => assertTrue(false)
    ,
    test("a tool root that is missing or duplicated is that fault, and a checksum is not a root fault"):
      val request    = ToolRequest(id, List(root), Repositories.central)
      val missing    = tool(Nil)
      val duplicated = tool(List(artifact(sha), artifact(other)))
      val changed    = tool(List(artifact(other)))
      assertTrue(
        LockedTools.validate(request, missing) == Left(ResolveError.ToolRoot(id, root, ToolRootFault.Missing)),
        LockedTools.validate(request, duplicated) == Left(ResolveError.ToolRoot(id, root, ToolRootFault.Duplicated)),
        LockedTools.validate(request, changed) == Right(()),
        LockedTools.validate(request, tool(List(artifact(sha)))) == Right(()),
      )
    ,
    test("an aggregate has no cell, and a test dependency follows the compile cell"):
      val compile = module(List(dep), Nil)
      val tests   = module(List(dep), List(Dep.trusted("org.example", "kit", "1.0.0", DepKind.Java)))
      val all     = Aggregate(ProjectName.trusted("all"), List(name), SourcePos("histos.scala", 1, 1))
      val cells   = Resolve.cells(ProjectGraph.trusted(List(all, compile, tests)), Repositories.central)
      assertTrue(
        cells.map(item => (item.module, item.scope)) == List(
          (name, ClasspathScope.Compile),
          (name, ClasspathScope.Compile),
          (name, ClasspathScope.Test),
        ),
        cells.lastOption.exists(_.dependencies.length == 2),
      )
    ,
    test("a fingerprint changes when the lock sha changes"):
      val row  = ToolInput.Row("org.example", "demo", "1.0.0", None, sha)
      val next = row.copy(sha256 = other)
      assertTrue(ToolInput.fingerprint(id, List(row)) != ToolInput.fingerprint(id, List(next))),
  )

  private val id: ToolId = ToolId(ToolRole.Compiler, version, ToolVariant.Jvm)

  private val root: ToolRoot = ToolRoot("org.example", "demo", "1.0.0", Transitivity.Closure, Some(Repository.Central))

  private def cell(dependencies: List[Dep]): ResolveCell =
    ResolveCell(name, dependencies, version, Platform.Jvm, Repositories.central)

  private def artifact(checksum: Sha256): LockedArtifact =
    LockedArtifact("org.example", "demo", "1.0.0", checksum, Repository.Central)

  private def lock(checksum: Sha256): DependencyLock =
    DependencyLock(List(LockedCell(name, version, Platform.Jvm, List(direct), List(artifact(checksum)), Nil)))

  private def tool(artifacts: List[LockedArtifact]): LockedTool =
    LockedTool(id, List(root), List(Repository.Central), artifacts, Nil)

  private def module(dependencies: List[Dep], tests: List[Dep]): Module =
    Module(
      name,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(version, Nil),
      dependencies,
      Nil,
      SourcePos("histos.scala", 1, 1),
      testDependencies = tests,
    )
end ResolveLawSpec
