package histos.resolve

import histos.jdk.Sha256
import histos.plugin.{LinkStrategy, PluginCoordinate, PluginLoadError}
import histos.workspace.{IoFailure, IoFault, Workspace}
import histos.version.Catalog
import histos.{
  ArtifactBase,
  Dep,
  DepKind,
  Organization,
  Platform,
  ProjectGraph,
  ProjectName,
  Resolution,
  SbtSeries,
  ScalaVersion,
  Splits,
}

import java.nio.file.{Files, Path}

import slayer.{any, stub, stubbed}

import zio.{Ref, Scope, ZIO}
import zio.test.*

object ResolutionServiceLawSpec extends ZIOSpecDefault:
  private val name      = ProjectName("core")
  private val otherName = ProjectName("other")
  private val version   = ScalaVersion("3.10.0")
  private val sha       = Sha256.trusted("ab" * 32)
  private val other     = Sha256.trusted("cd" * 32)
  private val direct    = LockedDirect("org.example", "demo", "1.0.0", DepKind.Java)
  private val dep       = Dep.trusted("org.example", "demo", "1.0.0", DepKind.Java)
  private val pluginDep = Dep.trusted("org.example", "plug", "1.0.0", DepKind.Java)
  private val gone      = Dep.trusted("org.example", "gone", "1.0.0", DepKind.Java)
  private val id        = ToolId(ToolRole.Compiler, version, ToolVariant.Jvm)
  private val otherId   = ToolId(ToolRole.Semanticdb, version, ToolVariant.Jvm)
  private val root      = ToolRoot("org.example", "demo", "1.0.0", Transitivity.Closure, Some(Repository.Central))
  private val asked     = ResolveCell(name, List(dep), version, Platform.Jvm, Repositories.central)
  private val absent    = ResolveCell(otherName, List(dep), version, Platform.Jvm, Repositories.central)

  def spec = suite("resolution service laws")(
    test("a pin is fetched once, dropped when the jar disappears, and a miss does not solve"):
      Workspace
        .work("histos-resolution-pin")
        .flatMap: dir =>
          val locked  = DependencyLock(List(held(sha, "demo")), List(plugin(sha)), List(tool(sha)))
          val program =
            for
              fetches <- Ref.make(0)
              solves  <- Ref.make(0)
              sourced <- Ref.make(0)
              _       <- stub[Resolve](_.fetch(any[LockedArtifact]))((artifact: LockedArtifact) =>
                fetches.update(_ + 1) *> material(dir, artifact)
              )
              _       <- quiet(solves, sourced)
              service <- openPinned(locked)
              first   <- service.module(asked)
              second  <- service.module(asked)
              _       <- ZIO.foreachDiscard(first.artifacts)(hit => ZIO.attemptBlocking(Files.deleteIfExists(hit.path)))
              third   <- service.module(asked)
              missed  <- service.module(absent).either
              found   <- service.tool(ToolRequest(id, List(root), Repositories.central))
              noTool  <- service.tool(ToolRequest(otherId, List(root), Repositories.central)).either
              drifted <- service.tool(ToolRequest(id, List(root.copy(revision = "9.0.0")), Repositories.central)).either
              linked  <- service.plugin(pluginDep, LinkStrategy.ExactBinary, Repositories.central)
              shifted <- service.plugin(pluginDep, LinkStrategy.Source, Repositories.central).either
              unknown <- service.plugin(gone, LinkStrategy.ExactBinary, Repositories.central).either
              api     <- openPinned(locked.copy(plugins = List(plugin(sha).copy(link = LinkStrategy.Api))))
              refused <- api.plugin(pluginDep, LinkStrategy.ExactBinary, Repositories.central).either
              mode    <- service.resolution
              _       <- service.adopt(lockGraph)
              kept    <- service.module(asked)
              after   <- service.resolution
              fetched <- fetches.get
              solved  <- solves.get
              sources <- sourced.get
            yield assertTrue(
              first.artifacts.map(hit => (hit.name, hit.sha256, hit.repository)) == List(
                ("demo", sha, Repository.Central)
              ),
              second == first,
              third.artifacts.map(_.sha256) == List(sha),
              third.artifacts.map(_.path) == first.artifacts.map(_.path),
              missed == Left(ResolveError.LockDrift(otherName, version, Platform.Jvm, ClasspathScope.Compile, Nil)),
              found.artifacts.map(_.sha256) == List(sha),
              noTool == Left(ResolveError.NoTool(otherId)),
              drifted == Left(ResolveError.ToolDrift(id)),
              linked.artifacts.map(_.name) == List("demo"),
              shifted == Left(
                ResolveError.Plugin(PluginLoadError.Unlocked(PluginCoordinate.Published(pluginDep).render))
              ),
              unknown == Left(ResolveError.Plugin(PluginLoadError.Unlocked(PluginCoordinate.Published(gone).render))),
              refused == Left(ResolveError.Plugin(PluginLoadError.ApiLink(pluginDep))),
              mode == Resolution.Lock && after == Resolution.Lock,
              kept == third,
              fetched == 4,
              solved == 0,
              sources == 0,
            )
          program.provideSomeLayer[TestEnvironment & Scope](stubbed[Resolve])
    ,
    test("a checksum on a pin is not solved, and solve fetches once, retries a checksum, then reloads the pin"):
      Workspace
        .work("histos-resolution-solve")
        .flatMap: dir =>
          val solvedJar = dir.resolve("solved.jar")
          val solved    = Classpath(
            List(ResolvedArtifact("org.example", "demo", "1.0.0", solvedJar, sha, Repository.Central)),
            Nil,
          )
          val program =
            for
              fetches <- Ref.make(0)
              bad     <- Ref.make(0)
              solves  <- Ref.make(0)
              plugins <- Ref.make(0)
              sourced <- Ref.make(0)
              _       <- stub[Resolve](_.fetch(any[LockedArtifact]))((artifact: LockedArtifact) =>
                if artifact.name == "bad" then
                  bad.update(_ + 1) *> ZIO.fail(ResolveError.Checksum(dir.resolve("bad.jar"), other, sha))
                else fetches.update(_ + 1) *> material(dir, artifact)
              )
              _ <- stub[Resolve](_.resolve(any[ResolveCell]))((_: ResolveCell) =>
                solves.update(_ + 1) *>
                  ZIO.attemptBlocking(Files.write(solvedJar, Array[Byte](1, 2, 3))).orDie.as(solved)
              )
              _ <- stub[Resolve](_.resolveTool(any[ToolRequest]))((request: ToolRequest) =>
                if request.id == id then material(dir, artifact(sha, "demo")).map(hit => Classpath(List(hit), Nil))
                else ZIO.succeed(Classpath(Nil, Nil))
              )
              _ <- stub[Resolve](_.resolvePlugin(any[Dep], any[Repositories]))((_: Dep, _: Repositories) =>
                plugins.update(_ + 1) *> material(dir, artifact(sha, "plug")).map(hit => Classpath(List(hit), Nil))
              )
              _ <- stub[Resolve](_.sources(any[String], any[String], any[String], any[Repository]))(
                (_: String, _: String, _: String, _: Repository) =>
                  sourced.update(_ + 1) *> ZIO.die(new IllegalStateException("sources"))
              )
              pin     <- openPinned(DependencyLock(List(held(sha, "bad"))))
              checked <- pin.module(asked).either
              checks  <- bad.get
              repo = dir.resolve("repo")
              _        <- ZIO.attemptBlocking(Files.createDirectories(repo))
              service  <- ResolutionService.open(repo, Resolution.Solve)
              first    <- service.module(asked)
              second   <- service.module(asked)
              report   <- ZIO.attemptBlocking(Files.readString(ResolutionService.reportPath(repo)))
              fresh    <- ResolutionService.open(repo, Resolution.Solve)
              retried  <- fresh.module(asked)
              again    <- fresh.module(asked)
              compiled <- service.tool(ToolRequest(id, List(root), Repositories.central))
              linked   <- service.plugin(pluginDep, LinkStrategy.ExactBinary, Repositories.central)
              _        <- Resolve.write(Resolve.lockPath(repo), DependencyLock(List(held(other, "demo"))))
              _        <- service.adopt(lockGraph)
              adopted  <- service.module(asked)
              mode     <- service.resolution
              solvedN  <- solves.get
              fetched  <- fetches.get
              plugs    <- plugins.get
              sources  <- sourced.get
            yield
              val parsed = DependencyLock.parse(report)
              assertTrue(
                checked == Left(ResolveError.Checksum(dir.resolve("bad.jar"), other, sha)),
                checks == 1,
                first.artifacts.map(_.sha256) == List(sha),
                second == first,
                parsed.map(_.cells.map(cell => (cell.module, cell.artifacts.map(_.sha256)))) == Right(
                  List((name, List(sha)))
                ),
                retried.artifacts.map(_.sha256) == List(sha),
                again == retried,
                compiled.artifacts.map(_.name) == List("demo"),
                linked.artifacts.map(_.name) == List("plug"),
                plugs == 1,
                sources == 0,
                mode == Resolution.Lock,
                adopted.artifacts.map(_.sha256) == List(other),
                solvedN == 1,
                fetched == 2,
              )
          program.provideSomeLayer[TestEnvironment & Scope](stubbed[Resolve])
    ,
    test("an exclusion or a series is a different solve, and the same cell is not"):
      Workspace
        .work("histos-resolution-key")
        .flatMap: dir =>
          val excluded = dep.excluding(Organization.trusted("org.example"), ArtifactBase.trusted("other"))
          val binary   = Dep.binary("org.example", "plug", histos.LibraryRevision.trusted("1.0.0"))
          val series   = binary.sbt(SbtSeries.trusted("2"))
          val program  =
            for
              solves  <- Ref.make(0)
              plugins <- Ref.make(0)
              _       <- stub[Resolve](_.resolve(any[ResolveCell]))((_: ResolveCell) =>
                solves.update(_ + 1) *> ZIO.succeed(Classpath(Nil, Nil))
              )
              _ <- stub[Resolve](_.resolvePlugin(any[Dep], any[Repositories]))((_: Dep, _: Repositories) =>
                plugins.update(_ + 1) *> ZIO.succeed(Classpath(Nil, Nil))
              )
              _ <- stub[Resolve](_.resolveTool(any[ToolRequest]))((_: ToolRequest) => ZIO.succeed(Classpath(Nil, Nil)))
              _ <- stub[Resolve](_.fetch(any[LockedArtifact]))((_: LockedArtifact) => ZIO.dieMessage("fetch"))
              _ <- stub[Resolve](_.sources(any[String], any[String], any[String], any[Repository]))(
                (_: String, _: String, _: String, _: Repository) => ZIO.dieMessage("sources")
              )
              _       <- ZIO.attemptBlocking(Files.createDirectories(dir))
              service <- ResolutionService.open(dir, Resolution.Solve)
              _       <- service.module(asked)
              _       <- service.module(asked)
              _       <- service.module(asked.copy(dependencies = List(excluded)))
              _       <- service.plugin(binary, LinkStrategy.ExactBinary, Repositories.central)
              _       <- service.plugin(binary, LinkStrategy.ExactBinary, Repositories.central)
              _       <- service.plugin(series, LinkStrategy.ExactBinary, Repositories.central)
              _       <- service.plugin(
                pluginDep.excluding(Organization.trusted("org.example"), ArtifactBase.trusted("other")),
                LinkStrategy.ExactBinary,
                Repositories.central,
              )
              solved <- solves.get
              plugs  <- plugins.get
            yield assertTrue(solved == 2, plugs == 3)
          program.provideSomeLayer[TestEnvironment & Scope](stubbed[Resolve])
    ,
    test("solve and lock name different files"):
      val repo = Path.of("/repo")
      assertTrue(
        ResolutionService.file(repo, Resolution.Solve) == repo.resolve(".histos").resolve("resolution"),
        ResolutionService.file(repo, Resolution.Lock) == repo.resolve("dependencies.lock"),
        ResolutionService.reportPath(repo) == repo.resolve(".histos").resolve("resolution"),
      ),
  )

  private def held(checksum: Sha256, artifactName: String): LockedCell =
    LockedCell(name, version, Platform.Jvm, List(direct), List(artifact(checksum, artifactName)), Nil)

  private def plugin(checksum: Sha256): LockedPlugin =
    LockedPlugin(
      "org.example",
      "plug",
      "1.0.0",
      LinkStrategy.ExactBinary,
      List(LockedDirect("org.example", "plug", "1.0.0", DepKind.Java)),
      List(artifact(checksum, "demo")),
      None,
    )

  private def tool(checksum: Sha256): LockedTool =
    LockedTool(id, List(root), List(Repository.Central), List(artifact(checksum, "demo")), Nil)

  private def artifact(checksum: Sha256, artifactName: String): LockedArtifact =
    LockedArtifact("org.example", artifactName, "1.0.0", checksum, Repository.Central)

  private val lockGraph =
    ProjectGraph.trusted(Nil, Splits.aggregate, Catalog.empty, None, Nil, resolution = Resolution.Lock)

  private def material(dir: Path, locked: LockedArtifact) =
    ZIO
      .attemptBlocking:
        Files.createDirectories(dir)
        val path = dir.resolve(s"${locked.name}.jar")
        Files.write(path, Array[Byte](1, 2, 3))
        ResolvedArtifact(
          locked.organization,
          locked.name,
          locked.revision,
          path,
          locked.sha256,
          locked.repository,
          locked.classifier,
        )
      .mapError(_ => ResolveError.Io(IoFailure(dir, IoFault.Write)))

  private def quiet(solves: Ref[Int], sourced: Ref[Int]) =
    stub[Resolve](_.resolve(any[ResolveCell]))((_: ResolveCell) =>
      solves.update(_ + 1) *> ZIO.succeed(Classpath(Nil, Nil))
    ) *>
      stub[Resolve](_.resolveTool(any[ToolRequest]))((_: ToolRequest) => ZIO.succeed(Classpath(Nil, Nil))) *>
      stub[Resolve](_.resolvePlugin(any[Dep], any[Repositories]))((_: Dep, _: Repositories) =>
        ZIO.succeed(Classpath(Nil, Nil))
      ) *>
      stub[Resolve](_.sources(any[String], any[String], any[String], any[Repository]))(
        (_: String, _: String, _: String, _: Repository) =>
          sourced.update(_ + 1) *>
            ZIO.succeed(ResolvedArtifact("org.example", "unused", "0", Path.of("unused.jar"), sha, Repository.Central))
      )

  private def openPinned(lock: DependencyLock) =
    ResolutionService.pinned(lock).build.map(_.get[ResolutionService])
end ResolutionServiceLawSpec
