package histos.resolve

import histos.jdk.Sha256
import histos.native.NativeOrigin
import histos.plugin.LinkStrategy
import histos.workspace.Workspace
import histos.{
  Dep,
  DepKind,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
}

import java.nio.file.Path

import slayer.{any, stub, stubbed}

import zio.{Scope, ZIO}
import zio.test.*

/** A fresh classpath agrees with the lock, or the disagreement is the drift. The tools are pinned, not downloaded.
  */
object ResolveAgreeLawSpec extends ZIOSpecDefault:
  private val name    = ProjectName.trusted("core")
  private val version = ScalaVersion.trusted("3.3.7")
  private val sha     = Sha256.trusted("ab" * 32)
  private val other   = Sha256.trusted("cd" * 32)
  private val plugin  = Dep.trusted("org.example", "plug", "1.2.3", DepKind.Binary)
  private val jar     = Path.of("fresh.jar")

  def spec = suite("resolve agreement")(
    test("a ranged module is refused before a tool is fetched, and an agreed cell is named"):
      Workspace.work("histos-resolve-agree").flatMap { root =>
        val graph = ProjectGraph.trusted(List(module(Nil)))
        val run   =
          for
            _      <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.dieMessage("fetched"))
            _      <- stub[Resolve](_.resolve(any[ResolveCell]))(ZIO.dieMessage("resolved"))
            ranged <- Resolve
              .check(root, ProjectGraph.trusted(List(module(List(rangedDep)))), Repositories.central)
              .either
            _      <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.succeed(artifact(sha)))
            _      <- stub[Resolve](_.resolve(any[ResolveCell]))(ZIO.succeed(Classpath(Nil, Nil)))
            _      <- record(root, graph, Nil)
            agreed <- Resolve.check(root, graph, Repositories.central).either
            _ <- stub[Resolve](_.resolve(any[ResolveCell]))(ZIO.succeed(Classpath(List(artifact(sha, "9.9.9")), Nil)))
            _ <- record(root, graph, List(held(sha)))
            drifted <- Resolve.check(root, graph, Repositories.central).either
            _       <- stub[Resolve](_.resolve(any[ResolveCell]))(ZIO.succeed(Classpath(List(artifact(other)), Nil)))
            summed  <- Resolve.check(root, graph, Repositories.central).either
          yield assertTrue(
            ranged == Left(ResolveError.Range(rangedDep)),
            agreed == Right(s"${name.value} ${version.value} ${Platform.Jvm.token}"),
            drifted == Left(
              ResolveError.LockDrift(
                name,
                version,
                Platform.Jvm,
                ClasspathScope.Compile,
                List("lock org.example:demo:1.0.0", "graph org.example:demo:9.9.9"),
              )
            ),
            summed == Left(ResolveError.Checksum(jar, sha, other)),
          )
        run.provideSomeLayer[Scope](stubbed[Resolve])
      }
    ,
    test("an empty plugin list does not resolve, and a plugin classpath that moved is drift"):
      Workspace.work("histos-resolve-plugin").flatMap { root =>
        val run =
          for
            compiler <- histos.toolchain.BuildCompiler.version.catchAll(error => ZIO.dieMessage(error.message))
            published = ArtifactName.of(plugin, compiler, Platform.Jvm)
            _ <- stub[Resolve](_.resolvePlugin(any[Dep], any[Repositories]))(ZIO.dieMessage("plugin"))
            _ <- stub[Resolve](_.sources(any[String], any[String], any[String], any[Repository]))(
              ZIO.dieMessage("sources")
            )
            idle    <- Resolve.checkPlugins(root, Nil, Repositories.central).either
            _       <- writePlugins(root, Nil)
            missing <- Resolve
              .checkPlugins(root, List(PluginRequest.Dep(plugin, LinkStrategy.BinaryOrSource)), Repositories.central)
              .either
            moved = ResolvedArtifact("org.example", published, "9.9.9", jar, sha, Repository.Central)
            _ <- stub[Resolve](_.resolvePlugin(any[Dep], any[Repositories]))(ZIO.succeed(Classpath(List(moved), Nil)))
            _ <- writePlugins(root, List(lockedPlugin(published, sha, None)))
            drifted <- Resolve
              .checkPlugins(root, List(PluginRequest.Dep(plugin, LinkStrategy.BinaryOrSource)), Repositories.central)
              .either
            fresh = ResolvedArtifact("org.example", published, plugin.revision.value, jar, other, Repository.Central)
            _ <- stub[Resolve](_.resolvePlugin(any[Dep], any[Repositories]))(ZIO.succeed(Classpath(List(fresh), Nil)))
            summed <- Resolve
              .checkPlugins(root, List(PluginRequest.Dep(plugin, LinkStrategy.BinaryOrSource)), Repositories.central)
              .either
            kept = ResolvedArtifact("org.example", published, plugin.revision.value, jar, sha, Repository.Central)
            _ <- stub[Resolve](_.resolvePlugin(any[Dep], any[Repositories]))(ZIO.succeed(Classpath(List(kept), Nil)))
            agreed <- Resolve
              .checkPlugins(root, List(PluginRequest.Dep(plugin, LinkStrategy.BinaryOrSource)), Repositories.central)
              .either
            _ <- stub[Resolve](_.sources(any[String], any[String], any[String], any[Repository]))(
              ZIO
                .succeed(ResolvedArtifact("org.example", "plug", plugin.revision.value, jar, other, Repository.Central))
            )
            _       <- writePlugins(root, List(lockedPlugin(published, sha, Some(held(sha)))))
            sourced <- Resolve
              .checkPlugins(root, List(PluginRequest.Dep(plugin, LinkStrategy.BinaryOrSource)), Repositories.central)
              .either
          yield assertTrue(
            idle == Right(()),
            missing == Left(ResolveError.NotFound(plugin.organization, published, plugin.revision.value)),
            drifted == Left(
              ResolveError.PluginDrift(
                plugin,
                List(s"lock org.example:$published:${plugin.revision.value}", s"graph org.example:$published:9.9.9"),
              )
            ),
            summed == Left(ResolveError.Checksum(jar, sha, other)),
            agreed == Right(()),
            sourced == Left(ResolveError.Checksum(jar, sha, other)),
          )
        run.provideSomeLayer[Scope](stubbed[Resolve])
      }
    ,
    test("planning a ship pin records the binary, and exact-binary omits sources"):
      val run =
        for
          compiler <- histos.toolchain.BuildCompiler.version.catchAll(error => ZIO.dieMessage(error.message))
          published = ArtifactName.of(plugin, compiler, Platform.Jvm)
          kept = ResolvedArtifact(plugin.organization, published, plugin.revision.value, jar, sha, Repository.Central)
          _ <- stub[Resolve](_.resolvePlugin(any[Dep], any[Repositories]))(ZIO.succeed(Classpath(List(kept), Nil)))
          _ <- stub[Resolve](_.sources(any[String], any[String], any[String], any[Repository]))(
            ZIO.succeed(
              ResolvedArtifact(plugin.organization, published, plugin.revision.value, jar, other, Repository.Central)
            )
          )
          planned <- Resolve.planPlugins(
            List(PluginRequest.Dep(plugin, LinkStrategy.BinaryOrSource)),
            Repositories.central,
          )
          _ <- stub[Resolve](_.sources(any[String], any[String], any[String], any[Repository]))(
            ZIO.dieMessage("sources")
          )
          exact <- Resolve.planPlugins(
            List(PluginRequest.Dep(plugin, LinkStrategy.ExactBinary)),
            Repositories.central,
          )
        yield
          val recorded = planned match
            case cell :: Nil =>
              cell.organization == plugin.organization &&
              cell.name == plugin.name &&
              cell.revision == plugin.revision.value &&
              cell.link == LinkStrategy.BinaryOrSource &&
              cell.artifacts.map(_.name) == List(published) &&
              cell.source.map(_.sha256) == Some(other)
            case _ => false
          val binary = exact match
            case cell :: Nil => cell.link == LinkStrategy.ExactBinary && cell.source.isEmpty
            case _           => false
          assertTrue(recorded, binary)
      run.provideSomeLayer[Scope](stubbed[Resolve]),
  )

  private val rangedDep = Dep.trusted("org.example", "demo", "1.+", DepKind.Java)

  private def module(dependencies: List[Dep]): Module =
    Module(
      name,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(version, Nil),
      dependencies,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )

  private def held(checksum: Sha256): LockedArtifact =
    LockedArtifact("org.example", "demo", "1.0.0", checksum, Repository.Central)

  private def artifact(checksum: Sha256, revision: String = "1.0.0"): ResolvedArtifact =
    ResolvedArtifact("org.example", "demo", revision, jar, checksum, Repository.Central)

  private def record(repo: Path, graph: ProjectGraph, artifacts: List[LockedArtifact]) =
    for
      planned <- LockedTools.plan(repo, graph, NativeOrigin.Central)
      tools = planned.map(pin)
      lock  = DependencyLock(
        List(LockedCell(name, version, Platform.Jvm, Nil, artifacts, Nil)),
        Nil,
        tools,
      )
      _ <- Resolve.write(Resolve.lockPath(repo), lock)
    yield ()

  private def pin(request: ToolRequest): LockedTool =
    val artifacts = request.roots.map: root =>
      val origin = root.origin match
        case Some(repository) => repository
        case None             => Repository.Central
      LockedArtifact(root.organization, root.name, root.revision, sha, origin)
    LockedTool(request.id, request.roots, Repositories.ordered(request.repositories), artifacts, Nil)

  private def lockedPlugin(published: String, checksum: Sha256, source: Option[LockedArtifact]): LockedPlugin =
    LockedPlugin(
      plugin.organization,
      plugin.name,
      plugin.revision.value,
      LinkStrategy.BinaryOrSource,
      Nil,
      List(LockedArtifact(plugin.organization, published, plugin.revision.value, checksum, Repository.Central)),
      source,
    )

  private def writePlugins(repo: Path, plugins: List[LockedPlugin]) =
    Resolve.write(
      Resolve.lockPath(repo),
      DependencyLock(List(LockedCell(name, version, Platform.Jvm, Nil, Nil, Nil)), plugins),
    )
end ResolveAgreeLawSpec
