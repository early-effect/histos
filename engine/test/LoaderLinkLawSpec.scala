package histos.load

import histos.cache.ContentStore
import histos.jdk.{Sha256, Sha256Digest}
import histos.plugin.{ApiDigest, HistosPlugin, LinkStrategy, PluginCoordinate, PluginLoadError}
import histos.resolve.{
  ArtifactName,
  DependencyLock,
  LockedArtifact,
  LockedPlugin,
  Repository,
  Resolve,
  ResolveError,
  ResolvedArtifact,
}
import histos.toolchain.BuildCompiler
import histos.workspace.Workspace
import histos.{Dep, DepKind, Platform, ProjectPath}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import slayer.{Stubbed, any, stub, stubbed}

import zio.{Chunk, Scope, ZIO, ZLayer}
import zio.test.*

/** The lock chooses the link. A header that only names the coordinate still has to obey that choice.
  */
object LoaderLinkLawSpec extends ZIOSpecDefault:
  private val dep      = Dep.trusted("org", "demo", "1.0.0", DepKind.Binary)
  private val where    = PluginCoordinate.Published(dep)
  private val otherSha = Sha256.trusted("cd" * 32)
  private val wrong    = ApiDigest.trusted("ab" * 32)

  def spec = suite("loader link laws")(
    test("a missing pin, a refused strategy, and a jar that is not the direct artifact stop before a class is defined"):
      Workspace.work("histos-loader-link").flatMap { root =>
        val store = ContentStore.in(root.resolve("store"))
        val run   =
          for
            api   <- LoaderApiJar.of(classOf[histos.HistosBuild], root.resolve("histos-api.jar"))
            bytes <- ZIO.attemptBlocking(Files.readAllBytes(api)).orDie
            sha = Sha256Digest.of(Chunk.fromArray(bytes))
            launcher <- ZIO.fromEither(ApiDigest.from(sha.value)).catchAll(error => ZIO.dieMessage(error.message))
            version  <- BuildCompiler.version.catchAll(error => ZIO.dieMessage(error.message))
            published = ArtifactName.of(dep, version, Platform.Jvm)
            mismatch <- pack(root.resolve("mismatch.jar"), wrong.value)
            leaked   <- pack(root.resolve("leak.jar"), launcher.value, leak = true)
            matched  <- pack(root.resolve("match.jar"), launcher.value, leak = false)
            sources  <- pack(root.resolve("sources.jar"), launcher.value, leak = false)
            _        <- Workspace
              .writeString(root.resolve("histos.scala"), pathHeader)
              .catchAll(error => ZIO.dieMessage(error.message))
            _ <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.dieMessage("fetched"))
            _ <- stub[Resolve](_.sources(any[String], any[String], any[String], any[Repository]))(
              ZIO.dieMessage("sources")
            )
            missingDir <- load(root, api, DependencyLock(Nil))
            _          <- Workspace
              .writeString(root.resolve("histos.scala"), depHeader)
              .catchAll(error => ZIO.dieMessage(error.message))
            unlocked <- load(root, api, DependencyLock(Nil))
            apiLink  <- load(root, api, lock(published, LinkStrategy.Api, None, sha))
            _        <- stub[Resolve](_.fetch(any[LockedArtifact]))(
              ZIO.succeed(fetched("not-the-plugin", root.resolve("nope.jar"), sha))
            )
            unnamed  <- load(root, api, lock(published, LinkStrategy.ExactBinary, None, sha))
            _        <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.succeed(fetched(published, mismatch, sha)))
            differed <- load(root, api, lock(published, LinkStrategy.ExactBinary, None, sha))
            _        <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.succeed(fetched(published, leaked, sha)))
            engine   <- load(root, api, lock(published, LinkStrategy.ExactBinary, None, sha))
            _        <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.succeed(fetched(published, matched, sha)))
            noSource <- load(root, api, lock(published, LinkStrategy.Source, None, sha))
            _        <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.succeed(fetched(published, mismatch, sha)))
            either   <- load(root, api, lock(published, LinkStrategy.BinaryOrSource, None, sha))
            wanted = LockedArtifact("org.example", "demo", "1.0.0", sha, Repository.Central)
            _ <- stub[Resolve](_.sources(any[String], any[String], any[String], any[Repository]))(
              ZIO.succeed(fetched("demo", sources, otherSha))
            )
            checksum <- load(root, api, lock(published, LinkStrategy.Source, Some(wanted), sha))
          yield assertTrue(
            missingDir == Left(LoadError.Plugin(PluginLoadError.NoEntry(PluginCoordinate.Path(missingPath).render))),
            unlocked == Left(LoadError.Plugin(PluginLoadError.Unlocked(where.render))),
            apiLink == Left(LoadError.Plugin(PluginLoadError.ApiLink(dep))),
            unnamed == Left(LoadError.Plugin(PluginLoadError.NoEntry(where.render))),
            differed == Left(LoadError.Plugin(PluginLoadError.VersionMismatch(dep, wrong, launcher))),
            engine == Left(LoadError.Plugin(PluginLoadError.EngineLeak(where, "histos.Foo"))),
            noSource == Left(LoadError.Plugin(PluginLoadError.NoSource(where))),
            either == Left(LoadError.Plugin(PluginLoadError.NoSource(where))),
            checksum == Left(
              LoadError.Resolve(ResolveError.Checksum(sources, sha, otherSha))
            ),
          )
        run.provideSomeLayer[Scope](wired(store))
      }
    ,
    test("a binary match compiles the build and then the missing entry is a defect"):
      Workspace.work("histos-loader-define").flatMap { root =>
        val store = ContentStore.in(root.resolve("store"))
        val run   =
          for
            api   <- LoaderApiJar.of(classOf[histos.HistosBuild], root.resolve("histos-api.jar"))
            bytes <- ZIO.attemptBlocking(Files.readAllBytes(api)).orDie
            sha = Sha256Digest.of(Chunk.fromArray(bytes))
            launcher <- ZIO.fromEither(ApiDigest.from(sha.value)).catchAll(error => ZIO.dieMessage(error.message))
            version  <- BuildCompiler.version.catchAll(error => ZIO.dieMessage(error.message))
            published = ArtifactName.of(dep, version, Platform.Jvm)
            jar <- pack(root.resolve("match.jar"), launcher.value, leak = false)
            _   <- Workspace
              .writeString(root.resolve("histos.scala"), build)
              .catchAll(error => ZIO.dieMessage(error.message))
            _ <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.succeed(fetched(published, jar, sha)))
            _ <- stub[Resolve](_.sources(any[String], any[String], any[String], any[Repository]))(
              ZIO.dieMessage("sources")
            )
            defined <- load(root, api, lock(published, LinkStrategy.ExactBinary, None, sha))
          yield assertTrue(
            defined == Left(
              LoadError.Plugin(PluginLoadError.Defect(where, "java.lang.ClassNotFoundException"))
            )
          )
        run.provideSomeLayer[Scope](wired(store))
      },
  )

  private val missingPath = ProjectPath.trusted("plugins/missing")

  private val pathHeader =
    """plugin(path"plugins/missing")
      |""".stripMargin

  private val depHeader =
    """plugin(dep"org::demo:1.0.0")
      |""".stripMargin

  private val build =
    """plugin(dep"org::demo:1.0.0")
      |import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val loaderlaw = module("loaderlaw", Jvm)(scala"3.10.0")
      |    Projects(loaderlaw)
      |""".stripMargin

  private def wired(store: ContentStore) =
    (stubbed[Resolve] ++ ZLayer.succeed(store)) >>> (Loader.live ++ ZLayer.environment[Stubbed[Resolve]])

  private def load(repo: Path, api: Path, locked: DependencyLock) =
    ZIO.scoped(ZIO.serviceWithZIO[Loader](_.loadLocked(repo, api, locked).either))

  private def lock(
      published: String,
      link: LinkStrategy,
      source: Option[LockedArtifact],
      sha: Sha256,
  ): DependencyLock =
    DependencyLock(
      Nil,
      List(
        LockedPlugin(
          dep.organization,
          dep.name,
          dep.revision,
          link,
          Nil,
          List(LockedArtifact(dep.organization, published, dep.revision, sha, Repository.Central)),
          source,
        )
      ),
    )

  private def fetched(name: String, path: Path, sha: Sha256): ResolvedArtifact =
    ResolvedArtifact(dep.organization, name, dep.revision, path, sha, Repository.Central)

  private def pack(path: Path, version: String, leak: Boolean = false): zio.UIO[Path] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      val out = new ZipOutputStream(Files.newOutputStream(path))
      try
        write(out, HistosPlugin.EntryResourceName, "demo.Plugin$\n")
        write(out, ApiDigest.resource, version + "\n")
        if leak then write(out, "histos/Foo.class", "x")
      finally out.close()
      path
    }.orDie

  private def write(out: ZipOutputStream, name: String, text: String): Unit =
    out.putNextEntry(new ZipEntry(name))
    out.write(text.getBytes(StandardCharsets.UTF_8))
    out.closeEntry()
end LoaderLinkLawSpec
