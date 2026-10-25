package histos.resolve

import histos.*
import histos.jdk.Sha256
import histos.workspace.Workspace
import zio.{Chunk, IO, Ref, ZIO, ZLayer}
import zio.test.*

object ToolFixtureSpec extends ZIOSpecDefault:
  def spec = suite("scoped tool fixtures")(
    test(
      "the requested graph and file-only resolver policy reach the supplied adapter"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-fixture-policy")
          url  <- ZIO.fromEither(
            RepositoryUrl.from(root.resolve("maven").toUri.toASCIIString)
          )
          graph <- ZIO.fromEither(
            Projects.resolvers(Resolver.Maven(url))(
              module("compiler", Jvm)(ScalaVersion("3.10.0"))
            )
          )
          expected <- LockedTools.plan(
            graph,
            histos.native.NativeOrigin.Central,
          )
          calls <- Ref.make(List.empty[ToolRequest])
          repositories = BuildResolvers(graph.resolvers)
          digest       = Sha256.of(Chunk.single(1.toByte))
          resolver     = new Resolve:
            def resolveTool(request: ToolRequest): IO[ResolveError, Classpath] =
              if request.repositories != repositories || !expected.contains(
                  request
                )
              then ZIO.fail(ResolveError.ToolDrift(request.id))
              else
                calls
                  .update(_ :+ request)
                  .as(
                    Classpath(
                      request.roots.map(tool =>
                        ResolvedArtifact(
                          tool.organization,
                          tool.name,
                          tool.revision,
                          root.resolve("cache").resolve(tool.name + ".jar"),
                          digest,
                          repositories.first,
                        )
                      ),
                      Nil,
                    )
                  )
            def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
              if cell.repositories == repositories && cell.dependencies.isEmpty
              then ZIO.succeed(Classpath(Nil, Nil))
              else ZIO.fail(ResolveError.NoLock(root))
            def fetch(
                artifact: LockedArtifact
            ): IO[ResolveError, ResolvedArtifact] =
              ZIO.fail(ResolveError.NoLock(root))
            def resolvePlugin(
                dependency: Dep,
                repositories: Repositories,
            ): IO[ResolveError, Classpath] =
              ZIO.fail(ResolveError.NoLock(root))
            def sources(
                organization: String,
                name: String,
                revision: String,
                repository: Repository,
            ): IO[ResolveError, ResolvedArtifact] =
              ZIO.fail(ResolveError.NoLock(root))
          view <- ToolFixture
            .locked(root, graph)
            .provide(ZLayer.succeed(resolver))
          used <- calls.get
        yield assertTrue(
          used == expected,
          view.repositories == repositories,
          view.lock.tools.map(_.id) == expected.map(_.id),
          view.lock.tools.forall(_.repositories == repositories.all),
          view.lock.tools
            .flatMap(_.artifacts)
            .forall(_.repository == repositories.first),
        )
  )
