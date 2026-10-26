package histos

import histos.resolve.{BuildResolvers, DependencyLock, LockedTool, LockedTools, Resolve, ResolveError}
import histos.workspace.{IoFailure, Workspace}
import java.nio.file.Path
import zio.{Scope, ULayer, ZIO, ZLayer}

object AppToolFixture:
  def layer(
      graph: Either[BuildError, ProjectGraph]
  ): ZLayer[Scope, ResolveError | BuildError | IoFailure, LockedTools] =
    ZLayer.scoped:
      for
        root  <- Workspace.work("histos-app-test-tools")
        built <- ZIO.fromEither(graph)
        tools <- (for
          _    <- Resolve.update(root, built, BuildResolvers(built.resolvers))
          lock <- Resolve.readLock(root)
          view <- LockedTools.make(lock, built)
        yield view).provide(Resolve.coursier)
      yield tools

  /** Coursier's cache. A coordinate already there is not downloaded again. */
  val resolve: ULayer[Resolve] = Resolve.coursier

  val resolver: ZLayer[LockedTools, Nothing, Resolve] =
    ZLayer.fromZIO(ZIO.serviceWith[LockedTools](_.resolve))

  def at[R, E, A](repo: Path, graph: ProjectGraph)(
      effect: ZIO[R, E, A]
  ): ZIO[R & LockedTools, E | ResolveError | IoFailure, A] =
    for
      tools <- ZIO.service[LockedTools]
      lock  <- Resolve
        .readLock(repo)
        .catchSome:
          case ResolveError.NoLock(_) => ZIO.succeed(DependencyLock(Nil))
      _ <- ZIO.when(lock.tools.isEmpty):
        for
          planned  <- LockedTools.plan(graph, histos.native.NativeOrigin.Central)
          selected <- ZIO.foreach(planned)(request =>
            tools.lock.tools.find(cell =>
              cell.id == request.id && LockedTools
                .validate(request, cell)
                .isRight
            ) match
              case Some(cell) => ZIO.succeed(cell)
              case None       =>
                tools.resolve
                  .resolveTool(request)
                  .map(classpath =>
                    LockedTool(
                      request.id,
                      request.roots,
                      request.repositories.all,
                      classpath.artifacts.map(artifact =>
                        histos.resolve.LockedArtifact(
                          artifact.organization,
                          artifact.name,
                          artifact.revision,
                          artifact.sha256,
                          artifact.repository,
                          artifact.classifier,
                        )
                      ),
                      classpath.evictions,
                    )
                  )
          )
          _ <- Workspace.writeString(
            Resolve.lockPath(repo),
            DependencyLock.render(lock.copy(tools = selected)),
          )
        yield ()
      result <- effect
    yield result
end AppToolFixture
