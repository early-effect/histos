package histos.resolve

import histos.Dep

import java.nio.file.Path

import zio.{ULayer, ZIO, ZLayer}

/** A resolve layer for a test that does not compile. A real compile fails here
  * instead of fetching.
  */
object IdleResolve:
  val layer: ULayer[Resolve] =
    ZLayer.succeed(new Resolve:
      private def stop[A]: ZIO[Any, ResolveError, A] =
        ZIO.fail(ResolveError.NoLock(Path.of("idle")))

      def resolveTool(request: ToolRequest): ZIO[Any, ResolveError, Classpath] =
        stop

      def resolve(cell: ResolveCell): ZIO[Any, ResolveError, Classpath] = stop

      def fetch(
          artifact: LockedArtifact
      ): ZIO[Any, ResolveError, ResolvedArtifact] = stop

      def resolvePlugin(
          dependency: Dep,
          repositories: Repositories
      ): ZIO[Any, ResolveError, Classpath] = stop

      def sources(
          organization: String,
          name: String,
          revision: String,
          repository: Repository
      ): ZIO[Any, ResolveError, ResolvedArtifact] =
        stop)
end IdleResolve
