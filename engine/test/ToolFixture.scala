package histos

import histos.native.{NativeOrigin, NativeToolchain}
import histos.resolve.{
  BuildResolvers,
  DependencyLock,
  LockedTool,
  LockedTools,
  Resolve,
  ResolveError
}
import histos.workspace.{IoFailure, Workspace}
import java.nio.file.Path
import zio.{Scope, ZIO, ZLayer}
import zio.stream.ZStream

object ToolFixture:
  def layer(
      graph: Either[BuildError, ProjectGraph]
  ): ZLayer[Scope, ResolveError | BuildError | IoFailure, LockedTools] =
    ZLayer.scoped:
      for
        root <- Workspace.work("histos-test-tools")
        built <- ZIO.fromEither(graph)
        tools <- locked(root, built).provide(
          Resolve.live(root.resolve("cache"))
        )
      yield tools

  def locked(
      repo: Path,
      graph: ProjectGraph
  ): ZIO[Resolve, ResolveError, LockedTools] =
    for
      _ <- Resolve.update(repo, graph, BuildResolvers(graph.resolvers))
      lock <- Resolve.readLock(repo)
      view <- LockedTools.make(lock, graph)
    yield view

  def jvm(
      first: ScalaVersion,
      rest: ScalaVersion*
  ): ZLayer[Scope, ResolveError | BuildError | IoFailure, LockedTools] =
    layer(Projects(module("compiler", Platform.Jvm)(first, rest*)))

  def nativeLayer(
      toolchain: NativeToolchain
  ): ZLayer[LockedTools, ResolveError | BuildError | IoFailure, LockedTools] =
    ZLayer.scoped:
      for
        current <- ZIO.service[LockedTools]
        root <- Workspace.work("histos-test-native-tools")
        built <- ZIO.fromEither(
          Projects.tools(current.catalog)(
            module("native", Platform.Native)(
              current.catalog.native.bridgeScala
            )
              .clang(ClangMajor(21))
              .nativeVersion(toolchain.revision)
          )
        )
        tools <- (for
          _ <- Resolve.update(
            root,
            built,
            BuildResolvers(built.resolvers),
            NativeOrigin.Chosen(toolchain.source)
          )
          lock <- Resolve.readLock(root)
          view <- LockedTools.make(lock, built)
        yield view).provide(Resolve.live(root.resolve("cache")))
      yield tools

  val resolver: ZLayer[LockedTools, Nothing, Resolve] =
    ZLayer.fromZIO(ZIO.serviceWith[LockedTools](_.resolve))

  def seed(
      repo: Path,
      graph: ProjectGraph,
      native: NativeOrigin = NativeOrigin.Central
  ): ZIO[LockedTools, ResolveError | IoFailure, Unit] =
    for
      tools <- ZIO.service[LockedTools]
      lock <- Resolve
        .readLock(repo)
        .catchSome:
          case ResolveError.NoLock(_) => ZIO.succeed(DependencyLock(Nil))
      _ <- ZIO.when(lock.tools.isEmpty):
        for
          planned <- LockedTools.plan(graph, native)
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
                          artifact.classifier
                        )
                      ),
                      classpath.evictions
                    )
                  )
          )
          _ <- Workspace.writeString(
            Resolve.lockPath(repo),
            DependencyLock.render(lock.copy(tools = selected))
          )
        yield ()
    yield ()

  def at[R, E, A](repo: Path, graph: ProjectGraph)(
      effect: ZIO[R, E, A]
  ): ZIO[R & LockedTools, E | ResolveError | IoFailure, A] =
    seed(repo, graph) *> effect

  def streamAt[R, E, A](repo: Path, graph: ProjectGraph)(
      stream: ZStream[R, E, A]
  ): ZStream[R & LockedTools, E | ResolveError | IoFailure, A] =
    ZStream.fromZIO(seed(repo, graph)) *> stream
