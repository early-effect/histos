package histos.testrun

import histos.*
import histos.build.{
  BuildEnv,
  BuildExecution,
  BuildNode,
  ExecutionError,
  ExecutionLeaves,
  ExecutionProducts,
  ExecutionRequest,
}
import histos.cache.{ContentStore, LinkerStarts}
import histos.jdk.{Machine, ToolchainError}
import histos.resolve.{LockedTools, ResolutionService, Resolve, ResolveError}
import histos.schedule.{ActionCache, Histos, Parallelism, RemoteCache}
import histos.toolchain.Runtimes
import histos.workspace.{IoFailure, Workspace}
import histos.zinc.{CompileMemo, CompileSession, CompilerStarts, Compilers, GraphCompile, GraphCompileError}

import java.nio.file.Path

import zio.{Scope, System, ZIO, ZLayer}

private[histos] object TestExecutionFixtures:
  val version = ScalaVersion("3.10.1")
  val nightly = NativeVersion("0.5.13-20261122-fa4437d-SNAPSHOT")
  type Env = BuildEnv & LinkerStarts & LockedTools & ProviderInstall

  final case class Fixture(
      repo: Path,
      session: CompileSession,
      runtimes: Runtimes,
      execution: BuildExecution[Env, ExecutionError],
      scheduler: Histos,
  ):
    def ensure(roots: List[BuildNode]): ZIO[Env & Scope, ExecutionError, ExecutionProducts] =
      execution.ensure(ExecutionRequest(session, roots))

    def ensureGenerated(roots: List[BuildNode])(
        generate: (ExecutionRequest, histos.plugin.PluginTaskId, ExecutionProducts) => ZIO[
          Env & Scope,
          ExecutionError,
          histos.build.GeneratedTask[ExecutionError],
        ]
    ): ZIO[Env & Scope, ExecutionError, ExecutionProducts] =
      val leaves = ExecutionLeaves[Env, ExecutionError](
        GraphCompile.stage,
        TestCompile.stage,
        generate,
        (_, cell, mode, _) => ZIO.fail(ExecutionError.Dependency(BuildNode.Link(cell, mode))),
      )
      new BuildExecution(leaves, scheduler).ensure(ExecutionRequest(session, roots))
  end Fixture

  def withGraph[E, A](
      graph: ProjectGraph,
      resolve: ZLayer[Any, Nothing, Resolve] = Resolve.coursier,
  )(use: Fixture => ZIO[Env & Scope, E, A]): ZIO[
    Any,
    E | IoFailure | ToolchainError | ResolveError | GraphCompileError,
    A,
  ] =
    val effect = ZIO.scoped:
      for
        repo       <- Workspace.work("histos-test-execution")
        machine    <- Machine.load(repo)
        jdk        <- GraphCompile.jdk(machine)
        runtimes   <- Runtimes.read(machine, None)
        resolution <- ResolutionService.open(repo, graph)
        lock       <- resolution.current
        tools      <- LockedTools.make(lock, graph, resolution)
        store      <- Workspace.createDirectories(repo.resolve("cas"))
        provider   <- ProviderInstall.make
        scheduler  <- Histos.live.build.provideSomeLayer[Scope](ZLayer.succeed(Parallelism.default)).map(_.get[Histos])
        leaves = ExecutionLeaves[Env, ExecutionError](
          GraphCompile.stage,
          TestCompile.stage,
          (_, task, _) => ZIO.fail(ExecutionError.Dependency(BuildNode.Generate(task))),
          (_, cell, mode, _) => ZIO.fail(ExecutionError.Dependency(BuildNode.Link(cell, mode))),
        )
        fixture = Fixture(
          repo,
          CompileSession(repo, graph, jdk, runtimes.native, lock),
          runtimes,
          new BuildExecution(leaves, scheduler),
          scheduler,
        )
        result <- use(fixture).provideSome[Resolve & Scope](
          ZLayer.succeed(resolution),
          ZLayer.succeed(tools),
          ZLayer.succeed(provider),
          ZLayer.succeed(ContentStore.in(store)),
          ZLayer.succeed(ActionCache.miss),
          RemoteCache.off,
          CompilerStarts.live,
          Compilers.live,
          CompileMemo.live,
          LinkerStarts.live,
        )
      yield result
    ZIO.withSystem(System.SystemLive)(effect).provide(resolve)
end TestExecutionFixtures
