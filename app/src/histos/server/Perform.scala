package histos.server

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.load.{Loader, LoadedBuild}
import histos.publish.IvyRoot
import histos.resolve.Resolve
import histos.report.{BuildFault, CallResult, Rejection}
import histos.task.RunId
import histos.zinc.{GraphCompile, GraphCompileError}

import java.nio.file.Path

import zio.{Fiber, Ref, UIO, ZIO}

/** One execution of a [[histos.server.Call]]. Reload, reboot, bsp, and watch stay on [[Perform.Edges]] because they own
  * the process, the socket, or the session. A missing graph is a fault. A fault is [[histos.report.CallResult.Fault]],
  * not a failed effect.
  */
object Perform:
  /** `compiled` is what a program run signals once its compile is done. The server releases its class-directory lock
    * then, so a restart can compile while the program runs.
    */
  final case class Edges[R](
      watch: (Call, WatchPolicy, Say) => ZIO[R, ServerError, Unit],
      reload: ZIO[R, ServerError, Unit],
      reboot: ZIO[R, ServerError, Unit],
      bsp: ZIO[R, ServerError, Unit],
      compiled: UIO[Unit],
  )

  /** The arm reads the adopted graph. Version, cache, resolve, and an empty clean do not. */
  def needsGraph(call: Call): Boolean =
    call match
      case Call.Compile(_) | Call.Test(_, _, _) | Call.PublishLocal(_) | Call.PublishSnapshots(_) |
          Call.PublishRelease(_, _) | Call.PublishAssets(_) | Call.PublishDrop(_) | Call.Graph | Call.Run(_, _) |
          Call.Client | Call.Task(_, _) | Call.Toolchain | Call.Workflow(_) | Call.Format(_, _) | Call.Assembly(_) |
          Call.PreviewCompile(_) | Call.PreviewTest(_) | Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) |
          Call.PreviewRun(_, _) =>
        true
      case Call.Clean(Nil) | Call.PreviewClean(Nil) =>
        false
      case Call.Clean(_) | Call.PreviewClean(_) =>
        true
      case Call.PublishPromote(_) | Call.Cancel(_) | Call.Resolve(_) | Call.CleanFull | Call.Reload | Call.Reboot |
          Call.Version | Call.CachePath | Call.CacheList | Call.CacheClean(_) | Call.CachePromote(_, _) |
          Call.Launcher(_) | Call.Watch(_, _) | Call.Bsp | Call.PreviewCleanFull =>
        false

  /** Publish keeps the compile fault. Every other call reports a cell that ran, and a compile that never started is its
    * fault: a lock is a resolve fault.
    */
  def jdkMissed(call: Call, error: GraphCompileError): CallResult =
    call match
      case Call.PublishLocal(_) | Call.PublishSnapshots(_) | Call.PublishRelease(_, _) | Call.PreviewPublishLocal(_) |
          Call.PreviewPublishSnapshots(_) =>
        CallResult.Fault(BuildFault.Compile(error))
      case _ =>
        Faults.compileStopped(error) match
          case Some(why) => CallResult.Fault(why)
          case None      => CallResult.reported(error.message)

  def run[R](
      repo: Path,
      apiJar: Path,
      loaded: Either[BuildFault, Option[LoadedBuild]],
      machine: Machine,
      runs: Ref[Map[RunId, Fiber[ServerError, Unit]]],
      track: Boolean,
      version: String,
      call: Call,
      say: Say,
      edges: Edges[R],
  ): ZIO[
    R & Resolve & Loader & ContentStore & histos.zinc.Compilers & histos.zinc.CompileMemo &
      histos.resolve.ResolutionService & histos.format.Formatters & IvyRoot,
    ServerError,
    Unit,
  ] =
    loaded match
      case Left(fault) => Calls.finish(say, CallResult.Fault(fault))
      case Right(have) =>
        def follow[R1](body: ZIO[R1, ServerError, Unit]): ZIO[R1, ServerError, Unit] =
          if track then Calls.tracked(runs, say)(body) else body

        def held[R1](use: LoadedBuild => ZIO[R1, ServerError, Unit]): ZIO[R1, ServerError, Unit] =
          have match
            case Some(build) => use(build)
            case None        =>
              Calls.finish(say, CallResult.Fault(BuildFault.Rejected(Rejection.RunFailed)))

        def withJdk[R1](
            use: histos.jdk.JdkRuntime => ZIO[R1, ServerError, Unit]
        ): ZIO[R1, ServerError, Unit] =
          GraphCompile
            .jdk(machine)
            .foldZIO(
              error => Calls.finish(say, jdkMissed(call, error)),
              use,
            )

        call match
          case Call.Compile(projects) =>
            held(build => follow(withJdk(jdk => Calls.compile(repo, build.graph, projects, jdk, machine, say))))
          case Call.Test(projects, patterns, mode) =>
            held(build =>
              follow(
                withJdk(jdk => Calls.test(repo, build.graph, projects, patterns, mode, jdk, machine, say))
              )
            )
          case Call.PublishLocal(projects) =>
            held(build =>
              follow(
                withJdk(jdk =>
                  Calls.publishLocal(
                    repo,
                    build.graph,
                    build.build,
                    machine,
                    jdk,
                    projects,
                    say,
                    build.plugins.launcher,
                  )
                )
              )
            )
          case Call.PublishSnapshots(projects) =>
            held(build =>
              follow(
                withJdk(jdk =>
                  Calls.publishSnapshots(
                    repo,
                    build.graph,
                    build.build,
                    machine,
                    jdk,
                    projects,
                    say,
                    build.plugins.launcher,
                  )
                )
              )
            )
          case Call.PublishRelease(projects, automatic) =>
            held(build =>
              follow(
                withJdk(jdk =>
                  Calls.publishRelease(
                    repo,
                    build.graph,
                    build.build,
                    machine,
                    jdk,
                    projects,
                    automatic,
                    say,
                    build.plugins.launcher,
                  )
                )
              )
            )
          case Call.PublishPromote(id) =>
            follow(Calls.promote(repo, machine, id, say))
          case Call.PublishDrop(id) =>
            held(build => follow(Calls.drop(repo, build.graph, machine, id, say)))
          case Call.PublishAssets(tokens) =>
            held(build => follow(Calls.publishAssets(repo, build.graph, machine, tokens, say)))
          case Call.Assembly(destination) =>
            held(build => follow(withJdk(jdk => Calls.assembly(repo, build.graph, jdk, machine, destination, say))))
          case Call.Client =>
            held(build => follow(withJdk(jdk => Calls.client(repo, build.graph, jdk, machine, say))))
          case Call.Run(id, platform) =>
            held(build =>
              follow(withJdk(jdk => Calls.run(repo, build.graph, jdk, machine, id, platform, say, edges.compiled)))
            )
          case Call.Task(id, args) =>
            held(build => follow(Calls.task(repo, build.graph, build.plugins, machine, id, args, say)))
          case Call.Graph =>
            held(build => Calls.graph(build.graph, build.plugins, say))
          case Call.Cancel(id) =>
            Calls.cancel(runs, id, say)
          case Call.Toolchain =>
            held(build => Calls.toolchain(machine, build.graph, say))
          case Call.Resolve(action) =>
            Calls.resolve(repo, apiJar, action, say, machine.native)
          case Call.Workflow(action) =>
            held(build => Calls.workflow(repo, build, action, say))
          case Call.Format(targets, mode) =>
            held(build => follow(Calls.format(repo, build.graph, targets, mode, say)))
          case Call.Clean(Nil) =>
            Calls.wipe(repo, say)
          case Call.Clean(projects) =>
            held(build => Calls.clean(repo, build.graph, projects, say))
          case Call.CleanFull =>
            Calls.cleanFull(repo, say)
          case Call.PreviewCompile(projects) =>
            held(build => Calls.previewCompile(repo, build.graph, projects, say))
          case Call.PreviewTest(projects) =>
            held(build => Calls.previewTest(repo, build.graph, projects, say))
          case Call.PreviewPublishLocal(projects) =>
            held(build => Calls.previewPublishLocal(repo, build.graph, projects, machine.env, say))
          case Call.PreviewPublishSnapshots(projects) =>
            held(build => Calls.previewPublishSnapshots(repo, build.graph, projects, machine.env, say))
          case Call.PreviewClean(Nil) =>
            Calls.previewCleanAll(repo, say)
          case Call.PreviewClean(projects) =>
            held(build => Calls.previewClean(repo, build.graph, projects, say))
          case Call.PreviewCleanFull =>
            Calls.previewCleanFull(repo, say)
          case Call.PreviewRun(id, platform) =>
            held(build => Calls.previewRun(build.graph, id, platform, say))
          case Call.Version =>
            Calls.version(repo, version, say)
          case Call.CachePath =>
            Calls.cachePath(say)
          case Call.CacheList =>
            Calls.cacheList(say)
          case Call.CacheClean(mode) =>
            Calls.cacheClean(mode, say)
          case Call.CachePromote(from, to) =>
            Calls.cachePromote(from, to, say)
          case Call.Launcher(action) =>
            Calls.launcher(repo, action, say)
          case Call.Watch(inner, policy) =>
            edges.watch(inner, policy, say)
          case Call.Reload =>
            edges.reload
          case Call.Reboot =>
            edges.reboot
          case Call.Bsp =>
            edges.bsp
end Perform
