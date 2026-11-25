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
    * then, so a restart can compile while the program runs. `reboot` starts the successor the call names.
    */
  final case class Edges[R](
      watch: (Call, WatchPolicy, Say) => ZIO[R, ServerError, Unit],
      reload: ZIO[R, ServerError, Unit],
      reboot: Successor => ZIO[R, ServerError, Unit],
      stop: ZIO[R, ServerError, Unit],
      bsp: ZIO[R, ServerError, Unit],
      compiled: UIO[Unit],
  )

  /** Clean adopts generated ownership. Graph-free recovery is a local launcher fallback. */
  def needsGraph(call: Call): Boolean =
    call match
      case Call.Compile(_, _, _) | Call.Test(_, _, _, _) | Call.PublishCheck(_, _) | Call.PublishLocal(_, _) |
          Call.PublishWithdraw | Call.PublishSnapshots(_) | Call.PublishRelease(_, _) | Call.PublishAssets(_) |
          Call.PublishDrop(_) | Call.Graph | Call.Run(_, _, _, _, _, _) | Call.Task(_, _) | Call.Workflow(_) |
          Call.Format(_, _) | Call.Assembly(_) | Call.PreviewCompile(_, _, _) | Call.PreviewTest(_, _, _, _) |
          Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) | Call.PreviewRun(_, _, _, _) |
          Call.CacheVerify(_) | Call.Catalog(_) =>
        true
      case Call.Clean(Nil, _) =>
        true
      case Call.PreviewClean(Nil, _) =>
        false
      case Call.Clean(_, CleanExtent.Full) | Call.PreviewClean(_, CleanExtent.Full) =>
        false
      case Call.Clean(_, _) | Call.PreviewClean(_, _) =>
        true
      case Call.PublishPromote(_) | Call.Cancel(_) | Call.Resolve(_) | Call.Reload | Call.Reboot(_) | Call.Stop |
          Call.Version | Call.CachePath | Call.CacheList | Call.CacheClean(_) | Call.CachePromote(_, _) |
          Call.CacheForget(_) | Call.Watch(_, _) | Call.Bsp =>
        false

  /** Publish keeps the compile fault. Every other call reports a cell that ran, and a compile that never started is its
    * fault: a lock is a resolve fault.
    */
  def jdkMissed(call: Call, error: GraphCompileError): CallResult =
    call match
      case Call.PublishCheck(_, _) | Call.PublishLocal(_, _) | Call.PublishSnapshots(_) | Call.PublishRelease(_, _) |
          Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) =>
        CallResult.Fault(BuildFault.Compile(error))
      case _ => notCompiled(error)

  /** A compile that never started, outside a publish. A server that finds no JDK reports the same way. */
  def notCompiled(error: GraphCompileError): CallResult =
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
      desk: ConsentDesk,
      relay: histos.process.RunRelay = histos.process.RunRelay.Closed,
      caller: Option[histos.plugin.InvocationContext] = None,
      events: histos.plugin.PluginEvents.Invocation = histos.plugin.PluginEvents.closedInvocation,
      lock: Option[histos.lock.LockMode] = None,
  ): ZIO[
    R & Resolve & Loader & ContentStore & histos.zinc.Compilers & histos.zinc.CompileMemo &
      histos.resolve.ResolutionService & histos.format.Formatters & IvyRoot,
    ServerError,
    Unit,
  ] =
    LockDisposition.unapplied(call, lock) match
      case Some(mode) =>
        Calls.finish(say, CallResult.Fault(BuildFault.Resolve(LockDisposition.refused(mode))))
      case None =>
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
              case Call.Compile(projects, platforms, versions) =>
                held(build =>
                  follow(
                    withJdk(jdk =>
                      Calls.compile(
                        repo,
                        build.graph,
                        projects,
                        jdk,
                        machine,
                        say,
                        build.plugins,
                        platforms,
                        versions,
                      )
                    )
                  )
                )
              case Call.Test(projects, patterns, mode, filter) =>
                held(build =>
                  follow(
                    withJdk(jdk =>
                      Calls.test(repo, build.graph, projects, patterns, mode, filter, jdk, machine, say, build.plugins)
                    )
                  )
                )
              case Call.PublishWithdraw =>
                held(build => follow(Calls.withdraw(repo, build.graph, machine, say)))
              case Call.PublishCheck(tokens, scope) =>
                held(build =>
                  follow(
                    withJdk(jdk =>
                      Calls.publishCheck(
                        repo,
                        build.graph,
                        machine,
                        jdk,
                        tokens.map(_.value),
                        scope,
                        say,
                        build.plugins.launcher,
                        Some(build.plugins),
                      )
                    )
                  )
                )
              case Call.PublishLocal(tokens, scope) =>
                held(build =>
                  follow(
                    withJdk(jdk =>
                      Calls.publishLocal(
                        repo,
                        build.graph,
                        build.build,
                        machine,
                        jdk,
                        tokens.map(_.value),
                        scope,
                        say,
                        build.plugins.launcher,
                        Some(build.plugins),
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
                        Some(build.plugins),
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
                        Some(build.plugins),
                      )
                    )
                  )
                )
              case Call.PublishPromote(id) =>
                follow(Calls.promote(repo, machine, id.value, say))
              case Call.PublishDrop(id) =>
                held(build => follow(Calls.drop(repo, build.graph, machine, id.value, say)))
              case Call.PublishAssets(tokens) =>
                held(build => follow(Calls.publishAssets(repo, build.graph, machine, tokens, say)))
              case Call.Assembly(modules) =>
                held(build => follow(withJdk(jdk => Calls.assembly(repo, build.graph, jdk, machine, modules, say))))
              case Call.Run(id, platform, launch, args, main, versions) =>
                held(build =>
                  follow(
                    withJdk(jdk =>
                      Calls.run(
                        repo,
                        build.graph,
                        jdk,
                        machine,
                        id,
                        platform,
                        launch,
                        args,
                        say,
                        edges.compiled,
                        relay,
                        main,
                        Some(build.plugins),
                        versions,
                      )
                    )
                  )
                )
              case Call.Task(id, args) =>
                held(build =>
                  follow(Calls.task(repo, build.graph, build.plugins, machine, id, args, say, caller, events))
                )
              case Call.Graph =>
                held(build => Calls.graph(build.graph, build.plugins, say))
              case Call.Cancel(id) =>
                Calls.cancel(runs, id, say)
              case Call.Resolve(action) =>
                Calls.resolve(repo, apiJar, action, say, machine.native, desk, caller, lock)
              case Call.Workflow(action) =>
                held(build => Calls.workflow(repo, build, action, say))
              case Call.Format(targets, mode) =>
                held(build => follow(Calls.format(repo, build.graph, targets, mode, say)))
              case Call.Clean(Nil, CleanExtent.Products) =>
                have match
                  case Some(build) => Calls.clean(repo, build.graph, Nil, say, build.plugins)
                  case None        => Calls.wipe(repo, say)
              case Call.Clean(Nil, CleanExtent.Full) =>
                Calls.cleanFull(repo, say, have)
              case Call.Clean(projects, CleanExtent.Products) =>
                held(build => Calls.clean(repo, build.graph, projects, say, build.plugins))
              case Call.Clean(_, CleanExtent.Full) =>
                Calls.finish(say, CallResult.Fault(BuildFault.Rejected(Rejection.PreviewProjects)))
              case Call.PreviewCompile(projects, platforms, versions) =>
                held(build =>
                  Calls.previewCompile(repo, build.graph, projects, say, build.plugins, platforms, versions)
                )
              case Call.PreviewTest(projects, patterns, _, _) =>
                held(build => Calls.previewTest(repo, build.graph, projects, patterns, say, build.plugins))
              case Call.PreviewPublishLocal(tokens) =>
                held(build => Calls.previewPublishLocal(repo, build.graph, tokens.map(_.value), machine.env, say))
              case Call.PreviewPublishSnapshots(projects) =>
                held(build => Calls.previewPublishSnapshots(repo, build.graph, projects, machine.env, say))
              case Call.PreviewClean(Nil, CleanExtent.Products) =>
                Calls.previewCleanAll(repo, say)
              case Call.PreviewClean(Nil, CleanExtent.Full) =>
                Calls.previewCleanFull(repo, say)
              case Call.PreviewClean(projects, CleanExtent.Products) =>
                held(build => Calls.previewClean(repo, build.graph, projects, say))
              case Call.PreviewClean(_, CleanExtent.Full) =>
                Calls.finish(say, CallResult.Fault(BuildFault.Rejected(Rejection.PreviewProjects)))
              case Call.PreviewRun(id, platform, main, versions) =>
                held(build => Calls.previewRun(build.graph, id, platform, say, main, versions))
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
              case Call.CacheForget(target) =>
                Calls.cacheForget(repo, target, say)
              case Call.CacheVerify(projects) =>
                held(build => follow(withJdk(jdk => Calls.cacheVerify(repo, build.graph, projects, jdk, machine, say))))
              case Call.Watch(inner, policy) =>
                edges.watch(inner, policy, say)
              case Call.Reload =>
                edges.reload
              case Call.Reboot(onto) =>
                edges.reboot(onto)
              case Call.Stop =>
                edges.stop
              case Call.Bsp =>
                edges.bsp
              case Call.Catalog(query) =>
                held(build => Calls.catalog(build.graph, query, say))
end Perform
