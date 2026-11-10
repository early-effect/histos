package histos.load

import histos.cache.ContentStore
import histos.plugin.{
  ApiDigest,
  Contribution,
  DeclaredTask,
  HistosPlugin,
  OutputName,
  PluginArgs,
  PluginId,
  PluginOutput,
  PluginRunError,
  PluginTaskId,
  RegisteredPlugin,
  RepoPath,
  RunRequest,
  Seal,
  TaskName,
  WrittenFile,
}
import histos.resolve.{DependencyLock, ResolutionService, Resolve}
import histos.schedule.{ActionCache, RemoteCache}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{ProjectGraph, Splits}

import slayer.{any, stub, stubbed}

import zio.{Scope, ZIO, ZLayer}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, test, suite}

/** A task with no cells runs on this machine. The output is what the plugin returned, and an undeclared file is
  * refused.
  */
object PluginRunLawSpec extends ZIOSpecDefault:
  private val pluginId = PluginId.trusted("law")
  private val pack     = TaskName.trusted("pack")
  private val extra    = TaskName.trusted("extra")

  def spec = suite("plugin run laws")(
    test("an empty task returns the plugin text, and an undeclared file is refused"):
      Workspace.work("histos-plugin-run").catchAll(error => ZIO.dieMessage(error.message)).flatMap { root =>
        val run =
          for
            _     <- stub[ResolutionService](_.current)(ZIO.succeed(DependencyLock(Nil)))
            _     <- stub[Resolve](_.fetch(any[histos.resolve.LockedArtifact]))(ZIO.dieMessage("fetched"))
            found <- ZIO.scoped {
              for
                threads <- PluginThreads.make(ClassLoader.getSystemClassLoader)
                runtime <- PluginRuntime.loaded(
                  root,
                  graph,
                  ClassLoader.getSystemClassLoader,
                  threads,
                  Nil,
                  ApiDigest.trusted("ab" * 32),
                )
                packed  <- runtime.run(PluginTaskId(pluginId, pack), PluginArgs.empty).either
                spilled <- runtime.run(PluginTaskId(pluginId, extra), PluginArgs.empty).either
              yield (packed, spilled)
            }
          yield assertTrue(
            found._1 == Right(PluginOutput("ran", Nil)),
            found._2 == Left(PluginFault.Task(PluginRunError.ExtraOutput("notes/out.txt"))),
          )
        run.provideSomeLayer[Scope](
          ZLayer.succeed(ActionCache.miss) ++
            RemoteCache.off ++
            stubbed[Resolve] ++
            ZLayer.succeed(ContentStore.in(root.resolve("store"))) ++
            stubbed[ResolutionService]
        )
      }
  ) @@ TestAspect.withLiveSystem

  private val graph =
    ProjectGraph.trusted(
      Nil,
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      List(
        RegisteredPlugin(
          pluginId,
          Contribution(List(task(pack), task(extra)), Nil, Nil),
        )(LawPlugin)
      ),
    )

  private def task(name: TaskName): DeclaredTask =
    DeclaredTask(name, Seal.Hermetic, Set.empty, Nil, Nil, Nil)

  private object LawPlugin extends HistosPlugin:
    def id: PluginId             = pluginId
    def contribute               = Contribution(Nil, Nil, Nil)
    def run(request: RunRequest) =
      val files =
        if request.id.name == extra then List(WrittenFile(OutputName.trusted("out"), RepoPath.trusted("notes/out.txt")))
        else Nil
      ZIO.succeed(PluginOutput(if request.id.name == extra then "extra" else "ran", files))
end PluginRunLawSpec
