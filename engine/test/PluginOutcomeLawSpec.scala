package histos.load

import histos.cache.ContentStore
import histos.jdk.{Sha256, Sha256Digest}
import histos.plugin.{
  ApiDigest,
  Contribution,
  DeclaredOutput,
  DeclaredTask,
  EntryName,
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
import histos.schedule.{ActionCache, CacheError, Digest, PluginManifest, RemoteCache}
import histos.task.TaskId
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{ProjectGraph, Splits}

import java.nio.charset.StandardCharsets
import java.nio.file.Files

import slayer.{any, stub, stubbed}

import zio.{Chunk, IO, Ref, Scope, ZIO, ZLayer}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** A hit is the stored text. A defect names the missing class, or the exception when the class is not the point. A
  * declared file is the manifest the cache stores.
  */
object PluginOutcomeLawSpec extends ZIOSpecDefault:
  private val pluginId = PluginId.trusted("law")
  private val pack     = TaskName.trusted("pack")
  private val link     = TaskName.trusted("link")
  private val gone     = TaskName.trusted("gone")
  private val bare     = TaskName.trusted("bare")
  private val boom     = TaskName.trusted("boom")
  private val filed    = TaskName.trusted("filed")
  private val absent   = TaskName.trusted("absent")
  private val out      = OutputName("out")
  private val path     = RepoPath.trusted("notes/out.txt")
  private val missing  = RepoPath.trusted("notes/missing.txt")

  def spec = suite("plugin outcome laws")(
    test("a cache hit is the stored text, and the plugin does not run"):
      Workspace.work("histos-plugin-hit").catchAll(error => ZIO.dieMessage(error.message)).flatMap { root =>
        val run =
          for
            _     <- stub[ResolutionService](_.current)(ZIO.succeed(DependencyLock(Nil)))
            _     <- stub[Resolve](_.fetch(any[histos.resolve.LockedArtifact]))(ZIO.dieMessage("fetched"))
            found <- ZIO.scoped {
              loaded(root, List(task(pack, Nil)), die("ran")).flatMap { runtime =>
                for
                  hit   <- runtime.run(PluginTaskId(pluginId, pack), PluginArgs.empty).either
                  bound <- runtime.bind.either
                yield (hit, bound)
              }
            }
          yield
            val key = TaskId.from("law/pack")
            assertTrue(
              found._1 == Right(PluginOutput(PluginRuntime.cacheHit("stored-body"), Nil)),
              found._2.map(_.keySet) == key.map(Set(_)),
            )
        run.provideSomeLayer[Scope](layers(root, hitCache))
      }
    ,
    test("a missing class is linkage, and any other defect is that exception"):
      Workspace.work("histos-plugin-defect").catchAll(error => ZIO.dieMessage(error.message)).flatMap { root =>
        val run =
          for
            _     <- stub[ResolutionService](_.current)(ZIO.succeed(DependencyLock(Nil)))
            _     <- stub[Resolve](_.fetch(any[histos.resolve.LockedArtifact]))(ZIO.dieMessage("fetched"))
            found <- ZIO.scoped {
              loaded(root, List(task(link, Nil), task(gone, Nil), task(bare, Nil), task(boom, Nil)), DefectPlugin)
                .flatMap { runtime =>
                  for
                    linked <- runtime.run(PluginTaskId(pluginId, link), PluginArgs.empty).either
                    missed <- runtime.run(PluginTaskId(pluginId, gone), PluginArgs.empty).either
                    empty  <- runtime.run(PluginTaskId(pluginId, bare), PluginArgs.empty).either
                    other  <- runtime.run(PluginTaskId(pluginId, boom), PluginArgs.empty).either
                  yield (linked, missed, empty, other)
                }
            }
          yield assertTrue(
            found._1 == Left(PluginFault.Task(PluginRunError.Linkage("com.example.Missing"))),
            found._2 == Left(PluginFault.Task(PluginRunError.Linkage("com.example.Gone"))),
            found._3 == Left(PluginFault.Task(PluginRunError.Linkage("java.lang.ClassNotFoundException"))),
            found._4 == Left(PluginFault.Task(PluginRunError.Defect("java.lang.IllegalStateException"))),
          )
        run.provideSomeLayer[Scope](layers(root, ActionCache.miss))
      }
    ,
    test("a declared file is the manifest the cache stores, and a missing file is refused"):
      Workspace.work("histos-plugin-file").catchAll(error => ZIO.dieMessage(error.message)).flatMap { root =>
        Ref.make(List.empty[String]).flatMap { kept =>
          val run =
            for
              _     <- stub[ResolutionService](_.current)(ZIO.succeed(DependencyLock(Nil)))
              _     <- stub[Resolve](_.fetch(any[histos.resolve.LockedArtifact]))(ZIO.dieMessage("fetched"))
              found <- ZIO.scoped {
                loaded(
                  root,
                  List(
                    task(filed, List(DeclaredOutput(out, path))),
                    task(absent, List(DeclaredOutput(out, missing))),
                  ),
                  FilePlugin,
                ).flatMap { runtime =>
                  for
                    wrote   <- runtime.run(PluginTaskId(pluginId, filed), PluginArgs.empty).either
                    missing <- runtime.run(PluginTaskId(pluginId, absent), PluginArgs.empty).either
                  yield (wrote, missing)
                }
              }
              stored <- kept.get
            yield
              val sha  = Sha256Digest.of(Chunk.fromArray("body".getBytes(StandardCharsets.UTF_8)))
              val line = s"${sha.value} ${path.value}"
              assertTrue(
                found._1 == Right(PluginOutput("wrote", List(WrittenFile(out, path)))),
                stored == List(PluginManifest.store("wrote", List(path -> sha))),
                stored.headOption.contains(s"${PluginManifest.Header}\n1\n$line\nwrote"),
                found._2 == Left(PluginFault.Task(PluginRunError.MissingOutput(out))),
              )
          run.provideSomeLayer[Scope](layers(root, remembering(kept)))
        }
      },
  ) @@ TestAspect.withLiveSystem

  private def layers(root: java.nio.file.Path, cache: ActionCache) =
    ZLayer.succeed(cache) ++
      RemoteCache.off ++
      stubbed[Resolve] ++
      ZLayer.succeed(ContentStore.in(root.resolve("store"))) ++
      stubbed[ResolutionService]

  private def loaded(root: java.nio.file.Path, tasks: List[DeclaredTask], plugin: HistosPlugin) =
    for
      threads <- PluginThreads.make(ClassLoader.getSystemClassLoader)
      runtime <- PluginRuntime.loaded(
        root,
        graph(tasks, plugin),
        ClassLoader.getSystemClassLoader,
        threads,
        Nil,
        ApiDigest.trusted("ab" * 32),
      )
    yield runtime

  private def graph(tasks: List[DeclaredTask], plugin: HistosPlugin) =
    ProjectGraph.trusted(
      Nil,
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      List(
        RegisteredPlugin(
          pluginId,
          EntryName.trusted("demo.Plugin$"),
          Contribution(tasks, Nil, Nil),
        )(plugin)
      ),
    )

  private def task(name: TaskName, outputs: List[DeclaredOutput]): DeclaredTask =
    DeclaredTask(name, Seal.Hermetic, Set.empty, Nil, outputs, Nil)

  private val hitCache: ActionCache = new ActionCache:
    def get(digest: Digest): IO[CacheError, Option[String]]      = ZIO.some("stored-body")
    def put(digest: Digest, value: String): IO[CacheError, Unit] = ZIO.dieMessage("stored a hit")

  private def remembering(kept: Ref[List[String]]): ActionCache = new ActionCache:
    def get(digest: Digest): IO[CacheError, Option[String]]      = ZIO.none
    def put(digest: Digest, value: String): IO[CacheError, Unit] =
      kept.update(_ :+ value)

  private def die(text: String): HistosPlugin = new HistosPlugin:
    def id: PluginId             = pluginId
    def contribute               = Contribution(Nil, Nil, Nil)
    def run(request: RunRequest) =
      ZIO.dieMessage(text)

  private object DefectPlugin extends HistosPlugin:
    def id: PluginId             = pluginId
    def contribute               = Contribution(Nil, Nil, Nil)
    def run(request: RunRequest) =
      if request.id.name == link then ZIO.die(new ClassNotFoundException("com.example.Missing"))
      else if request.id.name == gone then ZIO.die(new NoClassDefFoundError("com.example.Gone"))
      else if request.id.name == bare then ZIO.die(new ClassNotFoundException())
      else ZIO.die(new IllegalStateException("boom"))

  private object FilePlugin extends HistosPlugin:
    def id: PluginId             = pluginId
    def contribute               = Contribution(Nil, Nil, Nil)
    def run(request: RunRequest) =
      val write =
        if request.id.name == filed then
          ZIO
            .attemptBlocking {
              val file = request.repo.resolve(path.value)
              Option(file.getParent).foreach(dir => Files.createDirectories(dir))
              Files.writeString(file, "body")
              ()
            }
            .orElseFail(PluginRunError.Defect("write"))
        else ZIO.unit
      val declared = if request.id.name == filed then path else missing
      write.as(
        PluginOutput(if request.id.name == filed then "wrote" else "missing", List(WrittenFile(out, declared)))
      )
end PluginOutcomeLawSpec
