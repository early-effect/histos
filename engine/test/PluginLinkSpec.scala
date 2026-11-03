package histos.load

import histos.plugin.{
  ApiDigest,
  Contribution,
  DeclaredTask,
  HistosPlugin,
  PluginArgs,
  PluginId,
  PluginOutput,
  PluginTaskId,
  Seal,
  TaskDep,
  TaskName,
}
import histos.schedule.{ActionCache, Digest, RemoteCache}
import histos.{Js, MainClass, ProjectName, Projects, ScalaVersion, module, use}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Ref, Scope, UIO, ZIO, ZLayer, durationInt}
import zio.test.*

object PluginLinkSpec extends ZIOSpecDefault:
  private val pluginId = PluginId("test.link")
  private val launcher = ApiDigest.trusted("cd" * 32)
  private val docs     = ProjectName("docs")

  private final class Capture(seen: Ref[List[Path]]) extends HistosPlugin:
    def id: PluginId = pluginId

    def contribute: Contribution =
      Contribution(
        List(
          DeclaredTask(
            TaskName("site"),
            Seal.Open,
            Set.empty,
            Nil,
            Nil,
            List(TaskDep.Link(docs, None)),
          )
        ),
        Nil,
        Nil,
      )

    def run(request: histos.plugin.RunRequest): ZIO[Scope, histos.plugin.PluginRunError, PluginOutput] =
      seen.set(request.linked.map(_.script)) *> ZIO.succeed(PluginOutput("linked", Nil))
  end Capture

  private val linked = suite("link input")(
    test("a link dependency puts the script on the run request"):
      ZIO.scoped:
        for
          seen <- Ref.make(List.empty[Path])
          live = Capture(seen)
          page = module("docs", Js)(ScalaVersion("3.9.0")).main(MainClass("Check"))
          built <- ZIO.fromEither(Projects.locked(use(live), page))
          dir   <- WorkspaceDir.make
          _     <- ZIO.attempt {
            val source = dir.resolve("docs/src/main/scala/Check.scala")
            Files.createDirectories(source.getParent)
            Files.writeString(
              source,
              "object Check:\n  def main(args: Array[String]): Unit = ()\n",
            )
            Files.writeString(dir.resolve("dependencies.lock"), "cell docs 3.9.0 js\n")
          }
          _     <- histos.ToolFixture.seed(dir, built)
          tools <- ZIO.service[histos.resolve.LockedTools]
          cache <- emptyCache
          parent = Option(getClass.getClassLoader).getOrElse(ClassLoader.getSystemClassLoader)
          result <- PluginThreads
            .make(parent)
            .flatMap: threads =>
              PluginRuntime
                .loaded(dir, built, parent, threads, List(live), Nil, launcher)
                .flatMap: runtime =>
                  runtime
                    .run(PluginTaskId(pluginId, TaskName("site")), PluginArgs.empty)
                    .provide(
                      ZLayer.succeed(cache),
                      RemoteCache.off,
                      ZLayer.succeed(tools.resolve),
                      histos.zinc.CompileReplay.processStore,
                      histos.resolve.ResolutionService.layer(dir, built),
                    )
          scripts <- seen.get
          linked = scripts.exists: path =>
            Option(path.getFileName).exists(_.toString == "main.js") && Files.isRegularFile(path) && Files.size(
              path
            ) > 0
        yield assertTrue(result == PluginOutput("linked", Nil), linked)
  )

  private def emptyCache: UIO[ActionCache] =
    Ref
      .make(Map.empty[String, String])
      .map: values =>
        new ActionCache:
          def get(digest: Digest): UIO[Option[String]] =
            values.get.map(_.get(digest.value))
          def put(digest: Digest, value: String): UIO[Unit] =
            values.update(_ + (digest.value -> value))

  private object WorkspaceDir:
    def make: ZIO[Scope, Nothing, Path] =
      ZIO.acquireRelease(
        ZIO.attempt(Files.createTempDirectory("histos-plugin-link")).orDie
      )(delete)

    private def delete(root: Path): UIO[Unit] =
      ZIO.attempt {
        if Files.exists(root) then
          val walk  = Files.walk(root)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore
  end WorkspaceDir

  def spec =
    linked.provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      histos.ToolFixture.layer(Projects(module("js", Js)(ScalaVersion("3.9.0"))))
    ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(4.minutes)
end PluginLinkSpec
