package histos.load

import histos.cache.ContentStore
import histos.plugin.{ApiDigest, PluginArgs, PluginId, PluginRunError, PluginTaskId, TaskName}
import histos.resolve.{LockedArtifact, ResolutionService, Resolve}
import histos.schedule.{ActionCache, Digest, RemoteCache}
import histos.ProjectGraph

import java.nio.file.Path

import slayer.{any, stub, stubbed}

import zio.{Chunk, Scope, ZIO}
import zio.test.*

object PluginRuntimeLawSpec extends ZIOSpecDefault:
  def spec = suite("plugin runtime refusals")(
    test("an unknown task is refused before the machine or a resolver"):
      val id      = PluginTaskId(PluginId("demo"), TaskName("missing"))
      val program =
        for
          _ <- stub[ActionCache](_.get(any[Digest]))(ZIO.die(new IllegalStateException("cache")))
          _ <- stub[ActionCache](_.put(any[Digest], any[String]))(ZIO.die(new IllegalStateException("cache")))
          _ <- stub[RemoteCache](_.find(any[Digest]))(ZIO.die(new IllegalStateException("remote")))
          _ <- stub[RemoteCache](_.store(any[Digest], any[Chunk[Byte]]))(ZIO.die(new IllegalStateException("remote")))
          _ <- stub[RemoteCache](_.purge)(ZIO.die(new IllegalStateException("remote")))
          _ <- stub[Resolve](_.fetch(any[LockedArtifact]))(ZIO.die(new IllegalStateException("fetched")))
          _ <- stub[ContentStore](_.read(any[Digest]))(ZIO.die(new IllegalStateException("read")))
          _ <- stub[ResolutionService](_.current)(ZIO.die(new IllegalStateException("resolved")))
          found <- ZIO.scoped {
            for
              threads <- PluginThreads.make(ClassLoader.getSystemClassLoader)
              runtime <- PluginRuntime.loaded(
                Path.of("repo"),
                ProjectGraph.trusted(Nil),
                ClassLoader.getSystemClassLoader,
                threads,
                Nil,
                Nil,
                ApiDigest.trusted("ab" * 32),
              )
              out <- runtime.run(id, PluginArgs.empty).either
            yield out
          }
        yield assertTrue(found == Left(PluginFault.Task(PluginRunError.Unknown(id))))
      program.provideSomeLayer[TestEnvironment & Scope](
        stubbed[ActionCache] ++
          stubbed[RemoteCache] ++
          stubbed[Resolve] ++
          stubbed[ContentStore] ++
          stubbed[ResolutionService]
      )
  )
