package histos.schedule

import histos.plugin.{Need, Seal}
import java.nio.charset.StandardCharsets

import zio.{Chunk, Ref, ZIO, ZLayer, durationInt}
import zio.test.*

object RemoteSpec extends histos.SuiteClock:
  private val compileId = ActionId.trusted("compile")

  private val compiled: Action =
    Action(
      compileId,
      List(Input(InputName.trusted("compile"), InputValue.Text("cell"))),
      Nil,
    )

  private def digest(action: Action): Digest =
    Digest.of(action, Map.empty)

  private def utf8(text: String): Chunk[Byte] =
    Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))

  private def run(
      remote: RemoteCache,
      action: Action,
      local: Action => ZIO[Any, Nothing, String],
      verdict: Action => Verdict = Sendable.ofAction,
  ) =
    ZIO
      .serviceWithZIO[Scheduler](
        _.execute(action.id, List(action), (node, _) => local(node), verdict)
      )
      .provide(
        Scheduler.live,
        ZLayer.succeed(ActionCache.miss),
        ZLayer.succeed(remote),
      )

  private val policy = suite("what may leave")(
    test("a core compile is sendable, and a need vetoes it"):
      assertTrue(
        Sendable.decide(Runner.Compile, Set.empty) == Verdict.Send,
        Sendable.decide(Runner.Compile, Set(Need.Network)) == Verdict.Stay(
          Hold.Needs(Set(Need.Network))
        ),
        Sendable.decide(Runner.Compile, Set(Need.Docker)) == Verdict.Stay(
          Hold.Needs(Set(Need.Docker))
        ),
      )
    ,
    test("publish, tests, closures, and an open plugin stay"):
      assertTrue(
        Sendable.decide(Runner.Publish, Set.empty) == Verdict.Stay(
          Hold.Publish
        ),
        Sendable.decide(Runner.Test, Set.empty) == Verdict.Stay(Hold.Test),
        Sendable.decide(Runner.Closure, Set.empty) == Verdict.Stay(
          Hold.Closure
        ),
        Sendable.decide(Runner.Plugin(Seal.Open), Set.empty) == Verdict.Stay(
          Hold.Plugin
        ),
        Sendable.decide(Runner.Inspect, Set.empty) == Verdict.Stay(Hold.Inspect),
      )
    ,
    test("a hermetic plugin is sendable until it declares a need"):
      assertTrue(
        Sendable
          .decide(Runner.Plugin(Seal.Hermetic), Set.empty) == Verdict.Send,
        Sendable.decide(
          Runner.Plugin(Seal.Hermetic),
          Set(Need.Network, Need.Docker),
        ) ==
          Verdict.Stay(Hold.Needs(Set(Need.Network, Need.Docker))),
      ),
  )

  private val remote = suite("in-process remote")(
    test("an unconfigured cache runs locally and refuses a store"):
      for
        called <- Ref.make(false)
        out    <- run(
          RemoteCache.unconfigured,
          compiled,
          action => called.set(true) *> ZIO.succeed(action.id.value),
        )
        did     <- called.get
        refused <- RemoteCache.unconfigured
          .store(digest(compiled), Chunk.empty)
          .either
      yield assertTrue(
        out == "compile" && did && refused == Left(RemoteError.Off)
      )
    ,
    test("a read-only hit does not run locally"):
      for
        cache  <- TestRemoteCache.memory(RemoteMode.Read)
        _      <- cache.plant(digest(compiled), utf8("from-remote"))
        called <- Ref.make(false)
        out    <- run(
          cache,
          compiled,
          _ => called.set(true) *> ZIO.succeed("local"),
        )
        did <- called.get
      yield assertTrue(out == "from-remote" && !did)
    ,
    test("a read-only miss runs locally and does not upload"):
      for
        cache  <- TestRemoteCache.memory(RemoteMode.Read)
        called <- Ref.make(false)
        out    <- run(
          cache,
          compiled,
          _ => called.set(true) *> ZIO.succeed("local"),
        )
        did     <- called.get
        held    <- cache.holds(digest(compiled))
        refused <- cache.store(digest(compiled), utf8("local")).either
      yield assertTrue(
        out == "local" && did && !held && refused == Left(RemoteError.ReadOnly)
      )
    ,
    test("a write cache uploads a compile and the next run is a hit"):
      for
        cache <- TestRemoteCache.memory(RemoteMode.Write)
        calls <- Ref.make(0)
        local = (_: Action) => calls.update(_ + 1) *> ZIO.succeed("classes 1")
        first  <- run(cache, compiled, local)
        once   <- calls.get
        held   <- cache.holds(digest(compiled))
        second <- run(cache, compiled, local)
        twice  <- calls.get
      yield assertTrue(
        first == "classes 1" && once == 1 && held && second == "classes 1" && twice == 1
      )
    ,
    test("a write cache does not upload a stayed action"):
      for
        cache <- TestRemoteCache.memory(RemoteMode.Write)
        out   <- run(
          cache,
          compiled,
          _ => ZIO.succeed("local"),
          _ => Verdict.Stay(Hold.Closure),
        )
        held <- cache.holds(digest(compiled))
      yield assertTrue(out == "local" && !held)
    ,
    test("a hermetic plugin uploads, and a local hit is not uploaded"):
      val hit: ActionCache = new ActionCache:
        def get(key: Digest): zio.UIO[Option[String]]      = ZIO.some("local-hit")
        def put(key: Digest, value: String): zio.UIO[Unit] = ZIO.unit
      for
        cache <- TestRemoteCache.memory(RemoteMode.Write)
        plugin = compiled
        uploaded <- run(
          cache,
          plugin,
          _ => ZIO.succeed("plugin"),
          _ => Verdict.Send,
        )
        held      <- cache.holds(digest(plugin))
        localOnly <- TestRemoteCache.memory(RemoteMode.Write)
        skipped   <- ZIO
          .serviceWithZIO[Scheduler](
            _.execute(
              plugin.id,
              List(plugin),
              (_, _) => ZIO.succeed("ran"),
              _ => Verdict.Send,
            )
          )
          .provide(
            Scheduler.live,
            ZLayer.succeed(hit),
            ZLayer.succeed(localOnly),
          )
        kept <- localOnly.holds(digest(plugin))
      yield assertTrue(
        uploaded == "plugin" && held && skipped == "local-hit" && !kept
      )
    ,
    test(
      "a dangling output and a non-utf-8 blob fail instead of running locally"
    ):
      for
        broken  <- TestRemoteCache.memory(RemoteMode.Read)
        missing <- broken.plantDangling(digest(compiled))
        dangled <- run(broken, compiled, _ => ZIO.succeed("local")).either
        garbage <- TestRemoteCache.memory(RemoteMode.Read)
        _       <- garbage.plant(digest(compiled), Chunk.single(0xff.toByte))
        called  <- Ref.make(false)
        text    <- run(
          garbage,
          compiled,
          _ => called.set(true) *> ZIO.succeed("local"),
        ).either
        did <- called.get
      yield assertTrue(
        dangled == Left(
          ScheduleError.Remote(RemoteError.Dangling(digest(compiled), missing))
        ),
        text == Left(ScheduleError.Remote(RemoteError.Text(digest(compiled)))),
        !did,
      ),
  )

  def spec =
    suite("remote cache")(policy, remote) @@ TestAspect.timeout(30.seconds)
end RemoteSpec
