package histos.load

import histos.cache.ContentStore
import histos.plugin.*
import histos.schedule.{
  Action,
  ActionDigest,
  ActionId,
  Histos,
  Parallelism,
  PluginManifest,
  Policy,
  RemoteCache,
  ScheduleError,
  Scheduler,
}
import zio.{ZEnvironment, ZIO, ZLayer}
import zio.test.*

object PluginProductReplaySpec extends ZIOSpecDefault:
  private val tests = suite("scheduled plugin product replay")(
    test("a cache hit restores deleted owned products and stale entries without running the producer") {
      ProductFixture.use: fixture =>
        for
          store       <- ZIO.service[ContentStore]
          environment <- (ZLayer.succeed(Parallelism(1, 1, 1)) >>> Histos.live).build
          scheduler = Scheduler(environment.get[Histos], ContentStore.actions(store), RemoteCache.unconfigured)
          action <- ZIO
            .fromEither(ActionId.from("site"))
            .mapError(_ => ProductError.InvalidManifest)
            .map(id =>
              Action(
                id,
                Nil,
                Nil,
                Policy.Plugin(Seal.Hermetic, Set.empty, Some(fixture.root), ProductFixture.task.outputs.map(_.path)),
              )
            )
          digest = ActionDigest.of(action, Map.empty)
          local  = fixture
            .produce("old")
            .map(committed => PluginManifest.store(committed.manifest))
            .provideEnvironment(ZEnvironment(store))
          first <- scheduler
            .cached(action, digest, fixture.request, local)
            .provideSomeLayer[ContentStore](ZLayer.succeed(fixture.products))
          _      <- PluginProductFiles.delete(fixture.root.resolve("site"))
          _      <- PluginProductFiles.delete(fixture.root.resolve("page.txt"))
          _      <- PluginProductFiles.delete(fixture.root.resolve(".histos/products"))
          second <- scheduler
            .cached(action, digest, fixture.request, local)
            .provideSomeLayer[ContentStore](ZLayer.succeed(fixture.products))
          _ <- fixture.write(fixture.root.resolve("site/stale.html"), "stale")
          _ <- scheduler
            .cached(action, digest, fixture.request, local)
            .provideSomeLayer[ContentStore](ZLayer.succeed(fixture.products))
          page  <- fixture.read("site/index.html")
          stamp <- fixture.read("site/stamp")
          stale <- fixture.io(java.nio.file.Files.exists(fixture.root.resolve("site/stale.html")))
          count <- fixture.count.get
        yield assertTrue(first == second, page == "old", stamp == "old", !stale, count == 1)
    },
    test("unsafe cached entries retain the typed product error and do not launch the producer") {
      ProductFixture.use: fixture =>
        for
          store       <- ZIO.service[ContentStore]
          environment <- (ZLayer.succeed(Parallelism(1, 1, 1)) >>> Histos.live).build
          cache     = ContentStore.actions(store)
          scheduler = Scheduler(environment.get[Histos], cache, RemoteCache.unconfigured)
          committed <- fixture.produce("old")
          action    <- ZIO
            .fromEither(ActionId.from("unsafe"))
            .mapError(_ => ProductError.InvalidManifest)
            .map(id => Action(id, Nil, Nil, Policy.Plugin(Seal.Hermetic, Set.empty)))
          digest = ActionDigest.of(action, Map.empty)
          _ <- cache
            .put(digest, PluginManifest.store(committed.manifest).replace("assets/client.js", "../escape"))
            .mapError(ProductError.Cache(_))
          local = fixture
            .produce("new")
            .map(next => PluginManifest.store(next.manifest))
            .provideEnvironment(ZEnvironment(store))
          failed <- scheduler
            .cached(action, digest, fixture.request, local)
            .provideSomeLayer[ContentStore](ZLayer.succeed(fixture.products))
            .either
          count <- fixture.count.get
          page  <- fixture.read("site/index.html")
        yield assertTrue(
          failed == Left(ScheduleError.Products(ProductError.InvalidEntry("../escape"))),
          count == 1,
          page == "old",
        )
    },
    test("a failed local production never enters the action cache") {
      ProductFixture.use: fixture =>
        for
          store       <- ZIO.service[ContentStore]
          environment <- (ZLayer.succeed(Parallelism(1, 1, 1)) >>> Histos.live).build
          cache     = ContentStore.actions(store)
          scheduler = Scheduler(environment.get[Histos], cache, RemoteCache.unconfigured)
          _      <- fixture.produce("old")
          action <- ZIO
            .fromEither(ActionId.from("failed"))
            .mapError(_ => ProductError.InvalidManifest)
            .map(id => Action(id, Nil, Nil, Policy.Plugin(Seal.Hermetic, Set.empty)))
          digest = ActionDigest.of(action, Map.empty)
          local  = ZIO.scoped:
            for
              staged <- fixture.products.stage(fixture.request)
              _      <- fixture.fill(staged, "partial")
              result <- ZIO.fail(ProductError.Changed)
            yield result
          failed <- scheduler
            .cached(action, digest, fixture.request, local)
            .provideSomeLayer[ContentStore](ZLayer.succeed(fixture.products))
            .either
          stored <- cache.get(digest).mapError(ProductError.Cache(_))
          after  <- fixture.read("site/index.html")
        yield assertTrue(
          failed == Left(ScheduleError.Failed(action.id, ProductError.Changed)),
          stored.isEmpty,
          after == "old",
        )
    },
  )
  def spec = tests
