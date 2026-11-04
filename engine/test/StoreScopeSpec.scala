package histos.cache

import zio.{Chunk, ZIO}
import zio.test.{assertTrue, ZIOSpecDefault}

object StoreScopeSpec extends ZIOSpecDefault:
  private val classes = Chunk.fromArray("classes".getBytes)
  private val empty   = Chunk.empty[Byte]

  def spec = suite("a cache key is a scope")(
    test("a read that finishes first does not reserve the key"):
      for
        scope <- StoreScope.memory
        _     <- ZIO.scoped(scope.read("main"))
        _     <- ZIO.scoped(scope.write("main")(_ => ZIO.succeed(classes)))
        saved <- scope.published("main")
      yield assertTrue(saved.contains(classes))
    ,
    test("the first write to release keeps the key"):
      for
        scope <- StoreScope.memory
        _     <- ZIO.scoped(scope.write("main")(_ => ZIO.succeed(empty)))
        _     <- ZIO.scoped(scope.write("main")(_ => ZIO.succeed(classes)))
        saved <- scope.published("main")
      yield assertTrue(saved.contains(empty))
    ,
    test("a failed write publishes nothing"):
      for
        scope <- StoreScope.memory
        _     <- ZIO.scoped(scope.write("main")(_ => ZIO.interrupt)).exit
        saved <- scope.published("main")
      yield assertTrue(saved.isEmpty),
  )
end StoreScopeSpec
