package histos.cache

import histos.jdk.Sha256
import histos.schedule.{CacheExchange, Digest, RemoteCache, RemoteError, RemoteMode}
import histos.workspace.Workspace

import zio.{Chunk, IO, Ref, Scope, ZIO}
import zio.test.*

/** A download that does not match its digest is not stored. A timeout is a miss. Only a writing cache uploads. */
object ProductCacheLawSpec extends ZIOSpecDefault:
  private val key    = Sha256.trusted("ab" * 32)
  private val action = Digest.trusted("ab" * 32)
  private val other  = Digest.trusted("cd" * 32)
  private val bytes  = Chunk[Byte](1, 2, 3)

  def spec = suite("product cache")(
    test("a mismatched download is not stored"):
      ZIO.scoped:
        for
          dir  <- Workspace.work("histos-product-mismatch")
          puts <- Ref.make(0)
          remote = counting(RemoteMode.Read, ZIO.fail(RemoteError.Mismatch(action, other)), puts)
          store  = ContentStore.in(dir)
          failed <- ProductCache.stage(remote, store, key).either
          listed <- store.list
          count  <- puts.get
        yield assertTrue(failed == Left(RemoteError.Mismatch(action, other)), listed.isEmpty, count == 0)
    ,
    test("a connect timeout is a miss and the packer runs"):
      ZIO.scoped:
        for
          dir   <- Workspace.work("histos-product-timeout")
          puts  <- Ref.make(0)
          packs <- Ref.make(0)
          remote = counting(
            RemoteMode.Read,
            ZIO.fail(RemoteError.Exchange(CacheExchange.ConnectTimeout("127.0.0.1", 9))),
            puts,
          )
          store = ContentStore.in(dir)
          packed <- ProductCache.materialize(remote, store, key, dir.resolve("out"), packs.update(_ + 1).as(bytes))
          listed <- store.list
          wrote  <- puts.get
          ran    <- packs.get
        yield assertTrue(packed == bytes, listed.length == 1, wrote == 0, ran == 1)
    ,
    test("write publishes once and read does not"):
      ZIO.scoped:
        for
          dir  <- Workspace.work("histos-product-publish")
          puts <- Ref.make(0)
          remote = counting(RemoteMode.Write, ZIO.none, puts)
          store  = ContentStore.in(dir)
          packed <- ProductCache.materialize(remote, store, key, dir.resolve("out"), ZIO.succeed(bytes))
          wrote  <- puts.get
          _      <- puts.set(0)
          reader = counting(RemoteMode.Read, ZIO.none, puts)
          again <- ProductCache.materialize(reader, store, key, dir.resolve("again"), ZIO.succeed(Chunk.empty))
          quiet <- puts.get
        yield assertTrue(packed == bytes, wrote == 1, again == bytes, quiet == 0),
  )

  private final class Counting(
      val mode: RemoteMode,
      answer: IO[RemoteError, Option[Chunk[Byte]]],
      puts: Ref[Int],
  ) extends RemoteCache:
    def find(action: Digest): IO[RemoteError, Option[Chunk[Byte]]]       = answer
    def store(action: Digest, bytes: Chunk[Byte]): IO[RemoteError, Unit] = puts.update(_ + 1)
    def purge: IO[RemoteError, Unit]                                     = ZIO.unit

  private def counting(
      mode: RemoteMode,
      answer: IO[RemoteError, Option[Chunk[Byte]]],
      puts: Ref[Int],
  ): RemoteCache =
    Counting(mode, answer, puts)
end ProductCacheLawSpec
