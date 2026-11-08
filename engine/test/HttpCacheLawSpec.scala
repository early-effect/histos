package histos.schedule

import heddle.client.{Authority, Client, ClientError, TargetError}
import heddle.error.HttpError
import heddle.http.{Body, Method, Request, Response, Status, Url}

import slayer.{any, stub, stubbed}

import zio.{Chunk, Ref, ZIO}
import zio.stream.ZStream
import zio.test.{TestEnvironment, ZIOSpecDefault, assertTrue, suite, test}

object HttpCacheLawSpec extends ZIOSpecDefault:
  private val origin = Url.parse("http://127.0.0.1:9")
  private val dev    = CacheNamespace.Dev(DevName("law"))
  private val main   = CacheNamespace.Main
  private val action = Digest.trusted("ab" * 32)
  private val other  = Digest.trusted("cd" * 32)
  private val bytes  = Chunk[Byte](1, 2, 3)
  private val wrong  = Chunk[Byte](1, 2, 4)
  private val output = ActionDigest.ofBytes(bytes)
  private val index  = ActionResult.encode(output, bytes.length)

  def spec = suite("http cache laws")(
    test("off never opens a socket, and read refuses a write before a socket"):
      val program =
        for
          _       <- dieOnSend
          cache   <- HttpCache.make(origin, RemoteMode.Off, dev)
          found   <- cache.find(action)
          stored  <- cache.store(action, bytes).either
          purged  <- cache.purge.either
          reader  <- HttpCache.make(origin, RemoteMode.Read, dev)
          refused <- reader.store(action, bytes).either
          blocked <- reader.purge.either
        yield assertTrue(
          cache.mode == RemoteMode.Off,
          reader.mode == RemoteMode.Read,
          found == None,
          stored == Left(RemoteError.Off),
          purged == Left(RemoteError.Off),
          refused == Left(RemoteError.ReadOnly),
          blocked == Left(RemoteError.ReadOnly),
          stored.left.exists(_.message == "remote cache is not configured"),
          refused.left.exists(_.message == "remote cache is read-only"),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("a miss walks the namespace, a write puts the blob first, and a purge deletes only the index"):
      val program =
        for
          seen    <- Ref.make(Chunk.empty[Request])
          files   <- Ref.make(Map.empty[String, Chunk[Byte]])
          _       <- remember(seen, files)
          cache   <- HttpCache.make(origin, RemoteMode.Write, dev)
          missed  <- cache.find(action)
          missLog <- calls(seen)
          _       <- seen.set(Chunk.empty)
          stored  <- cache.store(action, bytes)
          empty   <- cache.store(other, Chunk.empty)
          putLog  <- calls(seen)
          _       <- seen.set(Chunk.empty)
          hit     <- cache.find(action)
          zero    <- cache.find(other)
          hitLog  <- calls(seen)
          _       <- seen.set(Chunk.empty)
          purged  <- cache.purge
          dropLog <- calls(seen)
          kept    <- files.get
        yield assertTrue(
          missed == None,
          missLog == Chunk(Method.GET -> ac(dev, action), Method.GET -> ac(main, action)),
          stored == (),
          empty == (),
          putLog == Chunk(
            Method.PUT -> cas(output),
            Method.PUT -> ac(dev, action),
            Method.PUT -> cas(ActionDigest.ofBytes(Chunk.empty)),
            Method.PUT -> ac(dev, other),
          ),
          hit == Some(bytes),
          zero == Some(Chunk.empty),
          hitLog == Chunk(
            Method.GET -> ac(dev, action),
            Method.GET -> cas(output),
            Method.GET -> ac(dev, other),
            Method.GET -> cas(ActionDigest.ofBytes(Chunk.empty)),
          ),
          purged == (),
          dropLog == Chunk(Method.DELETE -> root(dev)),
          kept.contains(cas(output)),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("a 200 commits a write and a delete, and any other status stops on the first request"):
      val program =
        for
          seen <- Ref.make(Chunk.empty[Request])
          _    <- answer(
            seen,
            Chunk((Method.PUT, cas(output), status(Status.Ok)), (Method.PUT, ac(dev, action), status(Status.Ok))),
          )
          cache    <- HttpCache.make(origin, RemoteMode.Write, dev)
          wrote    <- cache.store(action, bytes).either
          wroteLog <- calls(seen)
          _        <- seen.set(Chunk.empty)
          _        <- answer(seen, Chunk((Method.PUT, cas(output), status(Status.Created))))
          stopped  <- cache.store(action, bytes).either
          stopLog  <- calls(seen)
          _        <- seen.set(Chunk.empty)
          _        <- answer(seen, Chunk((Method.DELETE, root(dev), status(Status.Ok))))
          dropped  <- cache.purge.either
          _        <- seen.set(Chunk.empty)
          _        <- answer(seen, Chunk((Method.DELETE, root(dev), status(Status.InternalServerError))))
          failed   <- cache.purge.either
          failLog  <- calls(seen)
        yield assertTrue(
          wrote == Right(()),
          wroteLog.map(_._1) == Chunk(Method.PUT, Method.PUT),
          wroteLog.map(_._2) == Chunk(cas(output), ac(dev, action)),
          stopped == Left(RemoteError.Status(Status.Created.code)),
          stopLog.length == 1,
          dropped == Right(()),
          failed == Left(RemoteError.Status(500)),
          failLog == Chunk(Method.DELETE -> root(dev)),
          failed.left.exists(_.message == "remote cache answered 500"),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("a bad index, a dangling blob, a short blob, and a failed body are typed, and a 500 stops the walk"):
      val program =
        for
          seen <- Ref.make(Chunk.empty[Request])
          _    <- answer(
            seen,
            Chunk((Method.GET, ac(dev, action), body(Status.Ok, Chunk[Byte](1, 2, 3)))),
          )
          cache <- HttpCache.make(origin, RemoteMode.Read, dev)
          bad   <- cache.find(action).either
          _     <- seen.set(Chunk.empty)
          _     <- answer(
            seen,
            Chunk(
              (Method.GET, ac(dev, action), body(Status.Ok, index)),
              (Method.GET, cas(output), status(Status.NotFound)),
            ),
          )
          dangling <- cache.find(action).either
          _        <- seen.set(Chunk.empty)
          _        <- answer(
            seen,
            Chunk(
              (Method.GET, ac(dev, action), body(Status.Ok, index)),
              (Method.GET, cas(output), body(Status.Ok, wrong)),
            ),
          )
          mismatch <- cache.find(action).either
          _        <- seen.set(Chunk.empty)
          _        <- answer(
            seen,
            Chunk(
              (
                Method.GET,
                ac(dev, action),
                Response(Status.Ok).withBody(Body.stream(ZStream.fail(new java.io.IOException("body")))),
              )
            ),
          )
          broken  <- cache.find(action).either
          _       <- seen.set(Chunk.empty)
          _       <- answer(seen, Chunk((Method.GET, ac(dev, action), status(Status.InternalServerError))))
          halted  <- cache.find(action).either
          haltLog <- calls(seen)
        yield assertTrue(
          bad == Left(RemoteError.Index(action)),
          bad.left.exists(_.message == s"remote cache action ${action.value} is not an action result"),
          dangling == Left(RemoteError.Dangling(action, output)),
          dangling.left.exists(
            _.message == s"remote cache action ${action.value} names missing output ${output.value}"
          ),
          mismatch == Left(RemoteError.Mismatch(action, output)),
          mismatch.left.exists(
            _.message == s"remote cache action ${action.value} returned bytes that are not ${output.value}"
          ),
          broken == Left(RemoteError.Exchange(CacheExchange.Body)),
          broken.left.exists(_.message == "the remote cache response body failed"),
          halted == Left(RemoteError.Status(500)),
          haltLog == Chunk(Method.GET -> ac(dev, action)),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("copy moves action results, an empty body copies nothing, and a bad line or index is refused"):
      val program =
        for
          seen      <- Ref.make(Chunk.empty[Request])
          files     <- Ref.make(Map.empty[String, Chunk[Byte]])
          _         <- answer(seen, Chunk((Method.GET, root(dev), status(Status.InternalServerError))))
          denied    <- HttpCache.copy(origin, dev, main).either
          deniedLog <- calls(seen)
          _         <- seen.set(Chunk.empty)
          _         <- remember(seen, files)
          _         <- files.set(Map(root(dev) -> Chunk.empty))
          empty     <- HttpCache.copy(origin, dev, main).either
          emptyLog  <- calls(seen)
          _         <- seen.set(Chunk.empty)
          _         <- files.set(Map(root(dev) -> text("\n")))
          newline   <- HttpCache.copy(origin, dev, main).either
          _         <- seen.set(Chunk.empty)
          _         <- files.set(Map(root(dev) -> text("zz\n")))
          catalog   <- HttpCache.copy(origin, dev, main).either
          _         <- seen.set(Chunk.empty)
          _         <- files.set(
            Map(
              root(dev)       -> text(s"${action.value}\n"),
              ac(dev, action) -> Chunk[Byte](9),
            )
          )
          undecoded    <- HttpCache.copy(origin, dev, main).either
          undecodedLog <- calls(seen)
          _            <- seen.set(Chunk.empty)
          otherIndex = ActionResult.encode(ActionDigest.ofBytes(wrong), wrong.length)
          _ <- files.set(
            Map(
              root(dev)       -> text(s"${action.value}\n${other.value}\n"),
              ac(dev, action) -> index,
              ac(dev, other)  -> otherIndex,
            )
          )
          copied    <- HttpCache.copy(origin, dev, main)
          copiedLog <- calls(seen)
          kept      <- files.get
        yield assertTrue(
          denied == Left(RemoteError.Status(500)),
          deniedLog == Chunk(Method.GET -> root(dev)),
          empty == Right(()),
          emptyLog == Chunk(Method.GET -> root(dev)),
          newline == Left(RemoteError.Catalog("")),
          newline.left.exists(_.message == "'' is not an action digest"),
          catalog == Left(RemoteError.Catalog("zz")),
          catalog.left.exists(_.message == "'zz' is not an action digest"),
          undecoded == Left(RemoteError.Index(action)),
          undecodedLog == Chunk(Method.GET -> root(dev), Method.GET -> ac(dev, action)),
          copied == (),
          copiedLog == Chunk(
            Method.GET -> root(dev),
            Method.GET -> ac(dev, action),
            Method.PUT -> ac(main, action),
            Method.GET -> ac(dev, other),
            Method.PUT -> ac(main, other),
          ),
          kept.get(ac(main, action)) == Some(index),
          kept.get(ac(main, other)) == Some(otherIndex),
          kept.get(cas(output)) == None,
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("a client failure is that exchange, and the sentence does not repeat the throwable"):
      val auth = Authority("cache", 9)
      val down = new java.io.IOException("down")
      val rows = List(
        (
          ClientError.InvalidTarget("nope", TargetError.NoHost),
          CacheExchange.Target,
          "the remote cache target is not a url",
        ),
        (
          ClientError.Connect(auth, down),
          CacheExchange.Connect("cache", 9),
          "could not connect to the remote cache at cache:9",
        ),
        (
          ClientError.ConnectTimeout(auth),
          CacheExchange.ConnectTimeout("cache", 9),
          "the remote cache at cache:9 did not accept a connection",
        ),
        (ClientError.Tls(auth, down), CacheExchange.Tls("cache", 9), "tls failed with the remote cache at cache:9"),
        (
          ClientError.ReadTimeout(auth),
          CacheExchange.ReadTimeout("cache", 9),
          "the remote cache at cache:9 stopped answering",
        ),
        (ClientError.Io(auth, down), CacheExchange.Io("cache", 9), "the remote cache at cache:9 closed the exchange"),
        (
          ClientError.Protocol(auth, HttpError.HeadersTooLarge),
          CacheExchange.Protocol("cache", 9),
          "the remote cache at cache:9 sent an invalid response",
        ),
        (
          ClientError.NotAnEventStream(auth, Status.Ok),
          CacheExchange.Protocol("cache", 9),
          "the remote cache at cache:9 sent an invalid response",
        ),
        (
          ClientError.PoolExhausted(auth),
          CacheExchange.Pool("cache", 9),
          "no free connection to the remote cache at cache:9",
        ),
        (
          ClientError.InvalidTrust("secret-material"),
          CacheExchange.Trust,
          "the remote cache trust material is unusable",
        ),
        (ClientError.BodyFailed(down), CacheExchange.Body, "the remote cache response body failed"),
      )
      assertTrue(
        rows.map { case (error, exchange, message) => (HttpCache.exchange(error), exchange.message) } ==
          rows.map { case (_, exchange, message) => (exchange, message) }
      ),
  )

  private def ac(role: CacheNamespace, digest: Digest): String =
    s"http://127.0.0.1:9/${role.token}/ac/${digest.value}"

  private def cas(digest: Digest): String =
    s"http://127.0.0.1:9/cas/${digest.value}"

  private def root(role: CacheNamespace): String =
    s"http://127.0.0.1:9/${role.token}/ac"

  private def status(code: Status): Response =
    Response(code)

  private def body(code: Status, bytes: Chunk[Byte]): Response =
    Response(code).withBody(Body.fromBytes(bytes))

  private def text(value: String): Chunk[Byte] =
    Chunk.fromArray(value.getBytes(java.nio.charset.StandardCharsets.UTF_8))

  private def calls(seen: Ref[Chunk[Request]]) =
    seen.get.map(_.map(req => req.method -> req.url.render))

  private def dieOnSend =
    stub[Client](_.batched(any[Request]))((req: Request) => ZIO.die(new IllegalStateException(req.url.render))) *>
      stub[Client](_.streaming(any[Request]))(ZIO.die(new IllegalStateException("streamed")))

  private def answer(seen: Ref[Chunk[Request]], rows: Chunk[(Method, String, Response)]) =
    val reply = (req: Request) =>
      val found = rows.collectFirst {
        case (method, url, response) if req.method == method && req.url.render == url => response
      }
      seen.update(_ :+ req) *> found.fold(
        ZIO.die(new IllegalStateException(s"${req.method.render} ${req.url.render}"))
      )(ZIO.succeed)
    stub[Client](_.batched(any[Request]))(reply) *>
      stub[Client](_.streaming(any[Request]))(ZIO.die(new IllegalStateException("streamed")))

  private def remember(seen: Ref[Chunk[Request]], files: Ref[Map[String, Chunk[Byte]]]) =
    val reply = (req: Request) =>
      val change = req.method match
        case Method.GET =>
          files.get.map: stored =>
            stored.get(req.url.render) match
              case Some(bytes) => Response(Status.Ok).withBody(Body.fromBytes(bytes))
              case None        => Response(Status.NotFound)
        case Method.PUT =>
          files.update(_ + (req.url.render -> req.body.strict.getOrElse(Chunk.empty))).as(Response(Status.NoContent))
        case Method.DELETE =>
          files.update(_.removed(req.url.render)).as(Response(Status.NoContent))
        case other =>
          ZIO.die(new IllegalStateException(other.render))
      seen.update(_ :+ req) *> change
    stub[Client](_.batched(any[Request]))(reply) *>
      stub[Client](_.streaming(any[Request]))(ZIO.die(new IllegalStateException("streamed")))
end HttpCacheLawSpec
