package histos.central

import histos.version.{ReleaseVersion, ShipNumber, SnapshotPointer}
import histos.Organization

import heddle.client.{Client, ClientError}
import heddle.http.{Body, Method, Request, Response, Status}

import slayer.{any, stub, stubbed}

import zio.*
import zio.stream.ZStream
import zio.test.*

object SnapshotPublishLawSpec extends ZIOSpecDefault:
  private val ship    = ShipNumber("1.0.0")
  private val pk      = Chunk[Byte](0x50.toByte, 0x4b.toByte, 3.toByte, 4.toByte)
  private val pointer = Revision.Pointer(SnapshotPointer(ship))

  def spec = suite("snapshot publish")(
    test("a missing metadata file starts at build 1 and uploads the jar, the pom, and the metadata"):
      val program =
        for
          seen     <- Ref.make(Chunk.empty[Request])
          instant  <- ZIO.clockWith(_.instant)
          result   <- publish(pointer, pk, script(seen, Response(Status.NotFound), Response(Status.Ok)))
          requests <- seen.get
        yield
          val stamp = SnapshotStamp.from(instant).value
          val puts  = requests.filter(_.method == Method.PUT)
          val gets  = requests.filter(_.method == Method.GET)
          assertTrue(
            result.map(uniqueOf) == Right(Some(s"1.0.0-$stamp-1")),
            result.map(sumOf) == Right(Some(Digest.sha256(pk))),
            gets.length == 1,
            puts.length == 12,
            requests.forall(authorized),
            puts.forall(octet),
            gets.forall(req => req.header("Content-Type").isEmpty),
            puts.exists(metadataBuild(1)),
          )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("the stamp moves when the clock moves"):
      val program =
        for
          seen   <- Ref.make(Chunk.empty[Request])
          first  <- publish(pointer, pk, script(seen, Response(Status.NotFound), Response(Status.Created)))
          early  <- ZIO.clockWith(_.instant)
          _      <- TestClock.adjust(1.day)
          late   <- ZIO.clockWith(_.instant)
          second <- publish(pointer, pk, script(seen, Response(Status.NotFound), Response(Status.NoContent)))
        yield
          val earlyStamp = SnapshotStamp.from(early).value
          val lateStamp  = SnapshotStamp.from(late).value
          assertTrue(
            first.map(uniqueOf).map(_.exists(_.contains(earlyStamp))) == Right(true),
            second.map(uniqueOf).map(_.exists(_.contains(lateStamp))) == Right(true),
            earlyStamp != lateStamp,
          )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("a recorded build number increases by one"):
      val program =
        for
          seen   <- Ref.make(Chunk.empty[Request])
          result <- publish(pointer, pk, script(seen, body("<buildNumber>3</buildNumber>"), Response(Status.Ok)))
        yield assertTrue(result.map(uniqueOf).map(_.exists(_.endsWith("-4"))) == Right(true))
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("metadata that cannot name a build never uploads"):
      val cases = List(
        "<metadata></metadata>"                       -> MetadataFault.MissingBuild,
        "<buildNumber>0</buildNumber>"                -> MetadataFault.BadBuild,
        s"<buildNumber>${Int.MaxValue}</buildNumber>" -> MetadataFault.Saturated,
      )
      val program =
        ZIO.foreach(cases): (xml, fault) =>
          for
            seen   <- Ref.make(Chunk.empty[Request])
            result <- publish(pointer, pk, script(seen, body(xml), Response(Status.Ok)))
            puts   <- seen.get.map(_.count(_.method == Method.PUT))
          yield
            val expected = Left(PublishError.Metadata(fault))
            if result == expected && puts == 0 then Nil else List((fault, result, puts))
      program.provideSomeLayer[TestEnvironment](stubbed[Client]).map(rows => assertTrue(rows.flatten == Nil))
    ,
    test("a metadata body that is not in memory is unread"):
      val streamed = Response(Status.Ok).withBody(Body.stream(ZStream.succeed(1.toByte)))
      val program  =
        for
          seen   <- Ref.make(Chunk.empty[Request])
          result <- publish(pointer, pk, script(seen, streamed, Response(Status.Ok)))
        yield assertTrue(result == Left(PublishError.Metadata(MetadataFault.Unread)))
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("a metadata status of 401 or 403 is unauthorized and any other status is that status"):
      val cases = List(
        Status.Unauthorized   -> Left(PublishError.Unauthorized),
        Status.Forbidden      -> Left(PublishError.Unauthorized),
        Status(418, "teapot") -> Left(PublishError.Status(418)),
      )
      statuses(cases, put = Response(Status.Ok), metadata = true)
    ,
    test("a put that is not 200, 201, or 204 fails as unauthorized or that status"):
      val cases = List(
        Status.Forbidden   -> Left(PublishError.Unauthorized),
        Status(500, "err") -> Left(PublishError.Status(500)),
      )
      statuses(cases, put = Response(Status.Ok), metadata = false)
    ,
    test("a release revision, an empty jar, and a non-zip never call the client"):
      val program =
        for
          release <- publish(Revision.Release(ReleaseVersion(ship)), pk, die)
          empty   <- publish(pointer, Chunk.empty, die)
          other   <- publish(pointer, Chunk[Byte](1, 2), die)
        yield assertTrue(
          release == Left(PublishError.NotSnapshot(SnapshotRevisionError.NotSnapshot("1.0.0"))),
          empty == Left(PublishError.EmptyJar),
          other == Left(PublishError.NotAJar),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Client]),
  )

  private def statuses(
      cases: List[(Status, Either[PublishError, Published])],
      put: Response,
      metadata: Boolean,
  ) =
    val program =
      ZIO.foreach(cases): (status, expected) =>
        for
          seen   <- Ref.make(Chunk.empty[Request])
          result <-
            if metadata then publish(pointer, pk, script(seen, Response(status), put))
            else publish(pointer, pk, script(seen, Response(Status.NotFound), Response(status)))
        yield if result == expected then Nil else List((status.code, result))
    program.provideSomeLayer[TestEnvironment](stubbed[Client]).map(rows => assertTrue(rows.flatten == Nil))

  private def publish(
      revision: Revision,
      jar: Chunk[Byte],
      reply: Request => IO[ClientError, Response],
  ) =
    for
      _       <- stub[Client](_.batched(any[Request]))(reply)
      _       <- stub[Client](_.streaming(any[Request]))(ZIO.die(new IllegalStateException("streamed")))
      client  <- ZIO.service[Client]
      place   <- ZIO.fromEither(ready)
      planned <- ZIO.fromEither(artifact(revision, jar))
      result  <- new SnapshotPublisher(place.repo, place.secret, client).publish(planned).either
    yield result

  private def script(seen: Ref[Chunk[Request]], get: Response, put: Response): Request => UIO[Response] =
    req => seen.update(_ :+ req).as(if req.method == Method.GET then get else put)

  private def die(req: Request): UIO[Response] =
    ZIO.die(new IllegalStateException(req.method.toString))

  private def authorized(req: Request): Boolean =
    req.header("Authorization").exists(value => value.startsWith("Basic ") && !value.contains("s3cret"))

  private def octet(req: Request): Boolean =
    req.header("Content-Type").contains("application/octet-stream")

  private def metadataBuild(build: Int)(req: Request): Boolean =
    req.url.render.contains("maven-metadata.xml") &&
      req.body.text.exists(_.contains(s"<buildNumber>$build</buildNumber>"))

  private def body(xml: String): Response =
    Response(Status.Ok).withBody(Body.text(xml))

  private def uniqueOf(published: Published): Option[String] = published match
    case Published.Snapshot(unique, _) => Some(unique.value)
    case _                             => None

  private def sumOf(published: Published): Option[String] = published match
    case Published.Snapshot(_, sum) => Some(sum.value)
    case _                          => None

  private final case class Place(repo: SnapshotRepository, secret: RepositorySecret)

  private def ready: Either[SnapshotRepository.Error | RepositoryUser.Error, Place] =
    SnapshotRepository.from("http://127.0.0.1:9/repo") match
      case Left(error) => Left(error)
      case Right(repo) =>
        RepositoryUser.from("ann") match
          case Left(error) => Left(error)
          case Right(user) =>
            Right(Place(repo, RepositorySecret(user, Config.Secret(Chunk.fromArray("s3cret".toCharArray)))))

  private def artifact(revision: Revision, jar: Chunk[Byte]): Either[ArtifactIdError, Artifact] =
    ArtifactId.from("widget").map(name => Artifact(Organization("demo"), name, revision, jar))
end SnapshotPublishLawSpec
