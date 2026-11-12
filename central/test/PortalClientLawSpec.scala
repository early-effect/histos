package histos.central

import heddle.client.{Authority, Client, ClientError, TargetError}
import heddle.error.HttpError
import heddle.http.{Body, Method, Path, QueryParams, Request, Response, Scheme, Status, Url}
import heddle.http.header.BasicCredentials

import slayer.{any, stub, stubbed, Stubbed}

import zio.*
import zio.stream.ZStream
import zio.test.*

object PortalClientLawSpec extends ZIOSpecDefault:
  private val base: PortalBase =
    PortalBase.trusted(Url(Path.root, scheme = Some(Scheme.Http), host = Some("127.0.0.1"), port = Some(9)))

  private val bundle = Chunk[Byte](1, 2, 3)

  private val bearer = "Bearer " + BasicCredentials.render("ann", "s3cret")

  private val origin = "http://127.0.0.1:9"

  def spec = suite("portal client laws")(
    test("hold stops at validated, names the deployment, and sends the bearer token"):
      val program =
        drive(List(created("deploy-1"), json("VALIDATED")))(_.upload(bundle, ReleaseMode.Hold)).map: spoken =>
          assertTrue(
            spoken.result == Right(Deployment(id, DeploymentState.Validated)),
            spoken.requests.map(shown) == List(
              s"POST $origin/api/v1/publisher/upload?publishingType=USER_MANAGED",
              s"POST $origin/api/v1/publisher/status?id=deploy-1",
            ),
            spoken.notes == Chunk("uploading bundle to the portal", "portal deploy-1 is VALIDATED"),
            spoken.requests.forall(authorized),
          )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("a non-terminal state waits, a repeat is quiet, and validated ends the poll"):
      val program =
        for spoken <- driveAfter(
            List(created("deploy-1"), json("PENDING"), json("PENDING"), json("VALIDATING"), json("VALIDATED")),
            3,
          )(_.upload(bundle, ReleaseMode.Hold))
        yield assertTrue(
          spoken.result == Right(Deployment(id, DeploymentState.Validated)),
          spoken.requests.count(status) == 4,
          spoken.notes == Chunk(
            "uploading bundle to the portal",
            "portal deploy-1 is PENDING, waiting for VALIDATED",
            "portal deploy-1 is VALIDATING, waiting for VALIDATED",
            "portal deploy-1 is VALIDATED",
          ),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("automatic does not stop at validated and asks for an automatic publish"):
      val program =
        for spoken <- driveAfter(List(created("deploy-1"), json("VALIDATED"), json("PUBLISHED")), 1)(
            _.upload(bundle, ReleaseMode.Automatic)
          )
        yield assertTrue(
          spoken.result == Right(Deployment(id, DeploymentState.Published)),
          spoken.requests.headOption
            .map(shown)
            .contains(
              s"POST $origin/api/v1/publisher/upload?publishingType=AUTOMATIC"
            ),
          spoken.notes == Chunk(
            "uploading bundle to the portal",
            "portal deploy-1 is VALIDATED, waiting for PUBLISHED",
            "portal deploy-1 is PUBLISHED",
          ),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("an upload rejection is the status, and the poll is not asked"):
      val cases = List(
        Status.Unauthorized        -> PortalError.Unauthorized,
        Status.Forbidden           -> PortalError.Unauthorized,
        Status.InternalServerError -> PortalError.Status(500),
      )
      val program =
        ZIO.foreach(cases) { (status, error) =>
          drive(List(ZIO.succeed(Response(status))))(_.upload(bundle, ReleaseMode.Hold)).map { spoken =>
            val seen = (spoken.result, spoken.requests.length, spoken.notes)
            Option.when(seen != (Left(error), 1, Chunk("uploading bundle to the portal", error.message))):
              status.code -> seen
          }
        }
      program.provideSomeLayer[TestEnvironment](stubbed[Client]).map(rows => assertTrue(rows.flatten == Nil))
    ,
    test("a created body that does not name a deployment is unnamed"):
      val bodies = List(
        "empty"    -> Response.text("", Status.Created),
        "blank"    -> Response.text("  \n", Status.Created),
        "spaced"   -> Response.text("ab cd", Status.Created),
        "streamed" -> Response(Status.Created).withBody(Body.stream(ZStream.succeed(1.toByte))),
      )
      val program =
        ZIO.foreach(bodies) { (label, response) =>
          drive(List(ZIO.succeed(response)))(_.upload(bundle, ReleaseMode.Hold)).map { spoken =>
            val seen = (spoken.result, spoken.requests.length)
            Option.when(seen != (Left(PortalError.Unnamed), 1))(label -> seen)
          }
        }
      program.provideSomeLayer[TestEnvironment](stubbed[Client]).map(rows => assertTrue(rows.flatten == Nil))
    ,
    test("a status body is the first deploymentState key, and a broken body is a report"):
      val accepted = """{"note":"\"deploymentState\"","deploymentState":"VALIDATED"}"""
      val broken   = List("""{"deploymentState":"VALIDATED}""", """{"deploymentState" "VALIDATED"}""")
      val program  =
        for
          held <- drive(List(created("deploy-1"), ZIO.succeed(Response.text(accepted))))(
            _.upload(bundle, ReleaseMode.Hold)
          )
          missed <- ZIO.foreach(broken) { body =>
            drive(List(created("deploy-1"), ZIO.succeed(Response.text(body))))(_.upload(bundle, ReleaseMode.Hold)).map(
              _.result
            )
          }
        yield assertTrue(
          held.result == Right(Deployment(id, DeploymentState.Validated)),
          missed == List(Left(PortalError.Report), Left(PortalError.Report)),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("a failed deployment is that id, and a status rejection is not a state"):
      val program =
        for
          failed <- drive(List(created("deploy-1"), json("FAILED")))(_.upload(bundle, ReleaseMode.Hold))
          denied <- drive(List(created("deploy-1"), ZIO.succeed(Response(Status.Unauthorized))))(
            _.upload(bundle, ReleaseMode.Hold)
          )
        yield assertTrue(
          failed.result == Left(PortalError.Failed(id)),
          failed.notes == Chunk("uploading bundle to the portal", "portal deploy-1 is FAILED"),
          denied.result == Left(PortalError.Unauthorized),
          denied.notes == Chunk(
            "uploading bundle to the portal",
            "the portal rejected the credentials",
          ),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("promote and drop are the deployment path, and 401 and 409 stay typed"):
      val program =
        for
          promoted <- drive(List(ZIO.succeed(Response.empty())))(_.promote(id))
          dropped  <- drive(List(ZIO.succeed(Response.empty())))(_.drop(id))
          conflict <- drive(List(ZIO.succeed(Response(Status.Conflict))))(_.promote(id))
          denied   <- drive(List(ZIO.succeed(Response(Status.Forbidden))))(_.drop(id))
        yield
          val path = s"$origin/api/v1/publisher/deployment/deploy-1"
          assertTrue(
            promoted.result == Right(()),
            dropped.result == Right(()),
            promoted.requests.map(shown) == List(s"POST $path"),
            dropped.requests.map(shown) == List(s"DELETE $path"),
            promoted.requests.forall(authorized),
            conflict.result == Left(PortalError.Status(409)),
            denied.result == Left(PortalError.Unauthorized),
          )
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("a client failure is the transport fault, and the portal is not asked twice"):
      val down    = ClientError.ConnectTimeout(Authority("127.0.0.1", 9))
      val program =
        drive(List(ZIO.fail(down)))(_.upload(bundle, ReleaseMode.Hold)).map { spoken =>
          assertTrue(
            spoken.result == Left(PortalError.Transport(TransportFault.ConnectTimeout("127.0.0.1", 9))),
            spoken.requests.length == 1,
            spoken.notes == Chunk(
              "uploading bundle to the portal",
              "the portal at 127.0.0.1:9 did not accept a connection",
            ),
          )
        }
      program.provideSomeLayer[TestEnvironment](stubbed[Client])
    ,
    test("each client error keeps the authority it reported"):
      val host = Authority("h", 9)
      val rows = List(
        ClientError.InvalidTarget("ftp://nope", TargetError.NoHost) -> TransportFault.Target("ftp://nope"),
        ClientError.Connect(host, new java.io.IOException("down"))  -> TransportFault.Connect("h", 9),
        ClientError.ConnectTimeout(host)                            -> TransportFault.ConnectTimeout("h", 9),
        ClientError.Tls(host, new java.io.IOException("tls"))       -> TransportFault.Tls("h", 9),
        ClientError.ReadTimeout(host)                               -> TransportFault.ReadTimeout("h", 9),
        ClientError.Io(host, new java.io.IOException("io"))         -> TransportFault.Io("h", 9),
        ClientError.Protocol(host, HttpError.Timeout)               -> TransportFault.Protocol("h", 9),
        ClientError.PoolExhausted(host)                             -> TransportFault.Pool("h", 9),
        ClientError.NotAnEventStream(host, Status.Ok)               -> TransportFault.Protocol("h", 9),
        ClientError.InvalidTrust("empty")                           -> TransportFault.Trust,
        ClientError.BodyFailed(new java.io.IOException("body"))     -> TransportFault.Body,
      )
      val misses = rows.collect { case (error, fault) if TransportFaults.from(error) != fault => error }
      assertTrue(misses == Nil),
  )

  private final case class Spoken[A](
      result: Either[PortalError, A],
      requests: Chunk[Request],
      notes: Chunk[String],
  )

  private def drive[A](replies: List[IO[ClientError, Response]])(
      use: Portal => IO[PortalError, A]
  ): ZIO[Client & Stubbed[Client], RepositoryUser.Error, Spoken[A]] =
    spoken(replies)(use)

  private def driveAfter[A](replies: List[IO[ClientError, Response]], steps: Int)(
      use: Portal => IO[PortalError, A]
  ): ZIO[Client & Stubbed[Client], RepositoryUser.Error, Spoken[A]] =
    credentials.flatMap { secret =>
      for
        seen  <- Ref.make(Chunk.empty[Request])
        left  <- Ref.make(replies)
        notes <- Ref.make(Chunk.empty[String])
        _     <- stub[Client](_.batched(any[Request]))(scripted(seen, left))
        _     <- stub[Client](_.streaming(any[Request]))(ZIO.die(new IllegalStateException("streamed")))
        fiber <- called(secret, notes, use).either.fork
        _     <- ZIO.replicateZIODiscard(steps)(TestClock.adjust(1.second))
        heard <- fiber.join
        sent  <- seen.get
        lines <- notes.get
      yield Spoken(heard, sent, lines)
    }

  private def spoken[A](replies: List[IO[ClientError, Response]])(
      use: Portal => IO[PortalError, A]
  ): ZIO[Client & Stubbed[Client], RepositoryUser.Error, Spoken[A]] =
    credentials.flatMap { secret =>
      for
        seen   <- Ref.make(Chunk.empty[Request])
        left   <- Ref.make(replies)
        notes  <- Ref.make(Chunk.empty[String])
        _      <- stub[Client](_.batched(any[Request]))(scripted(seen, left))
        _      <- stub[Client](_.streaming(any[Request]))(ZIO.die(new IllegalStateException("streamed")))
        result <- called(secret, notes, use).either
        sent   <- seen.get
        lines  <- notes.get
      yield Spoken(result, sent, lines)
    }

  private def called[A](
      secret: RepositorySecret,
      notes: Ref[Chunk[String]],
      use: Portal => IO[PortalError, A],
  ): ZIO[Client, PortalError, A] =
    ZIO
      .serviceWithZIO[Portal](use)
      .provideSomeLayer[Client](Portal.connected(base, secret, line => notes.update(_ :+ line)))

  private def scripted(
      seen: Ref[Chunk[Request]],
      replies: Ref[List[IO[ClientError, Response]]],
  ): Request => IO[ClientError, Response] =
    req =>
      seen.update(_ :+ req) *>
        replies
          .modify:
            case head :: tail => (head, tail)
            case Nil          => (ZIO.succeed(Response(Status.InternalServerError)), Nil)
          .flatten

  private def credentials: IO[RepositoryUser.Error, RepositorySecret] =
    RepositoryUser.from("ann") match
      case Right(user) =>
        ZIO.succeed(RepositorySecret(user, Config.Secret(Chunk.fromArray("s3cret".toCharArray))))
      case Left(error) => ZIO.fail(error)

  private def created(name: String): IO[ClientError, Response] =
    ZIO.succeed(Response.text(name + "\n", Status.Created))

  private def json(token: String): IO[ClientError, Response] =
    ZIO.succeed(Response.text(s"""{"deploymentState":"$token"}"""))

  private val id = DeploymentId("deploy-1")

  private def shown(req: Request): String =
    s"${req.method} ${req.url.render}"

  private def status(req: Request): Boolean =
    req.method == Method.POST && req.url.render.contains("/status?")

  private def authorized(req: Request): Boolean =
    req.header("Authorization").contains(bearer) && req.header("User-Agent").contains("histos")
end PortalClientLawSpec
