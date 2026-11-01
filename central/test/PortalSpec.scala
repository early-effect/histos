package histos.central

import java.nio.charset.StandardCharsets

import heddle.{BasicCredentials, FormField, Handler, Method, QueryParams, Request, Response, Routes, Server, Status}
import heddle.client.ClientError
import heddle.error.ServerError
import heddle.server.Port

import zio.{Chunk, Config, Ref, Scope, UIO, ZIO, durationInt}
import zio.test.*

object PortalSpec extends ZIOSpecDefault:
  private val bytes = Chunk[Byte](0x50, 0x4b, 0x03, 0x04) ++ Chunk.fromArray("bundle".getBytes(StandardCharsets.UTF_8))
  private val Id    = "dep-1"
  private val User  = "maven"
  private val Password   = "secret"
  private val Upload     = "/api/v1/publisher/upload"
  private val StatusPath = "/api/v1/publisher/status"
  private val Nested     =
    """{"note":"see \"deploymentState\" now","deploymentState":"VALIDATED"}"""

  private val checks = suite("portal client")(
    test("an unchanged portal state is reported again after thirty quiet reads"):
      val (first, quiet0) = Portal.beat(0, Portal.PollChange.Changed)
      val (notes, quiet)  =
        List.fill(Portal.reportEvery - 1)(Portal.PollChange.Same).foldLeft((List.empty[Portal.PollNote], quiet0)) {
          case ((seen, soFar), change) =>
            val (note, next) = Portal.beat(soFar, change)
            (seen :+ note, next)
        }
      val (due, reset)        = Portal.beat(quiet, Portal.PollChange.Same)
      val (early, earlyQuiet) = Portal.beat(4, Portal.PollChange.Changed)
      assertTrue(
        first == Portal.PollNote.Say,
        quiet0 == 0,
        notes.forall(_ == Portal.PollNote.Wait),
        notes.length == Portal.reportEvery - 1,
        quiet == Portal.reportEvery - 1,
        due == Portal.PollNote.Say,
        reset == 0,
        early == Portal.PollNote.Say,
        earlyQuiet == 0,
      )
    ,
    test("the default portal is central and http stays on loopback"):
      val trust = PortalError.Transport(TransportFault.from(ClientError.InvalidTrust("s3cret")))
      val down  = PortalError.Transport(TransportFault.Connect("example.test", 443))
      assertTrue(
        PortalBase.from("https://central.sonatype.com") == Right(PortalBase.central),
        PortalBase.from("https://central.sonatype.com/") == Right(PortalBase.central),
        PortalBase.from("http://127.0.0.1:9").isRight,
        PortalBase.from("http://localhost:9").isRight,
        PortalBase.from("http://[::1]:9").isRight,
        PortalBase.from("http://example.com") == Left(PortalBase.Error.Insecure("example.com")),
        PortalBase.from("not a url") == Left(PortalBase.Error.Malformed("not a url")),
        DeploymentId.from("  dep-1\n").map(_.value) == Right("dep-1"),
        DeploymentId.from(" \n") == Left(PortalError.Unnamed),
        DeploymentId.from("dep 1") == Left(PortalError.Unnamed),
        trust == PortalError.Transport(TransportFault.Trust),
        !trust.message.contains("s3cret"),
        !trust.message.contains("snapshot"),
        down.message == "could not connect to the portal at example.test:443",
      )
    ,
    test("hold uploads the bundle and stops at VALIDATED"):
      ZIO.scoped:
        for
          open <- server(User, Password, Chunk("PENDING", "VALIDATED"))
          (base, seen) = open
          id    <- ZIO.fromEither(DeploymentId.from(Id))
          notes <- Ref.make(List.empty[String])
          built <- layer(base, User, Password, line => notes.update(_ :+ line))
          got   <- ZIO.serviceWithZIO[Portal](_.upload(bytes, ReleaseMode.Hold)).provide(built)
          calls <- seen.get
          lines <- notes.get
        yield assertTrue(
          got == Deployment(id, DeploymentState.Validated),
          loopback(calls, bearer(User, Password)),
          posted(calls, "USER_MANAGED", bytes),
          statuses(calls, Id) == 2,
          lines == List(
            "uploading bundle to the portal",
            s"portal $Id is PENDING, waiting for VALIDATED",
            s"portal $Id is VALIDATED",
          ),
        )
    ,
    test("a deploymentState inside another string is not the state"):
      ZIO.scoped:
        for
          open <- server(User, Password, Chunk.empty, statusBody = Some(Nested))
          (base, seen) = open
          id    <- ZIO.fromEither(DeploymentId.from(Id))
          built <- layer(base, User, Password)
          got   <- ZIO.serviceWithZIO[Portal](_.upload(bytes, ReleaseMode.Hold)).provide(built)
          calls <- seen.get
        yield assertTrue(
          got == Deployment(id, DeploymentState.Validated),
          statuses(calls, Id) == 1,
        )
    ,
    test("hold does not stop on PUBLISHED"):
      ZIO.scoped:
        for
          open <- server(User, Password, Chunk("PENDING", "PUBLISHED", "VALIDATED"))
          (base, seen) = open
          id    <- ZIO.fromEither(DeploymentId.from(Id))
          built <- layer(base, User, Password)
          got   <- ZIO.serviceWithZIO[Portal](_.upload(bytes, ReleaseMode.Hold)).provide(built)
          calls <- seen.get
        yield assertTrue(
          got == Deployment(id, DeploymentState.Validated),
          posted(calls, "USER_MANAGED", bytes),
          statuses(calls, Id) == 3,
        )
    ,
    test("automatic waits until PUBLISHED"):
      ZIO.scoped:
        for
          open <- server(User, Password, Chunk("PENDING", "VALIDATED", "PUBLISHED"))
          (base, seen) = open
          id    <- ZIO.fromEither(DeploymentId.from(Id))
          notes <- Ref.make(List.empty[String])
          built <- layer(base, User, Password, line => notes.update(_ :+ line))
          got   <- ZIO.serviceWithZIO[Portal](_.upload(bytes, ReleaseMode.Automatic)).provide(built)
          calls <- seen.get
          lines <- notes.get
        yield assertTrue(
          got == Deployment(id, DeploymentState.Published),
          posted(calls, "AUTOMATIC", bytes),
          statuses(calls, Id) == 3,
          lines == List(
            "uploading bundle to the portal",
            s"portal $Id is PENDING, waiting for PUBLISHED",
            s"portal $Id is VALIDATED, waiting for PUBLISHED",
            s"portal $Id is PUBLISHED",
          ),
        )
    ,
    test("FAILED stops the poll"):
      ZIO.scoped:
        for
          open <- server(User, Password, Chunk("PENDING", "FAILED"))
          (base, seen) = open
          id      <- ZIO.fromEither(DeploymentId.from(Id))
          notes   <- Ref.make(List.empty[String])
          built   <- layer(base, User, Password, line => notes.update(_ :+ line))
          refused <- ZIO.serviceWithZIO[Portal](_.upload(bytes, ReleaseMode.Automatic)).provide(built).either
          calls   <- seen.get
          lines   <- notes.get
          _       <- ZIO.sleep(Portal.interval) *> ZIO.sleep(250.millis)
          after   <- seen.get
        yield assertTrue(
          refused == Left(PortalError.Failed(id)),
          statuses(calls, Id) == 2,
          after.length == calls.length,
          lines == List(
            "uploading bundle to the portal",
            s"portal $Id is PENDING, waiting for PUBLISHED",
            s"portal $Id is FAILED",
          ),
        )
    ,
    test("promote posts and drop deletes the deployment"):
      ZIO.scoped:
        for
          open <- server(User, Password, Chunk.empty)
          (base, seen) = open
          id    <- ZIO.fromEither(DeploymentId.from(Id))
          built <- layer(base, User, Password)
          _     <- ZIO.serviceWithZIO[Portal](portal => portal.promote(id) *> portal.drop(id)).provide(built)
          calls <- seen.get
        yield assertTrue(
          calls.toList match
            case promote :: drop :: Nil =>
              promote.method == Method.POST && drop.method == Method.DELETE &&
              promote.path == deploymentPath && drop.path == deploymentPath
            case _ => false
          ,
          calls.forall(_.path == deploymentPath),
          statuses(calls, Id) == 0,
        )
    ,
    test("a wrong password is rejected and is not in the error"):
      ZIO.scoped:
        for
          open <- server(User, Password, Chunk("VALIDATED"))
          (base, seen) = open
          built   <- layer(base, User, "nope")
          refused <- ZIO.serviceWithZIO[Portal](_.upload(bytes, ReleaseMode.Hold)).provide(built).either
          calls   <- seen.get
        yield
          val hidden = refused match
            case Left(error) => !error.message.contains("nope") && !error.message.contains(Password)
            case Right(_)    => false
          assertTrue(
            refused == Left(PortalError.Unauthorized),
            hidden,
            calls.toList match
              case call :: Nil =>
                call.host.exists(_.startsWith("127.0.0.1")) && call.authorization != Some(bearer(User, Password))
              case _ => false,
          )
    ,
    test("an error status carries the code and not the body"):
      ZIO.scoped:
        val routes = Routes.fromHandler(Handler { (_: Request) =>
          ZIO.succeed(Response.text("s3cret-body", Status.InternalServerError))
        })
        for
          started <- Server.install(routes, Server.Config.default.copy(host = "127.0.0.1", port = Port(0)))
          port    <- started.port
          built   <- layer(s"http://127.0.0.1:$port", User, Password)
          refused <- ZIO.serviceWithZIO[Portal](_.upload(bytes, ReleaseMode.Hold)).provide(built).either
        yield
          val hidden = refused match
            case Left(error) => !error.message.contains("s3cret-body")
            case Right(_)    => false
          assertTrue(refused == Left(PortalError.Status(500)), hidden)
    ,
    test("a blank deployment id is refused"):
      ZIO.scoped:
        for
          open <- server(User, Password, Chunk.empty, uploadBody = " \n")
          (base, seen) = open
          built   <- layer(base, User, Password)
          refused <- ZIO.serviceWithZIO[Portal](_.upload(bytes, ReleaseMode.Hold)).provide(built).either
          calls   <- seen.get
        yield
          val named = refused match
            case Left(error) => error.message == "the portal upload did not name a deployment"
            case Right(_)    => false
          assertTrue(refused == Left(PortalError.Unnamed), named, statuses(calls, Id) == 0)
    ,
    test("a status without a state is refused"):
      ZIO.scoped:
        for
          open <- server(User, Password, Chunk.empty, statusBody = Some(s"""{"deploymentId":"$Id"}"""))
          (base, _) = open
          built   <- layer(base, User, Password)
          refused <- ZIO.serviceWithZIO[Portal](_.upload(bytes, ReleaseMode.Hold)).provide(built).either
        yield
          val named = refused match
            case Left(error) =>
              error.message == "the portal status did not name a deployment state" && !error.message.contains(Id)
            case Right(_) => false
          assertTrue(refused == Left(PortalError.Report), named),
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(10.seconds)

  private def deploymentPath: String = s"/api/v1/publisher/deployment/$Id"

  private def bearer(user: String, password: String): String =
    "Bearer " + BasicCredentials.render(user, password)

  private def layer(base: String, user: String, password: String, note: String => UIO[Unit] = _ => ZIO.unit) =
    for
      url  <- ZIO.fromEither(PortalBase.from(base))
      name <- ZIO.fromEither(RepositoryUser.from(user))
    yield Portal.layer(url, RepositorySecret(name, Config.Secret(Chunk.fromArray(password.toCharArray))), note)

  private def loopback(calls: Chunk[Call], token: String): Boolean =
    calls.nonEmpty && calls.forall { call =>
      call.authorization == Some(token) &&
      (call.host match
        case Some(host) => host.startsWith("127.0.0.1") && !host.contains("central.sonatype.com")
        case None       => false)
    }

  private def posted(calls: Chunk[Call], mode: String, body: Chunk[Byte]): Boolean =
    calls.toList match
      case call :: _ =>
        call.method == Method.POST && call.path == Upload && call.query.get("publishingType") == Some(mode) &&
        oneBundle(call, body)
      case Nil => false

  private def oneBundle(call: Call, body: Chunk[Byte]): Boolean =
    call.parts.toList match
      case FormField.Binary(name, data, _, filename) :: Nil =>
        name == "bundle" && filename == Some("bundle.zip") && data == body
      case _ => false

  private def statuses(calls: Chunk[Call], id: String): Int =
    calls.count { call =>
      call.method == Method.POST && call.path == StatusPath && call.query.get("id") == Some(id)
    }

  private def server(
      user: String,
      password: String,
      states: Chunk[String],
      uploadBody: String = s"$Id\n",
      statusBody: Option[String] = None,
  ): ZIO[Scope, ServerError, (String, Ref[Chunk[Call]])] =
    val expected = bearer(user, password)
    for
      seen   <- Ref.make(Chunk.empty[Call])
      cursor <- Ref.make(0)
      routes = Routes.fromHandler(Handler { (req: Request) =>
        val path                            = req.path.render
        def record(parts: Chunk[FormField]) =
          seen.update(_ :+ Call(req.method, path, req.query, req.header("Host"), req.header("Authorization"), parts))
        if req.header("Authorization") != Some(expected) then
          record(Chunk.empty) *> ZIO.succeed(Response.unauthorized())
        else if path == Upload && req.method == Method.POST then
          req.body.asMultipart.foldZIO(
            _ => ZIO.succeed(Response.internalServerError()),
            fields => record(fields).as(Response.text(uploadBody, Status.Created)),
          )
        else if path == StatusPath && req.method == Method.POST then
          statusBody match
            case Some(raw) => record(Chunk.empty).as(Response.text(raw))
            case None      =>
              cursor
                .modify { n =>
                  val index = if states.isEmpty then -1 else math.min(n, states.length - 1)
                  val next  = if states.isEmpty then 0 else math.min(n + 1, states.length - 1)
                  (states.lift(index), next)
                }
                .flatMap:
                  case Some(state) =>
                    record(Chunk.empty).as(
                      Response.json(s"""{"deploymentId":"$Id","deploymentState":"$state"}""")
                    )
                  case None => ZIO.succeed(Response.internalServerError())
        else if path.startsWith("/api/v1/publisher/deployment/") &&
          (req.method == Method.POST || req.method == Method.DELETE)
        then record(Chunk.empty).as(Response.empty())
        else record(Chunk.empty).as(Response.notFound())
      })
      started <- Server.install(routes, Server.Config.default.copy(host = "127.0.0.1", port = Port(0)))
      port    <- started.port
    yield (s"http://127.0.0.1:$port", seen)

  private final case class Call(
      method: Method,
      path: String,
      query: QueryParams,
      host: Option[String],
      authorization: Option[String],
      parts: Chunk[FormField],
  )

  def spec = checks
end PortalSpec
