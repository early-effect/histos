package histos.central

import histos.suite.HistosSpec

import heddle.{Handler, Method, Request, Response, Routes, Server, Status}
import heddle.route.PathDsl.*
import heddle.server.Port

import zio.*
import zio.test.*

object PortalGatewaySpec extends HistosSpec:
  private val id = DeploymentId("11111111-1111-1111-1111-111111111111")

  private val retried =
    test("a status 502 is retried until the deployment is published") {
      ZIO.scoped {
        for
          notes <- Ref.make(Chunk.empty[String])
          left  <- Ref.make(2)
          status = left.modify { remaining =>
            if remaining > 0 then (Response(Status.BadGateway), remaining - 1)
            else (Response.json(PortalReport.render(id, DeploymentState.Published, Chunk.empty)), remaining)
          }
          base       <- served(accept, status)
          deployment <- upload(base, notes)
          lines      <- notes.get
        yield assertTrue(
          deployment.state == DeploymentState.Published,
          lines.contains(s"portal ${id.value} answered HTTP 502, retrying"),
          !lines.exists(_.contains("the portal failed")),
        )
      }
    } @@ TestAspect.withLiveClock

  private val refused =
    test("an upload 502 fails and is not retried") {
      ZIO.scoped {
        for
          notes   <- Ref.make(Chunk.empty[String])
          uploads <- Ref.make(0)
          denied = uploads.update(_ + 1).as(Response(Status.BadGateway))
          base   <- served(denied, ZIO.succeed(Response(Status.BadGateway)))
          result <- upload(base, notes).either
          lines  <- notes.get
          calls  <- uploads.get
        yield assertTrue(
          result == Left(PortalError.Status(502)),
          calls == 1,
          lines.contains("the portal failed with HTTP 502"),
          !lines.exists(_.contains("retrying")),
        )
      }
    }

  def spec = suite("portal gateway")(
    test("a gateway status is spent until the wait is over") {
      assertTrue(
        Portal.gateway(502),
        Portal.gateway(503),
        Portal.gateway(504),
        !Portal.gateway(500),
        !Portal.gateway(401),
        Portal.gatewaySpend(Duration.Zero, 1.second, 2.seconds) == Portal.Gateway.Again(1.second),
        Portal.gatewaySpend(1.second, 1.second, 2.seconds) == Portal.Gateway.Again(2.seconds),
        Portal.gatewaySpend(2.seconds, 1.second, 2.seconds) == Portal.Gateway.Stop,
      )
    },
    retried,
    refused,
  )

  private val accept: UIO[Response] =
    ZIO.succeed(Response.text(id.value + "\n", Status.Created))

  private def served(posted: UIO[Response], status: UIO[Response]): ZIO[Scope, Nothing, PortalBase] =
    val http   = Server.Config.default.copy(host = "127.0.0.1", port = Port(0))
    val routes = Routes(
      Method.POST / "api" / "v1" / "publisher" / "upload" ->
        Handler(request => request.body.collect.ignore *> posted),
      Method.POST / "api" / "v1" / "publisher" / "status" ->
        Handler(request => request.body.collect.ignore *> status),
    )
    for
      started <- Server.install(routes, http).mapError(error => new RuntimeException(error.message)).orDie
      port    <- started.port
      base    <- ZIO
        .fromEither(PortalBase.from(s"http://127.0.0.1:$port"))
        .mapError(error => new RuntimeException(error.message))
        .orDie
    yield base

  private def upload(base: PortalBase, notes: Ref[Chunk[String]]): ZIO[Scope, PortalError, Deployment] =
    credentials.flatMap { secret =>
      ZIO
        .serviceWithZIO[Portal](_.upload(Chunk.empty, ReleaseMode.Automatic))
        .provideLayer(Portal.layer(base, secret, line => notes.update(_ :+ line)))
    }

  private def credentials: UIO[RepositorySecret] =
    RepositoryUser.from("sonatype") match
      case Right(user) =>
        ZIO.succeed(RepositorySecret(user, Config.Secret(Chunk.fromArray("secret".toCharArray))))
      case Left(error) => ZIO.dieMessage(error.message)
end PortalGatewaySpec
