package histos.central

import java.nio.file.{Files, Path}

import heddle.{Body, FormField, Handler, Method, Request, Response, Routes, Server, Status, string}
import heddle.http.header.{BasicCredentials, Headers}
import heddle.route.PathDsl.*
import heddle.server.{BodyBytes, Port}

import zio.{Chunk, Config, Random, Scope, UIO, ZIO}

/** `./histos portal`. Loopback only. Deployments stay on disk. */
object PortalServer:
  final case class Bound(base: PortalBase):
    def url: String = base.root.render

  def credentials(env: Map[String, String]): Either[PortalOpenError, RepositorySecret] =
    val user     = env.get("SONATYPE_USERNAME").map(_.trim).filter(_.nonEmpty)
    val password = env.get("SONATYPE_PASSWORD").filter(_.nonEmpty)
    (user, password) match
      case (Some(name), Some(secret)) =>
        RepositoryUser.from(name) match
          case Right(parsed) =>
            Right(RepositorySecret(parsed, Config.Secret(Chunk.fromArray(secret.toCharArray))))
          case Left(_) => Left(PortalOpenError.Credentials)
      case _ => Left(PortalOpenError.Credentials)

  def serve(root: Path, credentials: RepositorySecret): ZIO[Scope, PortalOpenError, Bound] =
    open(root, credentials, ZIO.unit)

  /** `noted` runs on every request, including a rejected token. `serve` notes nothing. */
  private[histos] def open(
      root: Path,
      credentials: RepositorySecret,
      noted: UIO[Unit],
  ): ZIO[Scope, PortalOpenError, Bound] =
    val token = bearer(credentials)
    for
      cap <- ZIO.fromEither(BodyBytes.make(PortalReport.Cap + 1024L * 1024L)).mapError(_ => PortalOpenError.Bind)
      http = Server.Config.default.copy(
        host = "127.0.0.1",
        port = Port(0),
        maxBodyBytes = cap,
      )
      _       <- ZIO.attemptBlocking(Files.createDirectories(root)).mapError(_ => PortalOpenError.Bind)
      started <- Server.install(guarded(token, routes(root), noted), http).mapError(_ => PortalOpenError.Bind)
      port    <- started.port
      base    <- ZIO.fromEither(PortalBase.from(s"http://127.0.0.1:$port")).mapError(_ => PortalOpenError.Bind)
    yield Bound(base)

  private def bearer(credentials: RepositorySecret): String =
    "Bearer " + BasicCredentials.render(credentials.user.value, new String(credentials.password.value.toArray))

  private def guarded(token: String, inner: Routes[Any, Nothing], noted: UIO[Unit]): Routes[Any, Nothing] =
    Routes.wrap(inner): next =>
      Handler: (request: Request) =>
        noted *> {
          if request.header("Authorization") == Some(token) then next.run(request)
          else ZIO.succeed(Response.unauthorized())
        }

  private def routes(root: Path): Routes[Any, Nothing] =
    Routes(
      Method.POST / "api" / "v1" / "publisher" / "upload" ->
        Handler(request => upload(root, request)),
      Method.POST / "api" / "v1" / "publisher" / "status" ->
        Handler(request => status(root, request)),
      Method.POST / "api" / "v1" / "publisher" / "deployment" / string("id") -> { id =>
        change(root, id, promote = true)
      },
      Method.DELETE / "api" / "v1" / "publisher" / "deployment" / string("id") -> { id =>
        change(root, id, promote = false)
      },
    )

  private def upload(root: Path, request: Request): ZIO[Any, Nothing, Response] =
    mode(request) match
      case None        => ZIO.succeed(Response.badRequest("the upload did not name a publishing type"))
      case Some(found) =>
        request.body.asMultipart.foldZIO(
          _ => ZIO.succeed(Response.badRequest("the upload was not a bundle")),
          fields =>
            bundle(fields) match
              case None       => ZIO.succeed(Response.badRequest("the upload was not a bundle"))
              case Some(data) => accept(root, data, found),
        )

  private def accept(root: Path, data: Chunk[Byte], mode: ReleaseMode): ZIO[Any, Nothing, Response] =
    val created =
      for
        known <- PortalStore.published(root)
        report = PortalReport.inspect(data, data.length.toLong, known)
        id <- Random.nextUUID.map(DeploymentId.of)
        _  <- PortalStore.create(root, id, PortalStore.Record(judged(report, mode), report.findings, report.gavs))
      yield Response.text(id.value + "\n", Status.Created)
    created.catchAll(failed)

  private def judged(report: Report, mode: ReleaseMode): DeploymentState =
    if report.findings.nonEmpty then DeploymentState.Failed
    else
      mode match
        case ReleaseMode.Hold      => DeploymentState.Validated
        case ReleaseMode.Automatic => DeploymentState.Published

  private def status(root: Path, request: Request): ZIO[Any, Nothing, Response] =
    request.query.get("id") match
      case None      => ZIO.succeed(Response.notFound())
      case Some(raw) =>
        DeploymentId.from(raw) match
          case Left(_)   => ZIO.succeed(Response.notFound())
          case Right(id) =>
            PortalStore
              .read(root, id)
              .foldZIO(
                failed,
                {
                  case None         => ZIO.succeed(Response.notFound())
                  case Some(record) =>
                    ZIO.succeed(Response.json(PortalReport.render(id, record.state, record.findings)))
                },
              )

  private def change(root: Path, raw: String, promote: Boolean): ZIO[Any, Nothing, Response] =
    DeploymentId.from(raw) match
      case Left(_)   => ZIO.succeed(Response.notFound())
      case Right(id) =>
        val acted = if promote then PortalStore.promote(root, id) else PortalStore.drop(root, id)
        acted.foldZIO(failed, _ => ZIO.succeed(Response.empty()))

  private def mode(request: Request): Option[ReleaseMode] =
    request.query.get("publishingType") match
      case Some("USER_MANAGED") => Some(ReleaseMode.Hold)
      case Some("AUTOMATIC")    => Some(ReleaseMode.Automatic)
      case _                    => None

  private def bundle(fields: Chunk[FormField]): Option[Chunk[Byte]] =
    fields.collectFirst:
      case FormField.Binary("bundle", data, _, _) => data

  private def failed(error: PortalStore.Error): ZIO[Any, Nothing, Response] =
    ZIO.succeed:
      error match
        case PortalStore.Error.Missing(_)       => Response.notFound()
        case PortalStore.Error.NotPromotable(_) =>
          Response.text("the deployment cannot be promoted", Status.Conflict)
        case PortalStore.Error.Read(_) =>
          Response.internalServerError("the rehearsal portal could not read its store")
        case PortalStore.Error.Write(_) =>
          Response.internalServerError("the rehearsal portal could not store the deployment")
        case PortalStore.Error.Unreadable(_, _) =>
          Response.internalServerError("the rehearsal portal could not read the deployment")
end PortalServer

enum PortalOpenError:
  case Credentials
  case Bind

  def message: String = this match
    case Credentials => "SONATYPE_USERNAME and SONATYPE_PASSWORD are required"
    case Bind        => "the rehearsal portal did not bind"

  def diagnose: histos.doctor.Diagnostic = this match
    case Credentials =>
      histos.doctor.Noted.error(
        "publish.credentials",
        message,
        "Neither variable is set. This is not a rejected login.",
        histos.doctor.Noted.steps(
          "Export SONATYPE_USERNAME and SONATYPE_PASSWORD.",
          "A 401 after they are set says the repository rejected the credentials.",
        ),
      )
    case Bind =>
      histos.doctor.Noted.carry(
        "publish.portal",
        message,
        "The loopback portal could not listen.",
        "histos help portal",
      )
end PortalOpenError
