package histos.publish

import heddle.{Handler, Method, Request, Response, Routes, Server, Status, string}
import heddle.route.PathDsl.*
import heddle.server.Port

import zio.{Chunk, Ref, Scope, UIO, ZIO}

/** Loopback stand-in for the GitHub release asset routes. Not api.github.com. */
object AssetStand:
  enum Failure:
    case Bind

  final case class Upload(owner: String, repo: String, name: String, bytes: Chunk[Byte], contentType: String)

  final case class Seen(requests: Int, tags: List[String], uploads: List[Upload])

  final case class Bound(base: String, seen: UIO[Seen])

  def open(token: String, present: Boolean = true): ZIO[Scope, Failure, Bound] =
    val http = Server.Config.default.copy(host = "127.0.0.1", port = Port(0))
    for
      noted   <- Ref.make(Seen(0, Nil, Nil))
      started <- Server.install(guarded(token, noted, routes(present, noted)), http).mapError(_ => Failure.Bind)
      port    <- started.port
      base    <- ZIO.fromEither(AssetBase.from(s"http://127.0.0.1:$port")).mapError(_ => Failure.Bind)
    yield Bound(base.root.render, noted.get)

  private def guarded(token: String, noted: Ref[Seen], inner: Routes[Any, Nothing]): Routes[Any, Nothing] =
    Routes.wrap(inner): next =>
      Handler: (request: Request) =>
        noted.update(seen => seen.copy(requests = seen.requests + 1)) *> {
          if request.header("Authorization") == Some("Bearer " + token) then next.run(request)
          else ZIO.succeed(Response.unauthorized())
        }

  private def routes(present: Boolean, noted: Ref[Seen]): Routes[Any, Nothing] =
    Routes(
      Method.GET / "repos" / string("owner") / string("slug") / "releases" / "tags" / string("tag") -> {
        (owner, slug, tag) => lookup(present, noted, owner, slug, tag)
      },
      Method.POST / "repos" / string("owner") / string("slug") / "releases" / string("id") / "assets" handle {
        (owner, slug, id, request) => store(noted, owner, slug, id, request)
      },
    )

  private def lookup(present: Boolean, noted: Ref[Seen], owner: String, slug: String, tag: String) =
    if owner.isEmpty || slug.isEmpty then ZIO.succeed(Response.badRequest("the release did not name a repository"))
    else if !present then ZIO.succeed(Response.notFound())
    else noted.update(seen => seen.copy(tags = seen.tags :+ tag)) *> ZIO.succeed(Response.json(Decoy))

  private def store(noted: Ref[Seen], owner: String, slug: String, id: String, request: Request) =
    if id != "7" then ZIO.succeed(Response.notFound())
    else
      request.query.get("name") match
        case Some(name) if name.nonEmpty =>
          request.body.collect.foldZIO(
            _ => ZIO.succeed(Response.badRequest("the upload had no body")),
            bytes =>
              val typed  = request.header("Content-Type").orElse(request.body.mediaType.map(_.base)).getOrElse("")
              val upload = Upload(owner, slug, name, bytes, typed)
              noted.update(seen => seen.copy(uploads = seen.uploads :+ upload)) *>
                ZIO.succeed(Response.empty(Status.Created)),
          )
        case _ => ZIO.succeed(Response.badRequest("the upload did not name a file"))

  /** `id` is a decoy. The upload path is `/releases/7/` on this host, not the host written in the url. */
  private val Decoy =
    """{"note":"upload_url","id":99,"upload_url":"https://uploads.github.com/repos/early-effect/histos/releases/7/assets{?name,label}"}"""
end AssetStand
