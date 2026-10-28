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

  /** `onDelete` runs after the asset is selected and before the response, so a test can see that the deployment is
    * already gone. Asset ids start at 11. They are not the release id and not the decoy id.
    */
  def open(
      token: String,
      present: Boolean = true,
      onDelete: UIO[Unit] = ZIO.unit,
  ): ZIO[Scope, Failure, Bound] =
    val http = Server.Config.default.copy(host = "127.0.0.1", port = Port(0))
    for
      noted   <- Ref.make(Note(0, Nil, Nil))
      started <- Server
        .install(guarded(token, noted, routes(present, noted, onDelete)), http)
        .mapError(_ => Failure.Bind)
      port <- started.port
      base <- ZIO.fromEither(AssetBase.from(s"http://127.0.0.1:$port")).mapError(_ => Failure.Bind)
    yield Bound(base.root.render, noted.get.map(note => Seen(note.requests, note.tags, note.stored.map(_.upload))))

  private def guarded(token: String, noted: Ref[Note], inner: Routes[Any, Nothing]): Routes[Any, Nothing] =
    Routes.wrap(inner): next =>
      Handler: (request: Request) =>
        noted.update(note => note.copy(requests = note.requests + 1)) *> {
          if request.header("Authorization") == Some("Bearer " + token) then next.run(request)
          else ZIO.succeed(Response.unauthorized())
        }

  private def routes(present: Boolean, noted: Ref[Note], onDelete: UIO[Unit]): Routes[Any, Nothing] =
    Routes(
      Method.GET / "repos" / string("owner") / string("slug") / "releases" / "tags" / string("tag") -> {
        (owner, slug, tag) => lookup(present, noted, owner, slug, tag)
      },
      Method.GET / "repos" / string("owner") / string("slug") / "releases" / string("id") / "assets" -> {
        (owner, slug, id) => noted.get.flatMap(note => listed(note, owner, slug, id))
      },
      Method.POST / "repos" / string("owner") / string("slug") / "releases" / string("id") / "assets" handle {
        (owner, slug, id, request) => store(noted, owner, slug, id, request)
      },
      Method.DELETE / "repos" / string("owner") / string("slug") / "releases" / "assets" / string("asset") -> {
        (owner, slug, asset) => discard(noted, owner, slug, asset, onDelete)
      },
    )

  private def lookup(present: Boolean, noted: Ref[Note], owner: String, slug: String, tag: String) =
    if owner.isEmpty || slug.isEmpty then ZIO.succeed(Response.badRequest("the release did not name a repository"))
    else if !present then ZIO.succeed(Response.notFound())
    else noted.update(note => note.copy(tags = note.tags :+ tag)) *> ZIO.succeed(Response.json(Decoy))

  private def listed(note: Note, owner: String, slug: String, id: String) =
    if owner.isEmpty || slug.isEmpty then ZIO.succeed(Response.badRequest("the release did not name a repository"))
    else if id != ReleaseId then ZIO.succeed(Response.notFound())
    else
      val rows = note.stored.filter(item => item.upload.owner == owner && item.upload.repo == slug)
      val json = rows.map(item => s"""{"id":${item.id},"name":"${item.upload.name}"}""").mkString("[", ",", "]")
      ZIO.succeed(Response.json(json))

  private def store(noted: Ref[Note], owner: String, slug: String, id: String, request: Request) =
    if id != ReleaseId then ZIO.succeed(Response.notFound())
    else
      request.query.get("name") match
        case Some(name) if name.nonEmpty =>
          request.body.collect.foldZIO(
            _ => ZIO.succeed(Response.badRequest("the upload had no body")),
            bytes =>
              val typed  = request.header("Content-Type").orElse(request.body.mediaType.map(_.base)).getOrElse("")
              val upload = Upload(owner, slug, name, bytes, typed)
              noted.update { note =>
                val asset  = (FirstAsset + note.stored.length).toString
                val stored = Stored(asset, upload)
                note.copy(stored = note.stored :+ stored)
              } *> ZIO.succeed(Response.empty(Status.Created)),
          )
        case _ => ZIO.succeed(Response.badRequest("the upload did not name a file"))

  private def discard(noted: Ref[Note], owner: String, slug: String, asset: String, onDelete: UIO[Unit]) =
    if owner.isEmpty || slug.isEmpty || asset.isEmpty then ZIO.succeed(Response.badRequest("the asset was not named"))
    else
      noted
        .modify { note =>
          val kept =
            note.stored.filterNot(item => item.id == asset && item.upload.owner == owner && item.upload.repo == slug)
          (kept.length != note.stored.length, note.copy(stored = kept))
        }
        .flatMap { gone =>
          if gone then onDelete.as(Response.empty(Status.NoContent))
          else ZIO.succeed(Response.notFound())
        }

  /** `id` is a decoy. The upload path is `/releases/7/` on this host, not the host written in the url. */
  private val ReleaseId  = "7"
  private val FirstAsset = 11
  private val Decoy      =
    s"""{"note":"upload_url","id":99,"upload_url":"https://uploads.github.com/repos/early-effect/histos/releases/$ReleaseId/assets{?name,label}"}"""

  private final case class Note(requests: Int, tags: List[String], stored: List[Stored])

  private final case class Stored(id: String, upload: Upload)
end AssetStand
