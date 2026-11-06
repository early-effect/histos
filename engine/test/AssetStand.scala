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
    * already gone. Asset ids start at 11. They are not the release id and not the uploader id. `prior` is already on
    * the release. A second upload of the same name is HTTP 422, which is what GitHub does.
    */
  def open(
      token: String,
      present: Boolean = true,
      onDelete: UIO[Unit] = ZIO.unit,
      prior: List[Upload] = Nil,
  ): ZIO[Scope, Failure, Bound] =
    val http   = Server.Config.default.copy(host = "127.0.0.1", port = Port(0))
    val seeded = prior.zipWithIndex.map { (upload, index) =>
      Stored((FirstAsset + index).toString, upload)
    }
    for
      noted   <- Ref.make(Note(0, Nil, seeded))
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
      Method.GET / "repos" / string("owner") / string("slug") / "releases" / string("id") / "assets" handle {
        (owner, slug, id, request) => noted.get.flatMap(note => listed(note, owner, slug, id, request))
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

  private def listed(note: Note, owner: String, slug: String, id: String, request: Request) =
    if owner.isEmpty || slug.isEmpty then ZIO.succeed(Response.badRequest("the release did not name a repository"))
    else if id != ReleaseId then ZIO.succeed(Response.notFound())
    else
      val rows  = note.stored.filter(item => item.upload.owner == owner && item.upload.repo == slug)
      val page  = request.query.get("page").flatMap(_.toIntOption).filter(_ > 0).getOrElse(1)
      val size  = request.query.get("per_page").flatMap(_.toIntOption).filter(n => n > 0 && n <= 100).getOrElse(30)
      val start = (page - 1) * size
      val slice = if start >= rows.length then Nil else rows.slice(start, start + size)
      val json  = slice.map(rowJson).mkString("[", ",", "]")
      val more  = start + slice.length < rows.length
      val body  = Response.json(json)
      val next  = s"""</repos/$owner/$slug/releases/$id/assets?page=${page + 1}&per_page=$size>; rel="next""""
      ZIO.succeed(if more then body.withHeader("Link", next) else body)

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
              noted
                .modify { note =>
                  val clash = note.stored.exists { item =>
                    item.upload.owner == owner && item.upload.repo == slug && item.upload.name == name
                  }
                  if clash then (true, note)
                  else
                    val asset = (FirstAsset + note.stored.length).toString
                    (false, note.copy(stored = note.stored :+ Stored(asset, upload)))
                }
                .flatMap { clash =>
                  if clash then ZIO.succeed(Response.json(DuplicateName, Status.UnprocessableContent))
                  else ZIO.succeed(Response.empty(Status.Created))
                },
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

  /** `id` is a decoy. The upload path is `/releases/7/` on this host, not the host written in the url. The uploader
    * `id` is also a decoy, and its `following_url` contains braces inside a string.
    */
  private val ReleaseId  = "7"
  private val FirstAsset = 11
  private val Decoy      =
    s"""{"note":"upload_url","id":99,"upload_url":"https://uploads.github.com/repos/early-effect/histos/releases/$ReleaseId/assets{?name,label}"}"""
  private val DuplicateName =
    """{"message":"Validation Failed","errors":[{"resource":"ReleaseAsset","code":"already_exists","field":"name"}]}"""

  private def rowJson(item: Stored): String =
    val owner = item.upload.owner
    val slug  = item.upload.repo
    val name  = item.upload.name
    val file  = s"https://github.com/$owner/$slug/releases/download/dist/$name"
    val user  = "https://api.github.com/users/octocat/following{/other_user}"
    val asset = s"https://api.github.com/repos/$owner/$slug/releases/assets/${item.id}"
    s"""{"url":"$asset","id":${item.id},"name":"$name",""" +
      s""""uploader":{"login":"octocat","id":1,"following_url":"$user"},""" +
      s""""browser_download_url":"$file"}"""

  private final case class Note(requests: Int, tags: List[String], stored: List[Stored])

  private final case class Stored(id: String, upload: Upload)
end AssetStand
