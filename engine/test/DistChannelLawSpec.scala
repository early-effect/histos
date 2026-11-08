package histos.publish

import com.sun.net.httpserver.{HttpExchange, HttpServer}

import histos.jdk.Sha256
import histos.version.{CommitSha, ShipName, SnapshotCoordinate, SnapshotPointer}
import histos.{Jvm, MainClass, ProjectName, Projects, Ship, asset, module, organization, ship, ships}
import histos.`scala`

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentLinkedQueue

import zio.{Chunk, Scope, ZIO}
import zio.test.*

/** A SHA snapshot gets `channel-<sha>`, and the pointer channel is a separate file. Credentials come from the
  * environment, or from `gh` and `origin`.
  */
object DistChannelLawSpec extends ZIOSpecDefault:
  private val scala3  = scala"3.10.0"
  private val number  = ship"1.2.3"
  private val commit  = CommitSha("0123456789abcdef0123456789abcdef01234567")
  private val stored  = SnapshotCoordinate(number, commit.abbreviate)
  private val pointer = SnapshotPointer(number)
  private val stamp   = "20261106.153000-1"
  private val jar     = Chunk[Byte](1, 2, 9)

  def spec = suite("dist channel laws")(
    test("an empty plan does not read gh, and a missing jar is refused before a token"):
      ZIO.acquireRelease(directory("histos-channel-empty"))(delete).flatMap { repo =>
        val die = ZIO.dieMessage("gh")
        graph match
          case Left(_)    => ZIO.succeed(assertTrue(false))
          case Right(built) =>
            val plans = DistChannel.plan(built, List(row(stored.render)))
            for
              quiet   <- DistChannel.prepare(repo, Map("KEEP" -> "1"), Nil, die, _ => sys.error("origin"))
              missing <- plans match
                case Left(_)     => ZIO.succeed(Left(LocalPublishError.NothingToPublish))
                case Right(found) =>
                  DistChannel.prepare(repo, Map.empty, found, die, _ => None).either
            yield assertTrue(
              quiet == Map("KEEP" -> "1"),
              missing == Left(
                LocalPublishError.Stock("dist/app.jar is not a file. ./histos assembly writes the install jar.")
              ),
            )
      }
    ,
    test("gh and origin fill the channel credentials, and an env token is left alone"):
      ZIO.acquireRelease(directory("histos-channel-cred"))(delete).flatMap { repo =>
        graph match
          case Left(_)         => ZIO.succeed(assertTrue(false))
          case Right(built)    =>
            DistChannel.plan(built, List(row(stored.render))) match
              case Left(_)      => ZIO.succeed(assertTrue(false))
              case Right(plans) =>
                val env = Map("GITHUB_TOKEN" -> "env-token", "GITHUB_REPOSITORY" -> "early-effect/histos")
                for
                  _      <- plant(repo)
                  filled <- DistChannel.prepare(
                    repo,
                    Map("SONATYPE_USERNAME" -> "user"),
                    plans,
                    ZIO.succeed(Some("from-gh")),
                    _ => Some("early-effect/histos"),
                  )
                  kept <- DistChannel.prepare(repo, env, plans, ZIO.dieMessage("gh"), _ => sys.error("origin"))
                  bare <- DistChannel.prepare(repo, Map.empty, plans, ZIO.succeed(None), _ => None).either
                yield assertTrue(
                  filled.get("GITHUB_TOKEN").contains("from-gh"),
                  filled.get("GITHUB_REPOSITORY").contains("early-effect/histos"),
                  filled.get("SONATYPE_USERNAME").contains("user"),
                  kept == env,
                  bare == Left(LocalPublishError.Stock("GITHUB_TOKEN and GITHUB_REPOSITORY are required")),
                )
      }
    ,
    test("a sha coordinate and a pointer upload two channels"):
      ZIO.acquireRelease(directory("histos-channel-upload"))(delete).flatMap { repo =>
        graph match
          case Left(_)      => ZIO.succeed(assertTrue(false))
          case Right(built) =>
            DistChannel.plan(built, List(row(stored.render), row(pointer.render))) match
              case Left(_)      => ZIO.succeed(assertTrue(false))
              case Right(plans) =>
                val seen = new ConcurrentLinkedQueue[Hit]()
                ZIO.scoped {
                  for
                    port <- listen(seen)
                    _    <- plant(repo)
                    base = s"http://127.0.0.1:$port"
                    env  = Map(
                      "HISTOS_GITHUB"     -> base,
                      "GITHUB_TOKEN"      -> "secret",
                      "GITHUB_REPOSITORY" -> "early-effect/histos",
                    )
                    noted <- DistChannel.upload(repo, env, plans, List(receipt(stored.render), receipt(pointer.render)))
                  yield
                    val posts = seen.toArray(Array.empty[Hit]).toList.filter(_.method == "POST")
                    val names = posts.map(hit => value(hit.query, "name"))
                    val shaChannel = DistLayout.channel(stored.render)
                    val pointerChannel = DistLayout.channel(pointer.render)
                    assertTrue(
                      plans.map(_.revision) == List(stored.render, pointer.render),
                      noted == List(
                        s"demo dist $shaChannel ${storedUnique}",
                        s"demo dist $pointerChannel ${pointerUnique}",
                      ),
                      names.contains(Some(shaChannel)),
                      names.contains(Some(pointerChannel)),
                      body(posts, shaChannel).contains(s"$storedUnique\n"),
                      body(posts, pointerChannel).contains(s"$pointerUnique\n"),
                      names.contains(Some(DistLayout.jar(storedUnique))),
                      names.contains(Some(DistLayout.jar(pointerUnique))),
                      names.contains(Some(DistLayout.manifest(storedUnique))),
                      names.contains(Some(DistLayout.manifest(pointerUnique))),
                    )
                }
      }
    ,
  )

  private def storedUnique: String =
    s"${number.render}-${commit.abbreviate.value}-$stamp"

  private def pointerUnique: String =
    s"${number.render}-$stamp"

  private def graph =
    val app = module("app", Jvm)(scala3).assembly(MainClass("demo.Main")).asset(asset"dist/app.jar")
    Projects.organization(organization"example.com")(ships(Ship("demo").next(1, 2, 3)(app)), app)

  private def row(revision: String): PublishedModule =
    PublishedModule(
      ProjectName("app"),
      organization"example.com",
      "app",
      revision,
      None,
      None,
      None,
      None,
      ShipName("demo"),
      Sha256.of(Chunk.empty),
      None,
      Nil,
    )

  private def receipt(revision: String): String =
    val unique = if revision == pointer.render then pointerUnique else storedUnique
    row(revision).copy(unique = Some(unique)).rendered(None)

  private def body(posts: List[Hit], name: String): Option[String] =
    posts.collectFirst {
      case hit if value(hit.query, "name").contains(name) =>
        new String(hit.body, StandardCharsets.UTF_8)
    }

  private def value(query: String, key: String): Option[String] =
    query.split("&", -1).iterator.collectFirst {
      case pair if pair.startsWith(s"$key=") => pair.drop(key.length + 1)
    }

  private def plant(repo: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      val path = repo.resolve("dist").resolve("app.jar")
      Files.createDirectories(path.getParent)
      Files.write(path, jar.toArray)
      ()
    }.orDie

  private def listen(seen: ConcurrentLinkedQueue[Hit]): ZIO[Scope, Nothing, Int] =
    ZIO.acquireRelease(
      ZIO.attempt {
        val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
        server.createContext("/", exchange => reply(exchange, seen))
        server.start()
        server
      }.orDie
    )(server => ZIO.attempt(server.stop(0)).orDie).map(_.getAddress.getPort)

  private def reply(exchange: HttpExchange, seen: ConcurrentLinkedQueue[Hit]): Unit =
    val bytes = exchange.getRequestBody.readAllBytes()
    val uri   = exchange.getRequestURI
    val hit   = Hit(
      exchange.getRequestMethod,
      uri.getPath,
      Option(uri.getRawQuery).getOrElse(""),
      bytes,
    )
    seen.add(hit)
    val answer =
      if hit.method == "GET" && hit.path.endsWith("/releases/tags/dist") then
        """{"upload_url":"http://127.0.0.1/releases/42/assets{?name}"}"""
      else if hit.method == "GET" && hit.path.contains("/assets") then "[]"
      else if hit.method == "POST" && hit.path.contains("/assets") then ""
      else "no"
    val status = if answer == "no" then 500 else if hit.method == "POST" then 201 else 200
    val out    = answer.getBytes(StandardCharsets.UTF_8)
    exchange.sendResponseHeaders(status, out.length.toLong)
    val body = exchange.getResponseBody
    body.write(out)
    body.close()

  private final case class Hit(method: String, path: String, query: String, body: Array[Byte])

  private def directory(prefix: String): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking(Files.createTempDirectory(prefix)).orDie

  private def delete(root: Path): zio.UIO[Unit] =
    ZIO.attemptBlocking {
      if Files.exists(root) then
        val walk  = Files.walk(root)
        val paths = new java.util.ArrayList[Path]()
        try
          val found = walk.iterator()
          while found.hasNext do paths.add(found.next())
        finally walk.close()
        paths.sort(java.util.Comparator.comparingInt((path: Path) => path.getNameCount).reversed)
        paths.forEach(path => Files.deleteIfExists(path))
    }.orDie
end DistChannelLawSpec
