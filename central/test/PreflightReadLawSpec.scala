package histos.central

import heddle.client.{Authority, Client, ClientError}
import heddle.http.{Method, Request, Response, Status}

import slayer.{any, stub, stubbed, Stubbed}

import zio.*
import zio.test.*

object PreflightReadLawSpec extends ZIOSpecDefault:
  private val gav     = Gav("com.demo", "widget", "1.0.0")
  private val pom     = "com/demo/widget/1.0.0/widget-1.0.0.pom"
  private val repo    = s"https://repo1.maven.org/maven2/$pom"
  private val key     = Fingerprint("0123456789abcdef0123456789abcdef01234567")
  private val armor   = "-----BEGIN PGP PUBLIC KEY BLOCK-----"
  private val keyHost = "https://keyserver.ubuntu.com/pks/lookup"

  def spec = suite("preflight read laws")(
    test("a dotted group is a maven path, and an empty, dotted, or parent segment is not"):
      val good = Preflight.located(Chunk(gav))
      val bad  = List(
        Gav("", "widget", "1.0.0"),
        Gav("com.demo", "", "1.0.0"),
        Gav("com.demo", "widget", ""),
        Gav("com/demo", "widget", "1.0.0"),
        Gav("com.demo", "..", "1.0.0"),
        Gav(".", "widget", "1.0.0"),
        Gav("com.demo", "widget", "1.0.0\\x"),
      )
      val misses = bad.filter { item =>
        Preflight.located(Chunk(item)) != Left(PreflightError.Coordinate(item))
      }
      assertTrue(good == Right(Chunk((gav, pom))), misses == Nil)
    ,
    test("a missing pom is unpublished, a 2xx is already public, and any other answer is that failure"):
      val rows: List[(String, IO[ClientError, Response], Either[PreflightError, Unit])] = List(
        ("404", ZIO.succeed(Response(Status.NotFound)), Right(())),
        ("200", ZIO.succeed(Response(Status.Ok)), Left(PreflightError.Published(gav))),
        ("201", ZIO.succeed(Response(Status.Created)), Left(PreflightError.Published(gav))),
        ("500", ZIO.succeed(Response(Status.InternalServerError)), Left(PreflightError.Repository(gav, 500))),
        (
          "timeout",
          ZIO.fail(ClientError.ConnectTimeout(Authority("repo1.maven.org", 443))),
          Left(PreflightError.RepositoryDown(gav, TransportFault.ConnectTimeout("repo1.maven.org", 443))),
        ),
      )
      val program =
        ZIO.foreach(rows) { (label, response, expected) =>
          asked(response)(client => Preflight.head(client, Preflight.Hosts.central.repo1, gav, pom)).map {
            (found, requests) =>
              val seen = (found, requests.map(aimed))
              Option.when(seen != (expected, Chunk((Method.HEAD, repo, Some("histos")))))(label -> seen)
          }
        }
      program.provideSomeLayer[TestEnvironment](stubbed[Client]).map(misses => assertTrue(misses.flatten == Nil))
    ,
    test("a key is present only when the body is armored, and a miss or a status stays typed"):
      val lookup = s"$keyHost?op=get&search=0x0123456789ABCDEF0123456789ABCDEF01234567"
      val rows: List[(String, IO[ClientError, Response], Either[PreflightError, Unit])] = List(
        ("armored", ZIO.succeed(Response.text(armor)), Right(())),
        ("plain", ZIO.succeed(Response.text("nope")), Left(PreflightError.KeyMissing(key))),
        ("404", ZIO.succeed(Response(Status.NotFound)), Left(PreflightError.KeyMissing(key))),
        ("500", ZIO.succeed(Response(Status.InternalServerError)), Left(PreflightError.KeyStatus(500))),
        (
          "timeout",
          ZIO.fail(ClientError.ReadTimeout(Authority("keyserver.ubuntu.com", 443))),
          Left(PreflightError.KeyDown(TransportFault.ReadTimeout("keyserver.ubuntu.com", 443))),
        ),
      )
      val program =
        ZIO.foreach(rows) { (label, response, expected) =>
          asked(response)(client => Preflight.lookup(client, Preflight.Hosts.central.keyserver, key)).map {
            (found, requests) =>
              val seen = (found, requests.map(aimed))
              Option.when(seen != (expected, Chunk((Method.GET, lookup, Some("histos")))))(label -> seen)
          }
        }
      program.provideSomeLayer[TestEnvironment](stubbed[Client]).map(misses => assertTrue(misses.flatten == Nil)),
  )

  private def aimed(req: Request): (Method, String, Option[String]) =
    (req.method, req.url.render, req.header("User-Agent"))

  private def asked[A](response: IO[ClientError, Response])(
      use: Client => IO[PreflightError, A]
  ): ZIO[Client & Stubbed[Client], Nothing, (Either[PreflightError, A], Chunk[Request])] =
    for
      seen     <- Ref.make(Chunk.empty[Request])
      _        <- stub[Client](_.batched(any[Request]))((req: Request) => seen.update(_ :+ req) *> response)
      _        <- stub[Client](_.streaming(any[Request]))(ZIO.die(new IllegalStateException("streamed")))
      client   <- ZIO.service[Client]
      result   <- use(client).either
      requests <- seen.get
    yield (result, requests)
end PreflightReadLawSpec
