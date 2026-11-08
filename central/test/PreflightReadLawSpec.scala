package histos.central

import heddle.client.{Authority, Client, ClientError}
import heddle.http.{Body, Method, Request, Response, Status}

import slayer.{any, stub, stubbed, Stubbed}

import zio.*
import zio.test.*

object PreflightReadLawSpec extends ZIOSpecDefault:
  private val gav     = Gav("com.demo", "widget", "1.0.0")
  private val pom     = "com/demo/widget/1.0.0/widget-1.0.0.pom"
  private val repo    = s"https://repo1.maven.org/maven2/$pom"
  private val hex     = "0123456789abcdef0123456789abcdef01234567"
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
      val rows = List(
        ZIO.succeed(Response(Status.NotFound))            -> (Right(()): Either[PreflightError, Unit]),
        ZIO.succeed(Response(Status.Ok))                  -> Left(PreflightError.Published(gav)),
        ZIO.succeed(Response(Status.Created))             -> Left(PreflightError.Published(gav)),
        ZIO.succeed(Response(Status.InternalServerError)) -> Left(PreflightError.Repository(gav, 500)),
        ZIO.fail(ClientError.ConnectTimeout(Authority("repo1.maven.org", 443))) -> Left(
          PreflightError.RepositoryDown(gav, TransportFault.ConnectTimeout("repo1.maven.org", 443))
        ),
      )
      val program =
        ZIO.foreach(rows) { (response, expected) =>
          asked(response)(client => Preflight.head(client, Preflight.Hosts.central.repo1, gav, pom)).map {
            (found, requests) =>
              val aimed = requests match
                case Chunk(req) =>
                  req.method == Method.HEAD && req.url.render == repo && req.header("User-Agent").contains("histos")
                case _ => false
              found == expected && aimed
          }
        }
      program.provideSomeLayer[TestEnvironment](stubbed[Client]).map(flags => assertTrue(flags.forall(identity)))
    ,
    test("a key is present only when the body is armored, and a miss or a status stays typed"):
      fingerprint match
        case Left(error) => ZIO.succeed(assertTrue(error == PreflightError.BadFingerprint))
        case Right(key)  =>
          val lookup = s"$keyHost?op=get&search=0x${key.value}"
          val rows   = List(
            ZIO.succeed(Response.text(armor))                 -> (Right(()): Either[PreflightError, Unit]),
            ZIO.succeed(Response.text("nope"))                -> Left(PreflightError.KeyMissing(key)),
            ZIO.succeed(Response(Status.NotFound))            -> Left(PreflightError.KeyMissing(key)),
            ZIO.succeed(Response(Status.InternalServerError)) -> Left(PreflightError.KeyStatus(500)),
            ZIO.fail(ClientError.ReadTimeout(Authority("keyserver.ubuntu.com", 443))) -> Left(
              PreflightError.KeyDown(TransportFault.ReadTimeout("keyserver.ubuntu.com", 443))
            ),
          )
          val program =
            ZIO.foreach(rows) { (response, expected) =>
              asked(response)(client => Preflight.lookup(client, Preflight.Hosts.central.keyserver, key)).map {
                (found, requests) =>
                  val aimed = requests match
                    case Chunk(req) =>
                      req.method == Method.GET && req.url.render == lookup &&
                      req.header("User-Agent").contains("histos")
                    case _ => false
                  found == expected && aimed
              }
            }
          program.provideSomeLayer[TestEnvironment](stubbed[Client]).map(flags => assertTrue(flags.forall(identity))),
  )

  private def fingerprint: Either[PreflightError, Fingerprint] =
    Fingerprint.from(hex)

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
