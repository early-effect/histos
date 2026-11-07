package histos.publish

import histos.central.TransportFault

import zio.test.*

object AssetsLawSpec extends ZIOSpecDefault:
  private val host = "release.example"
  private val port = 443

  def spec = suite("asset refusal laws")(
    test("a repository part starts with a letter or digit, and a bracketed loopback is not a bare colon"):
      val named = AssetRepo.from("Good.Name_1/also-ok")
      val digit = AssetRepo.from("1abc/n9")
      assertTrue(
        named.map(_.owner) == Right("Good.Name_1"),
        named.map(_.name) == Right("also-ok"),
        digit.map(repo => repo.owner -> repo.name) == Right("1abc" -> "n9"),
        AssetRepo.from("") == Left(AssetError.Repository("")),
        AssetRepo.from("-no/ok") == Left(AssetError.Repository("-no/ok")),
        AssetRepo.from("no/-ok") == Left(AssetError.Repository("no/-ok")),
        AssetRepo.from("no/") == Left(AssetError.Repository("no/")),
        AssetRepo.from("/ok") == Left(AssetError.Repository("/ok")),
        AssetRepo.from("a b/ok") == Left(AssetError.Repository("a b/ok")),
        AssetRepo.from(".a/b") == Left(AssetError.Repository(".a/b")),
        AssetRepo.from("_a/b") == Left(AssetError.Repository("_a/b")),
        AssetBase.from("http://[::1]").isRight,
        AssetBase.from("http://::1") == Left(AssetError.Host("'http://::1' is not a release host")),
        AssetBase.from("http://0.0.0.0:9") ==
          Left(AssetError.Host("http release host '0.0.0.0' is not loopback")),
      )
    ,
    test("a token renders for the header and its toString stays redacted"):
      val token = AssetToken("super-secret")
      assertTrue(token.render == "super-secret", !token.toString.contains("super-secret"))
    ,
    test("an asset failure names the release host, not the snapshot repository"):
      val transport = List(
        TransportFault.Target("not-a-url")        -> "could not reach the release host: 'not-a-url' is not a target",
        TransportFault.Connect(host, port)        -> s"could not connect to the release host at $host:$port",
        TransportFault.ConnectTimeout(host, port) ->
          s"the release host at $host:$port did not accept a connection",
        TransportFault.Tls(host, port)         -> s"tls failed with the release host at $host:$port",
        TransportFault.ReadTimeout(host, port) -> s"the release host at $host:$port stopped answering",
        TransportFault.Io(host, port)          -> s"the release host at $host:$port closed the exchange",
        TransportFault.Protocol(host, port)    -> s"the release host at $host:$port sent an invalid response",
        TransportFault.Pool(host, port)        -> s"no free connection to the release host at $host:$port",
        TransportFault.Trust                   -> "the release host trust material is unusable",
        TransportFault.Body                    -> "the release host response body failed",
      )
      assertTrue(
        transport.forall((fault, text) => AssetError.Transport(fault).message == text),
        AssetError.Credentials.message == "GITHUB_TOKEN and GITHUB_REPOSITORY are required",
        AssetError.Host("down").message == "down",
        AssetError.Repository("nope").message == "'nope' is not a repository",
        AssetError.UnknownShip("demo").message == "ship 'demo' is not in the catalog",
        AssetError.Duplicate("demo").message == "ship 'demo' is named twice",
        AssetError.Unauthorized.message == "the release host rejected the credentials",
        AssetError.Status(401).message == "the release host failed with HTTP 401",
        AssetError.Status(422).message == "the release host failed with HTTP 422",
        AssetError.Missing("dist").message == "the release dist was not found",
        AssetError.Report.message == "the release did not name an upload",
        AssetError.Inventory.message == "the release asset list could not be read",
      ),
  )
end AssetsLawSpec
