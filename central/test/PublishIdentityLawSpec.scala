package histos.central

import histos.version.{CiVersion, CommitSha, ReleaseVersion, ShipNumber, SnapshotCoordinate, SnapshotPointer}
import histos.{Organization, OrganizationError}

import heddle.client.{Authority, ClientError, TargetError}
import heddle.error.HttpError
import heddle.http.Status

import zio.{Chunk, Config}
import zio.test.*

object PublishIdentityLawSpec extends ZIOSpecDefault:
  private val ship = ShipNumber("1.0.0")
  private val sha  = CommitSha("0123456789abcdef0123456789abcdef01234567")
  private val host = Authority("repo.example", 9)
  private val boom = new java.io.IOException("boom")

  def spec = suite("publish identity")(
    test("a revision renders the form a repository stores"):
      val coordinate = SnapshotCoordinate(ship, sha.abbreviate)
      assertTrue(
        Revision.Ci(CiVersion(ship)).render == "1.0.0-ci",
        Revision.Release(ReleaseVersion(ship)).render == "1.0.0",
        Revision.Pointer(SnapshotPointer(ship)).render == "1.0.0-SNAPSHOT",
        Revision.Snapshot(coordinate).render == "1.0.0-0123456789ab-SNAPSHOT",
        sha.abbreviate.value == "0123456789ab",
        CommitSha.from("0123456789abcdef0123456789abcdef0123456") == Left(
          histos.version.CommitShaError.NotHex("0123456789abcdef0123456789abcdef0123456")
        ),
        CommitSha.from("") == Left(histos.version.CommitShaError.Empty),
      )
    ,
    test("an organization is dotted segments and rejects an empty segment"):
      assertTrue(
        Organization("demo").segments == List("demo"),
        Organization("demo.group").segments == List("demo", "group"),
        Organization.from("..") == Left(OrganizationError.Bad("..")),
        Organization.from("") == Left(OrganizationError.Bad("")),
        Organization.from("demo.") == Left(OrganizationError.Bad("demo.")),
      )
    ,
    test("an artifact id is one path segment"):
      val cases = List(
        "widget" -> true,
        "a.b"    -> true,
        "a-b_c"  -> true,
        "1a"     -> false,
        ".."     -> false,
        "a/b"    -> false,
        ""       -> false,
        "a..b"   -> false,
      )
      val misses = cases.flatMap: (raw, legal) =>
        val parsed = ArtifactId.from(raw)
        val ok     = if legal then parsed.map(_.value) == Right(raw) else parsed == Left(ArtifactIdError.Bad(raw))
        if ok then Nil else List(raw)
      assertTrue(misses == Nil)
    ,
    test("a dependency version is not a Histos revision"):
      val cases = List(
        "1.2.3"          -> true,
        "1.2.3-SNAPSHOT" -> true,
        "v1_2+3"         -> true,
        ""               -> false,
        ".1"             -> false,
        ".."             -> false,
        "1..2"           -> false,
      )
      val misses = cases.flatMap: (raw, legal) =>
        val parsed = DependencyVersion.from(raw)
        val ok     =
          if legal then parsed.map(_.value) == Right(raw) else parsed == Left(DependencyVersionError.Bad(raw))
        if ok then Nil else List(raw)
      assertTrue(misses == Nil)
    ,
    test("digests of abc and of nothing are the known vectors"):
      val abc   = Chunk.fromArray("abc".getBytes(java.nio.charset.StandardCharsets.UTF_8))
      val empty = Chunk.empty[Byte]
      assertTrue(
        Digest.md5(abc) == "900150983cd24fb0d6963f7d28e17f72",
        Digest.sha1(abc) == "a9993e364706816aba3e25717850c26c9cd0d89d",
        Digest.sha256(abc) == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
        Digest.sha256(empty) == "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
        Checksum.sha256(abc).value == Digest.sha256(abc),
      )
    ,
    test("a portal base is https anywhere and http only on loopback"):
      val loop   = PortalBase.from("http://127.0.0.1:9")
      val local  = PortalBase.from("http://localhost/portal")
      val v6     = PortalBase.from("http://[::1]")
      val extra  = PortalBase.from("https://central.sonatype.com/api")
      val secure = extra.map(_ != PortalBase.central)
      val opened = List(loop, local, v6).map(_.map(_ => true))
      assertTrue(
        PortalBase.from("https://central.sonatype.com/") == Right(PortalBase.central),
        opened == List(Right(true), Right(true), Right(true)),
        secure == Right(true),
        PortalBase.from("http://example.com") == Left(PortalBase.Error.Insecure("example.com")),
        PortalBase.from("http://::1") == Left(PortalBase.Error.Malformed("http://::1")),
        PortalBase.from("ftp://example.com") == Left(PortalBase.Error.Malformed("ftp://example.com")),
        PortalBase.from("not a url") == Left(PortalBase.Error.Malformed("not a url")),
      )
    ,
    test("a snapshot repository uses the same scheme rule and a different central path"):
      val central = SnapshotRepository.from("https://central.sonatype.com/repository/maven-snapshots/")
      val other   = SnapshotRepository.from("https://central.sonatype.com/repository/maven-snapshots/extra")
      val loop    = SnapshotRepository.from("http://127.0.0.1:9/repo").map(_ => true)
      val shifted = other.map(_ != SnapshotRepository.central)
      assertTrue(
        central == Right(SnapshotRepository.central),
        shifted == Right(true),
        loop == Right(true),
        SnapshotRepository.from("http://example.com") == Left(SnapshotRepository.Error.Insecure("example.com")),
        SnapshotRepository.from("http://::1") == Left(SnapshotRepository.Error.Malformed("http://::1")),
        SnapshotRepository.from("ftp://example.com") == Left(
          SnapshotRepository.Error.Malformed("ftp://example.com")
        ),
      )
    ,
    test("a repository user rejects a blank and a colon, and the secret redacts the password"):
      val password = "s3cret"
      val secret   = RepositoryUser
        .from(" ann ")
        .map: user =>
          RepositorySecret(user, Config.Secret(Chunk.fromArray(password.toCharArray)))
      assertTrue(
        RepositoryUser.from("ann").map(_.value) == Right("ann"),
        RepositoryUser.from("  ") == Left(RepositoryUser.Error.Empty),
        RepositoryUser.from("a:b") == Left(RepositoryUser.Error.Colon),
        secret.map(_.user.value) == Right("ann"),
        secret.map(_.toString.contains(password)) == Right(false),
        secret.map(_.toString.toLowerCase.contains("redacted")) == Right(true),
      )
    ,
    test("a deployment id rejects empty or whitespace and keeps a slash"):
      assertTrue(
        DeploymentId.from("  ") == Left(PortalError.Unnamed),
        DeploymentId.from("a b") == Left(PortalError.Unnamed),
        DeploymentId.from("deploy-1").map(_.value) == Right("deploy-1"),
        DeploymentId.from("a/b").map(_.value) == Right("a/b"),
        DeploymentId.from("..").map(_.value) == Right(".."),
      )
    ,
    test("every client failure becomes the transport fault that names the target"):
      val rows = List(
        ClientError.InvalidTarget("nope", TargetError.NoHost) -> TransportFault.Target("nope"),
        ClientError.Connect(host, boom)                       -> TransportFault.Connect(host.host, host.port),
        ClientError.ConnectTimeout(host)                      -> TransportFault.ConnectTimeout(host.host, host.port),
        ClientError.Tls(host, boom)                           -> TransportFault.Tls(host.host, host.port),
        ClientError.ReadTimeout(host)                         -> TransportFault.ReadTimeout(host.host, host.port),
        ClientError.Io(host, boom)                            -> TransportFault.Io(host.host, host.port),
        ClientError.Protocol(host, HttpError.HeadersTooLarge) -> TransportFault.Protocol(host.host, host.port),
        ClientError.PoolExhausted(host)                       -> TransportFault.Pool(host.host, host.port),
        ClientError.NotAnEventStream(host, Status.Ok)         -> TransportFault.Protocol(host.host, host.port),
        ClientError.InvalidTrust("bad")                       -> TransportFault.Trust,
        ClientError.BodyFailed(boom)                          -> TransportFault.Body,
      )
      val misses = rows.collect:
        case (error, expected) if TransportFaults.from(error) != expected => expected
      assertTrue(misses == Nil)
    ,
    test("a rejected publish diagnoses as publish.rejected"):
      assertTrue(PublishError.Unauthorized.diagnose.check.value == "publish.rejected"),
  )
end PublishIdentityLawSpec
