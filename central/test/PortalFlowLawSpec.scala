package histos.central

import zio.{Chunk, Config, Ref, ZIO, durationInt}
import zio.test.*

object PortalFlowLawSpec extends ZIOSpecDefault:
  private val fingerprintHex = "0123456789abcdef0123456789abcdef01234567"

  def spec = suite("portal flow")(
    test("a changed poll speaks and resets, and a quiet poll speaks on the 30th read"):
      import Portal.PollChange.{Changed, Same}
      import Portal.PollNote.{Say, Wait}
      val quiet = List.fill(30)(Same).scanLeft((Say, 0))((last, change) => Portal.beat(last._2, change)).drop(1)
      assertTrue(
        Portal.beat(0, Changed) == (Say, 0),
        Portal.beat(17, Changed) == (Say, 0),
        Portal.beat(29, Changed) == (Say, 0),
        Portal.beat(0, Same) == (Wait, 1),
        Portal.beat(28, Same) == (Wait, 29),
        Portal.beat(29, Same) == (Say, 0),
        quiet.map(_._1) == List.fill(29)(Wait) :+ Say,
      )
    ,
    test("a fast body says nothing"):
      for
        notes <- Ref.make(Chunk.empty[String])
        _     <- Portal.keep(line => notes.update(_ :+ line), "tick")(ZIO.unit)
        heard <- notes.get
      yield assertTrue(heard == Chunk.empty)
    ,
    test("a body that outlasts one keep period says that line once"):
      for
        notes  <- Ref.make(Chunk.empty[String])
        fiber  <- Portal.keep(line => notes.update(_ :+ line), "tick")(ZIO.sleep(45.seconds)).fork
        _      <- TestClock.adjust(30.seconds)
        once   <- notes.get
        _      <- TestClock.adjust(15.seconds)
        _      <- fiber.join
        stayed <- notes.get
      yield assertTrue(once == Chunk("tick"), stayed == Chunk("tick"))
    ,
    test("portal credentials trim the user, reject a colon, and redact the password"):
      val password = "s3cret"
      val opened   = PortalServer.credentials(Map("SONATYPE_USERNAME" -> " ann ", "SONATYPE_PASSWORD" -> password))
      assertTrue(
        PortalServer.credentials(Map.empty) == Left(PortalOpenError.Credentials),
        PortalServer.credentials(Map("SONATYPE_USERNAME" -> "ann", "SONATYPE_PASSWORD" -> "")) == Left(
          PortalOpenError.Credentials
        ),
        PortalServer.credentials(Map("SONATYPE_USERNAME" -> "a:b", "SONATYPE_PASSWORD" -> password)) == Left(
          PortalOpenError.Credentials
        ),
        opened.map(_.user.value) == Right("ann"),
        opened.map(_.toString.contains(password)) == Right(false),
        PortalOpenError.Credentials.diagnose.check.value == "publish.credentials",
        PortalOpenError.Bind.diagnose.check.value == "publish.portal",
      )
    ,
    test("a fingerprint is 16, 40, or 64 hex digits, ignoring whitespace and a leading 0x"):
      val short = "aabbccddeeff0011"
      val long  = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
      assertTrue(
        Fingerprint.from(short).map(_.value) == Right(short.toUpperCase),
        Fingerprint.from("  0X" + fingerprintHex).map(_.value) == Right(fingerprintHex.toUpperCase),
        Fingerprint.from(long).map(_.value) == Right(long.toUpperCase),
        Fingerprint.from("abc") == Left(PreflightError.BadFingerprint),
        Fingerprint.from("0x") == Left(PreflightError.BadFingerprint),
        Fingerprint.from("") == Left(PreflightError.BadFingerprint),
        Fingerprint.from(fingerprintHex + "zz") == Left(PreflightError.BadFingerprint),
      )
    ,
    test("an unset or blank portal is central, and a set value is one base"):
      assertTrue(
        Preflight.portal(Map.empty) == Right(PortalBase.central),
        Preflight.portal(Map("HISTOS_PORTAL" -> "  ")) == Right(PortalBase.central),
        Preflight.portal(Map("HISTOS_PORTAL" -> "http://127.0.0.1:9")) == PortalBase.from("http://127.0.0.1:9"),
        Preflight.portal(Map("HISTOS_PORTAL" -> "http://example.com")) == Left(
          PortalBase.Error.Insecure("example.com")
        ),
      )
    ,
    test("a bad portal fails before the upload"):
      val upload = ZIO.die(new IllegalStateException("uploaded"))
      val gavs   = Chunk(Gav("demo", "widget", "1.0.0"))
      for
        key  <- ZIO.fromEither(Fingerprint.from(fingerprintHex))
        ftp  <- Preflight.release(Map("HISTOS_PORTAL" -> "ftp://example.com"), gavs, key, upload).either
        http <- Preflight.release(Map("HISTOS_PORTAL" -> "http://example.com"), gavs, key, upload).either
      yield assertTrue(
        ftp == Left(PreflightError.Portal(PortalBase.Error.Malformed("ftp://example.com"))),
        http == Left(PreflightError.Portal(PortalBase.Error.Insecure("example.com"))),
      )
    ,
    test("a loopback portal skips the reads and maps an upload failure"):
      val gavs = Chunk(Gav("demo", "widget", "1.0.0"))
      val env  = Map("HISTOS_PORTAL" -> "http://127.0.0.1:9")
      for
        key <- ZIO.fromEither(Fingerprint.from(fingerprintHex))
        id  <- ZIO.fromEither(DeploymentId.from("deploy-1"))
        deployment = Deployment(id, DeploymentState.Validated)
        refused <- Preflight.release(env, gavs, key, ZIO.fail(PortalError.Unauthorized)).either
        passed  <- Preflight.release(env, gavs, key, ZIO.succeed(deployment))
      yield assertTrue(
        refused == Left(PreflightError.Upload(PortalError.Unauthorized)),
        passed == deployment,
      )
    ,
    test("a repo1 or keyserver fault is named in the preflight message"):
      val gav  = Gav("demo", "widget", "1.0.0")
      val rows = List(
        PreflightError.RepositoryDown(gav, TransportFault.Connect("h", 1)).message ->
          "could not reach repo1 for demo:widget:1.0.0 (no connection to h:1)",
        PreflightError.RepositoryDown(gav, TransportFault.Tls("h", 1)).message ->
          "could not reach repo1 for demo:widget:1.0.0 (tls failed at h:1)",
        PreflightError.KeyDown(TransportFault.Trust).message ->
          "could not reach keyserver.ubuntu.com (trust material is unusable)",
        PreflightError.KeyDown(TransportFault.Body).message ->
          "could not reach keyserver.ubuntu.com (the response body failed)",
        PreflightError.Client(TransportFault.Pool("h", 9)).message ->
          "could not open the preflight client (no free connection to h:9)",
        PreflightError.Coordinate(gav).message -> "demo:widget:1.0.0 is not a Maven path",
        PreflightError.BadFingerprint.message  -> "the signing key is not a fingerprint",
      )
      val misses = rows.collect { case (found, expected) if found != expected => found }
      assertTrue(misses == Nil),
  )
end PortalFlowLawSpec
