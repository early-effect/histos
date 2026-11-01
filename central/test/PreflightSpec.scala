package histos.central

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import heddle.{Handler, Method, QueryParams, Request, Response, Routes, Server, Status}
import heddle.client.{Client, ClientError}
import heddle.error.ServerError
import heddle.http.{Body, Url}
import heddle.http.header.Headers
import heddle.server.Port

import zio.{Chunk, Ref, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object PreflightSpec extends ZIOSpecDefault:
  private val Group    = "com.example"
  private val Artifact = "widget"
  private val Version  = "1.0.0"
  private val Other    = "other"
  private val Second   = "2.0.0"
  private val Hex      = "ab" * 20
  private val Widget   = Gav(Group, Artifact, Version)
  private val Sibling  = Gav(Group, Other, Second)
  private val User     = "maven"
  private val Password = "secret"

  private val classJar: Chunk[Byte] =
    val out     = new ByteArrayOutputStream
    val archive = new ZipOutputStream(out)
    try
      archive.putNextEntry(new ZipEntry("Widget.class"))
      archive.write(Array[Byte](1, 2, 3))
      archive.closeEntry()
    finally archive.close()
    Chunk.fromArray(out.toByteArray)

  private val checks = suite("preflights")(
    test("an unset portal is central, and the public registries are repo1 and the ubuntu keyserver"):
      val gav    = Widget
      val trust  = TransportFault.from(ClientError.InvalidTrust("s3cret"))
      val down   = PreflightError.RepositoryDown(gav, trust)
      val key    = PreflightError.KeyDown(trust)
      val spaced = ("ab" * 20).grouped(4).mkString(" ")
      assertTrue(
        Preflight.portal(Map.empty) == Right(PortalBase.central),
        Preflight.portal(Map("HISTOS_PORTAL" -> "  ")) == Right(PortalBase.central),
        Preflight.portal(Map("HISTOS_PORTAL" -> "https://central.sonatype.com/")) == Right(PortalBase.central),
        Preflight.portal(Map("HISTOS_PORTAL" -> "http://example.com")) ==
          Left(PortalBase.Error.Insecure("example.com")),
        Preflight.Hosts.central.repo1.root.render == "https://repo1.maven.org/maven2",
        Preflight.Hosts.central.keyserver.root.render == "https://keyserver.ubuntu.com/",
        Fingerprint.from(Hex).map(_.value) == Right(Hex.toUpperCase),
        Fingerprint.from("0x" + Hex).map(_.value) == Right(Hex.toUpperCase),
        Fingerprint.from(spaced).map(_.value) == Right(Hex.toUpperCase),
        Fingerprint.from("ab" * 8).map(_.value) == Right("AB" * 8),
        Fingerprint.from("ab" * 32).map(_.value) == Right("AB" * 32),
        Fingerprint.from("ab" * 16) == Left(PreflightError.BadFingerprint),
        Fingerprint.from("zz") == Left(PreflightError.BadFingerprint),
        Fingerprint.from("") == Left(PreflightError.BadFingerprint),
        PreflightError.BadFingerprint.message == "the signing key is not a fingerprint",
        down == PreflightError.RepositoryDown(gav, TransportFault.Trust),
        !down.message.contains("s3cret"),
        !down.message.contains("snapshot"),
        !down.message.contains("portal"),
        key == PreflightError.KeyDown(TransportFault.Trust),
        !key.message.contains("snapshot"),
        PreflightError.Upload(PortalError.Unauthorized).message == "the portal rejected the credentials",
      )
    ,
    test("central heads the pom and looks up the signing key before the upload"):
      ZIO.scoped:
        for
          stand <- standIn(Set.empty, KeyAnswer.Armored)
          seen  <- Ref.make(0)
          key   <- ZIO.fromEither(Fingerprint.from(Hex))
          id    <- ZIO.fromEither(DeploymentId.from("dep-1"))
          first <- Preflight.release(Map.empty, Chunk(Widget), key, upload(seen), stand.hosts)
          slash <- Preflight.release(
            Map("HISTOS_PORTAL" -> "https://central.sonatype.com/"),
            Chunk(Widget),
            key,
            upload(seen),
            stand.hosts,
          )
          calls <- stand.hits.get
          count <- seen.get
        yield
          val heads = calls.filter(_.method == Method.HEAD)
          val looks = calls.filter(_.method == Method.GET)
          val want  = Deployment(id, DeploymentState.Validated)
          assertTrue(
            first == want,
            slash == want,
            count == 2,
            heads.map(_.path) == Chunk(pom(Artifact, Version), pom(Artifact, Version)),
            looks.length == 2,
            looks.forall(lookedUp(key)),
            loopback(calls),
          )
    ,
    test("a pom central already has fails before the upload and does not ask the keyserver"):
      ZIO.scoped:
        for
          stand  <- standIn(Set(pom(Artifact, Version)), KeyAnswer.Missing)
          seen   <- Ref.make(0)
          key    <- ZIO.fromEither(Fingerprint.from(Hex))
          missed <- Preflight
            .release(Map.empty, Chunk(Widget), key, upload(seen), stand.hosts)
            .either
          calls <- stand.hits.get
          count <- seen.get
        yield
          val heads = calls.filter(_.method == Method.HEAD)
          assertTrue(
            missed == Left(PreflightError.Published(Widget)),
            missed.left.forall(_.message == s"${Widget.render} is already on Central"),
            !missed.fold(_.message, _ => "").contains(Password),
            count == 0,
            heads.map(_.path) == Chunk(pom(Artifact, Version)),
            calls.forall(_.method == Method.HEAD),
            loopback(calls),
          )
    ,
    test("a later public coordinate fails the upload"):
      ZIO.scoped:
        for
          stand  <- standIn(Set(pom(Other, Second)), KeyAnswer.Armored)
          seen   <- Ref.make(0)
          key    <- ZIO.fromEither(Fingerprint.from(Hex))
          missed <- Preflight
            .release(Map.empty, Chunk(Widget, Sibling), key, upload(seen), stand.hosts)
            .either
          calls <- stand.hits.get
          count <- seen.get
        yield assertTrue(
          missed == Left(PreflightError.Published(Sibling)),
          count == 0,
          calls.filter(_.method == Method.HEAD).map(_.path) == Chunk(pom(Artifact, Version), pom(Other, Second)),
          calls.forall(_.method == Method.HEAD),
        )
    ,
    test("a missing signing key fails before the upload"):
      ZIO.scoped:
        for
          stand  <- standIn(Set.empty, KeyAnswer.Missing)
          seen   <- Ref.make(0)
          key    <- ZIO.fromEither(Fingerprint.from(Hex))
          missed <- Preflight.release(Map.empty, Chunk(Widget), key, upload(seen), stand.hosts).either
          calls  <- stand.hits.get
          count  <- seen.get
        yield assertTrue(
          missed == Left(PreflightError.KeyMissing(key)),
          missed.left.forall(error => !error.message.contains("missing") && error.message.contains(key.value)),
          count == 0,
          calls.filter(_.method == Method.HEAD).length == 1,
          calls.filter(_.method == Method.GET).forall(lookedUp(key)),
        )
    ,
    test("a page that is not a public key fails before the upload"):
      ZIO.scoped:
        for
          stand  <- standIn(Set.empty, KeyAnswer.Page)
          seen   <- Ref.make(0)
          key    <- ZIO.fromEither(Fingerprint.from(Hex))
          missed <- Preflight.release(Map.empty, Chunk(Widget), key, upload(seen), stand.hosts).either
          count  <- seen.get
        yield assertTrue(
          missed == Left(PreflightError.KeyMissing(key)),
          missed.left.forall(error => !error.message.contains("no results")),
          count == 0,
        )
    ,
    test("repo1 answering with anything but 404 or success fails before the upload"):
      ZIO.scoped:
        for
          stand  <- standIn(Set.empty, KeyAnswer.Armored, absent = 500)
          seen   <- Ref.make(0)
          key    <- ZIO.fromEither(Fingerprint.from(Hex))
          missed <- Preflight.release(Map.empty, Chunk(Widget), key, upload(seen), stand.hosts).either
          calls  <- stand.hits.get
          count  <- seen.get
        yield assertTrue(
          missed == Left(PreflightError.Repository(Widget, 500)),
          count == 0,
          calls.forall(_.method == Method.HEAD),
          calls.length == 1,
        )
    ,
    test("a rehearsal portal uploads a public version and calls neither stand-in"):
      ZIO.scoped:
        for
          (root, base, secret) <- serve
          stand                <- standIn(Set(pom(Artifact, Version)), KeyAnswer.Missing)
          key                  <- ZIO.fromEither(Fingerprint.from(Hex))
          got                  <- Preflight
            .release(
              Map("HISTOS_PORTAL" -> base.root.render),
              Chunk(Widget),
              key,
              ZIO.serviceWithZIO[Portal](_.upload(bundle(), ReleaseMode.Hold)),
              stand.hosts,
            )
            .provideSome[Scope](Portal.layer(base, secret, _ => ZIO.unit))
          during <- stand.hits.get
          repo   <- probe(stand.origin, Method.HEAD, pom(Artifact, Version))
          looked <- probe(
            stand.origin,
            Method.GET,
            "/pks/lookup",
            QueryParams.of("op" -> "get", "search" -> s"0x${key.value}"),
          )
          text   <- ZIO.attempt(Files.readString(stateFile(root, got.id), StandardCharsets.UTF_8).trim)
          marked <- ZIO.attempt(Files.isRegularFile(marker(root)))
        yield assertTrue(
          Preflight.portal(Map("HISTOS_PORTAL" -> base.root.render)) == Right(base),
          base != PortalBase.central,
          base.root.render.startsWith("http://127.0.0.1:"),
          !base.root.render.contains("central.sonatype.com"),
          got.state == DeploymentState.Validated,
          text == "VALIDATED",
          !marked,
          during.isEmpty,
          repo == 200,
          looked == 404,
        )
    ,
    test("a portal that is not central fails before either read"):
      ZIO.scoped:
        for
          stand  <- standIn(Set(pom(Artifact, Version)), KeyAnswer.Missing)
          seen   <- Ref.make(0)
          key    <- ZIO.fromEither(Fingerprint.from(Hex))
          missed <- Preflight
            .release(
              Map("HISTOS_PORTAL" -> "http://example.com"),
              Chunk(Widget),
              key,
              upload(seen),
              stand.hosts,
            )
            .either
          calls <- stand.hits.get
          count <- seen.get
        yield assertTrue(
          missed == Left(PreflightError.Portal(PortalBase.Error.Insecure("example.com"))),
          count == 0,
          calls.isEmpty,
        )
    ,
    test("a coordinate outside the maven path fails before either read"):
      ZIO.scoped:
        for
          stand <- standIn(Set.empty, KeyAnswer.Armored)
          seen  <- Ref.make(0)
          key   <- ZIO.fromEither(Fingerprint.from(Hex))
          gav = Gav(Group, "..", Version)
          missed <- Preflight.release(Map.empty, Chunk(gav), key, upload(seen), stand.hosts).either
          calls  <- stand.hits.get
          count  <- seen.get
        yield assertTrue(
          missed == Left(PreflightError.Coordinate(gav)),
          count == 0,
          calls.isEmpty,
        ),
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)

  private def upload(seen: Ref[Int]): ZIO[Any, PortalError, Deployment] =
    seen.update(_ + 1) *> ZIO.fromEither(DeploymentId.from("dep-1")).map(Deployment(_, DeploymentState.Validated))

  private def lookedUp(key: Fingerprint)(hit: Hit): Boolean =
    hit.path == "/pks/lookup" && hit.query.get("op") == Some("get") && hit.query.get("search") == Some(
      s"0x${key.value}"
    )

  private def loopback(hits: Chunk[Hit]): Boolean =
    hits.nonEmpty && hits.forall { hit =>
      hit.authorization.isEmpty && hit.userAgent == Some("histos") &&
      (hit.host match
        case Some(host) =>
          host.startsWith("127.0.0.1") &&
          !host.contains("repo1.maven.org") &&
          !host.contains("keyserver.ubuntu.com") &&
          !host.contains("central.sonatype.com")
        case None => false)
    }

  private def pom(artifact: String, version: String): String =
    s"/maven2/${Group.replace('.', '/')}/$artifact/$version/$artifact-$version.pom"

  private def serve: ZIO[Scope, PortalOpenError | Throwable, (Path, PortalBase, RepositorySecret)] =
    for
      root   <- directory
      secret <- ZIO.fromEither(
        PortalServer.credentials(Map("SONATYPE_USERNAME" -> User, "SONATYPE_PASSWORD" -> Password))
      )
      bound <- PortalServer.serve(root, secret)
    yield (root, bound.base, secret)

  private def directory: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-preflight"))): path =>
      ZIO.attempt {
        if Files.exists(path) then
          val walk  = Files.walk(path)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(item => Files.deleteIfExists(item))
      }.ignore

  private def stateFile(root: Path, id: DeploymentId): Path =
    root.resolve("deployments").resolve(id.value).resolve("state")

  private def marker(root: Path): Path =
    root.resolve("published").resolve("com").resolve("example").resolve(Artifact).resolve(Version)

  private def probe(origin: String, method: Method, path: String, query: QueryParams = QueryParams.empty) =
    val url = Url.parse(origin + path).copy(query = query)
    ZIO
      .serviceWithZIO[Client](_.batched(Request(method, url, Headers.empty, Body.empty)).map(_.status.code))
      .provide(ZLayer.succeed(Client.Config.default) >>> Client.layer)

  private enum KeyAnswer:
    case Armored, Missing, Page

  private final case class Hit(
      method: Method,
      path: String,
      query: QueryParams,
      host: Option[String],
      authorization: Option[String],
      userAgent: Option[String],
  )

  private final case class Stand(hosts: Preflight.Hosts, hits: Ref[Chunk[Hit]], origin: String)

  private def standIn(
      present: Set[String],
      key: KeyAnswer,
      absent: Int = 404,
  ): ZIO[Scope, ServerError | PortalBase.Error, Stand] =
    for
      hits <- Ref.make(Chunk.empty[Hit])
      routes = Routes.fromHandler(Handler { (request: Request) =>
        val path = request.path.render
        val hit  = Hit(
          request.method,
          path,
          request.query,
          request.header("Host"),
          request.header("Authorization"),
          request.header("User-Agent"),
        )
        val answer =
          if request.method == Method.HEAD && path.startsWith("/maven2/") then
            // A HEAD body would desync the next request on the same connection.
            if present.contains(path) then Response(Status.Ok)
            else if absent == 404 then Response(Status.NotFound)
            else Response(Status(absent, "status"))
          else if request.method == Method.GET && path == "/pks/lookup" then
            key match
              case KeyAnswer.Armored => Response.text(armor)
              case KeyAnswer.Missing => Response.notFound("missing")
              case KeyAnswer.Page    => Response.text("no results")
          else Response.notFound()
        hits.update(_ :+ hit).as(answer)
      })
      started <- Server.install(routes, Server.Config.default.copy(host = "127.0.0.1", port = Port(0)))
      port    <- started.port
      origin = s"http://127.0.0.1:$port"
      repo <- ZIO.fromEither(Preflight.Registry.from(s"$origin/maven2"))
      keys <- ZIO.fromEither(Preflight.Registry.from(origin))
    yield Stand(Preflight.Hosts(repo, keys), hits, origin)

  private val armor =
    """-----BEGIN PGP PUBLIC KEY BLOCK-----
      |
      |mQIN
      |-----END PGP PUBLIC KEY BLOCK-----
      |""".stripMargin

  private def bundle(): Chunk[Byte] =
    val stem  = s"$Artifact-$Version"
    val dir   = s"${Group.replace('.', '/')}/$Artifact/$Version"
    val files = List(
      s"$stem.jar"         -> classJar,
      s"$stem-sources.jar" -> classJar,
      s"$stem-javadoc.jar" -> classJar,
      s"$stem.pom"         -> utf(pomXml),
    )
    zip(files.flatMap((name, bytes) => sums(s"$dir/$name", bytes)))

  private def sums(path: String, bytes: Chunk[Byte]): List[(String, Chunk[Byte])] =
    List(
      path            -> bytes,
      s"$path.md5"    -> utf(Digest.md5(bytes) + "\n"),
      s"$path.sha1"   -> utf(Digest.sha1(bytes) + "\n"),
      s"$path.sha256" -> utf(Digest.sha256(bytes) + "\n"),
      s"$path.asc"    -> utf("sig\n"),
    )

  private val pomXml: String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<project xmlns="http://maven.apache.org/POM/4.0.0">
       |  <modelVersion>4.0.0</modelVersion>
       |  <groupId>$Group</groupId>
       |  <artifactId>$Artifact</artifactId>
       |  <version>$Version</version>
       |  <packaging>jar</packaging>
       |  <name>widget</name>
       |  <licenses>
       |    <license>
       |      <name>Apache-2.0</name>
       |      <url>https://www.apache.org/licenses/LICENSE-2.0.txt</url>
       |      <distribution>repo</distribution>
       |    </license>
       |  </licenses>
       |  <developers>
       |    <developer>
       |      <id>ada</id>
       |      <name>Ada</name>
       |    </developer>
       |  </developers>
       |  <scm>
       |    <url>https://example.com/widget</url>
       |    <connection>scm:git:https://example.com/widget.git</connection>
       |    <developerConnection>scm:git:https://example.com/widget.git</developerConnection>
       |  </scm>
       |</project>
       |""".stripMargin

  private def utf(text: String): Chunk[Byte] =
    Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))

  private def zip(entries: List[(String, Chunk[Byte])]): Chunk[Byte] =
    val out     = new ByteArrayOutputStream
    val archive = new ZipOutputStream(out)
    try
      entries.foreach: (name, bytes) =>
        archive.putNextEntry(new ZipEntry(name))
        archive.write(bytes.toArray)
        archive.closeEntry()
    finally archive.close()
    Chunk.fromArray(out.toByteArray)

  def spec = checks
end PreflightSpec
