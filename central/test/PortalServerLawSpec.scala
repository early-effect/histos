package histos.central

import histos.doctor.Fix
import histos.version.{ReleaseVersion, ShipNumber}
import histos.{Developer, DeveloperId, HttpUrl, License, LicenseName, Organization, PomText, Scm, ScmConnection}

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import heddle.client.{Client, ClientError}
import heddle.http.header.{BasicCredentials, Headers}
import heddle.http.{Body, FormField, MediaType, Method, QueryParams, Response, Status}

import zio.{Chunk, Config, IO, Scope, UIO, ZIO}
import zio.test.*

object PortalServerLawSpec extends ZIOSpecDefault:
  private val jar = Chunk[Byte](0x50.toByte, 0x4b.toByte, 3.toByte, 4.toByte)
  private val asc = Chunk.single(1.toByte)

  // Each law opens a heddle server, which shuts down on the Clock it was installed under: closing the scope polls a
  // connection that is still writing its last response every 10 ms, up to `gracefulShutdownTimeout`. On TestClock that
  // poll never wakes.
  def spec = suite("rehearsal portal")(
    test("a file is not a portal root, and each open error keeps its sentence"):
      val credentials   = PortalOpenError.Credentials.diagnose
      val bind          = PortalOpenError.Bind.diagnose
      val credentialFix = credentials.fixes match
        case Fix.Steps(items) :: Nil =>
          items == ::(
            "Export SONATYPE_USERNAME and SONATYPE_PASSWORD.",
            List("A 401 after they are set says the repository rejected the credentials."),
          )
        case _ => false
      val bindFix = bind.fixes match
        case Fix.Command(line) :: Nil => line.value == "histos help portal"
        case _                        => false
      ZIO.scoped {
        for
          parent <- ZIO.serviceWithZIO[Path](LawDir.child(_, "file"))
          file = parent.resolve("not-a-directory")
          _      <- ZIO.attemptBlocking(Files.writeString(file, "x")).orDie
          opened <- secret.flatMap(value => PortalServer.open(file, value, ZIO.unit).either)
        yield assertTrue(
          opened == Left(PortalOpenError.Bind),
          PortalOpenError.Credentials.message == "SONATYPE_USERNAME and SONATYPE_PASSWORD are required",
          credentials.explain == "Neither variable is set. This is not a rejected login.",
          credentialFix,
          bind.summary == "the rehearsal portal did not bind",
          bind.explain == "The loopback portal could not listen.",
          bindFix,
        )
      }
    ,
    test("a rejected token is still noted, and a bad upload or a missing deployment is not a publish"):
      ZIO.scoped {
        for
          notes   <- zio.Ref.make(0)
          opened  <- rehearse((root, credentials) => PortalServer.open(root, credentials, notes.update(_ + 1)))
          denied  <- ask(opened, Method.POST, "api/v1/publisher/upload", authorize = false)
          heard   <- notes.get
          untyped <- ask(opened, Method.POST, "api/v1/publisher/upload")
          unknown <- ask(opened, Method.POST, "api/v1/publisher/upload?publishingType=NOPE")
          plain   <- ask(
            opened,
            Method.POST,
            "api/v1/publisher/upload?publishingType=USER_MANAGED",
            Body.text("nope"),
          )
          named <- Body
            .multipart(Chunk(FormField.Text("bundle", "nope")))
            .flatMap(body => ask(opened, Method.POST, "api/v1/publisher/upload?publishingType=USER_MANAGED", body))
          blank    <- ask(opened, Method.POST, "api/v1/publisher/status")
          gap      <- ask(opened, Method.POST, "api/v1/publisher/status", query = QueryParams.of("id" -> " "))
          missing  <- ask(opened, Method.POST, "api/v1/publisher/status?id=deploy-missing")
          parent   <- ask(opened, Method.POST, "api/v1/publisher/status?id=..")
          dropped  <- ask(opened, Method.DELETE, "api/v1/publisher/deployment/deploy-missing")
          heardAll <- notes.get
        yield
          val rows = List(
            "rejected token" -> (denied.status == Status.Unauthorized && heard == 1 &&
              denied.body.text.contains("Unauthorized")),
            "no publishing type" -> (untyped.status == Status.BadRequest &&
              untyped.body.text.contains("the upload did not name a publishing type")),
            "unknown publishing type" -> (unknown.status == Status.BadRequest &&
              unknown.body.text.contains("the upload did not name a publishing type")),
            "not multipart" -> (plain.status == Status.BadRequest &&
              plain.body.text.contains("the upload was not a bundle")),
            "bundle was not a file" -> (named.status == Status.BadRequest &&
              named.body.text.contains("the upload was not a bundle")),
            "status without an id" -> (blank.status == Status.NotFound),
            "blank id"             -> (gap.status == Status.NotFound),
            "missing deployment"   -> (missing.status == Status.NotFound),
            "parent id"            -> (parent.status == Status.InternalServerError &&
              parent.body.text.contains("the rehearsal portal could not store the deployment")),
            "drop of a missing deployment" -> (dropped.status == Status.NotFound),
            "every request is noted"       -> (heardAll == 10),
          )
          assertTrue(rows.collect { case (name, false) => name } == Nil)
      }
    ,
    test("hold validates a clean bundle, promote publishes it, and drop removes it"):
      staged(ReleaseMode.Hold, DeploymentState.Validated)
    ,
    test("automatic publishes a clean bundle, promote leaves it published, and drop removes it"):
      staged(ReleaseMode.Automatic, DeploymentState.Published)
    ,
    test("a bundle that is not a zip fails, and that deployment cannot be promoted"):
      ZIO.scoped {
        for
          opened  <- rehearse((root, credentials) => PortalServer.serve(root, credentials))
          body    <- packed(Chunk[Byte](1, 2, 3))
          created <- ask(opened, Method.POST, "api/v1/publisher/upload?publishingType=USER_MANAGED", body)
          id      <- deployment(created)
          status  <- ask(opened, Method.POST, s"api/v1/publisher/status?id=${id.value}")
          denied  <- ask(opened, Method.POST, s"api/v1/publisher/deployment/${id.value}")
          _       <- ask(opened, Method.DELETE, s"api/v1/publisher/deployment/${id.value}")
          gone    <- ask(opened, Method.POST, s"api/v1/publisher/status?id=${id.value}")
        yield assertTrue(
          created.status == Status.Created,
          report(status) == Right((DeploymentState.Failed, Chunk(upload))),
          denied.status == Status.Conflict,
          denied.body.text.contains("the deployment cannot be promoted"),
          gone.status == Status.NotFound,
        )
      }
    ,
    test("an empty bundle fails in either mode and is never validated"):
      ZIO.scoped {
        for
          opened <- rehearse((root, credentials) => PortalServer.serve(root, credentials))
          body   <- packed(zipped(Nil))
          states <- ZIO.foreach(List("USER_MANAGED", "AUTOMATIC")): publishingType =>
            for
              created <- ask(opened, Method.POST, s"api/v1/publisher/upload?publishingType=$publishingType", body)
              id      <- deployment(created)
              status  <- ask(opened, Method.POST, s"api/v1/publisher/status?id=${id.value}")
            yield report(status)
        yield assertTrue(states == List.fill(2)(Right((DeploymentState.Failed, Chunk(upload)))))
      }
    ,
    test("a stored deployment that does not decode is a server error that says it could not be read"):
      val broken  = DeploymentId("broken")
      val pending = PortalStore.Record(DeploymentState.Pending, Chunk.empty, Chunk.empty)
      ZIO.scoped {
        for
          opened <- rehearse((root, credentials) => PortalServer.serve(root, credentials))
          _      <- PortalStore.create(opened.root, broken, pending)
          status <- ask(opened, Method.POST, s"api/v1/publisher/status?id=${broken.value}")
        yield assertTrue(
          status.status == Status.InternalServerError,
          status.body.text.contains("the rehearsal portal could not read the deployment"),
        )
      },
  ).provideSomeLayerShared[TestEnvironment & Scope](LawDir.layer) @@ TestAspect.withLiveClock

  /** Why a law could not finish its exchange with the portal. */
  private type Fault = PortalOpenError | RepositoryUser.Error | ArtifactIdError | ClientError | PortalError |
    DeploymentId.Error

  /** Why a status response does not read as a report. */
  private enum Unread:
    case Code(code: Int)
    case Empty
    case Parse(error: StatusError)

  private val upload: Finding = Finding.Layout(LayoutPlace.Upload)

  private def staged(mode: ReleaseMode, ready: DeploymentState): ZIO[Path & Scope, Fault, TestResult] =
    val publishingType = mode match
      case ReleaseMode.Hold      => "USER_MANAGED"
      case ReleaseMode.Automatic => "AUTOMATIC"
    ZIO.scoped {
      for
        opened <- rehearse((root, credentials) => PortalServer.serve(root, credentials))
        zip    <- clean
        judged = PortalReport.inspect(zip, zip.length.toLong, Set.empty)
        body    <- packed(zip)
        created <- ask(opened, Method.POST, s"api/v1/publisher/upload?publishingType=$publishingType", body)
        id      <- deployment(created)
        before  <- ask(opened, Method.POST, s"api/v1/publisher/status?id=${id.value}")
        _       <- ask(opened, Method.POST, s"api/v1/publisher/deployment/${id.value}")
        after   <- ask(opened, Method.POST, s"api/v1/publisher/status?id=${id.value}")
        _       <- ask(opened, Method.DELETE, s"api/v1/publisher/deployment/${id.value}")
        gone    <- ask(opened, Method.POST, s"api/v1/publisher/status?id=${id.value}")
      yield assertTrue(
        judged == Report(Chunk.empty, Chunk(Gav("demo", "widget", "1.0.0"))),
        created.status == Status.Created,
        created.body.text.contains(id.value + "\n"),
        report(before) == Right((ready, Chunk.empty)),
        report(after) == Right((DeploymentState.Published, Chunk.empty)),
        gone.status == Status.NotFound,
      )
    }

  private final case class Opened(base: String, token: String, root: Path)

  private def rehearse(
      bind: (Path, RepositorySecret) => ZIO[Scope, PortalOpenError, PortalServer.Bound]
  ): ZIO[Path & Scope, PortalOpenError | RepositoryUser.Error, Opened] =
    for
      root        <- ZIO.serviceWithZIO[Path](LawDir.child(_, "portal"))
      credentials <- secret
      bound       <- bind(root, credentials)
    yield Opened(bound.url, bearer(credentials), root)

  private def secret: ZIO[Any, RepositoryUser.Error, RepositorySecret] =
    RepositoryUser.from("ann") match
      case Right(user) =>
        ZIO.succeed(RepositorySecret(user, Config.Secret(Chunk.fromArray("s3cret".toCharArray))))
      case Left(error) => ZIO.fail(error)

  private def bearer(credentials: RepositorySecret): String =
    "Bearer " + BasicCredentials.render(credentials.user.value, new String(credentials.password.value.toArray))

  private def ask(
      opened: Opened,
      method: Method,
      path: String,
      body: Body = Body.empty,
      authorize: Boolean = true,
      query: QueryParams = QueryParams.empty,
  ): ZIO[Any, ClientError, Response] =
    val headers = if authorize then Headers("Authorization", opened.token) else Headers.empty
    val target  = if query.isEmpty then path else s"$path?${query.render}"
    Client.request(method, s"${opened.base}$target", headers, body)

  /** The id a `201` named. A body that is not an id keeps its parse error. */
  private def deployment(response: Response): IO[PortalError | DeploymentId.Error, DeploymentId] =
    if response.status != Status.Created then ZIO.fail(PortalError.Status(response.status.code))
    else
      ZIO
        .fromOption(response.body.text)
        .orElseFail(PortalError.Unnamed)
        .flatMap(text => ZIO.fromEither(DeploymentId.from(text)))

  private def report(response: Response): Either[Unread, (DeploymentState, Chunk[Finding])] =
    if response.status != Status.Ok then Left(Unread.Code(response.status.code))
    else response.body.text.toRight(Unread.Empty).flatMap(PortalReport.parse(_).left.map(Unread.Parse(_)))

  private def packed(bytes: Chunk[Byte]): UIO[Body] =
    Body.multipart(Chunk(FormField.Binary("bundle", bytes, MediaType.OctetStream, Some("bundle.zip"))))

  private def clean: ZIO[Any, ArtifactIdError, Chunk[Byte]] =
    ArtifactId.from("widget") match
      case Left(error) => ZIO.fail(error)
      case Right(name) =>
        val artifact = Artifact(
          Organization("demo"),
          name,
          Revision.Release(ReleaseVersion(ShipNumber("1.0.0"))),
          jar,
          document = PomDocument(
            license =
              Some(License(LicenseName("Apache-2.0"), HttpUrl("https://www.apache.org/licenses/LICENSE-2.0.txt"))),
            developer = Some(Developer(DeveloperId("ada"), PomText("Ada"))),
            scm = Some(Scm(HttpUrl("https://example.com/demo"), ScmConnection("scm:git:https://example.com/demo.git"))),
          ),
          sources = Some(jar),
          javadoc = Some(jar),
        )
        val pom   = utf8(Layout.pom(artifact))
        val dir   = "demo/widget/1.0.0"
        val stem  = "widget-1.0.0"
        val files =
          List(s"$stem.jar" -> jar, s"$stem-sources.jar" -> jar, s"$stem-javadoc.jar" -> jar, s"$stem.pom" -> pom)
        ZIO.succeed(zipped(files.flatMap((file, bytes) => flanked(dir, file, bytes))))

  private def flanked(dir: String, name: String, bytes: Chunk[Byte]): List[(String, Chunk[Byte])] =
    val path = s"$dir/$name"
    List(
      path          -> bytes,
      s"$path.md5"  -> utf8(Digest.md5(bytes) + "\n"),
      s"$path.sha1" -> utf8(Digest.sha1(bytes) + "\n"),
      s"$path.asc"  -> asc,
    )

  private def zipped(entries: List[(String, Chunk[Byte])]): Chunk[Byte] =
    val out = new ByteArrayOutputStream
    val zip = new ZipOutputStream(out)
    entries.foreach: (name, bytes) =>
      zip.putNextEntry(new ZipEntry(name))
      if bytes.nonEmpty then zip.write(bytes.toArray)
      zip.closeEntry()
    zip.close()
    Chunk.fromArray(out.toByteArray)

  private def utf8(text: String): Chunk[Byte] =
    Chunk.fromArray(text.getBytes(StandardCharsets.UTF_8))
end PortalServerLawSpec
