package histos.central

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import heddle.client.Client
import heddle.http.{Body, Method, QueryParams, Request, Response}
import heddle.http.header.{BasicCredentials, Headers}

import zio.{Chunk, Scope, ZIO, ZLayer, durationInt}
import zio.test.*

object PortalServerSpec extends histos.SuiteClock:
  private val User     = "maven"
  private val Password = "secret"
  private val Group    = "com.example"
  private val Artifact = "widget"
  private val Version  = "1.0.0"
  private val GavLine  = s"$Group:$Artifact:$Version"

  private val classJar: Chunk[Byte] =
    val out     = new ByteArrayOutputStream
    val archive = new ZipOutputStream(out)
    try
      archive.putNextEntry(new ZipEntry("Widget.class"))
      archive.write(Array[Byte](1, 2, 3))
      archive.closeEntry()
    finally archive.close()
    Chunk.fromArray(out.toByteArray)

  private val checks = suite("rehearsal portal")(
    test("hold stops at VALIDATED on loopback and automatic publishes the coordinate"):
      ZIO.scoped:
        for
          (root, base, secret) <- serve
          zip = bundle()
          held       <- upload(base, secret, zip, ReleaseMode.Hold)
          heldReport <- shown(base, secret, held.id)
          automatic  <- upload(base, secret, zip, ReleaseMode.Automatic)
          published  <- shown(base, secret, automatic.id)
          marked     <- present(marker(root))
        yield assertTrue(
          address(base).startsWith("http://127.0.0.1:"),
          !address(base).contains("central.sonatype.com"),
          held.state == DeploymentState.Validated,
          heldReport == Some((DeploymentState.Validated, Chunk.empty)),
          automatic.state == DeploymentState.Published,
          published == Some((DeploymentState.Published, Chunk.empty)),
          marked,
        )
    ,
    test("a held coordinate is not already published"):
      ZIO.scoped:
        for
          (root, base, secret) <- serve
          zip = bundle()
          first  <- upload(base, secret, zip, ReleaseMode.Hold)
          second <- upload(base, secret, zip, ReleaseMode.Hold)
          marked <- present(marker(root))
        yield assertTrue(
          first.state == DeploymentState.Validated,
          second.state == DeploymentState.Validated,
          !marked,
        )
    ,
    test("layout, the file set, and a checksum are the report"):
      ZIO.scoped:
        for
          (_, base, secret) <- serve
          layout            <- refused(base, secret, bundle(rootFile = Some("stray.txt")))
          missing = s"${Group.replace('.', '/')}/$Artifact/$Version/$Artifact-$Version-javadoc.jar"
          files <- refused(base, secret, bundle(omit = Set(s"$Artifact-$Version-javadoc.jar")))
          sum = s"${Group.replace('.', '/')}/$Artifact/$Version/$Artifact-$Version.jar.md5"
          checksum <- refused(base, secret, bundle(corruptJarMd5 = true))
        yield assertTrue(
          layout == Right((DeploymentState.Failed, Chunk(Finding.Layout("stray.txt")))),
          files == Right((DeploymentState.Failed, Chunk(Finding.FileSet(missing)))),
          checksum == Right((DeploymentState.Failed, Chunk(Finding.Checksum(sum)))),
        )
    ,
    test("pom fields, a snapshot version, a snapshot dependency, and an empty pom fail the deployment"):
      ZIO.scoped:
        for
          (_, base, secret) <- serve
          license = bundle(pom = Some(pomXml(license = false)))
          licensed <- refused(base, secret, license)
          version = "1.0.0-SNAPSHOT"
          snapshot   <- refused(base, secret, bundle(version = version))
          dependency <- refused(base, secret, bundle(pom = Some(pomXml(dependency = true))))
          empty      <- refused(base, secret, bundle(pom = Some("")))
          unnamed   = inspect(bundle(pom = Some(pomXml(name = false))))
          developer = inspect(bundle(pom = Some(pomXml(developer = false))))
          scm       = inspect(bundle(pom = Some(pomXml(scm = false))))
        yield assertTrue(
          licensed == Right((DeploymentState.Failed, inspect(license).findings)),
          inspect(license).findings == Chunk(Finding.PomFields("license")),
          snapshot == Right((DeploymentState.Failed, Chunk(Finding.SnapshotVersion(version)))),
          dependency == Right(
            (DeploymentState.Failed, Chunk(Finding.SnapshotDependency("com.example:other:2.0.0-SNAPSHOT")))
          ),
          empty == Right((DeploymentState.Failed, Chunk(Finding.EmptyPom))),
          unnamed.findings == Chunk(Finding.PomFields("name")),
          developer.findings == Chunk(Finding.PomFields("developer")),
          scm.findings == Chunk(Finding.PomFields("scm")),
        )
    ,
    test("a published coordinate refuses the next upload"):
      ZIO.scoped:
        for
          (root, base, secret) <- serve
          zip = bundle()
          first  <- upload(base, secret, zip, ReleaseMode.Automatic)
          second <- refused(base, secret, zip)
          marked <- present(marker(root))
        yield assertTrue(
          first.state == DeploymentState.Published,
          second == Right((DeploymentState.Failed, Chunk(Finding.AlreadyPublished(GavLine)))),
          marked,
        )
    ,
    test("a size over the cap is too large and the cap itself is accepted"):
      val zip      = bundle()
      val accepted = PortalReport.inspect(zip, PortalReport.Cap, Set.empty)
      assertTrue(
        PortalReport.Cap == 1024L * 1024L * 1024L,
        PortalReport.inspect(zip, PortalReport.Cap + 1, Set.empty) == Report(Chunk(Finding.TooLarge), Chunk.empty),
        accepted == Report(Chunk.empty, Chunk(Gav(Group, Artifact, Version))),
        PortalReport.inspect(Chunk.empty, 0L, Set.empty) == Report(Chunk(Finding.Layout("zip")), Chunk.empty),
      )
    ,
    test("promote and drop are the deployment routes"):
      ZIO.scoped:
        for
          (root, base, secret) <- serve
          held                 <- upload(base, secret, bundle(), ReleaseMode.Hold)
          _                    <- ZIO.serviceWithZIO[Portal](_.promote(held.id)).provide(Portal.layer(base, secret))
          after                <- shown(base, secret, held.id)
          marked               <- present(marker(root))
          _                    <- ZIO.serviceWithZIO[Portal](_.drop(held.id)).provide(Portal.layer(base, secret))
          dropped              <- raw(base, secret, held.id)
          gone                 <- present(marker(root))
        yield assertTrue(
          held.state == DeploymentState.Validated,
          after == Some((DeploymentState.Published, Chunk.empty)),
          marked,
          dropped.status.code == 404,
          !gone,
        )
    ,
    test("a second process promotes and drops the deployment on disk"):
      ZIO.scoped:
        for
          (root, base, secret) <- serve
          zip = bundle()
          held       <- upload(base, secret, zip, ReleaseMode.Hold)
          promoted   <- disk(root, "promote", held.id)
          after      <- shown(base, secret, held.id)
          marked     <- present(marker(root))
          dropped    <- disk(root, "drop", held.id)
          gone       <- raw(base, secret, held.id)
          markerGone <- present(marker(root))
          again      <- upload(base, secret, zip, ReleaseMode.Hold)
        yield assertTrue(
          held.state == DeploymentState.Validated,
          promoted == Child(0, promoted.output),
          after == Some((DeploymentState.Published, Chunk.empty)),
          marked,
          dropped == Child(0, dropped.output),
          gone.status.code == 404,
          !markerGone,
          again.state == DeploymentState.Validated,
        )
    ,
    test("a wrong password is unauthorized and the message keeps the secret"):
      ZIO.scoped:
        for
          (_, base, _) <- serve
          bad          <- ZIO.fromEither(
            PortalServer.credentials(Map("SONATYPE_USERNAME" -> User, "SONATYPE_PASSWORD" -> "nope"))
          )
          refused <- ZIO
            .serviceWithZIO[Portal](_.upload(bundle(), ReleaseMode.Hold))
            .provide(Portal.layer(base, bad))
            .either
        yield refused match
          case Left(error) =>
            assertTrue(
              error == PortalError.Unauthorized,
              !error.message.contains("nope"),
              !error.message.contains(Password),
            )
          case Right(deployment) =>
            assertTrue(deployment == Deployment(deployment.id, DeploymentState.Failed)),
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)

  private final case class Child(code: Int, output: String)

  private def serve: ZIO[Scope, PortalOpenError | Throwable, (Path, PortalBase, RepositorySecret)] =
    for
      root   <- directory
      secret <- ZIO.fromEither(
        PortalServer.credentials(Map("SONATYPE_USERNAME" -> User, "SONATYPE_PASSWORD" -> Password))
      )
      bound <- PortalServer.serve(root, secret)
    yield (root, bound.base, secret)

  private def directory: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-portal"))): path =>
      ZIO.attempt {
        if Files.exists(path) then
          val walk  = Files.walk(path)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(item => Files.deleteIfExists(item))
      }.ignore

  private def upload(base: PortalBase, secret: RepositorySecret, zip: Chunk[Byte], mode: ReleaseMode) =
    ZIO.serviceWithZIO[Portal](_.upload(zip, mode)).provide(Portal.layer(base, secret))

  private def refused(
      base: PortalBase,
      secret: RepositorySecret,
      bytes: Chunk[Byte],
  ) =
    for
      result <- ZIO
        .serviceWithZIO[Portal](_.upload(bytes, ReleaseMode.Hold))
        .provide(Portal.layer(base, secret))
        .either
      read <- result match
        case Left(PortalError.Failed(id)) =>
          shown(base, secret, id).map:
            case Some(parsed) => Right(parsed)
            case None         => Left(PortalError.Report)
        case other => ZIO.succeed(other.map(deployment => (deployment.state, Chunk.empty[Finding])))
    yield read

  private def shown(base: PortalBase, secret: RepositorySecret, id: DeploymentId) =
    raw(base, secret, id).map { response =>
      if response.status.code == 200 then body(response).flatMap(PortalReport.parse) else None
    }

  private def raw(base: PortalBase, secret: RepositorySecret, id: DeploymentId) =
    val url     = (base.root / "api" / "v1" / "publisher" / "status").copy(query = QueryParams.of("id" -> id.value))
    val headers = Headers.empty.add("Authorization", bearer(secret)).add("User-Agent", "histos")
    ZIO
      .serviceWithZIO[Client](_.batched(Request(Method.POST, url, headers, Body.empty)))
      .provide(ZLayer.succeed(Client.Config.default) >>> Client.layer)

  private def body(response: Response): Option[String] =
    response.body.strict.map(bytes => new String(bytes.toArray, StandardCharsets.UTF_8))

  private def bearer(secret: RepositorySecret): String =
    "Bearer " + BasicCredentials.render(secret.user.value, new String(secret.password.value.toArray))

  private def address(base: PortalBase): String = base.root.render

  private def present(path: Path): ZIO[Any, Throwable, Boolean] =
    ZIO.attempt(Files.isRegularFile(path))

  private def marker(root: Path): Path =
    root.resolve("published").resolve("com").resolve("example").resolve(Artifact).resolve(Version)

  private def inspect(bytes: Chunk[Byte]): Report =
    PortalReport.inspect(bytes, bytes.length.toLong, Set.empty)

  private def disk(root: Path, action: String, id: DeploymentId): ZIO[Scope, Throwable, Child] =
    ZIO
      .attemptBlocking:
        val java    = Path.of(System.getProperty("java.home"), "bin", "java")
        val argfile = root.resolve(s"$action.args")
        val log     = root.resolve(s"$action.log")
        val lines   = List(
          "-cp",
          System.getProperty("java.class.path"),
          "--enable-native-access=ALL-UNNAMED",
          "histos.central.PortalDisk",
          action,
          root.toAbsolutePath.toString,
          id.value,
        )
        Files.writeString(argfile, lines.map(quoteArg).mkString("\n") + "\n", StandardCharsets.UTF_8)
        val builder = new ProcessBuilder(java.toString, s"@${argfile.toAbsolutePath}")
        builder.environment().clear()
        val path = System.getenv("PATH")
        if path != null then builder.environment().put("PATH", path)
        builder.redirectErrorStream(true)
        builder.redirectOutput(ProcessBuilder.Redirect.to(log.toFile))
        (builder, log)
      .flatMap: (builder, log) =>
        ZIO
          .acquireRelease(ZIO.attemptBlocking(builder.start())): process =>
            ZIO.attemptBlocking(process.destroyForcibly()).ignore
          .flatMap: process =>
            ZIO
              .attemptBlockingInterrupt(process.waitFor())
              .flatMap: code =>
                ZIO.attempt(Files.readString(log, StandardCharsets.UTF_8)).map(output => Child(code, output))

  private def quoteArg(line: String): String =
    if line.exists(char => char.isWhitespace || char == '"' || char == '\\') then
      val body = line.flatMap:
        case '\\' => "\\\\"
        case '"'  => "\\\""
        case char => String.valueOf(char)
      s"\"$body\""
    else line

  private def bundle(
      version: String = Version,
      pom: Option[String] = None,
      omit: Set[String] = Set.empty,
      corruptJarMd5: Boolean = false,
      rootFile: Option[String] = None,
  ): Chunk[Byte] =
    rootFile match
      case Some(name) => zip(List(name -> utf("x")))
      case None       =>
        val stem  = s"$Artifact-$version"
        val dir   = s"${Group.replace('.', '/')}/$Artifact/$version"
        val files = List(
          s"$stem.jar"         -> classJar,
          s"$stem-sources.jar" -> classJar,
          s"$stem-javadoc.jar" -> classJar,
          s"$stem.pom"         -> utf(pom.getOrElse(pomXml(version = version))),
        )
        val entries = files.flatMap: (name, bytes) =>
          if omit.contains(name) then Nil
          else
            val md5 = if corruptJarMd5 && name == s"$stem.jar" then "00" else Digest.md5(bytes)
            sums(s"$dir/$name", bytes, md5)
        zip(entries)

  private def sums(path: String, bytes: Chunk[Byte], md5: String): List[(String, Chunk[Byte])] =
    List(
      path            -> bytes,
      s"$path.md5"    -> utf(md5 + "\n"),
      s"$path.sha1"   -> utf(Digest.sha1(bytes) + "\n"),
      s"$path.sha256" -> utf(Digest.sha256(bytes) + "\n"),
      s"$path.asc"    -> utf("sig\n"),
    )

  private def pomXml(
      version: String = Version,
      name: Boolean = true,
      license: Boolean = true,
      developer: Boolean = true,
      scm: Boolean = true,
      dependency: Boolean = false,
  ): String =
    val named =
      if name then "  <name>widget</name>\n" else ""
    val licensed =
      if license then """  <licenses>
          |    <license>
          |      <name>Apache-2.0</name>
          |      <url>https://www.apache.org/licenses/LICENSE-2.0.txt</url>
          |      <distribution>repo</distribution>
          |    </license>
          |  </licenses>
          |""".stripMargin
      else ""
    val developed =
      if developer then """  <developers>
          |    <developer>
          |      <id>ada</id>
          |      <name>Ada</name>
          |    </developer>
          |  </developers>
          |""".stripMargin
      else ""
    val source =
      if scm then """  <scm>
          |    <url>https://example.com/widget</url>
          |    <connection>scm:git:https://example.com/widget.git</connection>
          |    <developerConnection>scm:git:https://example.com/widget.git</developerConnection>
          |  </scm>
          |""".stripMargin
      else ""
    val depends =
      if dependency then """  <dependencies>
          |    <dependency>
          |      <groupId>com.example</groupId>
          |      <artifactId>other</artifactId>
          |      <version>2.0.0-SNAPSHOT</version>
          |    </dependency>
          |  </dependencies>
          |""".stripMargin
      else ""
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<project xmlns="http://maven.apache.org/POM/4.0.0">
       |  <modelVersion>4.0.0</modelVersion>
       |  <groupId>$Group</groupId>
       |  <artifactId>$Artifact</artifactId>
       |  <version>$version</version>
       |  <packaging>jar</packaging>
       |$named$licensed$developed$source$depends</project>
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
end PortalServerSpec
