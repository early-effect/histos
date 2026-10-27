package histos.publish

import histos.cache.ContentStore
import histos.central.{DeploymentState, PortalBase, PortalServer, ReleaseMode, RepositorySecret}
import histos.load.{ApiJar, Loader}
import histos.resolve.Resolve
import histos.version.{Catalog, ManualVersion}
import histos.zinc.GraphCompile
import histos.{
  ArtifactBase,
  Developer,
  Jvm,
  License,
  MainClass,
  Module,
  Organization,
  Projects,
  Publication,
  ScalaVersion,
  Scm,
  connection,
  developer,
  license,
  module,
  pom,
  url,
}

import java.nio.file.{Files, Path}
import java.security.KeyPairGenerator
import java.util.{Base64, Date}

import scala.jdk.CollectionConverters.*

import org.bouncycastle.bcpg.{
  ArmoredOutputStream,
  HashAlgorithmTags,
  PublicKeyAlgorithmTags,
  PublicKeyPacket,
  SymmetricKeyAlgorithmTags,
}
import org.bouncycastle.bcpg.sig.KeyFlags
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.{PGPKeyRingGenerator, PGPSignature}
import org.bouncycastle.openpgp.operator.jcajce.{
  JcaPGPContentSignerBuilder,
  JcaPGPDigestCalculatorProviderBuilder,
  JcaPGPKeyPair,
  JcePBESecretKeyEncryptorBuilder,
}
import zio.{Chunk, Ref, Scope, System, ZIO, ZLayer, durationInt}
import zio.test.*

object ReleaseSpec extends ZIOSpecDefault:
  private val passphrase = "test-passphrase"
  private val password   = "secret"

  private val licenseValue =
    License(license"Apache-2.0", url"https://www.apache.org/licenses/LICENSE-2.0.txt")
  private val developerValue = Developer(developer"early-effect", pom"Early Effect")
  private val scmValue       =
    Scm(url"https://example.com/source", connection"scm:git:https://example.com/source.git")

  private val hello      = "package demo\n\nobject Hello {\n  def greet: String = \"hi\"\n}\n"
  private val libSource  = "package lib\n\nobject Lib {\n  def word: String = \"lib\"\n}\n"
  private val appSource  = "package demo\n\nobject App {\n  def go: String = lib.Lib.word\n}\n"
  private val siteSource = "package demo\n\nobject Site {\n  def go: String = \"site\"\n}\n"

  private val scala3 = ScalaVersion("3.10.0")

  private val hold =
    test("a snapshot ship is refused and the release ship is held, then promote publishes it once"):
      ZIO.scoped {
        for
          jdk    <- GraphCompile.jdkHere
          root   <- temp
          portal <- temp
          hits   <- Ref.make(0)
          secret <- secretOf
          bound  <- PortalServer.open(portal, secret, hits.update(_ + 1))
          key    <- Keys.signing
          built  <- ZIO.fromEither(mixed)
          _      <- plant(
            root,
            built,
            List(
              "core/src/main/scala/demo/Hello.scala" -> hello,
              "lib/src/main/scala/lib/Lib.scala"     -> libSource,
              "app/src/main/scala/demo/App.scala"    -> appSource,
            ),
            "cell core 3.10.0 jvm\ncell lib 3.10.0 jvm\ncell app 3.10.0 jvm\n",
          )
          env = signing(bound.url, key.armor)
          report <- PublishRun
            .release(root, built, Nil, jdk, env, ReleaseMode.Hold)
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          ids      <- deploymentIds(portal)
          record   <- text(deployment(root, "libs"))
          state    <- states(portal, ids)
          marker   <- exists(published(portal, "core_3", "1.0.0"))
          releases <- exists(root.resolve(".histos").resolve("releases"))
          promoted <- report.deployed match
            case List(deployed) =>
              PublishRun.promote(root, env, deployed.id.value) *>
                text(deployment(root, "libs"))
                  .zip(states(portal, ids))
                  .zip(exists(published(portal, "core_3", "1.0.0")))
            case _ => ZIO.succeed(("", List.empty[(String, String)], false))
          (after, publishedState, marked) = promoted
          again <- PublishRun
            .release(root, built, Nil, jdk, env, ReleaseMode.Hold)
            .provideSomeLayer[histos.resolve.LockedTools](
              histos.ToolFixture.resolver
            )
          stayed <- deploymentIds(portal)
        yield assertTrue(
          report.refused.contains("snaps is 1.2.3-SNAPSHOT, which is not a release"),
          report.refused.contains("tool depends on snapshot lib, so it is not published"),
          report.failed.isEmpty,
          report.deployed.map(item => (item.ship.value, item.state, item.version)) == List(
            ("libs", DeploymentState.Validated, "1.0.0")
          ),
          ids.length == 1,
          state == List("VALIDATED"),
          record == file(report.deployed.map(_.id.value), "VALIDATED", "1.0.0"),
          !marker,
          !releases,
          after == file(report.deployed.map(_.id.value), "PUBLISHED", "1.0.0"),
          publishedState == List("PUBLISHED"),
          marked,
          again.deployed.isEmpty,
          again.held.contains("libs 1.0.0 is already published"),
          stayed == ids,
        )
      }

  private val automatic =
    test("automatic publishes, a second release does not upload, and drop frees the version"):
      ZIO.scoped {
        for
          jdk    <- GraphCompile.jdkHere
          root   <- temp
          portal <- temp
          secret <- secretOf
          bound  <- PortalServer.open(portal, secret, ZIO.unit)
          key    <- Keys.signing
          built  <- ZIO.fromEither(library)
          _      <- plant(root, built, List("core/src/main/scala/demo/Hello.scala" -> hello), "cell core 3.10.0 jvm\n")
          env = signing(bound.url, key.armor)
          first   <- publish(root, built, jdk, env, ReleaseMode.Automatic)
          ids     <- deploymentIds(portal)
          record  <- text(deployment(root, "libs"))
          state   <- states(portal, ids)
          marker  <- exists(published(portal, "core_3", "1.0.0"))
          second  <- publish(root, built, jdk, env, ReleaseMode.Automatic)
          stayed  <- deploymentIds(portal)
          dropped <- first.deployed match
            case List(deployed) => PublishRun.drop(root, env, deployed.id.value).as(deployed.id.value)
            case _              => ZIO.succeed("")
          goneId   <- exists(portal.resolve("deployments").resolve(dropped))
          goneMark <- exists(published(portal, "core_3", "1.0.0"))
          goneFile <- exists(deployment(root, "libs"))
          third    <- publish(root, built, jdk, env, ReleaseMode.Automatic)
          fresh    <- deploymentIds(portal)
        yield assertTrue(
          first.deployed.map(item => (item.ship.value, item.state, item.version)) == List(
            ("libs", DeploymentState.Published, "1.0.0")
          ),
          ids.length == 1,
          state == List("PUBLISHED"),
          record == file(first.deployed.map(_.id.value), "PUBLISHED", "1.0.0"),
          marker,
          second.deployed.isEmpty,
          second.held.contains("libs 1.0.0 is already published"),
          stayed == ids,
          dropped.nonEmpty,
          !goneId,
          !goneMark,
          !goneFile,
          fresh.length == 1,
          fresh != ids,
          third.deployed.map(_.state) == List(DeploymentState.Published),
        )
      }

  private val failedBundle =
    test("an emptied pom fails that ship and the other ship is validated"):
      ZIO.scoped {
        for
          jdk    <- GraphCompile.jdkHere
          root   <- temp
          portal <- temp
          secret <- secretOf
          bound  <- PortalServer.open(portal, secret, ZIO.unit)
          key    <- Keys.signing
          built  <- ZIO.fromEither(pair)
          _      <- plant(
            root,
            built,
            List(
              "core/src/main/scala/demo/Hello.scala" -> hello,
              "site/src/main/scala/demo/Site.scala"  -> siteSource,
            ),
            "cell core 3.10.0 jvm\ncell site 3.10.0 jvm\n",
          )
          env = signing(bound.url, key.armor)
          staged <- ReleaseRun
            .stage(root, built, Nil, jdk, env, _ => ZIO.unit)
            .provideSomeLayer[histos.resolve.LockedTools & Scope](
              histos.ToolFixture.resolver
            )
          pom = staged.root.resolve("com/example/site_3/2.0.0/site_3-2.0.0.pom")
          _        <- ZIO.attempt(Files.writeString(pom, ""))
          report   <- ReleaseRun.upload(root, staged, ReleaseMode.Hold)
          ids      <- deploymentIds(portal)
          found    <- ZIO.foreach(ids)(id => token(portal, id).map(id -> _))
          good     <- text(deployment(root, "libs"))
          extra    <- exists(deployment(root, "extra"))
          releases <- exists(root.resolve(".histos").resolve("releases"))
        yield
          val failed = found.collect { case (id, "FAILED") => id }
          val held   = found.collect { case (id, "VALIDATED") => id }
          assertTrue(
            failed.length == 1,
            held.length == 1,
            report.failed == failed.map(id => s"extra deployment $id failed"),
            report.deployed.map(item => (item.ship.value, item.id.value, item.state)) == held.map(id =>
              ("libs", id, DeploymentState.Validated)
            ),
            good == file(held, "VALIDATED", "1.0.0"),
            !extra,
            !releases,
          )
      }

  private val missingSecrets =
    test("missing secrets and an insecure portal fail before a request"):
      ZIO.scoped {
        for
          jdk    <- GraphCompile.jdkHere
          root   <- temp
          portal <- temp
          hits   <- Ref.make(0)
          secret <- secretOf
          bound  <- PortalServer.open(portal, secret, hits.update(_ + 1))
          key    <- Keys.signing
          built  <- ZIO.fromEither(library)
          bare   <- publish(root, built, jdk, Map.empty, ReleaseMode.Hold).either
          creds = Map("SONATYPE_USERNAME" -> "maven", "SONATYPE_PASSWORD" -> password)
          unsigned <- publish(root, built, jdk, creds, ReleaseMode.Hold).either
          insecure <- publish(
            root,
            built,
            jdk,
            signing(bound.url, key.armor).updated("HISTOS_PORTAL", "http://example.com"),
            ReleaseMode.Hold,
          ).either
          count <- hits.get
        yield assertTrue(
          bare == Left(ReleaseError.Credentials),
          bare.left.forall(error => !error.message.contains(password)),
          unsigned == Left(ReleaseError.Signer(SignerError.NoSecret)),
          insecure == Left(ReleaseError.Portal(PortalBase.Error.Insecure("example.com"))),
          count == 0,
        )
      }

  private val shaped =
    test("a library and an assembly at a release number are validated, then published"):
      ZIO.scoped {
        for
          jdk    <- GraphCompile.jdkHere
          root   <- temp
          portal <- temp
          secret <- secretOf
          bound  <- PortalServer.open(portal, secret, ZIO.unit)
          key    <- Keys.signing
          built  <- ZIO.fromEither(libraryAndAssembly)
          _      <- plant(
            root,
            built,
            List(
              "api/src/main/scala/lib/Lib.scala"  -> libSource,
              "app/src/main/scala/demo/App.scala" -> appSource,
            ),
            "cell api 3.10.0 jvm\ncell app 3.10.0 jvm\n",
          )
          env = signing(bound.url, key.armor)
          report       <- publish(root, built, jdk, env, ReleaseMode.Hold)
          ids          <- deploymentIds(portal)
          record       <- text(deployment(root, "histos"))
          state        <- states(portal, ids)
          libraryHeld  <- exists(published(portal, "histos-api_3", "1.0.0"))
          assemblyHeld <- exists(published(portal, "histos", "1.0.0"))
          id = report.deployed match
            case List(deployed) => Some(deployed.id.value)
            case _              => None
          _ <- id match
            case Some(value) => PublishRun.promote(root, env, value)
            case None        => ZIO.unit
          after <- id match
            case Some(_) => text(deployment(root, "histos"))
            case None    => ZIO.succeed("")
          publishedState <- states(portal, ids)
          libraryMarked  <- exists(published(portal, "histos-api_3", "1.0.0"))
          assemblyMarked <- exists(published(portal, "histos", "1.0.0"))
        yield assertTrue(
          report.refused.isEmpty,
          report.failed.isEmpty,
          report.deployed.map(item => (item.ship.value, item.state, item.version)) == List(
            ("histos", DeploymentState.Validated, "1.0.0")
          ),
          ids.length == 1,
          state == List("VALIDATED"),
          record == file(report.deployed.map(_.id.value), "VALIDATED", "1.0.0"),
          !libraryHeld,
          !assemblyHeld,
          after == file(report.deployed.map(_.id.value), "PUBLISHED", "1.0.0"),
          publishedState == List("PUBLISHED"),
          libraryMarked,
          assemblyMarked,
        )
      }

  private val snapshotCatalog =
    test("this repository's snapshot catalog refuses before any http"):
      ZIO.scoped {
        for
          jdk    <- GraphCompile.jdkHere
          repo   <- repository
          portal <- temp
          work   <- temp
          hits   <- Ref.make(0)
          secret <- secretOf
          bound  <- PortalServer.open(portal, secret, hits.update(_ + 1))
          key    <- Keys.signing
          _      <- ZIO.attempt {
            Files.createDirectories(work.resolve("cache"))
            Files.createDirectories(work.resolve("store"))
            Files.createDirectories(work.resolve("publish"))
          }
          api    <- ApiJar.locate.provideSomeLayer[Scope](ZLayer.succeed(System.SystemLive))
          loaded <- ZIO
            .serviceWithZIO[Loader](_.load(repo, api))
            .provideSome[Scope](
              Resolve.live(work.resolve("cache")),
              ZLayer.succeed(ContentStore.in(work.resolve("store"))),
              Loader.live,
            )
          report <- PublishRun
            .release(work.resolve("publish"), loaded.graph, Nil, jdk, signing(bound.url, key.armor), ReleaseMode.Hold)
            .provideSomeLayer[histos.resolve.LockedTools](histos.ToolFixture.resolver)
          count  <- hits.get
          record <- exists(deployment(work.resolve("publish"), "histos"))
          (library, assembly) = forms(loaded.graph)
        yield assertTrue(
          snapshotShip(loaded.graph),
          library,
          assembly,
          report.refused.contains("histos is 0.0.1-SNAPSHOT, which is not a release"),
          report.failed.isEmpty,
          report.deployed.isEmpty,
          !record,
          count == 0,
        )
      }

  def spec =
    suite("release")(hold, automatic, failedBundle, missingSecrets, shaped, snapshotCatalog)
      .provideSomeLayerShared[zio.test.TestEnvironment & Scope](
        histos.ToolFixture.layer(Projects(module("library", Jvm)(scala3)))
      ) @@ TestAspect.withLiveSystem @@ TestAspect.timeout(5.minutes)

  private def publish(
      root: Path,
      built: histos.ProjectGraph,
      jdk: histos.jdk.JdkRuntime,
      env: Map[String, String],
      mode: ReleaseMode,
  ) =
    PublishRun
      .release(root, built, Nil, jdk, env, mode)
      .provideSomeLayer[histos.resolve.LockedTools](histos.ToolFixture.resolver)

  private def signing(url: String, armor: Chunk[Byte]): Map[String, String] =
    Map(
      "SONATYPE_USERNAME" -> "maven",
      "SONATYPE_PASSWORD" -> password,
      "PGP_SECRET"        -> Base64.getEncoder.encodeToString(armor.toArray),
      "PGP_PASSPHRASE"    -> passphrase,
      "HISTOS_PORTAL"     -> url,
    )

  private def file(ids: List[String], state: String, version: String): String =
    ids match
      case List(id) => s"id $id\nstate $state\nversion $version\n"
      case _        => ""

  private def deployment(root: Path, ship: String): Path =
    root.resolve(".histos").resolve("deployments").resolve(ship)

  private def published(root: Path, artifact: String, version: String): Path =
    root.resolve("published").resolve("com").resolve("example").resolve(artifact).resolve(version)

  private def secretOf: ZIO[Any, IllegalStateException, RepositorySecret] =
    PortalServer.credentials(Map("SONATYPE_USERNAME" -> "maven", "SONATYPE_PASSWORD" -> password)) match
      case Right(secret) => ZIO.succeed(secret)
      case Left(_)       => ZIO.fail(new IllegalStateException("fixture credentials"))

  private def plant(
      root: Path,
      graph: histos.ProjectGraph,
      files: List[(String, String)],
      lock: String,
  ) =
    ZIO.attempt {
      files.foreach((name, body) => write(root, name, body))
      write(root, ".histos/dependencies.lock", lock)
    } *> histos.ToolFixture.seed(root, graph)

  private def deploymentIds(root: Path): ZIO[Any, Throwable, List[String]] =
    ZIO.attempt {
      val dir = root.resolve("deployments")
      if !Files.isDirectory(dir) then Nil
      else
        val stream = Files.list(dir)
        try stream.iterator().asScala.flatMap(path => Option(path.getFileName).map(_.toString)).toList.sorted
        finally stream.close()
    }

  private def states(root: Path, ids: List[String]): ZIO[Any, Throwable, List[String]] =
    ZIO.foreach(ids)(id => token(root, id))

  private def token(root: Path, id: String): ZIO[Any, Throwable, String] =
    ZIO.attempt(Files.readString(root.resolve("deployments").resolve(id).resolve("state")).stripLineEnd)

  private def text(path: Path): ZIO[Any, Throwable, String] =
    ZIO.attempt(Files.readString(path))

  private def exists(path: Path): ZIO[Any, Throwable, Boolean] =
    ZIO.attempt(Files.exists(path))

  private def released(catalog: histos.CatalogSpec, modules: histos.ProjectDef*) =
    Projects
      .organization(Organization("com.example"))
      .license(licenseValue)
      .developer(developerValue)
      .scm(scmValue)(catalog, modules*)

  private val mixed =
    val core = module("core", Jvm)(scala3)
    val lib  = module("lib", Jvm)(scala3)
    val app  = module("app", Jvm)(scala3).dependsOn(lib)
    released(
      histos.ships(
        histos.manual("libs", ManualVersion("1.0.0"))(core),
        histos.manual("snaps", ManualVersion("1.2.3-SNAPSHOT"))(lib),
        histos.manual("tool", ManualVersion("2.0.0"))(app),
      ),
      core,
      lib,
      app,
    )

  private val library =
    val core = module("core", Jvm)(scala3)
    released(histos.ships(histos.manual("libs", ManualVersion("1.0.0"))(core)), core)

  private val pair =
    val core = module("core", Jvm)(scala3)
    val site = module("site", Jvm)(scala3)
    released(
      histos.ships(
        histos.manual("libs", ManualVersion("1.0.0"))(core),
        histos.manual("extra", ManualVersion("2.0.0"))(site),
      ),
      core,
      site,
    )

  private val libraryAndAssembly =
    val library = module("api", Jvm)(scala3).artifact(ArtifactBase("histos-api"))
    val program = module("app", Jvm)(scala3)
      .artifact(ArtifactBase("histos"))
      .assembly(MainClass("demo.App"))
      .dependsOn(library)
    released(histos.ships(histos.manual("histos", ManualVersion("1.0.0"))(library, program)), library, program)

  private def repository: ZIO[Any, Throwable, Path] =
    System
      .env("HISTOS_REPO")
      .provide(ZLayer.succeed(System.SystemLive))
      .flatMap: named =>
        val cwd  = Path.of("").toAbsolutePath.normalize
        val root = named.filter(_.nonEmpty).fold(cwd)(Path.of(_))
        val file = root.resolve("histos.scala")
        ZIO
          .attempt(Files.isRegularFile(file))
          .flatMap:
            case true  => ZIO.succeed(root)
            case false => ZIO.fail(new IllegalStateException(s"histos.scala is not at $file"))

  private def snapshotShip(graph: histos.ProjectGraph): Boolean =
    graph.catalog match
      case Catalog.Manual(ship :: rest) =>
        rest.isEmpty && ship.name.value == "histos" && ship.version.value == "0.0.1-SNAPSHOT"
      case Catalog.Empty | Catalog.Lines(_) => false

  private def forms(graph: histos.ProjectGraph): (Boolean, Boolean) =
    val modules  = graph.projects.collect { case item: Module => item }
    val library  = modules.exists(item => item.name.value == "api" && item.publication == Publication.Library)
    val assembly = modules.exists: item =>
      item.publication match
        case Publication.Assembly(main) => item.name.value == "app" && main.value == "histos.cli.Main"
        case _                          => false
    (library, assembly)

  private def write(root: Path, name: String, body: String): Unit =
    val path = root.resolve(name)
    Option(path.getParent).foreach(parent => Files.createDirectories(parent))
    Files.writeString(path, body)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-release"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk  = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  private object Keys:
    final case class Generated(armor: Chunk[Byte])

    def signing: ZIO[Any, Throwable, Generated] =
      ZIO
        .attemptBlocking(ring)
        .flatMap:
          case Some(key) => ZIO.succeed(key)
          case None      => ZIO.fail(new IllegalStateException("generated ring is missing its subkey"))

    private def ring: Option[Generated] =
      val provider  = new BouncyCastleProvider()
      val generator = KeyPairGenerator.getInstance("Ed25519", provider)
      val created   = new Date()
      val primary   = pair(generator, created)
      val sub       = pair(generator, created)
      val sha1      =
        new JcaPGPDigestCalculatorProviderBuilder().setProvider(provider).build().get(HashAlgorithmTags.SHA1)
      val signer =
        new JcaPGPContentSignerBuilder(PublicKeyAlgorithmTags.Ed25519, HashAlgorithmTags.SHA512).setProvider(provider)
      val encryptor =
        new JcePBESecretKeyEncryptorBuilder(SymmetricKeyAlgorithmTags.AES_256, sha1)
          .setProvider(provider)
          .build(passphrase.toCharArray)
      val ringGen = new PGPKeyRingGenerator(
        PGPSignature.POSITIVE_CERTIFICATION,
        primary,
        "histos-test",
        sha1,
        packets(KeyFlags.CERTIFY_OTHER),
        packets(0),
        signer,
        encryptor,
      )
      ringGen.addSubKey(sub, packets(KeyFlags.SIGN_DATA), packets(0), signer)
      val secret  = ringGen.generateSecretKeyRing()
      val wanted  = sub.getPublicKey.getFingerprint
      val matched =
        secret.getSecretKeys.asScala
          .map(_.getPublicKey)
          .find(key => java.util.Arrays.equals(key.getFingerprint, wanted))
      matched.map { _ =>
        val raw     = new java.io.ByteArrayOutputStream()
        val armored = new ArmoredOutputStream(raw)
        try secret.encode(armored)
        finally armored.close()
        Generated(Chunk.fromArray(raw.toByteArray))
      }

    private def pair(generator: KeyPairGenerator, created: Date) =
      new JcaPGPKeyPair(PublicKeyPacket.VERSION_4, PublicKeyAlgorithmTags.Ed25519, generator.generateKeyPair(), created)

    private def packets(flag: Int) =
      val generator = new org.bouncycastle.openpgp.PGPSignatureSubpacketGenerator()
      generator.setKeyFlags(false, flag)
      generator.generate()
  end Keys
end ReleaseSpec
