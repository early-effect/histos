package histos.publish

import histos.central.{Fingerprint, PortalBase, ReleaseMode, RepositorySecret, RepositoryUser}
import histos.jdk.{Sha256, Sha256Digest}
import histos.version.{CiVersion, Revision, ShipName, SnapshotPointer}
import histos.{PublishFault, PublishRequest, SonaPublish, organization, ship}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import slayer.{any, stub, stubbed}

import zio.{Chunk, Clock, Config, Scope, ZIO}
import zio.test.*

object SignerLawSpec extends ZIOSpecDefault:
  private val number    = ship"1.2.3"
  private val armorText = "ARMOR-kept"
  private val creds     = Map("SONATYPE_USERNAME" -> "user", "SONATYPE_PASSWORD" -> "pw")

  def spec = suite("signer laws")(
    test("a missing secret, a bad secret, and an unknown signer are typed errors"):
      for
        _       <- Clock.instant
        missing <- Signer.fromEnv(Map.empty).either
        blank   <- Signer.fromEnv(Map("HISTOS_SIGNER" -> " ")).either
        empty   <- Signer.fromEnv(Map("PGP_SECRET" -> "%%%")).either
        bad     <- Signer.fromEnv(Map("PGP_SECRET" -> "not a key")).either
        unknown <- Signer.fromEnv(Map("HISTOS_SIGNER" -> "nope")).either
      yield assertTrue(
        missing == Left(SignerError.NoSecret),
        blank == Left(SignerError.NoSecret),
        empty == Left(SignerError.NoSigningKey),
        bad == Left(SignerError.BadSecret),
        unknown == Left(SignerError.UnknownSigner("nope")),
      )
    ,
    test("gpg that is not on the given path is missing, and a fingerprint is not asked of gpg"):
      ZIO.acquireRelease(directory("histos-gpg-missing"))(delete).flatMap { root =>
        val bin = root.resolve("bin")
        for
          _       <- Clock.instant
          _       <- install(bin, failScript)
          absent  <- Signer.fromEnv(Map("HISTOS_SIGNER" -> "gpg", "PATH" -> "")).either
          signer  <- Signer.fromEnv(gpgEnv(bin, hex = None))
          unnamed <- signer.fingerprint.either
          named   <- Signer.fromEnv(gpgEnv(bin, hex = Some("0123456789abcdef"))).flatMap(_.fingerprint)
        yield (named, Fingerprint.from("0123456789abcdef")) match
          case (found, Right(want)) =>
            assertTrue(
              absent == Left(SignerError.GpgMissing),
              unnamed == Left(SignerError.NoFingerprint),
              found == want,
            )
          case _ => assertTrue(false)
      }
    ,
    test("a gpg signer returns the armor the key source wrote, and the child does not see the secret"):
      ZIO.acquireRelease(directory("histos-gpg-ok"))(delete).flatMap { root =>
        val bin = root.resolve("bin")
        for
          _      <- Clock.instant
          _      <- install(bin, keepScript)
          signer <- Signer.fromEnv(gpgEnv(bin, hex = None) ++ Map("PGP_SECRET" -> "secret", "PGP_PASSPHRASE" -> "pw"))
          armor  <- signer.sign(Chunk.single(1.toByte)).either
        yield assertTrue(armor.map(_.text) == Right(armorText))
      }
    ,
    test("a key source that exits is a gpg failure"):
      ZIO.acquireRelease(directory("histos-gpg-fail"))(delete).flatMap { root =>
        val bin = root.resolve("bin")
        for
          _      <- Clock.instant
          _      <- install(bin, failScript)
          signer <- Signer.fromEnv(gpgEnv(bin, hex = None))
          failed <- signer.sign(Chunk.single(1.toByte)).either
        yield assertTrue(failed == Left(SignerError.GpgFailed(7)))
      }
    ,
    test("a release gate maps a signer failure, and a ci revision never asks for credentials"):
      val ci      = Revision.Ci(CiVersion(number))
      val pointer = Revision.Pointer(SnapshotPointer(number))
      for
        _            <- Clock.instant
        credsFirst   <- SonaStock.gate(Map.empty).either
        noSecret     <- SonaStock.gate(creds).either
        unknown      <- SonaStock.gate(creds + ("HISTOS_SIGNER" -> "nope")).either
        missing      <- SonaStock.gate(creds ++ Map("HISTOS_SIGNER" -> "gpg", "PATH" -> "")).either
        ciFault      <- publish(Map.empty, ci)
        pointerFault <- publish(Map.empty, pointer)
        signerFault  <- publish(
          creds + ("HISTOS_SIGNER" -> "nope"),
          Revision.Release(histos.version.ReleaseVersion(number)),
        )
      yield assertTrue(
        credsFirst == Left(ReleaseError.Credentials),
        noSecret == Left(ReleaseError.Signer(SignerError.NoSecret)),
        unknown == Left(ReleaseError.Signer(SignerError.UnknownSigner("nope"))),
        missing == Left(ReleaseError.Signer(SignerError.GpgMissing)),
        ciFault == Left(PublishFault.Version(ci)),
        pointerFault == Left(PublishFault.Credentials),
        signerFault == Left(PublishFault.Signer),
      )
    ,
    test("a signature failure is a failed ship, and the portal stays closed"):
      ZIO
        .acquireRelease(directory("histos-sign-upload"))(delete)
        .flatMap { repo =>
          val stage = repo.resolve("stage")
          val jar   = stage.resolve("app.jar")
          for
            _      <- Clock.instant
            _      <- stub[Signer](_.sign(any[Chunk[Byte]]))(ZIO.fail(SignerError.GpgFailed(7)))
            signer <- ZIO.service[Signer]
            _      <- ZIO.attemptBlocking {
              Files.createDirectories(stage)
              Files.write(jar, Array[Byte](1))
            }.orDie
            report <- upload(repo, stage, jar, signer)
          yield report match
            case None        => assertTrue(false)
            case Some(found) =>
              assertTrue(found.failed == List(s"demo ${SignerError.GpgFailed(7).message}"), found.deployed.isEmpty)
        }
        .provideSomeLayer[TestEnvironment & Scope](stubbed[Signer]),
  )

  private def publish(
      env: Map[String, String],
      revision: Revision,
  ): ZIO[Any, Nothing, Either[PublishFault, histos.PublishReceipt]] =
    val request = PublishRequest(revision, false, Path.of("stage"), Path.of("checkout"), Chunk.empty)
    ZIO
      .serviceWithZIO[SonaPublish](_.publish(request))
      .provide(SonaStock.layer(env, _ => ZIO.unit))
      .either

  private def upload(
      repo: Path,
      stage: Path,
      jar: Path,
      signer: Signer,
  ): ZIO[Any, ReleaseError, Option[ReleaseReport]] =
    (RepositoryUser.from("user"), PortalBase.from("http://127.0.0.1:9"), Fingerprint.from("0123456789abcdef")) match
      case (Right(user), Right(base), Right(fingerprint)) =>
        val secret = RepositorySecret(user, Config.Secret(Chunk.fromArray("pw".toCharArray)))
        val ready  = ReleaseStage(
          stage,
          List(published(jar)),
          Nil,
          Nil,
          Nil,
          signer,
          base,
          secret,
          fingerprint,
          Map.empty,
        )
        ReleaseRun.upload(repo, ready, ReleaseMode.Hold, _ => ZIO.unit).map(Some(_))
      case _ => ZIO.succeed(None)

  private def published(jar: Path): PublishedModule =
    PublishedModule(
      histos.ProjectName("app"),
      organization"example.com",
      "app",
      number.render,
      Some(jar),
      None,
      None,
      None,
      ShipName("demo"),
      Sha256Digest.of(Chunk.empty),
      None,
      Nil,
    )

  private def gpgEnv(bin: Path, hex: Option[String]): Map[String, String] =
    val base = Map("HISTOS_SIGNER" -> "gpg", "PATH" -> bin.toString, "HISTOS_KEEP" -> "kept")
    hex match
      case None      => base
      case Some(raw) => base + ("PGP_KEY_HEX" -> raw)

  private val keepScript: String =
    s"""#!/bin/sh
       |out=""
       |prev=""
       |for arg in "$$@"; do
       |  if [ "$$prev" = "--output" ]; then
       |    out="$$arg"
       |  fi
       |  prev="$$arg"
       |done
       |cat >/dev/null
       |if [ -n "$${PGP_SECRET+x}" ] || [ -n "$${PGP_PASSPHRASE+x}" ] || [ -n "$${PGP_KEY_HEX+x}" ] || [ -n "$${HISTOS_SIGNER+x}" ]; then
       |  exit 9
       |fi
       |printf 'ARMOR-%s' "$${HISTOS_KEEP-}" > "$$out"
       |exit 0
       |""".stripMargin

  private val failScript: String =
    """#!/bin/sh
      |cat >/dev/null
      |exit 7
      |""".stripMargin

  private def install(dir: Path, text: String): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking {
      Files.createDirectories(dir)
      val script = dir.resolve("gpg")
      Files.writeString(script, text)
      script.toFile.setExecutable(true)
      script
    }.orDie

  private def directory(prefix: String): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking(Files.createTempDirectory(prefix)).orDie

  private def delete(root: Path): zio.UIO[Unit] =
    ZIO.attemptBlocking {
      if Files.exists(root) then
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.orDie
end SignerLawSpec
