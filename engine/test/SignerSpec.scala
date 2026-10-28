package histos.publish

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions
import java.security.{KeyPairGenerator, Security}
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
import org.bouncycastle.openpgp.{
  PGPException,
  PGPKeyRingGenerator,
  PGPObjectFactory,
  PGPPublicKey,
  PGPPublicKeyRingCollection,
  PGPSignature,
  PGPSignatureList,
  PGPSignatureSubpacketGenerator,
  PGPUtil,
}
import org.bouncycastle.openpgp.operator.bc.BcKeyFingerprintCalculator
import org.bouncycastle.openpgp.operator.jcajce.{
  JcaPGPContentSignerBuilder,
  JcaPGPContentVerifierBuilderProvider,
  JcaPGPDigestCalculatorProviderBuilder,
  JcaPGPKeyPair,
  JcePBESecretKeyEncryptorBuilder,
}
import org.bouncycastle.util.encoders.Hex
import zio.{Chunk, Scope, System, ZIO}
import zio.test.*

object SignerSpec extends ZIOSpecDefault:
  private val passphrase = "test-passphrase"
  private val payload    = Chunk.fromArray("histos-signer".getBytes(StandardCharsets.UTF_8))

  private val local =
    test("Signer.local signs with a generated key and the public half verifies"):
      val before = Security.getProviders.map(_.getName).toList
      for
        key    <- Keys.signing
        signer <- Signer.local(key.armor, passphrase)
        armor  <- signer.sign(payload)
        after = Security.getProviders.map(_.getName).toList
      yield assertTrue(Verify(key.publicKey, payload, armor) == Check.Verified, before == after)

  private val noSigningKey =
    test("a secret with no signing subkey fails before a signer exists"):
      for
        key    <- Keys.unusable
        opened <- Signer.local(key.armor, passphrase).either
      yield assertTrue(opened == Left(SignerError.NoSigningKey))

  private val wrongPassphrase =
    test("a wrong passphrase fails before a signer exists"):
      for
        key    <- Keys.signing
        opened <- Signer.local(key.armor, "not-the-passphrase").either
      yield assertTrue(opened == Left(SignerError.Passphrase))

  private val ambiguous =
    test("several signing keys and no fingerprint fails before a signer exists"):
      for
        first  <- Keys.signing
        second <- Keys.signing
        opened <- Signer.local(first.binary ++ second.binary, passphrase).either
      yield assertTrue(opened == Left(SignerError.AmbiguousSigningKey))

  private val sameKey =
    test("PGP_SECRET and PGP_KEY_HEX must be the same signing key"):
      for
        key    <- Keys.signing
        other  <- Keys.signing
        signed <- signEnv(key, key.fingerprint.toUpperCase)
        byId   <- signEnv(key, key.fingerprint.takeRight(16))
        missed <- Signer
          .fromEnv(env(key, other.fingerprint))
          .either
      yield assertTrue(
        Verify(key.publicKey, payload, signed) == Check.Verified,
        Verify(key.publicKey, payload, byId) == Check.Verified,
        missed == Left(SignerError.KeyMismatch),
      )

  private val hexAlone =
    test("PGP_KEY_HEX alone cannot sign"):
      Signer
        .fromEnv(Map("PGP_KEY_HEX" -> ("ab" * 20)))
        .either
        .map: opened =>
          assertTrue(opened == Left(SignerError.NoSecret))

  private val unknown =
    test("an unknown HISTOS_SIGNER fails before a signer exists"):
      Signer
        .fromEnv(Map("HISTOS_SIGNER" -> "pgp"))
        .either
        .map: opened =>
          assertTrue(opened == Left(SignerError.UnknownSigner("pgp")))

  private val badSecret =
    test("a secret that is not base64 fails before a signer exists"):
      Signer
        .fromEnv(Map("PGP_SECRET" -> "not-base64!!!"))
        .either
        .map: opened =>
          assertTrue(opened == Left(SignerError.BadSecret))

  private val closed =
    test("the layer fails closed when the environment has no secret"):
      ZIO
        .service[Signer]
        .provide(Signer.layer)
        .either
        .map: opened =>
          assertTrue(opened == Left(SignerError.NoSecret))

  private val arguments =
    test("HISTOS_SIGNER=gpg is the argument list and the passphrase on stdin"):
      ZIO.scoped:
        for
          dir    <- temp
          _      <- fake(dir)
          signer <- Signer.fromEnv(gpgEnv(dir, "0"))
          armor  <- signer.sign(payload)
          argv   <- text(dir.resolve("argv"))
          stdin  <- text(dir.resolve("stdin"))
          leaked <- text(dir.resolve("passphrase-env"))
          lines          = argv.split("\n").filter(_.nonEmpty).toList
          (flags, paths) = lines.splitAt(8)
        yield flags match
          case head =>
            paths match
              case signed :: file :: Nil =>
                assertTrue(
                  head == List(
                    "--batch",
                    "--detach-sign",
                    "--armor",
                    "--pinentry-mode",
                    "loopback",
                    "--passphrase-fd",
                    "0",
                    "--output",
                  ),
                  signed.endsWith("payload.asc"),
                  file.endsWith("payload"),
                  stdin == s"$passphrase\n",
                  leaked.isEmpty,
                  !argv.contains(passphrase),
                  armor.text.contains("FAKE"),
                )
              case other => assertTrue(other == Nil, head == Nil)

  private val nonZero =
    test("a non-zero gpg exit fails before a signature exists"):
      ZIO.scoped:
        for
          dir    <- temp
          _      <- fake(dir)
          signer <- Signer.fromEnv(gpgEnv(dir, "3"))
          opened <- signer.sign(payload).either
          stdin  <- text(dir.resolve("stdin"))
        yield assertTrue(opened == Left(SignerError.GpgFailed(3)), stdin == s"$passphrase\n")

  private val missing =
    test("a missing gpg fails before a signer exists"):
      ZIO.scoped:
        for
          dir    <- temp
          opened <- Signer
            .fromEnv(Map("HISTOS_SIGNER" -> "gpg", "PATH" -> dir.toString, "PGP_PASSPHRASE" -> passphrase))
            .either
        yield assertTrue(opened == Left(SignerError.GpgMissing))

  private val pathIgnored =
    test("gpg on PATH with HISTOS_SIGNER unset stays on the local signer"):
      ZIO.scoped:
        for
          key    <- Keys.signing
          dir    <- temp
          _      <- fake(dir)
          signer <- Signer.fromEnv(
            Map(
              "PATH"           -> dir.resolve("bin").toString,
              "RECORD"         -> dir.toString,
              "PGP_SECRET"     -> Base64.getEncoder.encodeToString(key.armor.toArray),
              "PGP_PASSPHRASE" -> passphrase,
            )
          )
          armor <- signer.sign(payload)
        yield assertTrue(
          Verify(key.publicKey, payload, armor) == Check.Verified,
          !Files.exists(dir.resolve("argv")),
        )

  private val roundTrip =
    test("a real gpg signs and the public half verifies"):
      ZIO.scoped:
        for
          path   <- Live.live(System.env("PATH").orElseSucceed(None))
          result <- path match
            case None       => ZIO.succeed(assertTrue(path.isDefined))
            case Some(path) =>
              Keys.binary(path) match
                case None         => ZIO.succeed(assertTrue(false))
                case Some(binary) => gpgRoundTrip(binary, path)
        yield result

  def spec =
    suite("signer")(
      local,
      noSigningKey,
      wrongPassphrase,
      ambiguous,
      sameKey,
      hexAlone,
      unknown,
      badSecret,
      closed,
      arguments,
      nonZero,
      missing,
      pathIgnored,
      suite("gpg on this machine")(roundTrip).whenZIO(gpgPresent),
    )

  private val gpgPresent =
    Live
      .live(System.env("PATH").orElseSucceed(None))
      .flatMap:
        case Some(path) => ZIO.attemptBlocking(Keys.binary(path).isDefined).orElseSucceed(false)
        case None       => ZIO.succeed(false)

  private def signEnv(key: Generated, fingerprint: String) =
    Signer.fromEnv(env(key, fingerprint)).flatMap(_.sign(payload))

  private def env(key: Generated, fingerprint: String): Map[String, String] =
    Map(
      "PGP_SECRET"     -> Base64.getEncoder.encodeToString(key.armor.toArray),
      "PGP_KEY_HEX"    -> fingerprint,
      "PGP_PASSPHRASE" -> passphrase,
    )

  private def gpgEnv(dir: Path, status: String): Map[String, String] =
    Map(
      "HISTOS_SIGNER"  -> "gpg",
      "PGP_PASSPHRASE" -> passphrase,
      "PATH"           -> dir.resolve("bin").toString,
      "RECORD"         -> dir.toString,
      "FAKE_STATUS"    -> status,
    )

  private def fake(dir: Path): ZIO[Any, Throwable, Unit] =
    ZIO.attemptBlocking:
      val bin = dir.resolve("bin")
      Files.createDirectories(bin)
      val script = bin.resolve("gpg")
      Files.writeString(script, fakeScript)
      Files.setPosixFilePermissions(script, PosixFilePermissions.fromString("rwxr-xr-x"))

  private val fakeScript =
    """|#!/bin/sh
       |record="$RECORD"
       |: > "$record/argv"
       |for arg in "$@"; do
       |  printf '%s\n' "$arg" >> "$record/argv"
       |done
       |: > "$record/stdin"
       |while IFS= read -r line || [ -n "$line" ]; do
       |  printf '%s\n' "$line" >> "$record/stdin"
       |done
       |printf '%s' "${PGP_PASSPHRASE-}" > "$record/passphrase-env"
       |status="${FAKE_STATUS:-0}"
       |if [ "$status" = "0" ]; then
       |  prev=""
       |  for arg in "$@"; do
       |    if [ "$prev" = "--output" ]; then
       |      printf '%s\n' "-----BEGIN PGP SIGNATURE-----" "" "FAKE" "-----END PGP SIGNATURE-----" > "$arg"
       |    fi
       |    prev="$arg"
       |  done
       |fi
       |exit "$status"
       |""".stripMargin

  private def gpgRoundTrip(binary: Path, path: String) =
    for
      home <- temp
      _    <- ZIO.attemptBlocking:
        Files.setPosixFilePermissions(home, PosixFilePermissions.fromString("rwx------"))
      _ <- runGpg(
        binary,
        List(
          "--homedir",
          home.toString,
          "--batch",
          "--pinentry-mode",
          "loopback",
          "--passphrase-fd",
          "0",
          "--quick-gen-key",
          "Histos Signer <signer@example.test>",
          "ed25519",
          "default",
          "never",
        ),
        Some(passphrase),
      )
      exported <- runGpg(binary, List("--homedir", home.toString, "--batch", "--export", "--armor"), None)
      signer   <- Signer.fromEnv(
        Map(
          "HISTOS_SIGNER"  -> "gpg",
          "PGP_PASSPHRASE" -> passphrase,
          "GNUPGHOME"      -> home.toString,
          "PATH"           -> path,
        )
      )
      armor <- signer.sign(payload)
    yield assertTrue(Verify.ring(exported.getBytes(StandardCharsets.UTF_8), payload, armor) == Check.Verified)

  private def runGpg(binary: Path, args: List[String], input: Option[String]) =
    ZIO
      .attemptBlocking {
        val builder = new ProcessBuilder((binary.toString :: args).asJava)
        builder.redirectOutput(ProcessBuilder.Redirect.PIPE)
        builder.redirectError(ProcessBuilder.Redirect.PIPE)
        builder.redirectInput(ProcessBuilder.Redirect.PIPE)
        val process = builder.start()
        val stdin   = process.getOutputStream
        input.foreach: text =>
          stdin.write(text.getBytes(StandardCharsets.UTF_8))
          stdin.write("\n".getBytes(StandardCharsets.UTF_8))
        stdin.close()
        val stdout = new String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
        val stderr = new String(process.getErrorStream.readAllBytes, StandardCharsets.UTF_8)
        val status = process.waitFor()
        (status, stdout, stderr)
      }
      .flatMap: (status, stdout, stderr) =>
        if status == 0 then ZIO.succeed(stdout)
        else ZIO.fail(new IllegalStateException(s"gpg setup exited $status: $stderr"))

  private def text(path: Path) =
    ZIO.attemptBlocking(Files.readString(path, StandardCharsets.UTF_8))

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-signer"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk  = Files.walk(dir)
          val paths =
            try walk.iterator().asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  private final case class Generated(
      armor: Chunk[Byte],
      binary: Chunk[Byte],
      publicKey: PGPPublicKey,
      fingerprint: String,
  )

  private object Keys:
    def signing: ZIO[Any, Throwable, Generated] =
      ZIO.attemptBlocking(ring(KeyFlags.SIGN_DATA, binding = true)).flatMap(found)

    def unusable: ZIO[Any, Throwable, Generated] =
      ZIO.attemptBlocking(ring(KeyFlags.ENCRYPT_COMMS | KeyFlags.ENCRYPT_STORAGE, binding = false)).flatMap(found)

    def binary(path: String): Option[Path] =
      path
        .split(java.io.File.pathSeparatorChar)
        .iterator
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(executable)
        .collectFirst:
          case Some(binary) => binary

    private def found(generated: Option[Generated]): ZIO[Any, Throwable, Generated] =
      generated match
        case Some(key) => ZIO.succeed(key)
        case None      => ZIO.fail(new IllegalStateException("generated ring is missing its subkey"))

    private def executable(dir: String): Option[Path] =
      try
        val binary = Path.of(dir, "gpg")
        if Files.isExecutable(binary) then Some(binary) else None
      catch case _: java.nio.file.InvalidPathException => None

    private def ring(subFlags: Int, binding: Boolean): Option[Generated] =
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
      if binding then ringGen.addSubKey(sub, packets(subFlags), packets(0), signer)
      else ringGen.addSubKey(sub, packets(subFlags), packets(0))
      val secret  = ringGen.generateSecretKeyRing()
      val wanted  = sub.getPublicKey.getFingerprint
      val matched =
        secret.getSecretKeys.asScala
          .map(_.getPublicKey)
          .find(key => java.util.Arrays.equals(key.getFingerprint, wanted))
      matched.map: publicKey =>
        val raw     = new ByteArrayOutputStream()
        val armored = new ArmoredOutputStream(raw)
        try secret.encode(armored)
        finally armored.close()
        Generated(
          Chunk.fromArray(raw.toByteArray),
          Chunk.fromArray(secret.getEncoded),
          publicKey,
          Hex.toHexString(publicKey.getFingerprint),
        )

    private def pair(generator: KeyPairGenerator, created: Date) =
      new JcaPGPKeyPair(PublicKeyPacket.VERSION_4, PublicKeyAlgorithmTags.Ed25519, generator.generateKeyPair(), created)

    private def packets(flag: Int) =
      val generator = new PGPSignatureSubpacketGenerator()
      generator.setKeyFlags(false, flag)
      generator.generate()
  end Keys

  private enum Check:
    case Verified
    case Rejected
    case Unreadable

  private object Verify:
    def apply(key: PGPPublicKey, payload: Chunk[Byte], armor: Armor): Check =
      parsed(armor) match
        case None            => Check.Unreadable
        case Some(signature) => check(key, payload, signature)

    def ring(encoded: Array[Byte], payload: Chunk[Byte], armor: Armor): Check =
      parsed(armor) match
        case None            => Check.Unreadable
        case Some(signature) =>
          published(encoded, signature.getKeyID) match
            case None      => Check.Unreadable
            case Some(key) => check(key, payload, signature)

    private def check(key: PGPPublicKey, payload: Chunk[Byte], signature: PGPSignature): Check =
      val provider = new BouncyCastleProvider()
      try
        signature.init(new JcaPGPContentVerifierBuilderProvider().setProvider(provider), key)
        signature.update(payload.toArray)
        if signature.verify() then Check.Verified else Check.Rejected
      catch
        case _: PGPException => Check.Rejected
        case _: IOException  => Check.Unreadable

    private def parsed(armor: Armor): Option[PGPSignature] =
      try
        val factory = new PGPObjectFactory(
          PGPUtil.getDecoderStream(new ByteArrayInputStream(armor.text.getBytes(StandardCharsets.UTF_8))),
          new BcKeyFingerprintCalculator(),
        )
        val parsed = factory.nextObject()
        if parsed == null then None
        else
          parsed match
            case list: PGPSignatureList if list.size() > 0 => Some(list.get(0))
            case one: PGPSignature                         => Some(one)
            case _                                         => None
      catch
        case _: PGPException => None
        case _: IOException  => None

    private def published(encoded: Array[Byte], keyId: Long): Option[PGPPublicKey] =
      try
        val rings = new PGPPublicKeyRingCollection(
          PGPUtil.getDecoderStream(new ByteArrayInputStream(encoded)),
          new BcKeyFingerprintCalculator(),
        )
        Option(rings.getPublicKey(keyId))
      catch
        case _: PGPException => None
        case _: IOException  => None
  end Verify
end SignerSpec
