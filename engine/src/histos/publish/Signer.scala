package histos.publish

import histos.process.ProcessTree

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, InvalidPathException, Path}
import java.util.Base64

import scala.jdk.CollectionConverters.*

import org.bouncycastle.bcpg.{ArmoredOutputStream, HashAlgorithmTags, SymmetricKeyAlgorithmTags}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.{
  PGPException,
  PGPPrivateKey,
  PGPPublicKey,
  PGPSecretKey,
  PGPSecretKeyRingCollection,
  PGPSignature,
  PGPSignatureGenerator,
  PGPSignatureSubpacketVector,
  PGPUtil,
}
import org.bouncycastle.openpgp.operator.bc.BcKeyFingerprintCalculator
import org.bouncycastle.openpgp.operator.jcajce.{JcaPGPContentSignerBuilder, JcePBESecretKeyDecryptorBuilder}
import org.bouncycastle.bcpg.sig.KeyFlags
import org.bouncycastle.util.encoders.Hex
import zio.{Chunk, IO, System, UIO, ZIO, ZLayer}

/** Armored detached signature. The bytes are not a golden file: a verifier checks them. */
opaque type Armor = String

object Armor:
  def apply(text: String): Armor = text

  extension (armor: Armor) def text: String = armor
end Armor

/** Signs a payload. The build file does not name an implementation. */
trait Signer:
  def sign(payload: Chunk[Byte]): IO[SignerError, Armor]

object Signer:
  /** Decoded secret-key bytes and the passphrase that unlocks them. Fails before yielding a signer when the secret has
    * no signing key.
    */
  def local(secret: Chunk[Byte], passphrase: String): IO[SignerError, Signer] =
    open(secret.toArray, passphrase, None)

  /** `HISTOS_SIGNER=gpg` shells out. Unset, `PGP_SECRET` signs in process. `PGP_KEY_HEX` is the fingerprint
    * `usePgpKeyHex` reads, not a second encoding of the secret. Both set means that fingerprint is the signing key.
    */
  def fromEnv(env: Map[String, String]): IO[SignerError, Signer] =
    text(env, "HISTOS_SIGNER") match
      case None        => inProcess(env)
      case Some("gpg") => gpg(env)
      case Some(name)  => ZIO.fail(SignerError.UnknownSigner(name))

  def layer: ZLayer[Any, SignerError, Signer] =
    ZLayer.fromZIO(System.envs.mapError(_ => SignerError.Environment).flatMap(fromEnv))

  private def inProcess(env: Map[String, String]): IO[SignerError, Signer] =
    text(env, "PGP_SECRET") match
      case None          => ZIO.fail(SignerError.NoSecret)
      case Some(encoded) =>
        decode(encoded) match
          case Left(error)  => ZIO.fail(error)
          case Right(bytes) => open(bytes, env.getOrElse("PGP_PASSPHRASE", ""), text(env, "PGP_KEY_HEX"))

  private def open(bytes: Array[Byte], passphrase: String, fingerprint: Option[String]): IO[SignerError, Signer] =
    ZIO
      .attemptBlocking(material(bytes, passphrase, fingerprint))
      .mapError(_ => SignerError.BadSecret)
      .flatMap(ZIO.fromEither)

  private def material(
      bytes: Array[Byte],
      passphrase: String,
      fingerprint: Option[String],
  ): Either[SignerError, Signer] =
    secretKeys(bytes).flatMap(select(_, fingerprint)).flatMap(unlock(_, passphrase))

  private def secretKeys(bytes: Array[Byte]): Either[SignerError, List[PGPSecretKey]] =
    try
      val parsed = new PGPSecretKeyRingCollection(
        PGPUtil.getDecoderStream(new ByteArrayInputStream(bytes)),
        new BcKeyFingerprintCalculator(),
      )
      val keys = parsed.getKeyRings.asScala.flatMap(_.getSecretKeys.asScala).toList
      Right(keys)
    catch
      case _: PGPException => Left(SignerError.BadSecret)
      case _: IOException  => Left(SignerError.BadSecret)

  private def select(keys: List[PGPSecretKey], fingerprint: Option[String]): Either[SignerError, PGPSecretKey] =
    val signing = keys.filter(key => signs(key.getPublicKey))
    fingerprint match
      case Some(hex) =>
        signing.filter(key => names(key.getPublicKey, hex)) match
          case only :: Nil => Right(only)
          case Nil         => Left(if signing.isEmpty then SignerError.NoSigningKey else SignerError.KeyMismatch)
          case _           => Left(SignerError.AmbiguousSigningKey)
      case None =>
        signing match
          case only :: Nil => Right(only)
          case Nil         => Left(SignerError.NoSigningKey)
          case _           => Left(SignerError.AmbiguousSigningKey)

  private def signs(key: PGPPublicKey): Boolean =
    key.getSignatures.asScala.exists(signature => (flags(signature.getHashedSubPackets) & KeyFlags.SIGN_DATA) != 0)

  private def flags(packets: PGPSignatureSubpacketVector): Int =
    if packets == null then 0 else packets.getKeyFlags

  private def names(key: PGPPublicKey, raw: String): Boolean =
    val want        = raw.filter(ch => !ch.isWhitespace).toLowerCase
    val fingerprint = Hex.toHexString(key.getFingerprint)
    want == fingerprint || (want.length == 16 && fingerprint.takeRight(16) == want)

  private def unlock(key: PGPSecretKey, passphrase: String): Either[SignerError, Signer] =
    // Passed into each JCA call. Registering it would publish the provider on the JVM.
    val provider  = new BouncyCastleProvider()
    val encrypted = key.getKeyEncryptionAlgorithm != SymmetricKeyAlgorithmTags.NULL
    val decryptor =
      try Right(new JcePBESecretKeyDecryptorBuilder().setProvider(provider).build(passphrase.toCharArray))
      catch case _: PGPException => Left(SignerError.BadSecret)
    decryptor.flatMap: decryptor =>
      try
        val privateKey = key.extractPrivateKey(decryptor)
        if privateKey == null then Left(locked(encrypted))
        else Right(new Local(provider, key.getPublicKey, privateKey))
      catch case _: PGPException => Left(locked(encrypted))

  private def locked(encrypted: Boolean): SignerError =
    if encrypted then SignerError.Passphrase else SignerError.BadSecret

  private def decode(encoded: String): Either[SignerError, Array[Byte]] =
    try Right(Base64.getMimeDecoder.decode(encoded))
    catch case _: IllegalArgumentException => Left(SignerError.BadSecret)

  private def gpg(env: Map[String, String]): IO[SignerError, Signer] =
    ZIO.attemptBlocking(resolve(env)).mapError(_ => SignerError.GpgIo).flatMap(ZIO.fromEither)

  private def resolve(env: Map[String, String]): Either[SignerError, Signer] =
    onPath(env.getOrElse("PATH", "")) match
      case None         => Left(SignerError.GpgMissing)
      case Some(binary) =>
        Right(new Gpg(binary, env.getOrElse("PGP_PASSPHRASE", ""), childEnv(env)))

  private val hiddenFromChild = Set("PGP_SECRET", "PGP_PASSPHRASE", "PGP_KEY_HEX", "HISTOS_SIGNER")

  private def childEnv(env: Map[String, String]): Map[String, String] =
    env.filterNot((key, _) => hiddenFromChild.contains(key))

  private def onPath(path: String): Option[Path] =
    path
      .split(File.pathSeparatorChar)
      .iterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(executable)
      .collectFirst:
        case Some(binary) => binary

  private def executable(dir: String): Option[Path] =
    try
      val binary = Path.of(dir, "gpg")
      if Files.isExecutable(binary) then Some(binary) else None
    catch
      case _: InvalidPathException => None
      case _: IOException          => None

  private def text(env: Map[String, String], name: String): Option[String] =
    env.get(name).map(_.trim).filter(_.nonEmpty)

  private final class Local(
      provider: java.security.Provider,
      publicKey: PGPPublicKey,
      privateKey: PGPPrivateKey,
  ) extends Signer:
    private val contentSigner =
      new JcaPGPContentSignerBuilder(publicKey.getAlgorithm, HashAlgorithmTags.SHA512).setProvider(provider)

    def sign(payload: Chunk[Byte]): IO[SignerError, Armor] =
      ZIO
        .attemptBlocking(detached(payload.toArray))
        .refineOrDie:
          case _: PGPException => SignerError.Signature
          case _: IOException  => SignerError.Signature

    private def detached(payload: Array[Byte]): Armor =
      val generator = new PGPSignatureGenerator(contentSigner, publicKey)
      generator.init(PGPSignature.BINARY_DOCUMENT, privateKey)
      generator.update(payload)
      val signature = generator.generate()
      val buffer    = new ByteArrayOutputStream()
      val armored   = new ArmoredOutputStream(buffer)
      try signature.encode(armored)
      finally armored.close()
      Armor(buffer.toString(StandardCharsets.UTF_8))
  end Local

  private final class Gpg(command: Path, passphrase: String, env: Map[String, String]) extends Signer:
    def sign(payload: Chunk[Byte]): IO[SignerError, Armor] =
      ZIO.scoped:
        for
          dir <- ZIO.acquireRelease(directory)(delete)
          file   = dir.resolve("payload")
          signed = dir.resolve("payload.asc")
          _      <- ZIO.attemptBlocking(Files.write(file, payload.toArray)).mapError(_ => SignerError.GpgIo)
          status <- spawn(file, signed)
          armor  <-
            if status == 0 then read(signed)
            else ZIO.fail(SignerError.GpgFailed(status))
        yield armor

    private def directory: IO[SignerError, Path] =
      ZIO.attemptBlocking(Files.createTempDirectory("histos-sign")).mapError(_ => SignerError.GpgIo)

    private def spawn(payload: Path, signed: Path): IO[SignerError, Int] =
      val start = ZIO.attemptBlocking(process(payload, signed)).mapError(_ => SignerError.GpgIo)
      ZIO.scoped:
        ZIO
          .acquireRelease(start)(stop)
          .flatMap: child =>
            ZIO.attemptBlockingCancelable(feed(child))(stop(child)).mapError(_ => SignerError.GpgIo)

    private def process(payload: Path, signed: Path): Process =
      val builder = new ProcessBuilder((command.toString :: arguments(payload, signed)).asJava)
      builder.redirectOutput(ProcessBuilder.Redirect.DISCARD)
      builder.redirectError(ProcessBuilder.Redirect.DISCARD)
      builder.redirectInput(ProcessBuilder.Redirect.PIPE)
      val child = builder.environment()
      child.clear()
      env.foreach((key, value) => child.put(key, value))
      builder.start()

    private def arguments(payload: Path, signed: Path): List[String] =
      List(
        "--batch",
        "--detach-sign",
        "--armor",
        "--pinentry-mode",
        "loopback",
        "--passphrase-fd",
        "0",
        "--output",
        signed.toString,
        payload.toString,
      )

    private def feed(process: Process): Int =
      val stdin = process.getOutputStream
      stdin.write(passphrase.getBytes(StandardCharsets.UTF_8))
      stdin.write("\n".getBytes(StandardCharsets.UTF_8))
      stdin.close()
      process.waitFor()

    private def read(signed: Path): IO[SignerError, Armor] =
      ZIO.attemptBlocking(textOf(signed)).mapError(_ => SignerError.GpgIo).flatMap(ZIO.fromEither)

    private def textOf(signed: Path): Either[SignerError, Armor] =
      if Files.exists(signed) && Files.size(signed) > 0 then
        Right(Armor(Files.readString(signed, StandardCharsets.UTF_8)))
      else Left(SignerError.GpgIo)

    private def stop(process: Process): UIO[Unit] =
      ProcessTree.of(process).flatMap(_.destroy)

    private def delete(dir: Path): UIO[Unit] =
      ZIO
        .attemptBlocking:
          if Files.exists(dir) then
            val walk  = Files.walk(dir)
            val paths =
              try walk.iterator().asScala.toList
              finally walk.close()
            paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
        .ignore
  end Gpg
end Signer

enum SignerError:
  case NoSecret
  case BadSecret
  case NoSigningKey
  case AmbiguousSigningKey
  case KeyMismatch
  case Passphrase
  case UnknownSigner(name: String)
  case GpgMissing
  case GpgFailed(status: Int)
  case GpgIo
  case Signature
  case Environment

  def message: String = this match
    case NoSecret            => "no PGP_SECRET to sign with"
    case BadSecret           => "PGP_SECRET is not a secret key"
    case NoSigningKey        => "the secret has no signing key"
    case AmbiguousSigningKey => "the secret has more than one signing key"
    case KeyMismatch         => "PGP_KEY_HEX does not match the signing key"
    case Passphrase          => "PGP_PASSPHRASE does not unlock the signing key"
    case UnknownSigner(name) => s"unknown HISTOS_SIGNER $name"
    case GpgMissing          => "HISTOS_SIGNER=gpg and gpg is not on PATH"
    case GpgFailed(status)   => s"gpg exited $status"
    case GpgIo               => "gpg produced no signature"
    case Signature           => "the signing key produced no signature"
    case Environment         => "the process environment could not be read"
end SignerError
