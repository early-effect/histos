package histos.publish

import histos.central.PortalBase

import java.io.ByteArrayOutputStream
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
import org.bouncycastle.openpgp.{PGPKeyRingGenerator, PGPSignature, PGPSignatureSubpacketGenerator}
import org.bouncycastle.openpgp.operator.jcajce.{
  JcaPGPContentSignerBuilder,
  JcaPGPDigestCalculatorProviderBuilder,
  JcaPGPKeyPair,
}
import org.bouncycastle.openpgp.operator.jcajce.JcePBESecretKeyEncryptorBuilder
import zio.{Chunk, ZIO}
import zio.test.*

object SonaStockSpec extends ZIOSpecDefault:
  private val passphrase = "test-passphrase"

  def spec = suite("SonaStock.gate")(
    test("missing secrets and an insecure portal fail before a socket"):
      val creds = Map("SONATYPE_USERNAME" -> "maven", "SONATYPE_PASSWORD" -> "secret")
      for
        bare     <- SonaStock.gate(Map.empty).either
        unsigned <- SonaStock.gate(creds).either
        key      <- Keys.signing
        insecure <- SonaStock
          .gate(
            creds ++ Map(
              "PGP_SECRET"     -> Base64.getEncoder.encodeToString(key.toArray),
              "PGP_PASSPHRASE" -> passphrase,
              "HISTOS_PORTAL"  -> "http://example.com",
            )
          )
          .either
      yield assertTrue(
        bare == Left(ReleaseError.Credentials),
        unsigned == Left(ReleaseError.Signer(SignerError.NoSecret)),
        insecure == Left(ReleaseError.Portal(PortalBase.Error.Insecure("example.com"))),
      )
  )

  private object Keys:
    def signing: ZIO[Any, Throwable, Chunk[Byte]] =
      ZIO
        .attemptBlocking(ring)
        .flatMap:
          case Some(armor) => ZIO.succeed(armor)
          case None        => ZIO.fail(new IllegalStateException("generated ring is missing its subkey"))

    private def ring: Option[Chunk[Byte]] =
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
      matched.map: _ =>
        val raw     = new ByteArrayOutputStream()
        val armored = new ArmoredOutputStream(raw)
        try secret.encode(armored)
        finally armored.close()
        Chunk.fromArray(raw.toByteArray)

    private def pair(generator: KeyPairGenerator, created: Date) =
      new JcaPGPKeyPair(PublicKeyPacket.VERSION_4, PublicKeyAlgorithmTags.Ed25519, generator.generateKeyPair(), created)

    private def packets(flag: Int) =
      val generator = new PGPSignatureSubpacketGenerator()
      generator.setKeyFlags(false, flag)
      generator.generate()
  end Keys
end SonaStockSpec
