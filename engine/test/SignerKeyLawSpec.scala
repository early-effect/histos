package histos.publish

import java.io.ByteArrayOutputStream
import java.security.KeyPairGenerator
import java.util.{Base64, Date}

import org.bouncycastle.bcpg.{HashAlgorithmTags, PublicKeyAlgorithmTags, SymmetricKeyAlgorithmTags}
import org.bouncycastle.bcpg.sig.KeyFlags
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.openpgp.{PGPKeyRingGenerator, PGPSignature, PGPSignatureSubpacketGenerator}
import org.bouncycastle.openpgp.operator.jcajce.{
  JcaPGPContentSignerBuilder,
  JcaPGPDigestCalculatorProviderBuilder,
  JcaPGPKeyPair,
  JcePBESecretKeyEncryptorBuilder,
}
import org.bouncycastle.util.encoders.Hex

import zio.Chunk
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** An in-process signer uses the one signing key the secret names. The armor is a detached signature. */
object SignerKeyLawSpec extends ZIOSpecDefault:
  def spec = suite("signer key laws")(
    test("the named key signs, a wrong passphrase stays locked, and a different fingerprint is refused"):
      val provider = new BouncyCastleProvider()
      val pairs    = KeyPairGenerator.getInstance("RSA", provider)
      pairs.initialize(2048)
      val key = new JcaPGPKeyPair(
        PublicKeyAlgorithmTags.RSA_GENERAL,
        pairs.generateKeyPair(),
        new Date(),
      )
      val flags = new PGPSignatureSubpacketGenerator()
      flags.setKeyFlags(false, KeyFlags.SIGN_DATA)
      val digests = new JcaPGPDigestCalculatorProviderBuilder().setProvider(provider).build()
      val sha1    = digests.get(HashAlgorithmTags.SHA1)
      val ring    = new PGPKeyRingGenerator(
        PGPSignature.POSITIVE_CERTIFICATION,
        key,
        "law <law@example.test>",
        sha1,
        flags.generate(),
        new PGPSignatureSubpacketGenerator().generate(),
        new JcaPGPContentSignerBuilder(key.getPublicKey.getAlgorithm, HashAlgorithmTags.SHA256)
          .setProvider(provider),
        new JcePBESecretKeyEncryptorBuilder(SymmetricKeyAlgorithmTags.AES_256, sha1)
          .setProvider(provider)
          .build("law-pass".toCharArray),
      ).generateSecretKeyRing()
      val encoded = new ByteArrayOutputStream()
      ring.encode(encoded)
      val secret = Base64.getMimeEncoder.encodeToString(encoded.toByteArray)
      val finger = Hex.toHexString(key.getPublicKey.getFingerprint)
      val env    = Map("PGP_SECRET" -> secret, "PGP_PASSPHRASE" -> "law-pass")
      for
        locked <- Signer.fromEnv(env.updated("PGP_PASSPHRASE", "nope")).either
        missed <- Signer.fromEnv(env.updated("PGP_KEY_HEX", "0123456789abcdef")).either
        signer <- Signer.fromEnv(env)
        armor  <- signer.sign(Chunk.fromArray("histos-sign".getBytes("UTF-8")))
        found  <- signer.fingerprint
      yield assertTrue(
        locked == Left(SignerError.Passphrase),
        missed == Left(SignerError.KeyMismatch),
        armor.text.contains("-----BEGIN PGP SIGNATURE-----"),
        armor.text.contains("-----END PGP SIGNATURE-----"),
        found.value.equalsIgnoreCase(finger),
      )
  )
