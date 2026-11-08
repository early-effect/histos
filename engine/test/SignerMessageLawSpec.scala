package histos.publish

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Every signer failure says what the operator can change. */
object SignerMessageLawSpec extends ZIOSpecDefault:
  def spec = suite("signer message laws")(
    test("each signer failure names the missing piece"):
      val messages = List(
        SignerError.NoSecret            -> "no PGP_SECRET to sign with",
        SignerError.BadSecret           -> "PGP_SECRET is not a secret key",
        SignerError.NoSigningKey        -> "the secret has no signing key",
        SignerError.AmbiguousSigningKey -> "the secret has more than one signing key",
        SignerError.KeyMismatch         -> "PGP_KEY_HEX does not match the signing key",
        SignerError.Passphrase          -> "PGP_PASSPHRASE does not unlock the signing key",
        SignerError.UnknownSigner("age") -> "unknown HISTOS_SIGNER age",
        SignerError.GpgMissing          -> "HISTOS_SIGNER=gpg and gpg is not on PATH",
        SignerError.GpgFailed(2)        -> "gpg exited 2",
        SignerError.GpgIo               -> "gpg produced no signature",
        SignerError.Signature           -> "the signing key produced no signature",
        SignerError.Environment         -> "the process environment could not be read",
        SignerError.NoFingerprint       -> "PGP_KEY_HEX is required to publish a release with gpg",
      )
      assertTrue(messages.forall { case (error, text) => error.message == text })
  )
end SignerMessageLawSpec
