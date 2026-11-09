package histos.doctor

private def fixes(head: Fix, tail: Fix*): ::[Fix] = ::(head, tail.toList)

private def lines(head: String, tail: String*): ::[String] = ::(head, tail.toList)

/** Release findings. A warning does not fail `histos doctor`. */
enum ReleaseDiagnostic(
    val check: CheckId,
    val level: Level,
    val title: String,
    val summary: String,
    val explain: String,
    val fixes: ::[Fix],
) extends Diagnostic:
  case CredentialsMissing()
      extends ReleaseDiagnostic(
        CheckId.trusted("central.credentials"),
        Level.Warning,
        "Central credentials are not set",
        "SONATYPE_USERNAME and SONATYPE_PASSWORD are not set.",
        "A snapshot and a release both send this pair. A 401 is a different failure.",
        fixes(
          Fix.Steps(lines("Add repository secrets SONATYPE_USERNAME and SONATYPE_PASSWORD."))
        ),
      )

  case CredentialsRejected(status: Int)
      extends ReleaseDiagnostic(
        CheckId.trusted("central.credentials"),
        Level.Warning,
        "the portal rejected the token",
        s"The portal returned $status for SONATYPE_USERNAME and SONATYPE_PASSWORD.",
        "The variables are set. The portal user token is what Central rejected.",
        fixes(Fix.Steps(lines("Regenerate the portal user token."))),
      )

  case KeyHex()
      extends ReleaseDiagnostic(
        CheckId.trusted("pgp.key"),
        Level.Warning,
        "PGP_KEY_HEX is not a fingerprint",
        "PGP_KEY_HEX is set and it is not a fingerprint.",
        "The keyserver lookup needs the 16, 40, or 64 hex digits of the signing key.",
        fixes(Fix.Steps(lines("Set PGP_KEY_HEX to the signing key fingerprint."))),
      )

  case KeyMissing(fingerprint: String)
      extends ReleaseDiagnostic(
        CheckId.trusted("pgp.key"),
        Level.Warning,
        "the signing key is not on the keyserver",
        s"keyserver.ubuntu.com has no key $fingerprint",
        "Central fetches the signing key before it accepts a bundle. Histos does not upload the key.",
        fixes(
          Fix.Command(
            ShellLine.trusted(s"gpg --keyserver keyserver.ubuntu.com --send-keys $fingerprint")
          )
        ),
      )
end ReleaseDiagnostic

/** Pages findings. The fix names both settings the deploy needs. */
enum PagesDiagnostic(
    val check: CheckId,
    val level: Level,
    val title: String,
    val summary: String,
    val explain: String,
    val fixes: ::[Fix],
) extends Diagnostic:
  case Source()
      extends PagesDiagnostic(
        CheckId.trusted("pages.enabled"),
        Level.Warning,
        "Pages is not built by GitHub Actions",
        "Pages Source is not GitHub Actions.",
        "The generated deploy runs actions/deploy-pages. A branch source does not run that workflow.",
        fixes(Fix.Steps(lines(PagesDiagnostic.fixText))),
      )

  case Branch(name: String)
      extends PagesDiagnostic(
        CheckId.trusted("pages.enabled"),
        Level.Warning,
        "github-pages blocks the default branch",
        s"The github-pages environment does not allow $name.",
        "A push to the default branch requests that environment. The allow-list has to include it.",
        fixes(Fix.Steps(lines(PagesDiagnostic.fixText))),
      )
end PagesDiagnostic

object PagesDiagnostic:
  val fixText: String =
    "Settings > Pages > Source: GitHub Actions. Environments > github-pages allow-list: include the default branch."
