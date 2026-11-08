package histos.doctor

import zio.{Ref, ZIO}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A token in the environment wins. `gh` is asked only after that, and only for the host that was set. */
object GitHubCredentialLawSpec extends ZIOSpecDefault:
  def spec = suite("github credential laws")(
    test("github.com, localhost, and ghe read GH_TOKEN, and another host reads the enterprise pair"):
      val github = Map("GH_TOKEN" -> "", "GITHUB_TOKEN" -> "second")
      val local  = Map("GITHUB_TOKEN" -> "loop")
      val spaces = Map("GH_ENTERPRISE_TOKEN" -> "   ")
      val enterprise = Map("GH_ENTERPRISE_TOKEN" -> "", "GITHUB_ENTERPRISE_TOKEN" -> "ent")
      assertTrue(
        GitHubCredential.githubHost("github.com"),
        GitHubCredential.githubHost("localhost"),
        GitHubCredential.githubHost("corp.ghe.com"),
        !GitHubCredential.githubHost("acme.example"),
        GitHubCredential.fromEnv(github, "github.com") == Some("second"),
        GitHubCredential.fromEnv(local, "localhost") == Some("loop"),
        GitHubCredential.fromEnv(github, "corp.ghe.com") == Some("second"),
        GitHubCredential.fromEnv(spaces, "acme.example") == Some("   "),
        GitHubCredential.fromEnv(enterprise, "acme.example") == Some("ent"),
        GitHubCredential.fromEnv(Map("GH_TOKEN" -> "pub"), "acme.example") == None,
        GitHubCredential.fromEnv(Map("GH_TOKEN" -> ""), "github.com") == None,
      )
    ,
    test("an environment token is not read from gh, and a blank host still asks gh with no hostname"):
      val asked = Ref.make(Option.empty[Option[String]])
      for
        seen     <- asked
        direct   <- GitHubCredential.resolve(
          Map("GH_HOST" -> " github.com ", "GH_TOKEN" -> "tok"),
          host => seen.set(Some(host)) *> ZIO.dieMessage("gh was asked"),
        )
        untouched <- seen.get
        _         <- seen.set(None)
        blank     <- GitHubCredential.resolve(
          Map("GH_HOST" -> "  "),
          host => seen.set(Some(host)).as(None),
        )
        blankHost <- seen.get
        _         <- seen.set(None)
        trimmed   <- GitHubCredential.resolve(
          Map("GH_HOST" -> " acme.example ", "GH_ENTERPRISE_TOKEN" -> "ent"),
          host => seen.set(Some(host)) *> ZIO.dieMessage("gh was asked"),
        )
        _         <- seen.set(None)
        empty     <- GitHubCredential.resolve(Map("GH_TOKEN" -> ""), host => seen.set(Some(host)).as(Some("cli")))
        kept      <- seen.get
        spaces    <- GitHubCredential.resolve(
          Map("GH_TOKEN" -> "   "),
          host => seen.set(Some(host)) *> ZIO.dieMessage("gh was asked"),
        )
        namedHost <- GitHubCredential.resolve(
          Map("GH_HOST" -> "ghe.example"),
          host => seen.set(Some(host)).as(Some("  cli ")),
        )
        named     <- seen.get
        missing   <- GitHubCredential.resolve(Map.empty, host => ZIO.succeed(None))
      yield
        val api = direct match
          case Some(token) => token.api
          case None        => ""
        val enterpriseApi = trimmed match
          case Some(token) => token.api
          case None        => ""
        assertTrue(
          direct == Some(GitHubCredential.Token("tok", "github.com")),
          api == "https://api.github.com",
          untouched == None,
          blank == None,
          blankHost == Some(None),
          trimmed == Some(GitHubCredential.Token("ent", "acme.example")),
          enterpriseApi == "https://acme.example/api/v3",
          empty == Some(GitHubCredential.Token("cli", "github.com")),
          kept == Some(None),
          spaces == Some(GitHubCredential.Token("   ", "github.com")),
          namedHost == Some(GitHubCredential.Token("cli", "ghe.example")),
          named == Some(Some("ghe.example")),
          missing == None,
        )
    ,
    test("gh with and without a hostname is no token when the command does not print one"):
      for
        plain <- GitHubCredential.gh(None)
        named <- GitHubCredential.gh(Some("127.0.0.1"))
      yield assertTrue(plain.forall(_.nonEmpty), named.forall(_.nonEmpty))
  )
end GitHubCredentialLawSpec
