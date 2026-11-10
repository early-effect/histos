package histos

import histos.doctor.CheckId
import histos.plugin.{ActionName, ActionPin, Cron, EnvironmentName, GroupName, PluginId, RepoPath}
import histos.supply.{
  AdvisoryId,
  BranchName,
  CellName,
  Coordinate,
  Cvss,
  Date,
  Days,
  Finding,
  Hours,
  LabelName,
  OutageStrategy,
  Purl,
  Reason,
  RepoId,
  RepoName,
  Severity,
  SourceName,
  SupplyFault,
  TimeZone,
}
import histos.version.{AbbrevSha, Catalog, CatalogError, CommitSha, Ship, ShipName, ShipNumber}

import zio.test.*

object SentenceLawSpec extends ZIOSpecDefault:
  private val sha    = CommitSha.trusted("0123456789abcdef0123456789abcdef01234567")
  private val other  = CommitSha.trusted("abcdef0123456789abcdef0123456789abcdef01")
  private val file   = RepoPath.trusted("ci.yml")
  private val action = ActionName.trusted("actions/checkout")
  private val id     = AdvisoryId.trusted("ghsa-1234-abcd-ef09")
  private val branch = BranchName.trusted("histos/security")

  def spec = suite("sentence laws")(
    test("each supply refusal names the file, the pin, or the branch it refused"):
      val found    = ActionPin(sha, "v4")
      val expected = ActionPin(other, "v5")
      val dep      = Coordinate.trusted("org.example:demo")
      val cells    = List(CellName.trusted("core"), CellName.trusted("app"))
      val finding  = Finding(id, Severity.High, None, Nil, Nil, "demo", "2020-01-02")
      val day      = Date.trusted("2020-01-02")
      val source   = SourceName.trusted("osv")
      assertTrue(
        SupplyFault.UnpinnedUses(file, 4, "actions/checkout@v4").message ==
          "ci.yml:4\n  actions/checkout@v4 is not pinned: need owner/repo@<40-hex> # <tag>",
        SupplyFault.PinWithoutTag(file, 4, action).message ==
          "ci.yml:4\n  actions/checkout is pinned without a tag comment",
        SupplyFault.PinMismatch(file, 4, action, found, expected).message ==
          "ci.yml:4\n  actions/checkout@01234567... # v4 does not match the catalog pin abcdef01... # v5",
        SupplyFault.TagNotReachable(action, "v4", sha).message ==
          "actions/checkout@v4 does not reach 0123456789abcdef0123456789abcdef01234567",
        SupplyFault.ActionRepoMoved(action, RepoId.trusted("old/repo"), RepoId.trusted("new/repo")).message ==
          "actions/checkout moved from old/repo to new/repo",
        SupplyFault.CrossVersionSplit(dep, cells).message == "org.example:demo would split core, app",
        SupplyFault.Advisory(finding).message == "ghsa-1234-abcd-ef09 High demo",
        SupplyFault.IgnoreExpired(id, day).message == "ignore ghsa-1234-abcd-ef09 expired on 2020-01-02",
        SupplyFault.IgnoreNeedsExpiry(id).message == "ignore ghsa-1234-abcd-ef09 needs an expiry",
        SupplyFault.IgnoreTooLong(id, Days.trusted(14)).message ==
          "ignore ghsa-1234-abcd-ef09 is longer than 14 days",
        SupplyFault.IgnoresDisallowed(List(id, AdvisoryId.trusted("ghsa-aaaa-bbbb-cccc"))).message ==
          "ignores are not allowed: ghsa-1234-abcd-ef09, ghsa-aaaa-bbbb-cccc",
        SupplyFault.ServiceDown(source, OutageStrategy.FailClosed).message == "osv is down",
        SupplyFault.IdentityMissing.message ==
          "Identity.FirstDeveloper needs one developer on the organization",
        SupplyFault.HumanCommitsOnBranch(branch, List(sha)).message ==
          "histos/security has human commits 0123456789ab",
        SupplyFault.HeadMoved(branch, sha).message ==
          "histos/security moved from 0123456789abcdef0123456789abcdef01234567",
      )
    ,
    test("a rejected token says what it is not"):
      assertTrue(
        TimeZone.from("Mars").left.map(_.message) == Left("'Mars' is not a time zone"),
        BranchName.from("has..dot").left.map(_.message) == Left("'has..dot' is not a branch name"),
        Hours.from(-3).left.map(_.message) == Left("-3 is negative"),
        AdvisoryId.from("GHSA-nope").left.map(_.message) == Left("'GHSA-nope' is not a GHSA id"),
        Reason.from("").left.map(_.message) == Left("a reason is one non-empty line"),
        Date.from("2020-13-40").left.map(_.message) == Left("'2020-13-40' is not a date"),
        Coordinate.from("only").left.map(_.message) == Left("'only' is not a group:artifact coordinate"),
        RepoName.from("only").left.map(_.message) == Left("'only' is not an owner/name repository"),
        Purl.from("maven/demo").left.map(_.message) == Left("'maven/demo' is not a package url"),
        Cvss.from("CVSS:3.0/AV:N").left.map(_.message) == Left("'CVSS:3.0/AV:N' is not a CVSS 3.1 vector"),
        LabelName.from("has space").left.map(_.message) == Left("'has space' is not a label"),
        GroupName.from("").left.map(_.message) == Left("a concurrency group is not empty"),
        GroupName.from("has space").left.map(_.message) == Left("'has space' is not a concurrency group"),
        EnvironmentName.from("").left.map(_.message) == Left("an environment name is not empty"),
        EnvironmentName.from("has space").left.map(_.message) == Left("'has space' is not an environment name"),
        Cron.from("").left.map(_.message) == Left("a cron expression is not empty"),
        Cron.from("0 0 * *").left.map(_.message) == Left("a cron expression has five fields, and this one has 4"),
        Cron.from("60 * * * *").left.map(_.message) == Left("cron field 1 is not a GitHub cron field: '60'"),
        SbtSeries.from("nope").left.map(_.message) == Left("'nope' is not an sbt series"),
        PomText.from("").left.map(_.message) == Left("pom text is not empty"),
        PomText.from("a\nb").left.map(_.message) == Left("'a\nb' is not pom text"),
        HttpUrl.from("http://example.com").left.map(_.message) == Left("'http://example.com' is not an https url"),
        LicenseName.from("").left.map(_.message) == Left("'' is not a license name"),
        DeveloperId.from("1abc").left.map(_.message) == Left("'1abc' is not a developer id"),
        ScmConnection.from("git@example.com:repo").left.map(_.message) ==
          Left("'git@example.com:repo' is not an scm connection"),
        ArtifactBase.from("..").left.map(_.message) == Left("'..' is not an artifact name"),
        Promote.from("ship").left.map(_.message) ==
          Left("'ship' is not a promote strategy. A strategy is rebuild or copy-index."),
        AbbrevSha.from("").left.map(_.message) == Left("an abbreviated sha is not empty"),
        AbbrevSha.from("abc").left.map(_.message) == Left(
          "'abc' is shorter than 12 hex characters. A snapshot sha is the 12-character abbreviation of the commit."
        ),
        CheckId.from("").left.map(_.message) == Left("a check id is not empty"),
      )
    ,
    test("a malformed coordinate says which colon form it almost was"):
      assertTrue(
        Dep.from("org:::name").left.map(_.message) ==
          Left(
            "'org:::name' is not an organization:name:revision coordinate\nA full cross-version is organization:::name:revision."
          ),
        Dep.from("org::name").left.map(_.message) ==
          Left(
            "'org::name' is not an organization:name:revision coordinate\nA Scala binary coordinate is organization::name:revision. Three colons is organization:::name:revision."
          ),
        Dep.from("org:name:").left.map(_.message) ==
          Left(
            "'org:name:' is not an organization:name:revision coordinate\n':' is a Java coordinate. A Scala library uses '::', for example org::name:."
          ),
      )
    ,
    test("portal, bundle, and client faults keep their own sentences"):
      assertTrue(
        PublishFault.Portal.message == "the portal URL is not usable",
        PublishFault.Bundle.message == "the bundle could not be packed",
        PublishFault.Client.message == "the portal rejected the upload",
      )
    ,
    test("a catalog clash names the ship and the module"):
      val api   = line("core", "api")
      val again = line("core", "app")
      val left  = line("left", "api")
      val right = line("right", "api")
      val twice = line("core", "api", "api")
      assertTrue(
        Catalog.from(List(api, again)).left.map(_.message) == Left("ship 'core' is declared twice"),
        Catalog.from(List(twice)).left.map(_.message) == Left("ship 'core' lists 'api' twice"),
        Catalog.from(List(left, right)).left.map(_.message) == Left("'api' is in ship 'left' and ship 'right'"),
        CatalogError.DuplicateName(api.name).message == "ship 'core' is declared twice",
      )
    ,
    test("a plugin that names a cell the module does not build says which cell"):
      val plugin = PluginId("law")
      val module = ProjectName("core")
      val scala3 = scala"3.10.0"
      assertTrue(
        PluginGraphFault.PluginMissingCell(plugin, module, Some(scala3), Some(Platform.Jvm)).message ==
          "plugin law names Scala 3.10.0 on jvm of 'core', and 'core' does not build it",
        PluginGraphFault.PluginMissingCell(plugin, module, Some(scala3), None).message ==
          "plugin law names Scala 3.10.0 of 'core', and 'core' does not build it",
        PluginGraphFault.PluginMissingCell(plugin, module, None, Some(Platform.Js)).message ==
          "plugin law names js of 'core', and 'core' does not build it",
        PluginGraphFault.PluginMissingCell(plugin, module, None, None).message ==
          "plugin law names a cell of 'core', and 'core' does not build it",
      ),
  )

  private def line(name: String, first: String, rest: String*): Ship.Line =
    Ship.Line(
      ShipName.trusted(name),
      ShipNumber.trusted(1, 0, 0),
      ::(ProjectName.trusted(first), rest.toList.map(ProjectName.trusted)),
    )
end SentenceLawSpec
