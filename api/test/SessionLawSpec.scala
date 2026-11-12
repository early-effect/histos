package histos.version

import zio.ConfigProvider
import zio.test.*

object SessionLawSpec extends ZIOSpecDefault:
  private val sha    = "0123456789abcdef0123456789abcdef01234567"
  private val stored = CommitSha.trusted(sha)

  def spec = suite("session laws")(
    test("development and release ignore the other fields, and a snapshot or pull request requires its own"):
      val stray    = SessionSource(None, Some("nope"), Some("main"), Some("0"))
      val named    = SessionSource(Some("development"), Some("nope"), Some("main"), Some("4"))
      val release  = SessionSource(Some("release"), Some("nope"), None, None)
      val snapshot = SessionSource(Some("snapshot"), Some(sha), Some("default"), None)
      val other    = SessionSource(Some("snapshot"), Some(sha), Some("other"), None)
      val pull     = SessionSource(Some("pr"), Some(sha), Some("main"), Some("4"))
      assertTrue(
        Session.Key == "HISTOS_SESSION",
        Session.ShaKey == "HISTOS_SHA",
        Session.BranchKey == "HISTOS_BRANCH",
        Session.PullRequestKey == "HISTOS_PR",
        Session.from(stray) == Right(Session.Development),
        Session.from(named) == Right(Session.Development),
        Session.from(release) == Right(Session.Release),
        Session.from(SessionSource(Some("nope"), None, None, None)) == Left(SessionError.Unknown("nope")),
        Session.from(snapshot) == Right(Session.Snapshot(stored, Branch.Default)),
        Session.from(other) == Right(Session.Snapshot(stored, Branch.Other)),
        Session.from(SessionSource(Some("snapshot"), None, Some("default"), None)) == Left(SessionError.MissingSha),
        Session.from(SessionSource(Some("snapshot"), Some(sha), None, None)) == Left(SessionError.MissingBranch),
        Session.from(SessionSource(Some("snapshot"), Some(sha), Some("main"), None)) ==
          Left(SessionError.BadBranch(BranchError.Unknown("main"))),
        Session.from(SessionSource(Some("snapshot"), Some(""), Some("default"), None)) ==
          Left(SessionError.Sha(CommitShaError.Empty)),
        Session.from(pull) == Right(Session.PullRequest(PullRequestNumber.trusted(4), stored)),
        Session.from(SessionSource(Some("pr"), Some(sha), None, None)) == Left(SessionError.MissingPullRequest),
        Session.from(SessionSource(Some("pr"), None, None, Some("4"))) == Left(SessionError.MissingSha),
        Session.from(SessionSource(Some("pr"), Some(sha), None, Some("0"))) ==
          Left(SessionError.PullRequest(PullRequestError.NotPositive(0))),
        Session.from(SessionSource(Some("pr"), Some(sha), None, Some("01"))) ==
          Left(SessionError.PullRequest(PullRequestError.NotANumber("01"))),
      )
    ,
    test("the four session keys load, and a missing session is development"):
      val loaded = ConfigProvider
        .fromMap(
          Map(
            Session.Key            -> "snapshot",
            Session.ShaKey         -> sha,
            Session.BranchKey      -> "other",
            Session.PullRequestKey -> "9",
          )
        )
        .load(SessionSource.config)
      val absent = ConfigProvider.fromMap(Map.empty[String, String]).load(SessionSource.config)
      for
        source <- loaded
        empty  <- absent
      yield assertTrue(
        Session.from(source) == Right(Session.Snapshot(stored, Branch.Other)),
        Session.from(empty) == Right(Session.Development),
      )
    ,
    test("a positive pull request number round-trips through from and its decimal text"):
      check(Gen.int(1, 10000)): number =>
        assertTrue(
          PullRequestNumber.from(number).map(_.value) == Right(number),
          PullRequestNumber.parse(number.toString).map(_.value) == Right(number),
        )
    ,
    test(
      "a branch is default or other, and a pull request number refuses zero, a negative, a leading zero, and non-digits"
    ):
      assertTrue(
        Branch.from("default") == Right(Branch.Default),
        Branch.from("other") == Right(Branch.Other),
        Branch.from("main") == Left(BranchError.Unknown("main")),
        PullRequestNumber.from(0) == Left(PullRequestError.NotPositive(0)),
        PullRequestNumber.from(-1) == Left(PullRequestError.NotPositive(-1)),
        PullRequestNumber.parse("0") == Left(PullRequestError.NotPositive(0)),
        PullRequestNumber.parse("01") == Left(PullRequestError.NotANumber("01")),
        PullRequestNumber.parse("") == Left(PullRequestError.NotANumber("")),
        PullRequestNumber.parse("4a") == Left(PullRequestError.NotANumber("4a")),
      )
    ,
    test("a commit is 40 hex stored lowercase, and an abbreviation is 12 lowercase hex"):
      val upper = sha.toUpperCase
      val short = "a" * 11
      val mixed = "A" + "b" * 11
      assertTrue(
        CommitSha.from(sha).map(_.value) == Right(sha),
        CommitSha.from(upper).map(_.value) == Right(sha),
        CommitSha.from(sha).map(_.abbreviate.value) == Right(sha.take(12)),
        CommitSha.from("") == Left(CommitShaError.Empty),
        CommitSha.from(sha.drop(1)) == Left(CommitShaError.NotHex(sha.drop(1))),
        CommitSha.from("g" * 40) == Left(CommitShaError.NotHex("g" * 40)),
        AbbrevSha.from(sha.take(12)).map(_.value) == Right(sha.take(12)),
        AbbrevSha.from("") == Left(AbbrevShaError.Empty),
        AbbrevSha.from(short) == Left(AbbrevShaError.TooShort(short)),
        AbbrevSha.from(mixed) == Left(AbbrevShaError.NotLower(mixed)),
        AbbrevSha.from("g" * 12) == Left(AbbrevShaError.NotAbbrev("g" * 12)),
        AbbrevSha.from(sha.take(13)) == Left(AbbrevShaError.NotAbbrev(sha.take(13))),
      )
    ,
    test("a registry keeps order, drops blanks and comments, and refuses a bad line"):
      val lines = List("", "  ", "# comment", "histos matches", "api moved")
      assertTrue(
        Registry.parse(lines).map(_.entries.map((name, relation) => (name.value, relation))) ==
          Right(List("histos" -> TagRelation.Matches, "api" -> TagRelation.Moved)),
        Registry.parse(List("histos matches extra")) == Left(RegistryError.BadLine("histos matches extra")),
        Registry.parse(List("histos matched")) ==
          Left(RegistryError.Relation(TagRelationError.Unknown("matched"))),
        Registry.parse(List("1bad matches")) == Left(RegistryError.Name(ShipNameError.Bad("1bad"))),
        Registry.parse(List("histos matches", "histos unknown")) ==
          Left(RegistryError.Duplicate(ShipName.trusted("histos"))),
        TagRelation.from("matches") == Right(TagRelation.Matches),
        TagRelation.from("moved") == Right(TagRelation.Moved),
        TagRelation.from("unknown") == Right(TagRelation.Unknown),
        TagRelation.from("matched") == Left(TagRelationError.Unknown("matched")),
      )
    ,
    test("a ship name is a letter, then letters, digits, underscore, or hyphen"):
      assertTrue(
        ShipName.from("histos_api-2").map(_.value) == Right("histos_api-2"),
        ShipName.from("") == Left(ShipNameError.Empty),
        ShipName.from("1bad") == Left(ShipNameError.Bad("1bad")),
        ShipName.from("has.dot") == Left(ShipNameError.Bad("has.dot")),
      ),
  )
end SessionLawSpec
