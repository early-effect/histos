package histos.version

import zio.{Cause, Chunk, Config}
import zio.test.*

object VersionOpenLawSpec extends ZIOSpecDefault:
  private val sha            = CommitSha.trusted("0123456789abcdef0123456789abcdef01234567")
  private val histos         = ShipName.trusted("histos")
  private val api            = ShipName.trusted("api")
  private val core           = _root_.histos.ProjectName.trusted("core")
  private val app            = _root_.histos.ProjectName.trusted("app")
  private val one            = ShipNumber.trusted(1, 4, 2)
  private val two            = ShipNumber.trusted(1, 4, 3)
  private val pull           = PullRequestNumber.trusted(7)
  private val row: Ship.Line = Ship.Line(histos, one, ::(core, Nil)) match
    case line: Ship.Line => line
  private val second: Ship.Line = Ship.Line(api, two, ::(app, Nil)) match
    case line: Ship.Line => line
  private val single = Catalog.Lines(::(row, Nil))
  private val both   = Catalog.Lines(::(row, List(second)))
  private val abbrev = sha.abbreviate.value

  def spec = suite("version open laws")(
    test("a checked registry of catalog ships assesses the session, including a hold that is not a release"):
      val fresh   = Registry.parse(Nil)
      val current = Registry.parse(List("histos matches"))
      val pr      = SessionSource(Some("pr"), Some(sha.value), None, Some("7"))
      val other   = SessionSource(Some("snapshot"), Some(sha.value), Some("other"), None)
      val onMain  = SessionSource(Some("snapshot"), Some(sha.value), Some("default"), None)
      val dev     = SessionSource(Some("development"), None, None, None)
      assertTrue(
        current.map(reg => Versions.assess(single, pr, Evidence.Checked(reg))) ==
          Right(Right(publish(PublishKind.PullRequest(pull, sha), List(everyday)))),
        current.map(reg => Versions.assess(single, other, Evidence.Checked(reg))) ==
          Right(Right(publish(PublishKind.Snapshot(sha, Branch.Other), List(everyday)))),
        fresh.map(reg => Versions.assess(both, onMain, Evidence.Checked(reg))) ==
          Right(
            Right(publish(PublishKind.Snapshot(sha, Branch.Default), List(pointer(histos, one), pointer(api, two))))
          ),
        fresh.map(reg => Versions.assess(both, dev, Evidence.Checked(reg))) ==
          Right(
            Right(
              Decision.Develop(
                Drift.Clear,
                ::(DevelopLine(histos, CiVersion(one)), List(DevelopLine(api, CiVersion(two)))),
              )
            )
          ),
      )
    ,
    test("the decision text names the session, the hold, and the pointer that only the default branch moves"):
      val moved = Registry.parse(List("histos moved", "api unknown"))
      val held  = Registry.parse(List("histos matches", "api matches"))
      assertTrue(
        Decision.text(Decision.NoCatalog) == "no catalog",
        Decision.text(Versions.decide(single, Session.Release, Evidence.Unchecked)) == ShipAdvice.unchecked,
        moved.map(reg => Decision.text(Versions.decide(both, Session.Release, Evidence.Checked(reg)))) ==
          Right(shadowedText),
        held.map(reg => Decision.text(Versions.decide(both, Session.Release, Evidence.Checked(reg)))) ==
          Right("every ship number is already released"),
        Decision.text(publish(PublishKind.PullRequest(pull, sha), List(everyday))) == prHoldText,
        Decision.text(publish(PublishKind.Snapshot(sha, Branch.Other), List(everyday))) == otherHoldText,
        Decision.text(
          publish(PublishKind.Snapshot(sha, Branch.Default), List(pointer(histos, one), pointer(api, two)))
        ) ==
          bothPointers,
        Decision.text(
          Decision.Develop(Drift.Clear, ::(DevelopLine(histos, CiVersion(one)), List(DevelopLine(api, CiVersion(two)))))
        ) == developText,
        Decision.rendered(everyday) == s"${histos.value} compile ${CiVersion(one).render} hold already-released",
        Decision.rendered(pointer(histos, one)) ==
          s"${histos.value} compile ${one.render}-ci publish ${one.render}-$abbrev-SNAPSHOT " +
          s"pointer ${one.render}-SNAPSHOT",
      )
    ,
    test("a version error keeps the underlying sentence, and config folds and, or, and a cause"):
      val invalid     = Config.Error.InvalidData(Chunk.empty, "bad")
      val missing     = Config.Error.MissingData(Chunk("HISTOS_SESSION"), "gone")
      val unsupported = Config.Error.Unsupported(Chunk("node", "floor"), "no")
      val source  = Config.Error.SourceUnavailable(Chunk("home"), "down", Cause.fail(new IllegalStateException("boom")))
      val nested  = (invalid && missing) || unsupported
      val session = SessionError.MissingSha
      val refused = ::(Refusal.Unchecked, Nil)
      assertTrue(
        VersionError.Session(session).message == session.message,
        VersionError.Registry(RegistryError.Duplicate(histos)).message ==
          s"ship '${histos.value}' is listed twice in releases",
        VersionError.UnknownShip(api).message == s"ship '${api.value}' is in releases and not in the catalog",
        VersionError.Refused(refused).message == Decision.refusalText(refused),
        VersionError.Unreadable.message == ShipAdvice.unreadable,
        VersionError.Config(invalid).message == "configuration: config bad",
        VersionError.Config(missing).message == "configuration: HISTOS_SESSION gone",
        VersionError.Config(unsupported).message == "configuration: node.floor no",
        VersionError.Config(source).message == "configuration: home down",
        !VersionError.Config(source).message.contains("boom"),
        VersionError.Config(nested).message == "configuration: config bad; HISTOS_SESSION gone or node.floor no",
      )
    ,
    test("ship advice is the sentence doctor and publish share"):
      val compile = CiVersion(one)
      val missing = SessionError.MissingBranch
      assertTrue(
        ShipAdvice.versionsCommand == "histos run versions",
        ShipAdvice.releasesFile ==
          "Create releases (an empty file is fine) or add a line: <ship> matches, <ship> moved, or <ship> unknown.",
        ShipAdvice.unchecked ==
          s"the release registry was not checked, so nothing publishes. ${ShipAdvice.releasesFile}",
        ShipAdvice.unreadable == s"could not read releases. ${ShipAdvice.releasesFile}",
        ShipAdvice.development(histos, compile) ==
          s"publishing ${histos.value} ${one.render} needs export HISTOS_SESSION=release. " +
          s"Then run ${ShipAdvice.versionsCommand}.",
        ShipAdvice.incomplete(missing) ==
          s"${missing.message} ${ShipAdvice.snapshotExport}, or export HISTOS_SESSION=release. " +
          s"Then run ${ShipAdvice.versionsCommand}.",
        ShipAdvice.shouldBump(histos, one, shadowed = true) ==
          s"${histos.value} ${one.render} is released and has changes since its tag. " +
          s"Edit Ship(\"${histos.value}\").next(...) to the next number. Do not open a bump pull request.",
        ShipAdvice.shouldBump(histos, one, shadowed = false) ==
          s"${histos.value} ${one.render} is already released. " +
          s"Edit Ship(\"${histos.value}\").next(...) to the next number. Do not open a bump pull request.",
      ),
  )

  private def publish(kind: PublishKind, lines: List[PublishLine]): Decision =
    lines match
      case head :: tail => Decision.Publish(kind, ::(head, tail))
      case Nil          => Decision.NoCatalog

  private def everyday: PublishLine =
    PublishLine(histos, PublishAction.Hold(HoldCompile.Everyday(CiVersion(one)), HoldReason.AlreadyReleased))

  private def pointer(ship: ShipName, number: ShipNumber): PublishLine =
    PublishLine(
      ship,
      PublishAction.Snapshot(
        CiVersion(number),
        SnapshotCoordinate(number, sha.abbreviate),
        Some(SnapshotPointer(number)),
      ),
    )

  private val prHoldText =
    s"session pr ${pull.value}\n${histos.value} compile ${one.render}-ci hold already-released"

  private val otherHoldText =
    s"session snapshot other\n${histos.value} compile ${one.render}-ci hold already-released"

  private val bothPointers =
    List(
      "session snapshot default",
      s"${histos.value} compile ${one.render}-ci publish ${one.render}-$abbrev-SNAPSHOT " +
        s"pointer ${one.render}-SNAPSHOT",
      s"${api.value} compile ${two.render}-ci publish ${two.render}-$abbrev-SNAPSHOT " +
        s"pointer ${two.render}-SNAPSHOT",
    ).mkString("\n")

  private val developText =
    s"""session development
       |drift clear
       |${histos.value} compile ${one.render}-ci
       |${api.value} compile ${two.render}-ci""".stripMargin

  private val shadowedText =
    s"""${histos.value} ${one.render} is released and has changes since its tag, so nothing publishes
       |${api.value} ${two.render} is released and its tag was not proven, so nothing publishes""".stripMargin
end VersionOpenLawSpec
