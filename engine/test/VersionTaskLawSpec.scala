package histos.version

import histos.task.TaskError
import histos.workspace.Workspace
import histos.{ProjectGraph, ProjectName, Splits}

import zio.{Exit, ZIO}
import zio.test.*

object VersionTaskLawSpec extends ZIOSpecDefault:
  private val sha         = "a" * 40
  private val coreName    = ShipName.trusted("core")
  private val extraName   = ShipName.trusted("extra")
  private val coreNumber  = ShipNumber.trusted(1, 2, 3)
  private val extraNumber = ShipNumber.trusted(1, 2, 4)
  private val core        = row(coreName, coreNumber, ::(ProjectName.trusted("core"), Nil))
  private val extra       = row(extraName, extraNumber, ::(ProjectName.trusted("extra"), Nil))
  private val lines       = Catalog.Lines(::(core, ::(extra, Nil)))
  private val one         = Catalog.Lines(::(core, Nil))
  private val release     = SessionSource(Some("release"), None, None, None)
  private val unread      = List(
    release,
    SessionSource(Some("snapshot"), Some(sha), Some("default"), None),
    SessionSource(Some("pr"), Some(sha), None, Some("7")),
  )

  def spec = suite("version task laws")(
    test("an empty catalog is no catalog, including when the session was not read"):
      assertTrue(
        Versions.assess(Catalog.Empty, release, Evidence.Unchecked) == Right(Decision.NoCatalog),
        Versions.assess(
          Catalog.Empty,
          SessionSource(Some("nope"), None, None, None),
          Evidence.Unchecked,
        ) == Right(Decision.NoCatalog),
      )
    ,
    test("a catalog with an unread registry refuses every publishing session, and development compiles"):
      val develop = Versions.assess(one, SessionSource(None, None, None, None), Evidence.Unchecked)
      assertTrue(
        unread.forall(source =>
          Versions.assess(one, source, Evidence.Unchecked) ==
            Right(Decision.Refused(::(Refusal.Unchecked, Nil)))
        ),
        develop == Right(
          Decision.Develop(Drift.Unchecked, ::(DevelopLine(coreName, CiVersion(coreNumber)), Nil))
        ),
      )
    ,
    test("an unreleased ship releases, a match is held, and every hold refuses"):
      val unreleased = checked(one, List("other-ship matches"))
      val mixed      = checked(lines, List("extra matches"))
      val held       = checked(lines, List("core matches", "extra matches"))
      val moved      = checked(one, List("core moved"))
      val unproven   = checked(one, List("core unknown"))
      assertTrue(
        unreleased == Left(VersionError.UnknownShip(ShipName.trusted("other-ship"))),
        publish(mixed),
        held == Right(Decision.Refused(::(Refusal.NothingToRelease, Nil))),
        moved == Right(Decision.Refused(::(Refusal.Shadowed(coreName, coreNumber), Nil))),
        unproven == Right(Decision.Refused(::(Refusal.Unproven(coreName, coreNumber), Nil))),
      )
    ,
    test("a registry line round-trips, a comment is ignored, and a duplicate or a bad line is typed"):
      assertTrue(
        Registry.parse(List("# note", "", "core matches")) ==
          Registry.parse(List("core matches")),
        Registry.parse(List("core matches", "core moved")) ==
          Left(RegistryError.Duplicate(coreName)),
        Registry.parse(List("not a line")) == Left(RegistryError.BadLine("not a line")),
        TagRelation.from("matches") == Right(TagRelation.Matches),
        TagRelation.from("moved") == Right(TagRelation.Moved),
        TagRelation.from("unknown") == Right(TagRelation.Unknown),
        TagRelation.from("nope").isLeft,
      )
    ,
    test("session tokens are development, snapshot, pr, and release"):
      val snapshot = Session.from(SessionSource(Some("snapshot"), Some(sha), Some("default"), None))
      val pull     = Session.from(SessionSource(Some("pr"), Some(sha), None, Some("7")))
      assertTrue(
        Session.from(SessionSource(None, None, None, None)) == Right(Session.Development),
        Session.from(SessionSource(Some("development"), None, None, None)) == Right(Session.Development),
        Session.from(release) == Right(Session.Release),
        Session.from(SessionSource(Some("nope"), None, None, None)) ==
          Left(SessionError.Unknown("nope")),
        Session.from(SessionSource(Some("snapshot"), None, None, None)) == Left(SessionError.MissingSha),
        Session.from(SessionSource(Some("pr"), Some(sha), None, None)) == Left(SessionError.MissingPullRequest),
        snapshot match
          case Right(Session.Snapshot(commit, Branch.Default)) => commit.value == sha
          case _                                               => false
        ,
        pull match
          case Right(Session.PullRequest(number, commit)) => number.value == 7 && commit.value == sha
          case _                                          => false,
      )
    ,
    test("an empty catalog ignores a bad releases file, and a catalog reports that file"):
      ZIO.scoped:
        Workspace
          .work("histos-versions")
          .flatMap: repo =>
            val env = Map("HISTOS_SESSION" -> "release")
            for
              _       <- Workspace.writeString(repo.resolve(VersionTask.fileName), "not a line")
              empty   <- VersionTask.decide(ProjectGraph.trusted(Nil, Splits.aggregate, Catalog.Empty), repo, env)
              bad     <- VersionTask.decide(ProjectGraph.trusted(Nil, Splits.aggregate, one), repo, env).exit
              refused <- VersionTask
                .fromRepo(ProjectGraph.trusted(Nil, Splits.aggregate, one), repo.resolve("absent"), env)
                .exit
            yield assertTrue(
              VersionTask.fileName == "releases",
              empty == Decision.NoCatalog,
              bad match
                case Exit.Failure(cause) =>
                  cause.failureOption match
                    case Some(VersionError.Registry(RegistryError.BadLine("not a line"))) => true
                    case _                                                                => false
                case Exit.Success(_) => false
              ,
              refused match
                case Exit.Failure(cause) =>
                  cause.failureOption.contains(
                    TaskError.Version(VersionError.Refused(::(Refusal.Unchecked, Nil)))
                  )
                case Exit.Success(_) => false,
            ),
  )

  private def checked(catalog: Catalog, rows: List[String]): Either[VersionError, Decision] =
    Registry.parse(rows) match
      case Left(error)     => Left(VersionError.Registry(error))
      case Right(registry) => Versions.assess(catalog, release, Evidence.Checked(registry))

  private def publish(found: Either[VersionError, Decision]): Boolean =
    found match
      case Right(Decision.Publish(PublishKind.Release, actions)) =>
        actions.map(_.action).toList == List(
          PublishAction.Release(ReleaseVersion(coreNumber)),
          PublishAction.Hold(
            HoldCompile.Release(ReleaseVersion(extraNumber)),
            HoldReason.AlreadyReleased,
          ),
        )
      case _ => false

  private def row(name: ShipName, number: ShipNumber, modules: ::[ProjectName]): Ship.Line =
    Ship.Line(name, number, modules) match
      case line: Ship.Line => line
end VersionTaskLawSpec
