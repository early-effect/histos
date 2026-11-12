package histos.preview

import histos.central.SnapshotRepository
import histos.publish.{LawGit, LocalPublishError}
import histos.select.SelectError
import histos.version.{CiVersion, CommitSha, Refusal, SnapshotCoordinate, SnapshotPointer, VersionError}
import histos.workspace.Workspace
import histos.`scala`
import histos.{
  Js,
  Jvm,
  MainClass,
  Native,
  ProjectGraph,
  ProjectName,
  Projects,
  Ship,
  aggregate,
  module,
  organization,
  ship,
  ships,
}

import java.nio.file.Files

import zio.ZIO
import zio.test.*

object PreviewLawSpec extends ZIOSpecDefault:
  private val number = ship"1.2.3"
  private val scala3 = scala"3.10.0"
  private val sha    = CommitSha("0123456789abcdef0123456789abcdef01234567")

  def spec = suite("preview laws")(
    test("a development local publish and a development snapshot publish name the commit coordinate"):
      for
        graph     <- ZIO.fromEither(catalog(module("app", Jvm)(scala3)))
        repo      <- Workspace.work("histos-preview-dev")
        sha       <- LawGit.ready(repo)
        local     <- Plan.publishLocal(repo, graph, Nil, Map.empty)
        snapshots <- Plan.publishSnapshots(repo, graph, Nil, Map.empty)
      yield
        val revision     = SnapshotCoordinate(number, sha.abbreviate).render
        val localText    = Plan.renderPublish(local)
        val snapshotText = Plan.renderPublish(snapshots)
        val moved        = s"${number.render}-SNAPSHOT"
        assertTrue(
          local.place == "ivy2Local",
          localText.startsWith(local.place),
          localText.contains(s"app demo $revision selected"),
          !localText.contains(s"app demo $moved selected"),
          !localText.contains(CiVersion(number).render),
          snapshots.place == SnapshotRepository.central.root.render,
          snapshotText.startsWith(snapshots.place),
          snapshotText.contains(s"app demo $revision selected"),
          snapshotText.contains(s"app demo $moved selected"),
          !snapshotText.contains("not published"),
          !snapshotText.contains(number.render + " selected"),
        )
    ,
    test("a snapshot session names the stored coordinate and the pointer, and neither is the release number"):
      val session = Map(
        "HISTOS_SESSION" -> "snapshot",
        "HISTOS_SHA"     -> sha.value,
        "HISTOS_BRANCH"  -> "default",
      )
      for
        graph <- ZIO.fromEither(catalog(module("app", Jvm)(scala3)))
        repo  <- Workspace.work("histos-preview-snap")
        _     <- Workspace.writeString(repo.resolve("releases"), "")
        plan  <- Plan.publishSnapshots(repo, graph, Nil, session)
      yield
        val revisions = plan.units.collect { case PublishUnit.Module(_, _, revision, _) => revision }
        assertTrue(
          revisions.contains(SnapshotCoordinate(number, sha.abbreviate).render),
          revisions.contains(SnapshotPointer(number).render),
          !revisions.contains(number.render),
        )
    ,
    test("a release of a ship that already matches is refused, and an empty catalog publishes nothing"):
      val session = Map("HISTOS_SESSION" -> "release")
      for
        graph   <- ZIO.fromEither(catalog(module("app", Jvm)(scala3)))
        empty   <- ZIO.fromEither(Projects(module("core", Jvm)(scala3)))
        repo    <- Workspace.work("histos-preview-rel")
        _       <- Workspace.writeString(repo.resolve("releases"), "demo matches\n")
        matched <- Plan.publishLocal(repo, graph, Nil, session).either
        none    <- Plan.publishLocal(repo, empty, Nil, Map.empty).either
      yield assertTrue(
        matched == Left(PreviewError.Version(VersionError.Refused(::(Refusal.NothingToRelease, Nil)))),
        none == Left(PreviewError.Publish(LocalPublishError.NothingToPublish)),
      )
    ,
    test("clean keeps the store, clean full forgets it, and planning does not delete"):
      for
        graph <- ZIO.fromEither(catalog(module("app", Jvm)(scala3)))
        repo  <- Workspace.work("histos-preview-clean")
        marker = repo.resolve(".histos").resolve("classes").resolve("keep")
        _       <- Workspace.writeString(marker, "keep")
        cleaned <- Plan.clean(repo, graph, Nil)
        unknown <- Plan.clean(repo, graph, List(ProjectName("missing"))).either
        still   <- ZIO.attemptBlocking(Files.exists(marker)).orDie
      yield
        val full      = Plan.cleanFull(repo)
        val kept      = Plan.renderClean(repo, Plan.cleanAll(repo))
        val forgotten = Plan.renderClean(repo, full)
        val name      = unknown match
          case Left(PreviewError.Target(SelectError.Unknown(found, _))) => Some(found)
          case _                                                        => None
        assertTrue(
          cleaned == Plan.cleanAll(repo),
          cleaned.store == StoreFate.Kept,
          full.store == StoreFate.Forgotten,
          !kept.contains("forgotten"),
          forgotten.contains("content-store entries for this repo would be forgotten"),
          still,
          name.contains(ProjectName("missing")),
        )
    ,
    test("run names the platform and the upstreams, and a module without a main is that miss"):
      val core  = module("core", Jvm)(scala3)
      val app   = module("app", Jvm)(scala3).dependsOn(core).main(MainClass("demo.Main"))
      val suite = aggregate("suite", core)
      val both  = module("both", Jvm, Js)(scala3).main(MainClass("demo.Main"))
      for
        graph <- ZIO.fromEither(Projects(core, app, suite))
        wide  <- ZIO.fromEither(Projects(both))
      yield
        val ran     = Plan.run(graph, "app", None)
        val chosen  = Plan.run(wide, "both", None)
        val unbuilt = Plan.run(wide, "both", Some(Native))
        assertTrue(
          Plan.run(graph, "core", None) == Left(PreviewError.Run(RunMiss.NoMain(ProjectName("core")))),
          Plan.run(graph, "suite", None) == Left(PreviewError.Run(RunMiss.Aggregate(ProjectName("suite")))),
          rendered(ran, "app jvm\nupstreams core"),
          absent(Plan.run(graph, "missing", None)),
          platforms(chosen, Set("jvm", "js")),
          unbuiltPlatform(unbuilt),
        ),
  )

  private def rendered(plan: Either[PreviewError, RunPlan], text: String): Boolean =
    plan match
      case Right(found) => Plan.renderRun(found) == text
      case Left(_)      => false

  private def absent(plan: Either[PreviewError, RunPlan]): Boolean =
    plan match
      case Left(PreviewError.Run(RunMiss.Absent(name, _))) => name == "missing"
      case _                                               => false

  private def platforms(plan: Either[PreviewError, RunPlan], tokens: Set[String]): Boolean =
    plan match
      case Left(PreviewError.Run(RunMiss.Choose(name, found))) =>
        name == ProjectName("both") && found.map(_.token).toSet == tokens
      case _ => false

  private def unbuiltPlatform(plan: Either[PreviewError, RunPlan]): Boolean =
    plan match
      case Left(PreviewError.Run(RunMiss.Unbuilt(name, platform, built))) =>
        name == ProjectName("both") && platform == Native && built.map(_.token).toSet == Set("jvm", "js")
      case _ => false

  private def catalog(first: histos.ProjectDef, rest: histos.ProjectDef*): Either[histos.BuildError, ProjectGraph] =
    val org  = Projects.organization(organization"example.com")
    val spec = ships(Ship("demo").next(1, 2, 3)(first, rest*))
    org(spec, (first :: rest.toList)*)
end PreviewLawSpec
