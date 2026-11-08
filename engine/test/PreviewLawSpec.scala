package histos.preview

import histos.central.SnapshotRepository
import histos.publish.{LawGit, LocalPublishError}
import histos.version.SnapshotCoordinate
import histos.select.SelectError
import histos.version.{CiVersion, CommitSha, Refusal, SnapshotCoordinate, SnapshotPointer, VersionError}
import histos.`scala`
import histos.{
  Js,
  Jvm,
  MainClass,
  Native,
  ProjectGraph,
  ProjectName,
  Projects,
  Splits,
  Ship,
  aggregate,
  module,
  organization,
  ship,
  ships,
}

import java.nio.file.{Files, Path}

import _root_.scala.jdk.CollectionConverters.*

import zio.{Clock, ZIO}
import zio.test.*

object PreviewLawSpec extends ZIOSpecDefault:
  private val number = ship"1.2.3"
  private val scala3 = scala"3.10.0"
  private val sha    = CommitSha("0123456789abcdef0123456789abcdef01234567")

  def spec = suite("preview laws")(
    test("a development local publish and a development snapshot publish name the commit coordinate"):
      catalog(module("app", Jvm)(scala3)) match
        case Left(_)      => ZIO.succeed(assertTrue(false))
        case Right(graph) =>
          ZIO.acquireRelease(directory("histos-preview-dev"))(delete).flatMap { repo =>
            for
              sha       <- LawGit.ready(repo)
              _         <- Clock.instant
              local     <- Plan.publishLocal(repo, graph, Nil, Map.empty)
              snapshots <- Plan.publishSnapshots(repo, graph, Nil, Map.empty)
            yield
              val revision     = SnapshotCoordinate(number, sha.abbreviate).render
              val localText    = Plan.renderPublish(local)
              val snapshotText = Plan.renderPublish(snapshots)
              val moved = s"${number.render}-SNAPSHOT"
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
          }
    ,
    test("a snapshot session names the stored coordinate and the pointer, and neither is the release number"):
      catalog(module("app", Jvm)(scala3)) match
        case Left(_)      => ZIO.succeed(assertTrue(false))
        case Right(graph) =>
          val session = Map(
            "HISTOS_SESSION" -> "snapshot",
            "HISTOS_SHA"     -> sha.value,
            "HISTOS_BRANCH"  -> "default",
          )
          ZIO.acquireRelease(directory("histos-preview-snap"))(delete).flatMap { repo =>
            for
              _    <- Clock.instant
              _    <- ZIO.attemptBlocking(Files.writeString(repo.resolve("releases"), "")).orDie
              plan <- Plan.publishSnapshots(repo, graph, Nil, session)
            yield
              val revisions = plan.units.collect { case PublishUnit.Module(_, _, revision, _) => revision }
              assertTrue(
                revisions.contains(SnapshotCoordinate(number, sha.abbreviate).render),
                revisions.contains(SnapshotPointer(number).render),
                !revisions.contains(number.render),
              )
          }
    ,
    test("a release of a ship that already matches is refused, and an empty catalog publishes nothing"):
      val session = Map("HISTOS_SESSION" -> "release")
      catalog(module("app", Jvm)(scala3)) match
        case Left(_)      => ZIO.succeed(assertTrue(false))
        case Right(graph) =>
          Projects(module("core", Jvm)(scala3)) match
            case Left(_)      => ZIO.succeed(assertTrue(false))
            case Right(empty) =>
              ZIO.acquireRelease(directory("histos-preview-rel"))(delete).flatMap { repo =>
                for
                  _       <- Clock.instant
                  _       <- ZIO.attemptBlocking(Files.writeString(repo.resolve("releases"), "demo matches\n")).orDie
                  matched <- Plan.publishLocal(repo, graph, Nil, session).either
                  none    <- Plan.publishLocal(repo, empty, Nil, Map.empty).either
                yield assertTrue(
                  matched == Left(PreviewError.Version(VersionError.Refused(::(Refusal.NothingToRelease, Nil)))),
                  none == Left(PreviewError.Publish(LocalPublishError.NothingToPublish)),
                )
              }
    ,
    test("compile names the selected module and its upstream, and an unselected module is omitted"):
      val core = module("core", Jvm)(scala3)
      val app  = module("app", Jvm)(scala3).dependsOn(core).main(MainClass("demo.Main"))
      val idle = module("idle", Jvm)(scala3)
      Projects(app, core, idle) match
        case Left(_)      => ZIO.succeed(assertTrue(false))
        case Right(graph) =>
          ZIO.acquireRelease(directory("histos-preview-compile"))(delete).flatMap { repo =>
            for
              _        <- Clock.instant
              compiled <- Plan.compile(repo, graph, List(ProjectName("app")))
              tested   <- Plan.test(repo, graph, List(ProjectName("app")))
            yield
              val text  = Plan.render(repo, compiled)
              val tests = Plan.renderTest(repo, tested)
              assertTrue(
                text.contains(s"app ${scala3.value} jvm selected"),
                text.contains(s"core ${scala3.value} jvm upstream"),
                text.contains("sources"),
                text.contains("no previous compile"),
                !text.contains("idle"),
                tests.contains("selected"),
                tests.contains("quick: no suite would be skipped"),
                !tests.contains("quick skip"),
              )
          }
    ,
    test("clean keeps the store, clean full forgets it, and planning does not delete"):
      catalog(module("app", Jvm)(scala3)) match
        case Left(_)      => ZIO.succeed(assertTrue(false))
        case Right(graph) =>
          ZIO.acquireRelease(directory("histos-preview-clean"))(delete).flatMap { repo =>
            val classes = repo.resolve(".histos").resolve("classes")
            val marker  = classes.resolve("keep")
            for
              _ <- Clock.instant
              _ <- ZIO.attemptBlocking {
                Files.createDirectories(classes)
                Files.writeString(marker, "keep")
                ()
              }.orDie
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
          }
    ,
    test("run names the platform and the upstreams, and a module without a main is that miss"):
      val core  = module("core", Jvm)(scala3)
      val app   = module("app", Jvm)(scala3).dependsOn(core).main(MainClass("demo.Main"))
      val suite = aggregate("suite", core)
      val both  = module("both", Jvm, Js)(scala3).main(MainClass("demo.Main"))
      (Projects(core, app, suite), Projects(both)) match
        case (Right(graph), Right(wide)) =>
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
          )
        case _ => assertTrue(false),
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
    rest.toList match
      case Nil           => org(spec, first)
      case second :: Nil => org(spec, first, second)
      case _             =>
        Projects.assemble(Some(organization"example.com"), Nil, Splits.aggregate, spec, first :: rest.toList)

  private def directory(prefix: String): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking(Files.createTempDirectory(prefix)).orDie

  private def delete(root: Path): zio.UIO[Unit] =
    ZIO.attemptBlocking {
      if Files.exists(root) then
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.orDie
end PreviewLawSpec
