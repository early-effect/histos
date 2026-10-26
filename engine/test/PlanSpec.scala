package histos.preview

import histos.clean.Products
import histos.publish.LocalPublishError
import histos.version.{ManualVersion, Refusal, Session, ShipNumber, VersionError}
import histos.select.SelectError
import histos.{
  ClangMajor,
  Jvm,
  Js,
  MainClass,
  Native,
  Module,
  Organization,
  Platforms,
  ProjectDep,
  ProjectGraph,
  ProjectName,
  ProjectNameError,
  Projects,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  aggregate,
  module,
}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.FileTime

import scala.jdk.CollectionConverters.*

import zio.{Scope, ZIO}
import zio.test.*

object PlanSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")
  private val older  = ScalaVersion("3.8.0")
  private val at     = SourcePos("histos.scala", 1, 1)

  private val plans = suite("compile preview")(
    test(
      "a selection lists its upstream and omits the rest, and writes nothing"
    ):
      ZIO.scoped {
        for
          repo <- keep("histos-preview")
          _    <- write(repo, "core/src/main/scala/Core.scala", "object Core")
          _    <- write(repo, "app/src/main/scala/App.scala", "object App")
          _    <- write(repo, "app/src/test/scala/AppSpec.scala", "object AppSpec")
          _    <- write(repo, "idle/src/main/scala/Idle.scala", "object Idle")
          core = module("core", Jvm)(scala3)
          app  = module("app", Jvm)(scala3).dependsOn(core)
          idle = module("idle", Jvm)(scala3)
          graph <- ZIO.fromEither(Projects(core, app, idle))
          plan  <- Plan.compile(repo, graph, List(ProjectName("app")))
          text = Plan.render(repo, plan)
          shown <- Plan.show(repo, graph, List(ProjectName("app")))
        yield assertTrue(
          shown == text,
          plan.staleness == FileStaleness.Unknown,
          text ==
            """file-level staleness: unknown
              |core 3.10.0 jvm upstream
              |upstreams
              |sources core/src/main/scala/Core.scala
              |app 3.10.0 jvm selected
              |upstreams core
              |sources app/src/main/scala/App.scala""".stripMargin,
          plan.units.collect { case PlanUnit.NotSelected(name) =>
            name.value
          } == List("idle"),
          !Files.exists(repo.resolve(".histos")),
        )
      }
    ,
    test(
      "cells follow numeric versions and Jvm, Js, Native, with platform upstreams"
    ):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-cells")
          core = module("core", Jvm)(scala3)
          app  = module("app", Native, Js, Jvm)(scala3, older)
            .dependsOn(Jvm)(core)
            .clang(ClangMajor(21))
          graph <- ZIO.fromEither(Projects(core, app))
          plan  <- Plan.compile(repo, graph, List(ProjectName("app")))
        yield assertTrue(
          plan.staleness == FileStaleness.Unknown,
          shapes(plan) == List(
            ("core", "3.10.0", "jvm", Reason.Upstream, Nil),
            ("app", "3.8.0", "jvm", Reason.Selected, List("core")),
            ("app", "3.8.0", "js", Reason.Selected, Nil),
            ("app", "3.8.0", "native", Reason.Selected, Nil),
            ("app", "3.10.0", "jvm", Reason.Selected, List("core")),
            ("app", "3.10.0", "js", Reason.Selected, Nil),
            ("app", "3.10.0", "native", Reason.Selected, Nil),
          ),
          plan.units.forall:
            case chosen: PlanUnit.Chosen => chosen.sources.isEmpty
            case PlanUnit.NotSelected(_) => true
          ,
          !Files.exists(repo.resolve(".histos")),
        )
      }
    ,
    test("no names selects every module"):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-all")
          core = module("core", Jvm)(scala3)
          app  = module("app", Jvm)(scala3).dependsOn(core)
          graph <- ZIO.fromEither(Projects(core, app))
          plan  <- Plan.compile(repo, graph, Nil)
        yield assertTrue(
          shapes(plan).map(_._4) == List(Reason.Selected, Reason.Selected),
          shapes(plan).map(_._5) == List(Nil, List("core")),
        )
      }
    ,
    test("an aggregate selects its modules and still lists upstreams"):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-agg")
          core = module("core", Jvm)(scala3)
          app  = module("app", Jvm)(scala3).dependsOn(core)
          root = aggregate("root", core, app)
          graph <- ZIO.fromEither(Projects(root, core, app))
          plan  <- Plan.compile(repo, graph, List(ProjectName("root")))
        yield assertTrue(
          shapes(plan) == List(
            ("core", "3.10.0", "jvm", Reason.Selected, Nil),
            ("app", "3.10.0", "jvm", Reason.Selected, List("core")),
          )
        )
      }
    ,
    test(
      "one module reads repository src, and its own src wins when both exist"
    ):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-src")
          _    <- write(repo, "src/main/scala/Shared.scala", "object Shared")
          only = module("only", Jvm)(scala3)
          alone  <- ZIO.fromEither(Projects(only))
          shared <- Plan.compile(repo, alone, Nil)
          _      <- write(repo, "only/src/main/scala/Own.scala", "object Own")
          named  <- Plan.compile(repo, alone, Nil)
        yield assertTrue(
          relativeSources(repo, shared) == List("src/main/scala/Shared.scala"),
          relativeSources(repo, named) == List("only/src/main/scala/Own.scala"),
        )
      }
    ,
    test("unknown, duplicate, and a bad name are target errors"):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-names")
          core = module("core", Jvm)(scala3)
          graph     <- ZIO.fromEither(Projects(core))
          missing   <- ZIO.fromEither(ProjectName.from("missing"))
          coreName  <- ZIO.fromEither(ProjectName.from("core"))
          unknown   <- Plan.compile(repo, graph, List(ProjectName("missing"))).either
          duplicate <- Plan.compile(repo, graph, List(ProjectName("core"), ProjectName("core"))).either
          bad = ProjectName.from("1core")
        yield assertTrue(
          unknown == Left(PreviewError.Target(SelectError.Unknown(missing))),
          duplicate == Left(PreviewError.Target(SelectError.Duplicate(coreName))),
          bad == Left(ProjectNameError.Bad("1core")),
        )
      }
    ,
    test("a cycle is a cycle error and writes nothing"):
      ZIO.scoped {
        for
          repo  <- keep("histos-preview-cycle")
          left  <- ZIO.fromEither(ProjectName.from("left"))
          right <- ZIO.fromEither(ProjectName.from("right"))
          graph = ProjectGraph.trusted(
            List(
              cell(left, List(right)),
              cell(right, List(left)),
            )
          )
          planned <- Plan.compile(repo, graph, List(ProjectName("left"))).either
        yield assertTrue(
          planned == Left(PreviewError.Cycle(left)),
          !Files.exists(repo.resolve(".histos")),
        )
      },
  )

  private val asked = suite("the other previews")(
    test("test lists test sources in numeric Scala order and writes nothing"):
      ZIO.scoped {
        for
          repo    <- keep("histos-preview-test")
          _       <- write(repo, "core/src/main/scala/Core.scala", "object Core")
          _       <- write(repo, "core/src/test/scala/CoreSpec.scala", "object CoreSpec")
          _       <- write(repo, "app/src/main/scala/App.scala", "object App")
          _       <- write(repo, "app/src/test/scala/AppSpec.scala", "object AppSpec")
          planted <- plant(repo)
          core = module("core", Jvm)(scala3)
          app  = module("app", Jvm)(older, scala3).dependsOn(core)
          graph <- ZIO.fromEither(Projects(core, app))
          plan  <- Plan.test(repo, graph, List(ProjectName("app")))
          text = Plan.renderTest(repo, plan)
          kept <- same(planted)
        yield assertTrue(
          plan.staleness == FileStaleness.Unknown,
          text.startsWith("file-level staleness: unknown"),
          !text.contains("testQuick"),
          !text.contains("skipped"),
          !text.contains("src/main"),
          !text.contains("would take"),
          lines(plan.units) == List(
            ("core", "3.10.0", "jvm", Reason.Upstream, List()),
            ("app", "3.8.0", "jvm", Reason.Selected, List("core")),
            ("app", "3.10.0", "jvm", Reason.Selected, List("core")),
          ),
          shownSources(repo, plan.units) == List(
            "core/src/test/scala/CoreSpec.scala",
            "app/src/test/scala/AppSpec.scala",
            "app/src/test/scala/AppSpec.scala",
          ),
          kept,
          !Files.exists(repo.resolve(".histos").resolve("test-classes")),
        )
      }
    ,
    test("publish names the modules and the upstream, and writes no jar"):
      ZIO.scoped {
        for
          repo    <- keep("histos-preview-publish")
          planted <- plant(repo)
          graph   <- ZIO.fromEither(manualLibrary)
          plan    <- Plan.publish(repo, graph, List(ProjectName("app")), Map.empty)
          text = Plan.renderPublish(plan)
          kept <- same(planted)
        yield assertTrue(
          plan.staleness == FileStaleness.Unknown,
          text ==
            """file-level staleness: unknown
              |core libs 1.0.0 upstream
              |app libs 1.0.0 selected""".stripMargin,
          kept,
          !Files.exists(repo.resolve(".histos").resolve("jars")),
        )
      }
    ,
    test("a held ship stays held and an unreleased ship names both revisions"):
      ZIO.scoped {
        for
          repo    <- keep("histos-preview-hold")
          planted <- plant(repo)
          _       <- write(repo, ".histos/releases", "libs matches\n")
          graph   <- ZIO.fromEither(twoShips)
          plan    <- Plan.publish(repo, graph, Nil, snapshotEnv)
          text = Plan.renderPublish(plan)
          kept <- same(planted)
        yield assertTrue(
          text ==
            """file-level staleness: unknown
              |app client 2.0.0-1234567890ab-SNAPSHOT selected
              |app client 2.0.0-SNAPSHOT selected
              |held libs 1.4.2-ci already-released""".stripMargin,
          kept,
          !Files.exists(repo.resolve(".histos").resolve("jars")),
        )
      }
    ,
    test("a ship that only matches is held and is not a publish line"):
      ZIO.scoped {
        for
          repo    <- keep("histos-preview-only-hold")
          planted <- plant(repo)
          _       <- write(repo, ".histos/releases", "libs matches\n")
          graph   <- ZIO.fromEither(heldShip)
          plan    <- Plan.publish(repo, graph, Nil, snapshotEnv)
          text = Plan.renderPublish(plan)
          kept <- same(planted)
        yield assertTrue(
          text ==
            """file-level staleness: unknown
              |held libs 1.4.2-ci already-released""".stripMargin,
          !text.contains("SNAPSHOT"),
          kept,
        )
      }
    ,
    test("development is not published and is not called held"):
      ZIO.scoped {
        for
          repo    <- keep("histos-preview-develop")
          planted <- plant(repo)
          graph   <- ZIO.fromEither(developShip)
          plan    <- Plan.publish(repo, graph, Nil, Map.empty)
          text = Plan.renderPublish(plan)
          kept <- same(planted)
        yield assertTrue(
          text ==
            """file-level staleness: unknown
              |libs 1.0.0-ci not published""".stripMargin,
          !text.contains("held"),
          kept,
          !Files.exists(repo.resolve(".histos").resolve("jars")),
        )
      }
    ,
    test("a snapshot with no registry is refused, not held"):
      ZIO.scoped {
        for
          repo    <- keep("histos-preview-refused")
          planted <- plant(repo)
          graph   <- ZIO.fromEither(heldShip)
          refused <- Plan.publish(repo, graph, Nil, snapshotEnv).either
          kept    <- same(planted)
        yield assertTrue(
          refused match
            case Left(
                  PreviewError.Version(
                    VersionError.Refused(Refusal.Unchecked :: Nil)
                  )
                ) =>
              true
            case _ => false
          ,
          kept,
        )
      }
    ,
    test("a module outside the ship is not published"):
      ZIO.scoped {
        for
          repo    <- keep("histos-preview-not-ship")
          planted <- plant(repo)
          idle    <- ZIO.fromEither(ProjectName.from("idle"))
          graph   <- ZIO.fromEither(withIdle)
          refused <- Plan.publish(repo, graph, List(ProjectName("idle")), Map.empty).either
          kept    <- same(planted)
        yield assertTrue(
          refused == Left(
            PreviewError.Publish(LocalPublishError.NotPublished(idle))
          ),
          kept,
        )
      }
    ,
    test("an empty clean names every product root and does not delete"):
      ZIO.scoped {
        for
          repo    <- keep("histos-preview-clean")
          planted <- plant(repo)
          _       <- write(repo, ".histos/dependencies.lock", "lock")
          _       <- write(repo, ".histos/server.json", "{}")
          empty   <- ZIO.fromEither(Projects())
          plan    <- Plan.clean(repo, empty, Nil)
          text = Plan.renderClean(repo, plan)
          kept <- same(planted)
          lock   = Files.isRegularFile(repo.resolve(".histos/dependencies.lock"))
          server = Files.isRegularFile(repo.resolve(".histos/server.json"))
        yield assertTrue(
          plan == Plan.cleanAll(repo),
          plan.staleness == FileStaleness.Unknown,
          plan.store == StoreFate.Kept,
          plan.roots == productRoots(repo),
          text.startsWith("file-level staleness: unknown"),
          !text.contains("server.json"),
          !text.contains("dependencies.lock"),
          !text.contains("assembly"),
          kept && lock && server,
        )
      }
    ,
    test("a named clean lists that module and leaves the class file"):
      ZIO.scoped {
        for
          repo    <- keep("histos-preview-clean-named")
          planted <- plant(repo)
          app  = module("app", Jvm)(scala3)
          idle = module("idle", Jvm)(scala3)
          graph <- ZIO.fromEither(Projects(app, idle))
          plan  <- Plan.clean(repo, graph, List(ProjectName("app")))
          text = Plan.renderClean(repo, plan)
          kept <- same(planted)
        yield assertTrue(
          plan.roots == Products.moduleRoots.map(name => repo.resolve(".histos").resolve(name).resolve("app")),
          !text.contains("provider"),
          !text.contains("idle"),
          text.startsWith("file-level staleness: unknown"),
          kept,
        )
      }
    ,
    test("cleanFull names the roots and the forgotten store, and deletes neither"):
      ZIO.scoped {
        for
          repo    <- keep("histos-preview-clean-full")
          planted <- plant(repo)
          plan = Plan.cleanFull(repo)
          text = Plan.renderClean(repo, plan)
          kept <- same(planted)
          sentence = "content-store entries for this repo would be forgotten"
        yield assertTrue(
          plan.roots == Plan.cleanAll(repo).roots,
          plan.store == StoreFate.Forgotten,
          plan.staleness == FileStaleness.Unknown,
          text.startsWith("file-level staleness: unknown"),
          text.endsWith(sentence),
          text.linesIterator.count(_ == sentence) == 1,
          !text.contains("byte"),
          kept,
        )
      }
    ,
    test("run names the module, the platform, and the upstreams"):
      ZIO.scoped {
        for
          repo     <- keep("histos-preview-run")
          planted  <- plant(repo)
          graph    <- ZIO.fromEither(runners)
          appName  <- ZIO.fromEither(ProjectName.from("app"))
          idleName <- ZIO.fromEither(ProjectName.from("idle"))
          rootName <- ZIO.fromEither(ProjectName.from("root"))
          planned = Plan.run(graph, "app", None)
          chosen  = Plan.run(graph, "app", Some(Jvm))
          missing = Plan.run(graph, "missing", None)
          wide    = Plan.run(graph, "wide", None)
          unbuilt = Plan.run(graph, "app", Some(Native))
          idle    = Plan.run(graph, "idle", None)
          root    = Plan.run(graph, "root", None)
          inspect = Plan.run(graph, "inspect", None)
          bad     = Plan.run(graph, "inspect", Some(Jvm))
          kept <- same(planted)
          text = planned.map(Plan.renderRun)
        yield assertTrue(
          planned == chosen,
          text == Right(
            """file-level staleness: unknown
              |app jvm
              |upstreams core""".stripMargin
          ),
          missing == Left(PreviewError.Run(RunMiss.Absent("missing"))),
          wideMatch(graph, wide),
          unbuilt == Left(
            PreviewError.Run(RunMiss.Unbuilt(appName, Native, List(Jvm)))
          ),
          idle == Left(PreviewError.Run(RunMiss.NoMain(idleName))),
          root == Left(PreviewError.Run(RunMiss.Aggregate(rootName))),
          inspect == Right(
            RunPlan(RunUnit.Task("inspect"), FileStaleness.Unknown)
          ),
          bad == Left(
            PreviewError.Run(
              RunMiss.BadId("run inspect does not take a platform")
            )
          ),
          kept,
        )
      },
  )

  private def lines(units: List[PlanUnit]): List[(String, String, String, Reason, List[String])] =
    units.collect { case chosen: PlanUnit.Chosen =>
      (
        chosen.module.value,
        chosen.version.value,
        chosen.platform.token,
        chosen.reason,
        chosen.upstreams.map(_.value),
      )
    }

  private def shownSources(repo: Path, units: List[PlanUnit]): List[String] =
    units.flatMap:
      case chosen: PlanUnit.Chosen =>
        chosen.sources.map: path =>
          repo.toAbsolutePath.normalize
            .relativize(path.toAbsolutePath.normalize)
            .iterator
            .asScala
            .map(_.toString)
            .mkString("/")
      case PlanUnit.NotSelected(_) => Nil

  private def wideMatch(
      graph: ProjectGraph,
      result: Either[PreviewError, RunPlan],
  ): Boolean =
    graph.projects.collect {
      case item: Module if item.name.value == "wide" =>
        item
    } match
      case item :: Nil =>
        result == Left(
          PreviewError.Run(RunMiss.Choose(item.name, item.platforms.all))
        )
      case _ => false

  private val snapshotEnv = Map(
    Session.Key       -> "snapshot",
    Session.ShaKey    -> "1234567890abcdef1234567890abcdef12345678",
    Session.BranchKey -> "default",
  )

  private def manualLibrary: Either[histos.BuildError, ProjectGraph] =
    val core = module("core", Jvm)(scala3)
    val app  = module("app", Jvm)(scala3).dependsOn(core)
    Projects.organization(Organization("com.example"))(
      histos.ships(
        histos.manual("libs", ManualVersion("1.0.0"))(core, app)
      ),
      core,
      app,
    )

  private def twoShips: Either[histos.BuildError, ProjectGraph] =
    val core = module("core", Jvm)(scala3)
    val app  = module("app", Jvm)(scala3)
    Projects.organization(Organization("com.example"))(
      histos.ships(
        histos.ship("libs", ShipNumber("1.4.2"))(core),
        histos.ship("client", ShipNumber("2.0.0"))(app),
      ),
      core,
      app,
    )

  private def heldShip: Either[histos.BuildError, ProjectGraph] =
    val core = module("core", Jvm)(scala3)
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.ship("libs", ShipNumber("1.4.2"))(core)),
      core,
    )

  private def developShip: Either[histos.BuildError, ProjectGraph] =
    val core = module("core", Jvm)(scala3)
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.ship("libs", ShipNumber("1.0.0"))(core)),
      core,
    )

  private def withIdle: Either[histos.BuildError, ProjectGraph] =
    val core = module("core", Jvm)(scala3)
    val idle = module("idle", Jvm)(scala3)
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.manual("libs", ManualVersion("1.0.0"))(core)),
      core,
      idle,
    )

  private def runners: Either[histos.BuildError, ProjectGraph] =
    val core = module("core", Jvm)(scala3)
    val app  = module("app", Jvm)(scala3)
      .dependsOn(core)
      .main(MainClass("demo.App"))
    val wide = module("wide", Jvm, Js)(scala3).main(MainClass("demo.App"))
    val idle = module("idle", Jvm)(scala3)
    val root = aggregate("root", app)
    Projects(core, app, wide, idle, root)

  private def productRoots(repo: Path): List[Path] =
    (Products.moduleRoots ++ Products.sharedRoots).map(name => repo.resolve(".histos").resolve(name))

  private final case class Planted(path: Path, time: FileTime, bytes: Array[Byte])

  private def plant(repo: Path): ZIO[Any, Throwable, Planted] =
    ZIO.attempt {
      val path = repo.resolve(".histos").resolve("classes").resolve("app").resolve("Demo.class")
      Files.createDirectories(path.getParent)
      val bytes = Array[Byte](1, 2, 3)
      Files.write(path, bytes)
      Files.setLastModifiedTime(path, FileTime.fromMillis(1700000000000L))
      Planted(path, Files.getLastModifiedTime(path), bytes)
    }

  private def same(planted: Planted): ZIO[Any, Throwable, Boolean] =
    ZIO.attempt {
      Files.isRegularFile(planted.path) &&
      Files.getLastModifiedTime(planted.path) == planted.time &&
      java.util.Arrays.equals(Files.readAllBytes(planted.path), planted.bytes)
    }

  private def shapes(
      plan: CompilePlan
  ): List[(String, String, String, Reason, List[String])] =
    lines(plan.units)

  private def relativeSources(repo: Path, plan: CompilePlan): List[String] =
    shownSources(repo, plan.units)

  private def cell(name: ProjectName, upstream: List[ProjectName]): Module =
    Module(
      name,
      Platforms(Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      upstream.map(item => ProjectDep(item, None)),
      at,
    )

  private def keep(prefix: String): ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory(prefix)))(delete)

  private def write(
      root: Path,
      name: String,
      text: String,
  ): ZIO[Any, Throwable, Unit] =
    ZIO.attempt {
      val path = root.resolve(name)
      Files.createDirectories(path.getParent)
      Files.writeString(path, text)
    }

  private def delete(root: Path): ZIO[Any, Nothing, Unit] =
    ZIO.attempt {
      if Files.exists(root) then
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths
          .sortBy(_.getNameCount)
          .reverse
          .foreach(path => Files.deleteIfExists(path))
    }.ignore

  def spec = suite("preview plans")(plans, asked)
end PlanSpec
