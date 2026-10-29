package histos.preview

import histos.clean.Products
import histos.jdk.{Machine, Sha256, Toolchain}
import histos.publish.LocalPublishError
import histos.resolve.{BuildResolvers, DependencyLock, LockedArtifact, LockedTool, Repository, Repositories}
import histos.testrun.{QuickRecord, SuiteAsk, SuiteCover, SuiteName, SuiteStamp, TestCompile}
import histos.version.{ManualVersion, Refusal, Session, ShipNumber, VersionError}
import histos.zinc.{AnalysisKind, CompilerRequests, FileStaleness, SourceStale, StoredAnalysis}
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

import org.objectweb.asm.{ClassWriter, Opcodes}

import scala.jdk.CollectionConverters.*

import zio.{Chunk, Scope, ZIO}
import zio.test.*

import java.nio.charset.StandardCharsets

object PlanSpec extends histos.SuiteClock:
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
          text ==
            """core 3.10.0 jvm upstream
              |upstreams
              |sources core/src/main/scala/Core.scala
              |no previous compile
              |stale core/src/main/scala/Core.scala
              |app 3.10.0 jvm selected
              |upstreams core
              |sources app/src/main/scala/App.scala
              |no previous compile
              |stale app/src/main/scala/App.scala""".stripMargin,
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
          text = Plan.render(repo, plan)
        yield assertTrue(
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
            case chosen: PlanUnit.Chosen =>
              chosen.sources.isEmpty && chosen.staleness == FileStaleness.Initial(Nil) && chosen.quick.isEmpty
            case PlanUnit.NotSelected(_) => true
          ,
          text.contains("no previous compile"),
          !text.contains("stale"),
          !text.contains("fresh"),
          !text.contains("unknown"),
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
          text.contains("no previous compile"),
          text.contains("stale core/src/test/scala/CoreSpec.scala"),
          text.contains("quick: no suite would be skipped"),
          !text.contains("file-level staleness: unknown"),
          !text.contains("testQuick"),
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
          plan    <- Plan.publishLocal(repo, graph, List("app"), Map.empty, Some("target/repo"), ZIO.succeed(repo))
          text = Plan.renderPublish(plan)
          kept <- same(planted)
        yield assertTrue(
          text ==
            """target/repo
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
          plan    <- Plan.publishSnapshots(repo, graph, Nil, snapshotEnv)
          text = Plan.renderPublish(plan)
          kept <- same(planted)
        yield assertTrue(
          text ==
            s"""${histos.central.SnapshotRepository.central.root.render}
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
          plan    <- Plan.publishSnapshots(repo, graph, Nil, snapshotEnv)
          text = Plan.renderPublish(plan)
          kept <- same(planted)
        yield assertTrue(
          text ==
            s"""${histos.central.SnapshotRepository.central.root.render}
               |held libs 1.4.2-ci already-released""".stripMargin,
          !text.contains("SNAPSHOT"),
          kept,
        )
      }
    ,
    test("development writes locally and snapshots leave it unpublished"):
      ZIO.scoped {
        for
          repo    <- keep("histos-preview-develop")
          planted <- plant(repo)
          graph   <- ZIO.fromEither(developShip)
          local   <- Plan.publishLocal(repo, graph, Nil, Map.empty, Some("target/repo"), ZIO.succeed(repo))
          remote  <- Plan.publishSnapshots(repo, graph, Nil, Map.empty)
          localText  = Plan.renderPublish(local)
          remoteText = Plan.renderPublish(remote)
          kept <- same(planted)
        yield assertTrue(
          localText ==
            """target/repo
              |core libs 1.0.0-ci selected""".stripMargin,
          remoteText ==
            s"""${histos.central.SnapshotRepository.central.root.render}
               |libs 1.0.0-ci not published""".stripMargin,
          !remoteText.contains("held"),
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
          refused <- Plan.publishSnapshots(repo, graph, Nil, snapshotEnv).either
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
          refused <- Plan
            .publishLocal(
              repo,
              graph,
              List("idle"),
              Map.empty,
              Some("target/repo"),
              ZIO.succeed(repo),
            )
            .either
          kept <- same(planted)
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
          plan.store == StoreFate.Kept,
          plan.roots == productRoots(repo),
          text.startsWith(".histos/classes"),
          !text.contains("staleness"),
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
          text.startsWith(".histos/classes/app"),
          !text.contains("staleness"),
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
          text.startsWith(".histos/classes"),
          !text.contains("staleness"),
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
            """app jvm
              |upstreams core""".stripMargin
          ),
          missing == Left(PreviewError.Run(RunMiss.Absent("missing"))),
          wideMatch(graph, wide),
          unbuilt == Left(
            PreviewError.Run(RunMiss.Unbuilt(appName, Native, List(Jvm)))
          ),
          idle == Left(PreviewError.Run(RunMiss.NoMain(idleName))),
          root == Left(PreviewError.Run(RunMiss.Aggregate(rootName))),
          inspect == Right(RunPlan(RunUnit.Task("inspect"))),
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

  private def shown(repo: Path, path: Path): String =
    repo.toAbsolutePath.normalize
      .relativize(path.toAbsolutePath.normalize)
      .iterator
      .asScala
      .map(_.toString)
      .mkString("/")

  private def shownSources(repo: Path, units: List[PlanUnit]): List[String] =
    units.flatMap:
      case chosen: PlanUnit.Chosen => chosen.sources.map(path => shown(repo, path))
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

  private def moduleNamed(graph: ProjectGraph, name: ProjectName): Either[PreviewError, Module] =
    graph.get(name) match
      case Some(item: Module) => Right(item)
      case _                  => Left(PreviewError.Run(RunMiss.Absent(name.value)))

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

  private final case class Mark(path: String, time: FileTime, bytes: Long)

  private def census(root: Path): ZIO[Any, Throwable, List[Mark]] =
    ZIO.attempt {
      if !Files.exists(root) then Nil
      else
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths
          .filter(path => Files.isRegularFile(path))
          .sortBy(_.toString)
          .map: path =>
            val name = root.relativize(path).iterator.asScala.map(_.toString).mkString("/")
            Mark(name, Files.getLastModifiedTime(path), Files.size(path))
    }

  private def writeClass(root: Path, internal: String, mark: String): Unit =
    val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    writer.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER,
      internal,
      null,
      "java/lang/Object",
      null,
    )
    val field = writer.visitField(
      Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL,
      "mark",
      "Ljava/lang/String;",
      null,
      mark,
    )
    field.visitEnd()
    val method = writer.visitMethod(Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC, "run", "()V", null, null)
    method.visitInsn(Opcodes.RETURN)
    method.visitMaxs(0, 0)
    method.visitEnd()
    writer.visitEnd()
    val path = root.resolve(internal + ".class")
    Files.createDirectories(path.getParent)
    Files.write(path, writer.toByteArray)
    Files.setLastModifiedTime(path, FileTime.fromMillis(1700000000000L))
    ()

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

  private val answers = suite("file staleness and quick")(
    test("a changed source and the source that depends on it are stale, and classes stay"):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-stale")
          _    <- write(repo, "app/src/main/scala/Core.scala", "object Core")
          _    <- write(repo, "app/src/main/scala/Use.scala", "object Use")
          appMod = module("app", Jvm)(scala3)
          graph <- ZIO.fromEither(Projects(appMod))
          app   <- ZIO.fromEither(moduleNamed(graph, ProjectName("app")))
          core    = repo.resolve("app/src/main/scala/Core.scala")
          used    = repo.resolve("app/src/main/scala/Use.scala")
          classes = repo.resolve(".histos").resolve("classes").resolve("app").resolve("3.10.0").resolve("jvm")
          _ <- ZIO.attempt {
            val kept = classes.resolve("demo").resolve("Keep.class")
            Files.createDirectories(kept.getParent)
            Files.write(kept, Array[Byte](9, 9))
            Files.setLastModifiedTime(kept, FileTime.fromMillis(1700000000000L))
          }
          before <- census(classes)
          zip = SourceStale.directory(repo, app.name, scala3, Jvm, AnalysisKind.Main).resolve("inc.zip")
          _ <- ZIO.attempt(
            StoredAnalysis.put(
              zip,
              classes,
              List(core       -> "demo.Core", used -> "demo.Use"),
              List("demo.Use" -> "demo.Core"),
            )
          )
          _         <- ZIO.attempt(Files.setLastModifiedTime(zip, FileTime.fromMillis(1700000000000L)))
          zipBefore <- ZIO.attempt(Files.readAllBytes(zip).toList)
          fresh     <- Plan.compile(repo, graph, List(ProjectName("app")))
          freshText = Plan.render(repo, fresh)
          _     <- write(repo, "app/src/main/scala/Core.scala", "object Core { def n = 1 }")
          stale <- Plan.compile(repo, graph, List(ProjectName("app")))
          staleText = Plan.render(repo, stale)
          after    <- census(classes)
          zipAfter <- ZIO.attempt(Files.readAllBytes(zip).toList)
          zipTime  <- ZIO.attempt(Files.getLastModifiedTime(zip))
        yield assertTrue(
          freshText.contains("fresh"),
          !freshText.contains("stale"),
          staleText.contains("stale app/src/main/scala/Core.scala"),
          staleText.contains("stale app/src/main/scala/Use.scala"),
          !staleText.contains("fresh"),
          before == after,
          zipBefore == zipAfter,
          zipTime == FileTime.fromMillis(1700000000000L),
        )
      }
    ,
    test("a present analysis that cannot be read is unknown, and a missing one is not"):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-unknown")
          _    <- write(repo, "app/src/main/scala/App.scala", "object App")
          appMod = module("app", Jvm)(scala3)
          graph <- ZIO.fromEither(Projects(appMod))
          app   <- ZIO.fromEither(moduleNamed(graph, ProjectName("app")))
          zip = SourceStale.directory(repo, app.name, scala3, Jvm, AnalysisKind.Main).resolve("inc.zip")
          missing <- Plan.compile(repo, graph, Nil)
          _       <- ZIO.attempt {
            Files.createDirectories(zip.getParent)
            Files.writeString(zip, "not an analysis")
          }
          unread <- Plan.compile(repo, graph, Nil)
          text = Plan.render(repo, unread)
        yield assertTrue(
          missing.units.collect { case chosen: PlanUnit.Chosen => chosen.staleness } match
            case FileStaleness.Initial(paths) :: Nil =>
              paths.map(path => shown(repo, path)) == List("app/src/main/scala/App.scala")
            case _ => false
          ,
          unread.units.collect { case chosen: PlanUnit.Chosen => chosen.staleness } ==
            List(FileStaleness.Unknown),
          text.contains("file-level staleness: unknown"),
          !text.contains("no previous compile"),
          !text.contains("fresh"),
          !text.contains("stale "),
        )
      }
    ,
    test("quick preview skips the digest a run would store, and a changed class runs"):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-quick")
          _    <- write(repo, "app/src/test/scala/AppSpec.scala", "object AppSpec")
          appMod = module("app", Jvm)(scala3)
          graph <- ZIO.fromEither(Projects(appMod))
          app   <- ZIO.fromEither(moduleNamed(graph, ProjectName("app")))
          repos   = BuildResolvers(graph.resolvers)
          request = CompilerRequests.compiler(scala3, repos)
          toolSha = Sha256.of(Chunk.fromArray("compiler-tool".getBytes(StandardCharsets.UTF_8)))
          lock    = DependencyLock(Nil, Nil, List(compilerTool(request, toolSha)))
          _ <- write(repo, ".histos/dependencies.lock", DependencyLock.render(lock))
          classes = TestCompile.testClasses(repo, app.name, scala3, Jvm)
          _ <- ZIO.attempt {
            writeClass(classes, "demo/Foo", "one")
            writeClass(classes, "demo/Bar", "one")
            val bad    = repo.resolve(".histos").resolve("test-quick").resolve("app").resolve("3.10.0").resolve("jvm")
            val record = bad.resolve("demo").resolve("Bar")
            Files.createDirectories(record.getParent)
            Files.writeString(record, "nope\n")
          }
          machine <- Machine.load(repo)
          jdk     <- Toolchain.resolve(machine)
          parsed  <- ZIO.fromEither(DependencyLock.parse(DependencyLock.render(lock)))
          stamp   <- ZIO.fromEither(
            SuiteCover.stored(
              repo,
              SuiteAsk(
                app,
                List(app),
                scala3,
                Jvm,
                parsed,
                repos,
                graph.tools,
                machine.native,
                jdk.identity,
                None,
              ),
            )
          )
          digest <- SuiteStamp.digest("demo.Foo", List(classes), stamp)
          saved  <- digest match
            case Some(value) =>
              QuickRecord.save(repo, app.name, scala3, Jvm, SuiteName("demo.Foo"), value).as(true)
            case None => ZIO.succeed(false)
          before <- census(classes)
          plan   <- Plan.test(repo, graph, List(ProjectName("app")))
          text = Plan.renderTest(repo, plan)
          after <- census(classes)
          mainZip = SourceStale.directory(repo, app.name, scala3, Jvm, AnalysisKind.Main).resolve("inc.zip")
          testZip = SourceStale.directory(repo, app.name, scala3, Jvm, AnalysisKind.Test).resolve("inc.zip")
          _      <- ZIO.attempt(writeClass(classes, "demo/Foo", "two"))
          again  <- census(classes)
          missed <- Plan.test(repo, graph, List(ProjectName("app")))
          missedText = Plan.renderTest(repo, missed)
          left <- census(classes)
        yield assertTrue(
          saved,
          text.contains("quick skip demo.Foo"),
          !text.contains("quick run demo.Foo"),
          text.contains("quick: not known to be skippable demo.Bar"),
          !text.contains("quick run demo.Bar"),
          !text.contains("quick skip demo.Bar"),
          before == after,
          !Files.exists(mainZip),
          !Files.exists(testZip),
          missedText.contains("quick run demo.Foo"),
          missedText.contains("quick: not known to be skippable demo.Bar"),
          !missedText.contains("quick skip"),
          again == left,
        )
      }
    @@ TestAspect.withLiveSystem,
    test("quick preview without a lock does not call a stored digest a run"):
      ZIO.scoped {
        for
          repo <- keep("histos-preview-nolock")
          _    <- write(repo, "app/src/test/scala/AppSpec.scala", "object AppSpec")
          appMod = module("app", Jvm)(scala3)
          graph <- ZIO.fromEither(Projects(appMod))
          app   <- ZIO.fromEither(moduleNamed(graph, ProjectName("app")))
          classes = TestCompile.testClasses(repo, app.name, scala3, Jvm)
          _ <- ZIO.attempt(writeClass(classes, "demo/Foo", "one"))
          digest = Sha256.of(Chunk.fromArray("stored".getBytes(StandardCharsets.UTF_8)))
          _      <- QuickRecord.save(repo, app.name, scala3, Jvm, SuiteName("demo.Foo"), digest)
          before <- census(classes)
          plan   <- Plan.test(repo, graph, List(ProjectName("app")))
          text = Plan.renderTest(repo, plan)
          after <- census(classes)
          zip = SourceStale.directory(repo, app.name, scala3, Jvm, AnalysisKind.Test).resolve("inc.zip")
        yield assertTrue(
          text.contains("quick: not known to be skippable demo.Foo"),
          !text.contains("quick run"),
          before == after,
          !Files.exists(zip),
          !Files.exists(repo.resolve(".histos").resolve("dependencies.lock")),
        )
      },
  )

  private def compilerTool(
      request: histos.resolve.ToolRequest,
      sha: Sha256,
  ): LockedTool =
    LockedTool(
      request.id,
      request.roots,
      Repositories.ordered(request.repositories),
      request.roots.map(root => LockedArtifact(root.organization, root.name, root.revision, sha, Repository.Central)),
      Nil,
    )

  def spec = suite("preview plans")(plans, asked, answers)
end PlanSpec
