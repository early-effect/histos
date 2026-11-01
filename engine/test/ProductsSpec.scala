package histos.clean

import histos.select.SelectError
import histos.workspace.Workspace
import histos.{Jvm, ProjectName, ProjectNameError, Projects, ScalaVersion, aggregate, module}

import java.nio.file.{Files, Path}

import zio.{Task, ZIO}
import zio.test.*

object ProductsSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")

  private val products = suite("products")(
    test(
      "a full wipe deletes product roots and leaves locks, the jar, and the server"
    ):
      ZIO.scoped:
        for
          repo <- Workspace.work("histos-clean")
          _    <- Products.wipe(repo)
          kept = List(
            ".histos/dependencies.lock",
            ".histos/build.properties",
            ".histos/assembly.sha256",
            ".histos/toolchain.local",
            ".histos/node.local",
            ".histos/server.json",
            ".histos/server.sock",
            "dist/histos.jar",
          )
          dropped = List(
            ".histos/classes/engine/Demo.class",
            ".histos/analysis/engine/inc.zip",
            ".histos/test-classes/engine/Demo.class",
            ".histos/test-analysis/engine/inc.zip",
            ".histos/test-work/engine/work",
            ".histos/test-quick/engine/stamp",
            ".histos/jars/engine/app.jar",
            ".histos/run/engine/out",
            ".histos/provider/3.10.0/bridge.jar",
            ".histos/provider-src/3.10.0/Bridge.scala",
            ".histos/observation.json",
          )
          _       <- ZIO.foreachDiscard(kept ++ dropped)(relative => plant(repo, relative))
          outside <- plant(repo, "kept.txt")
          link = repo.resolve(".histos").resolve("classes").resolve("linked")
          _            <- ZIO.attempt(Files.createSymbolicLink(link, outside))
          _            <- Products.wipe(repo)
          stayed       <- ZIO.foreach(kept)(relative => isFile(repo.resolve(relative)))
          gone         <- ZIO.foreach(dropped)(relative => exists(repo.resolve(relative)))
          outsideStays <- isFile(outside)
          classesGone  <- exists(repo.resolve(".histos").resolve("classes"))
        yield assertTrue(
          Products.line(Nil) == "cleaned",
          stayed.forall(identity),
          gone.forall(!_),
          outsideStays,
          !classesGone,
        )
    ,
    test("a module wipe leaves its sibling and the shared provider"):
      ZIO.scoped:
        for
          repo          <- Workspace.work("histos-clean-module")
          engineClass   <- plant(repo, ".histos/classes/engine/Demo.class")
          appClass      <- plant(repo, ".histos/classes/app/Demo.class")
          provider      <- plant(repo, ".histos/provider/3.10.0/bridge.jar")
          tally         <- plant(repo, ".histos/observation.json")
          built         <- graph
          cleaned       <- Products.named(repo, built, List(ProjectName("engine")))
          engineGone    <- exists(engineClass)
          appStays      <- isFile(appClass)
          providerStays <- isFile(provider)
          tallyGone     <- exists(tally)
        yield assertTrue(
          cleaned.map(_.value) == List("engine"),
          Products.line(cleaned) == "cleaned engine",
          !engineGone,
          appStays,
          providerStays,
          !tallyGone,
          tally == Products.observation(repo),
        )
    ,
    test("an unknown, repeated, or bad name deletes nothing"):
      ZIO.scoped:
        for
          repo      <- Workspace.work("histos-clean-name")
          klass     <- plant(repo, ".histos/classes/engine/Demo.class")
          tally     <- plant(repo, ".histos/observation.json")
          built     <- graph
          unknown   <- Products.named(repo, built, List(ProjectName("missing"))).either
          duplicate <- Products
            .named(repo, built, List(ProjectName("engine"), ProjectName("engine")))
            .either
          still      <- isFile(klass)
          tallyStays <- isFile(tally)
          missing    <- ZIO.fromEither(ProjectName.from("missing"))
          engine     <- ZIO.fromEither(ProjectName.from("engine"))
          bad = ProjectName.from("a/b")
        yield assertTrue(
          unknown match
            case Left(SelectError.Unknown(name)) => name == missing
            case _                               => false
          ,
          duplicate match
            case Left(SelectError.Duplicate(name)) => name == engine
            case _                                 => false
          ,
          bad == Left(ProjectNameError.Bad("a/b")),
          still,
          tallyStays,
        )
    ,
    test("an aggregate wipe does not delete an upstream module"):
      ZIO.scoped:
        for
          repo      <- Workspace.work("histos-clean-aggregate")
          coreClass <- plant(repo, ".histos/classes/core/Demo.class")
          appClass  <- plant(repo, ".histos/classes/app/Demo.class")
          provider  <- plant(repo, ".histos/provider/3.10.0/bridge.jar")
          core = module("core", Jvm)(scala3)
          app  = module("app", Jvm)(scala3).dependsOn(core)
          root = aggregate("root", app)
          built         <- ZIO.fromEither(Projects(root, core, app))
          cleaned       <- Products.named(repo, built, List(ProjectName("root")))
          coreStays     <- isFile(coreClass)
          appGone       <- exists(appClass)
          providerStays <- isFile(provider)
        yield assertTrue(
          cleaned.map(_.value) == List("app"),
          Products.line(cleaned) == "cleaned app",
          coreStays,
          !appGone,
          providerStays,
        )
    ,
    test("naming nothing is the full wipe, including the shared provider"):
      ZIO.scoped:
        for
          repo         <- Workspace.work("histos-clean-all")
          provider     <- plant(repo, ".histos/provider/3.10.0/bridge.jar")
          klass        <- plant(repo, ".histos/classes/engine/Demo.class")
          built        <- graph
          cleaned      <- Products.named(repo, built, Nil)
          providerGone <- exists(provider)
          classGone    <- exists(klass)
        yield assertTrue(
          cleaned.isEmpty,
          Products.line(cleaned) == "cleaned",
          !providerGone,
          !classGone,
        ),
  )

  private def graph =
    val engine = module("engine", Jvm)(scala3)
    val app    = module("app", Jvm)(scala3).dependsOn(engine)
    ZIO.fromEither(Projects(engine, app))

  private def plant(root: Path, relative: String): Task[Path] =
    ZIO.attempt {
      val path = root.resolve(relative)
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, "x")
      path
    }

  private def exists(path: Path): Task[Boolean] =
    ZIO.attempt(Files.exists(path))

  private def isFile(path: Path): Task[Boolean] =
    ZIO.attempt(Files.isRegularFile(path))

  def spec = products
end ProductsSpec
