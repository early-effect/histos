package histos.publish

import histos.preview.{Plan, PreviewError}
import histos.version.ShipName
import histos.{Jvm, MainClass, Organization, ProjectName, Projects, SbtSeries, ScalaVersion, aggregate, module}

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.*

object PublishSelectSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")
  private val repo   = Path.of(".")

  private val selection = suite("publish selection")(
    test("a ship name publishes that ship"):
      for
        graph <- ZIO.fromEither(ships)
        plan  <- Plan.publishLocal(repo, graph, List("libs"), Map.empty)
      yield assertTrue(
        Plan.renderPublish(plan) ==
          """ivy2Local
            |core libs 0.21.2-ci selected
            |site libs 0.21.2-ci selected""".stripMargin
      )
    ,
    test("an aggregate publishes listed modules at their own ship versions"):
      for
        graph <- ZIO.fromEither(ships)
        plan  <- Plan.publishLocal(repo, graph, List("root"), Map.empty)
        text = Plan.renderPublish(plan)
      yield assertTrue(
        text ==
          """ivy2Local
            |'stray' is not in a ship
            |core libs 0.21.2-ci selected
            |adapter extra 0.0.1-ci selected
            |lib base 1.5.0-ci upstream""".stripMargin,
        !text.contains("site"),
      )
    ,
    test("a module outside a ship is an error"):
      for
        graph   <- ZIO.fromEither(ships)
        stray   <- ZIO.fromEither(ProjectName.from("stray"))
        refused <- Plan.publishLocal(repo, graph, List("libs", "stray"), Map.empty).either
      yield assertTrue(
        refused == Left(PreviewError.Publish(LocalPublishError.NotPublished(stray)))
      )
    ,
    test("an unknown token is not a ship, an aggregate, or a module"):
      for
        graph  <- ZIO.fromEither(ships)
        local  <- Plan.publishLocal(repo, graph, List("target/repo", "libs"), Map.empty).either
        remote <- Plan.publishSnapshots(repo, graph, List("target/repo"), Map.empty).either
      yield assertTrue(
        local == Left(PreviewError.Publish(LocalPublishError.Unknown("target/repo"))),
        remote == Left(PreviewError.Publish(LocalPublishError.Unknown("target/repo"))),
        LocalPublishError.Unknown("target/repo").message == "'target/repo' is not a ship, an aggregate, or a module",
      )
    ,
    test("a ship name wins when a module shares it"):
      for
        graph <- ZIO.fromEither(collision)
        plan  <- Plan.publishLocal(repo, graph, List("core"), Map.empty)
        text = Plan.renderPublish(plan)
      yield assertTrue(
        text ==
          """ivy2Local
            |adapter core 9.0.0-ci selected""".stripMargin,
        !text.contains("libs"),
        !text.contains("site"),
      )
    ,
    test("no name publishes every ship and skips a module outside one"):
      for
        graph <- ZIO.fromEither(ships)
        plan  <- Plan.publishLocal(repo, graph, Nil, Map.empty)
        text = Plan.renderPublish(plan)
      yield assertTrue(
        text ==
          """ivy2Local
            |core libs 0.21.2-ci selected
            |site libs 0.21.2-ci selected
            |adapter extra 0.0.1-ci selected
            |lib base 1.5.0-ci selected""".stripMargin,
        !text.contains("stray"),
      )
    ,
    test("a snapshot dependency refuses that ship and the rest continues"):
      ZIO.scoped:
        for
          root <- ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-select"))): path =>
            ZIO.attempt {
              Files.deleteIfExists(path.resolve("releases"))
              Files.deleteIfExists(path)
            }.ignore
          _     <- ZIO.attempt(Files.writeString(root.resolve("releases"), ""))
          graph <- ZIO.fromEither(snapshotDependency)
          plan  <- Plan.publishSnapshots(
            root,
            graph,
            List("libs", "tool"),
            Map(
              "HISTOS_SESSION" -> "snapshot",
              "HISTOS_SHA"     -> "1234567890abcdef1234567890abcdef12345678",
              "HISTOS_BRANCH"  -> "default",
            ),
          )
          text = Plan.renderPublish(plan)
        yield assertTrue(
        text ==
          s"""${histos.central.SnapshotRepository.central.root.render}
             |tool depends on snapshot lib, so it is not published
             |core libs 1.0.0-1234567890ab-SNAPSHOT selected
             |site libs 1.0.0-1234567890ab-SNAPSHOT selected
             |core libs 1.0.0-SNAPSHOT selected
             |site libs 1.0.0-SNAPSHOT selected""".stripMargin,
        !text.contains("\nlib "),
        !text.contains("\napp "),
      )
    ,
    test("a snapshot ship may depend on its own member"):
      for
        graph <- ZIO.fromEither(sameShip)
        plan  <- Plan.publishLocal(repo, graph, Nil, Map.empty)
      yield assertTrue(
        Plan.renderPublish(plan) ==
          """ivy2Local
            |api histos 0.0.1-ci selected
            |engine histos 0.0.1-ci selected""".stripMargin
      )
    ,
    test("an assembly may depend on a module that is not in a ship"):
      for graph <- ZIO.fromEither(packedAssembly)
      yield assertTrue(
        PublishSelect
          .pick(graph, Nil, List(ShipWrite(ShipName("tool"), List(ProjectName("core"), ProjectName("app")), false))) ==
          Right(
            Picked(
              Set(ProjectName("core"), ProjectName("app")),
              Set(ProjectName("core"), ProjectName("app")),
              Nil,
              Nil,
            )
          )
      )
    ,
    test("a library, an application, or a plugin refuses a module that is not in a ship"):
      for
        library     <- ZIO.fromEither(libraryOutside)
        application <- ZIO.fromEither(applicationOutside)
        plugin      <- ZIO.fromEither(pluginOutside)
      yield assertTrue(
        PublishSelect.pick(library, Nil, List(ShipWrite(ShipName("tool"), List(ProjectName("core")), false))) ==
          Left(LocalPublishError.UnpublishedDependency(ProjectName("core"), ProjectName("inside"))),
        PublishSelect.pick(application, Nil, List(ShipWrite(ShipName("tool"), List(ProjectName("app")), false))) ==
          Left(LocalPublishError.UnpublishedDependency(ProjectName("app"), ProjectName("inside"))),
        PublishSelect.pick(plugin, Nil, List(ShipWrite(ShipName("tool"), List(ProjectName("plug")), false))) ==
          Left(LocalPublishError.UnpublishedDependency(ProjectName("plug"), ProjectName("inside"))),
      )
    ,
    test("an assembly refuses a module owned by a ship this run did not select"):
      for graph <- ZIO.fromEither(ownedElsewhere)
      yield assertTrue(
        PublishSelect.pick(
          graph,
          List(PublishName.Ship(ShipName("tool"))),
          List(ShipWrite(ShipName("tool"), List(ProjectName("app")), false)),
        ) == Left(LocalPublishError.UnpublishedDependency(ProjectName("app"), ProjectName("core")))
      ),
  )

  private def ships: Either[histos.BuildError, histos.ProjectGraph] =
    val core    = module("core", Jvm)(scala3)
    val site    = module("site", Jvm)(scala3)
    val lib     = module("lib", Jvm)(scala3)
    val adapter = module("adapter", Jvm)(scala3).dependsOn(lib)
    val stray   = module("stray", Jvm)(scala3)
    val root    = aggregate("root", core, adapter, stray)
    Projects.organization(Organization("com.example"))(
      histos.ships(
        histos.ship("libs", histos.version.ShipNumber("0.21.2"))(core, site),
        histos.ship("extra", histos.version.ShipNumber("0.0.1"))(adapter),
        histos.ship("base", histos.version.ShipNumber("1.5.0"))(lib),
      ),
      core,
      site,
      lib,
      adapter,
      stray,
      root,
    )

  private def collision: Either[histos.BuildError, histos.ProjectGraph] =
    val core    = module("core", Jvm)(scala3)
    val site    = module("site", Jvm)(scala3)
    val adapter = module("adapter", Jvm)(scala3)
    Projects.organization(Organization("com.example"))(
      histos.ships(
        histos.ship("libs", histos.version.ShipNumber("0.21.2"))(core, site),
        histos.ship("core", histos.version.ShipNumber("9.0.0"))(adapter),
      ),
      core,
      site,
      adapter,
    )

  private def snapshotDependency: Either[histos.BuildError, histos.ProjectGraph] =
    val core = module("core", Jvm)(scala3)
    val site = module("site", Jvm)(scala3)
    val lib  = module("lib", Jvm)(scala3)
    val app  = module("app", Jvm)(scala3).dependsOn(lib)
    Projects.organization(Organization("com.example"))(
      histos.ships(
        histos.ship("libs", histos.version.ShipNumber("1.0.0"))(core, site),
        histos.ship("snaps", histos.version.ShipNumber("1.2.3"))(lib),
        histos.ship("tool", histos.version.ShipNumber("2.0.0"))(app),
      ),
      core,
      site,
      lib,
      app,
    )

  private def packedAssembly: Either[histos.BuildError, histos.ProjectGraph] =
    val inside = module("inside", Jvm)(scala3)
    val core   = module("core", Jvm)(scala3)
    val app    = module("app", Jvm)(scala3).assembly(MainClass("demo.App")).dependsOn(core, inside)
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.ship("tool", histos.version.ShipNumber("1.0.0"))(core, app)),
      core,
      app,
      inside,
    )

  private def libraryOutside: Either[histos.BuildError, histos.ProjectGraph] =
    val inside = module("inside", Jvm)(scala3)
    val core   = module("core", Jvm)(scala3).dependsOn(inside)
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.ship("tool", histos.version.ShipNumber("1.0.0"))(core)),
      core,
      inside,
    )

  private def applicationOutside: Either[histos.BuildError, histos.ProjectGraph] =
    val inside = module("inside", Jvm)(scala3)
    val app    = module("app", Jvm)(scala3).application.dependsOn(inside)
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.ship("tool", histos.version.ShipNumber("1.0.0"))(app)),
      app,
      inside,
    )

  private def pluginOutside: Either[histos.BuildError, histos.ProjectGraph] =
    val inside = module("inside", Jvm)(scala3)
    val plug   = module("plug", Jvm)(scala3).sbtPlugin(SbtSeries("2")).dependsOn(inside)
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.ship("tool", histos.version.ShipNumber("1.0.0"))(plug)),
      plug,
      inside,
    )

  private def ownedElsewhere: Either[histos.BuildError, histos.ProjectGraph] =
    val core = module("core", Jvm)(scala3)
    val app  = module("app", Jvm)(scala3).assembly(MainClass("demo.App")).dependsOn(core)
    Projects.organization(Organization("com.example"))(
      histos.ships(
        histos.ship("libs", histos.version.ShipNumber("1.0.0"))(core),
        histos.ship("tool", histos.version.ShipNumber("2.0.0"))(app),
      ),
      core,
      app,
    )

  private def sameShip: Either[histos.BuildError, histos.ProjectGraph] =
    val api    = module("api", Jvm)(scala3)
    val engine = module("engine", Jvm)(scala3).dependsOn(api)
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.ship("histos", histos.version.ShipNumber("0.0.1"))(api, engine)),
      api,
      engine,
    )

  def spec = selection
end PublishSelectSpec
