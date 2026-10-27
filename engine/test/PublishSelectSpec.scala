package histos.publish

import histos.preview.{Plan, PreviewError}
import histos.version.ManualVersion
import histos.{Jvm, Organization, ProjectName, Projects, ScalaVersion, aggregate, module}

import java.nio.file.Path

import zio.ZIO
import zio.test.*

object PublishSelectSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")
  private val repo   = Path.of(".")
  private val home   = ZIO.succeed(repo)

  private val selection = suite("publish selection")(
    test("a ship name publishes that ship"):
      for
        graph <- ZIO.fromEither(ships)
        plan  <- Plan.publishLocal(repo, graph, List("libs"), Map.empty, Some("target/repo"), home)
      yield assertTrue(
        Plan.renderPublish(plan) ==
          """target/repo
            |core libs 0.21.2 selected
            |site libs 0.21.2 selected""".stripMargin
      )
    ,
    test("an aggregate publishes listed modules at their own ship versions"):
      for
        graph <- ZIO.fromEither(ships)
        plan  <- Plan.publishLocal(repo, graph, List("root"), Map.empty, Some("target/repo"), home)
        text = Plan.renderPublish(plan)
      yield assertTrue(
        text ==
          """target/repo
            |'stray' is not in a ship
            |core libs 0.21.2 selected
            |adapter extra 0.0.1 selected
            |lib base 1.5.0 upstream""".stripMargin,
        !text.contains("site"),
      )
    ,
    test("a module outside a ship is an error"):
      for
        graph   <- ZIO.fromEither(ships)
        stray   <- ZIO.fromEither(ProjectName.from("stray"))
        refused <- Plan.publishLocal(repo, graph, List("libs", "stray"), Map.empty, Some("target/repo"), home).either
      yield assertTrue(
        refused == Left(PreviewError.Publish(LocalPublishError.NotPublished(stray)))
      )
    ,
    test("a directory token is legal only on publishLocal"):
      for
        graph  <- ZIO.fromEither(ships)
        local  <- Plan.publishLocal(repo, graph, List("target/repo", "libs"), Map.empty, None, home)
        remote <- Plan.publishSnapshots(repo, graph, List("target/repo"), Map.empty).either
        twice  <- ZIO.succeed(PublishSelect.resolve(graph, List("target/repo", "out"), None, allowDirectory = true))
        both   <- ZIO.succeed(PublishSelect.resolve(graph, List("target/repo"), Some("out"), allowDirectory = true))
      yield assertTrue(
        Plan.renderPublish(local).startsWith("target/repo\ncore libs 0.21.2 selected"),
        remote == Left(PreviewError.Publish(LocalPublishError.Directory)),
        LocalPublishError.Directory.message == "a directory belongs on publishLocal",
        twice == Left(LocalPublishError.TwoDirectories),
        both == Left(LocalPublishError.TwoDirectories),
        LocalPublishError.TwoDirectories.message == "publishLocal takes one directory",
      )
    ,
    test("a ship name wins when a module shares it"):
      for
        graph <- ZIO.fromEither(collision)
        plan  <- Plan.publishLocal(repo, graph, List("core"), Map.empty, Some("target/repo"), home)
        text = Plan.renderPublish(plan)
      yield assertTrue(
        text ==
          """target/repo
            |adapter core 9.0.0 selected""".stripMargin,
        !text.contains("libs"),
        !text.contains("site"),
      )
    ,
    test("no name publishes every ship and skips a module outside one"):
      for
        graph <- ZIO.fromEither(ships)
        plan  <- Plan.publishLocal(repo, graph, Nil, Map.empty, Some("target/repo"), home)
        text = Plan.renderPublish(plan)
      yield assertTrue(
        text ==
          """target/repo
            |core libs 0.21.2 selected
            |site libs 0.21.2 selected
            |adapter extra 0.0.1 selected
            |lib base 1.5.0 selected""".stripMargin,
        !text.contains("stray"),
      )
    ,
    test("a snapshot dependency refuses that ship and the rest continues"):
      for
        graph <- ZIO.fromEither(snapshotDependency)
        plan  <- Plan.publishSnapshots(repo, graph, List("libs", "tool"), Map.empty)
        text = Plan.renderPublish(plan)
      yield assertTrue(
        text ==
          s"""${histos.central.SnapshotRepository.central.root.render}
             |tool depends on snapshot lib, so it is not published
             |core libs 1.0.0 selected
             |site libs 1.0.0 selected""".stripMargin,
        !text.contains("\nlib "),
        !text.contains("\napp "),
      )
    ,
    test("a snapshot ship may depend on its own member"):
      for
        graph <- ZIO.fromEither(sameShip)
        plan  <- Plan.publishLocal(repo, graph, Nil, Map.empty, Some("target/repo"), home)
      yield assertTrue(
        Plan.renderPublish(plan) ==
          """target/repo
            |api histos 0.0.1-SNAPSHOT selected
            |engine histos 0.0.1-SNAPSHOT selected""".stripMargin
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
        histos.manual("libs", ManualVersion("0.21.2"))(core, site),
        histos.manual("extra", ManualVersion("0.0.1"))(adapter),
        histos.manual("base", ManualVersion("1.5.0"))(lib),
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
        histos.manual("libs", ManualVersion("0.21.2"))(core, site),
        histos.manual("core", ManualVersion("9.0.0"))(adapter),
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
        histos.manual("libs", ManualVersion("1.0.0"))(core, site),
        histos.manual("snaps", ManualVersion("1.2.3-SNAPSHOT"))(lib),
        histos.manual("tool", ManualVersion("2.0.0"))(app),
      ),
      core,
      site,
      lib,
      app,
    )

  private def sameShip: Either[histos.BuildError, histos.ProjectGraph] =
    val api    = module("api", Jvm)(scala3)
    val engine = module("engine", Jvm)(scala3).dependsOn(api)
    Projects.organization(Organization("com.example"))(
      histos.ships(histos.manual("histos", ManualVersion("0.0.1-SNAPSHOT"))(api, engine)),
      api,
      engine,
    )

  def spec = selection
end PublishSelectSpec
