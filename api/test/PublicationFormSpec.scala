package histos

import zio.test.*

object PublicationFormSpec extends histos.SuiteClock:
  private val scala3 = ScalaVersion("3.10.0")

  def spec = suite("publication form")(
    test("unspecified is a library, and each form is one publication"):
      val library = module("lib", Jvm)(scala3)
      val plugin  = module("plug", Jvm)(scala3).sbtPlugin(sbt"2")
      val app     = module("app", Jvm)(scala3).assembly(MainClass("demo.App"))
      val thin    = module("thin", Jvm)(scala3).application
      val named   = module("named", Jvm)(scala3).library
      assertTrue(
        form(library) == Right(List(Publication.Library)),
        form(plugin) == Right(List(Publication.SbtPlugin(SbtSeries("2")))),
        form(app) == Right(List(Publication.Assembly(MainClass("demo.App")))),
        form(thin) == Right(List(Publication.Application)),
        form(named) == Right(List(Publication.Library)),
        SbtSeries.from("2").map(_.value) == Right("2"),
        SbtSeries.from("1.0").map(_.value) == Right("1.0"),
        SbtSeries.from("2.0.0-RC1") == Left(SbtSeriesError.Bad("2.0.0-RC1")),
        SbtSeriesError.Bad("2.0.0-RC1").message == "'2.0.0-RC1' is not an sbt series",
      )
    ,
    test("two publications are refused at load"):
      val both = module("app", Jvm)(scala3).assembly(MainClass("demo.App")).application
      val same = module("plug", Jvm)(scala3).sbtPlugin(sbt"2").sbtPlugin(sbt"2")
      assertTrue(
        refused(both, "app"),
        refused(same, "plug"),
      )
    ,
    test("an assembly is one jvm jar"):
      val js    = module("plug", Js)(scala3).assembly(MainClass("demo.App"))
      val cross = module("app", Jvm)(scala3, ScalaVersion("3.9.0")).assembly(MainClass("demo.App"))
      assertTrue(
        shape(js, "plug"),
        shape(cross, "app"),
      )
    ,
    test("an assembly names one asset, and a library names none"):
      val library = module("lib", Jvm)(scala3)
      val named   = module("lib", Jvm)(scala3).library
      val program = module("app", Jvm)(scala3).assembly(MainClass("demo.App")).asset(asset"dist/histos.jar")
      val swapped = module("app", Jvm)(scala3).asset(asset"dist/histos.jar").assembly(MainClass("demo.App"))
      val ship    = Projects(ships(manual("libs", manual"1.0.0")(library)), library)
      assertTrue(
        assets(library) == Right(List(None)),
        assets(named) == Right(List(None)),
        assets(program) == Right(List(Some("dist/histos.jar"))),
        assets(swapped) == Right(List(Some("dist/histos.jar"))),
        form(program) == Right(List(Publication.Assembly(MainClass("demo.App")))),
        form(swapped) == Right(List(Publication.Assembly(MainClass("demo.App")))),
        ship.map(graph => assetsOf(graph)) == Right(List(None)),
      )
    ,
    test("an asset on a module that is not an assembly is refused"):
      val bare    = module("lib", Jvm)(scala3).asset(asset"dist/histos.jar")
      val library = module("lib", Jvm)(scala3).library.asset(asset"dist/histos.jar")
      val thin    = module("thin", Jvm)(scala3).application.asset(asset"dist/app.jar")
      val plugin  = module("plug", Jvm)(scala3).sbtPlugin(sbt"2").asset(asset"dist/plug.jar")
      assertTrue(
        without(bare, "lib"),
        without(library, "lib"),
        without(thin, "thin"),
        without(plugin, "plug"),
      )
    ,
    test("two assets are refused"):
      val both = module("app", Jvm)(scala3)
        .assembly(MainClass("demo.App"))
        .asset(asset"dist/histos.jar")
        .asset(asset"dist/other.jar")
      assertTrue(two(both, "app")),
  )

  private def form(defn: ProjectDef): Either[BuildError, List[Publication]] =
    Projects(defn).map: graph =>
      graph.projects.collect:
        case module: Module => module.publication

  private def refused(defn: ProjectDef, name: String): Boolean =
    Projects(defn) match
      case Left(error: BuildError.TwoPublications) =>
        error.name.value == name &&
        error.message == s"'$name' publishes two artifacts (${error.position.line}:${error.position.column})"
      case _ => false

  private def shape(defn: ProjectDef, name: String): Boolean =
    Projects(defn) match
      case Left(error: BuildError.AssemblyShape) =>
        error.name.value == name &&
        error.message ==
          s"'$name' publishes an assembly, which is one jvm jar (${error.position.line}:${error.position.column})"
      case _ => false

  private def assets(defn: ProjectDef): Either[BuildError, List[Option[String]]] =
    Projects(defn).map(assetsOf)

  private def assetsOf(graph: ProjectGraph): List[Option[String]] =
    graph.projects.collect:
      case module: Module => module.asset.map(_.value)

  private def without(defn: ProjectDef, name: String): Boolean =
    Projects(defn) match
      case Left(error: BuildError.AssetWithoutAssembly) =>
        error.name.value == name &&
        error.message ==
          s"'$name' names an asset and does not publish an assembly (${error.position.line}:${error.position.column})"
      case _ => false

  private def two(defn: ProjectDef, name: String): Boolean =
    Projects(defn) match
      case Left(error: BuildError.TwoAssets) =>
        error.name.value == name &&
        error.message ==
          s"'$name' names two assets (${error.position.line}:${error.position.column})"
      case _ => false
end PublicationFormSpec
