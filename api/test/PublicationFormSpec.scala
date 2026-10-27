package histos

import zio.test.*

object PublicationFormSpec extends ZIOSpecDefault:
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
      ),
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
end PublicationFormSpec
