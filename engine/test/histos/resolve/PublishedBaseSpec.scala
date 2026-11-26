package histos.resolve

import histos.*
import histos.suite.HistosSpec
import histos.version.ShipName

import zio.ZIO
import zio.test.*

object PublishedBaseSpec extends HistosSpec:
  private val version = ScalaVersion("3.10.1")
  private val ship    = ShipName("histos")

  def spec = suite("published base")(
    test("the member named like the ship stays bare and every other member is prefixed") {
      val opened    = Ship("histos")
      val parent    = jvm(opened.parent(Jvm)(version)).assembly(MainClass("histos.cli.Main"))
      val sibling   = module("wire", Jvm, Native)(version).clang(ClangMajor(21))
      val prefixed  = module("histos-splice", Jvm)(version)
      val lookalike = module("historical", Jvm)(version)
      val glued     = module("histoswire", Jvm)(version)
      val chosen    = module("central", Jvm)(version).artifact(artifact"chosen")
      val outside   = module("suite", Jvm)(version)
      val row       = opened.next(0, 6, 0)(parent, sibling, prefixed, lookalike, glued, chosen)
      for built <- ZIO.fromEither(
          Projects(ships(row), parent, sibling, prefixed, lookalike, glued, chosen, outside)
        )
      yield assertTrue(
        id(built, ProjectName("histos"), Platform.Jvm).contains("histos"),
        id(built, ProjectName("wire"), Platform.Jvm).contains("histos-wire_3"),
        id(built, ProjectName("wire"), Platform.Native).contains("histos-wire_native0.5_3"),
        id(built, ProjectName("histos-splice"), Platform.Jvm).contains("histos-splice_3"),
        id(built, ProjectName("historical"), Platform.Jvm).contains("histos-historical_3"),
        id(built, ProjectName("histoswire"), Platform.Jvm).contains("histos-histoswire_3"),
        id(built, ProjectName("central"), Platform.Jvm).contains("chosen_3"),
        id(built, ProjectName("suite"), Platform.Jvm).contains("suite_3"),
        ArtifactName.owning(built, ProjectName("histos")).contains(ship),
        ArtifactName.owning(built, ProjectName("wire")).contains(ship),
        ArtifactName.owning(built, ProjectName("suite")).isEmpty,
      )
    }
  )

  private def jvm[P](module: ModuleDef[P])(using Platform.Jvm.type <:< P): ModuleDef[P] = module

  private def id(graph: ProjectGraph, name: ProjectName, platform: Platform): Option[String] =
    graph
      .get(name)
      .collect:
        case module: Module =>
          ArtifactName.module(module, version, platform, ArtifactName.owning(graph, module.name))
end PublishedBaseSpec
