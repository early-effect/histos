package histos

import histos.plugin.SecretName
import histos.supply.{PrAuthor, SupplyChain, UpdatePolicy}

import zio.test.*

object GraphLawSpec extends ZIOSpecDefault:
  private val scala3  = scala"3.10.0"
  private val id      = SecretName("HISTOS_APP_ID")
  private val key     = SecretName("HISTOS_APP_PRIVATE_KEY")
  private val authors = List(
    PrAuthor.Actions,
    PrAuthor.App(id, key),
    PrAuthor.OwnerOpens(id, key),
    PrAuthor.Pat(SecretName("HISTOS_TOKEN")),
  )

  def spec = suite("graph laws")(
    test("doc check keeps the supply the build named"):
      val core                                                             = module("core", Jvm)(scala3)
      def kept(author: PrAuthor): Either[BuildError, (DocCheck, PrAuthor)] =
        val policy = SupplyChain(updates = UpdatePolicy(author = author))
        Projects
          .organization(organization"rocks.earlyeffect")
          .supplyChain(policy)
          .docCheck(core)
          .map(graph => (graph.docCheck, graph.supply.updates.author))
      val misses = authors
        .map(author => author -> kept(author))
        .filterNot((author, found) => found == Right((DocCheck.On, author)))
      assertTrue(misses == Nil)
    ,
    test("a repeated name, a self edge, and a cycle are refused"):
      val core = module("core", Jvm)(scala3)
      val a    = module("a", Jvm)(scala3)
      val b    = module("b", Jvm)(scala3)
      assertTrue(
        kind(Projects(core, module("core", Jvm)(scala3))) == "duplicate",
        kind(Projects(core.dependsOn(core))) == "self",
        kind(Projects(a.dependsOn(b), b.dependsOn(a))) == "cycle",
      ),
  )

  private def kind(built: Either[BuildError, ProjectGraph]): String =
    built match
      case Left(_: BuildError.DuplicateName)  => "duplicate"
      case Left(_: BuildError.SelfDependency) => "self"
      case Left(_: BuildError.Cycle)          => "cycle"
      case Left(other)                        => other.productPrefix
      case Right(_)                           => "graph"
end GraphLawSpec
