package histos.cli

import histos.client.Batch

import zio.json.*
import zio.test.*
import zio.ZIO

object BatchCallSpec extends ZIOSpecDefault:
  private val samples = List(
    List("compile"),
    List("compile", "core"),
    List("compile", "core", "web"),
    List("test"),
    List("test", "core"),
    List("run", "core"),
    List("run", "core", "jvm"),
    List("run", "inspect"),
    List("run", "versions"),
    List("clean"),
    List("clean", "core"),
    List("cleanFull"),
  )

  def spec =
    suite("batch calls")(
      test("the native argv and the route encode the same call") {
        ZIO.foreach(samples)(same).map(rows => assertTrue(rows.forall(identity)))
      }
    )

  private def same(args: List[String]) =
    for
      command <- Grammar.parse(args)
      routed  <- ZIO.fromOption(Cli.callOf(command))
      parsed  <- ZIO.fromOption(Batch.parse(args))
    yield parsed.toJson == routed.toJson
end BatchCallSpec
