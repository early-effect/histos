package histos.zinc

import java.nio.file.Path

import scala.collection.mutable

import zio.test.*

object CellPulseSpec extends ZIOSpecDefault:
  private val checkout = Path.of("/repo")

  def spec = suite("a compiling cell")(
    test("a phase change is one pulse and typechecking names each new file") {
      val seen  = mutable.ListBuffer.empty[CompilePulse]
      val pulse = CellPulse(checkout, seen.addOne)
      pulse.start("parser", "/repo/src/A.scala")
      pulse.start("parser", "/repo/src/B.scala")
      pulse.start("typer[indexing]", "/repo/src/A.scala")
      pulse.start("typer[indexing]", "/repo/src/B.scala")
      pulse.start("typer[typechecking]", "/repo/src/A.scala")
      pulse.start("typer[typechecking]", "/repo/src/B.scala")
      pulse.start("typer[typechecking]", "/repo/src/B.scala")
      assertTrue(
        seen.toList == List(
          CompilePulse.Phase("parser", Some("src/A.scala")),
          CompilePulse.Phase("typer[indexing]", Some("src/A.scala")),
          CompilePulse.Phase("typer[typechecking]", Some("src/A.scala")),
          CompilePulse.Source("typer[typechecking]", "src/B.scala"),
        )
      )
    },
    test("an empty java path is a phase with no file and is not repeated") {
      val seen  = mutable.ListBuffer.empty[CompilePulse]
      val pulse = CellPulse(checkout, seen.addOne)
      pulse.start("Java compilation", "")
      pulse.start("Java compilation", "  ")
      pulse.start("Java analysis", "")
      assertTrue(
        seen.toList == List(
          CompilePulse.Phase("Java compilation", None),
          CompilePulse.Phase("Java analysis", None),
        )
      )
    },
    test("the typer phase names each new file") {
      val seen  = mutable.ListBuffer.empty[CompilePulse]
      val pulse = CellPulse(checkout, seen.addOne)
      pulse.start("typer", "/repo/src/A.scala")
      pulse.start("typer", "/repo/src/B.scala")
      assertTrue(
        seen.toList == List(
          CompilePulse.Phase("typer", Some("src/A.scala")),
          CompilePulse.Source("typer", "src/B.scala"),
        )
      )
    },
  )
end CellPulseSpec
