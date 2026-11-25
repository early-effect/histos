package histos.zinc

import histos.suite.HistosSpec

import zio.test.*

object UpstreamApisSpec extends HistosSpec:
  private val lib    = ClassSignature(BinaryName("Lib"), ApiHash(1))
  private val module = ClassSignature(BinaryName("Lib$"), ApiHash(1))
  private val idle   = ClassSignature(BinaryName("Idle"), ApiHash(7))

  private def changed(hash: Int): List[ClassSignature] =
    List(lib.copy(hash = ApiHash(hash)), module.copy(hash = ApiHash(hash)), idle)

  private def seen(names: BinaryName*): BinaryName => Boolean =
    val have = names.toSet
    have.contains

  def spec = suite("an upstream signature")(
    test("an unchanged table is the same compile") {
      val table = List(lib, module, idle)
      val found = UpstreamApis.decide(Some(table), table, seen(BinaryName("Lib")), table.map(_.name).toSet)
      assertTrue(found == UpstreamChange.Same)
    },
    test("a signature change is precise when the analysis recorded the source name") {
      val now   = changed(2)
      val found =
        UpstreamApis.decide(Some(List(lib, module, idle)), now, seen(BinaryName("Lib")), now.map(_.name).toSet)
      assertTrue(found == UpstreamChange.Precise)
    },
    test("a signature recorded as the module class is precise") {
      val now   = changed(2)
      val found =
        UpstreamApis.decide(Some(List(lib, module, idle)), now, seen(BinaryName("Lib$")), now.map(_.name).toSet)
      assertTrue(found == UpstreamChange.Precise)
    },
    test("a signature change the analysis never recorded recompiles the cell") {
      val now   = changed(2).take(2)
      val found = UpstreamApis.decide(Some(List(lib, module)), now, seen(), now.map(_.name).toSet)
      assertTrue(found == UpstreamChange.Unproven)
    },
    test("a cell with no stored table recompiles") {
      val found = UpstreamApis.decide(None, List(lib), seen(BinaryName("Lib")), Set(BinaryName("Lib")))
      assertTrue(found == UpstreamChange.Unproven)
    },
  )
end UpstreamApisSpec
