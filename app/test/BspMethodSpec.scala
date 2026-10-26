package histos.bsp

import zio.test.*

object BspMethodSpec extends ZIOSpecDefault:
  def spec = suite("bsp method")(
    test("each method has one wire, and an unknown wire is not a method"):
      val wires = Bsp.Method.all.map(_.wire)
      assertTrue(
        wires.distinct == wires,
        wires.length == Bsp.Method.all.length,
        Bsp.Method.from("build/exit").contains(Bsp.Method.Exit),
        Bsp.Method.from("nope").isEmpty,
        Bsp.Method.all.forall(method => Bsp.Method.from(method.wire).contains(method)),
      )
  )
