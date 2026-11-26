package histos.server

import histos.suite.HistosSpec

import zio.test.*

object BootNoticeSpec extends HistosSpec:
  def spec = suite("boot notices")(
    test("are the compiling lines appended after the mark, and nothing else") {
      val (first, rest)   = BootNotice.take("", "noise\ncompiling histos.scala\ncompiling plug")
      val (second, tail)  = BootNotice.take(rest, "ins.scala\nwarning\n")
      val (blank, held)   = BootNotice.take("", "compiling \ncompiling\nhistos.scala\ncompil")
      val (joined, empty) = BootNotice.take(held, "ing histos.plugins.scala\n")
      assertTrue(
        first == List("compiling histos.scala"),
        rest == "compiling plug",
        second == List("compiling plugins.scala"),
        tail.isEmpty,
        blank.isEmpty,
        held == "compil",
        joined == List("compiling histos.plugins.scala"),
        empty.isEmpty,
      )
    }
  )
