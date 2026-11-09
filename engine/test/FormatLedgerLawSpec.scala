package histos.format

import histos.jdk.Sha256
import histos.workspace.Workspace

import java.nio.file.Path

import zio.test.*

object FormatLedgerLawSpec extends ZIOSpecDefault:
  private val key   = FormatLedger.digest("jars and conf")
  private val other = FormatLedger.digest("another conf")

  private val file    = Gen.elements("a/src/A.scala", "b/test/B.scala", "histos.scala", "c/src/x/y/Long Name.scala")
  private val entries = Gen.mapOfBounded(0, 6)(file.map(Path.of(_)), Gen.alphaNumericString.map(FormatLedger.digest))

  def spec = suite("format ledger")(
    test("a ledger reads back what it rendered, and only under its own key"):
      check(entries): formatted =>
        val ledger = FormatLedger(key, formatted)
        assertTrue(
          FormatLedger.parse(key, ledger.render) == ledger,
          FormatLedger.parse(other, ledger.render) == FormatLedger(other, Map.empty),
        )
    ,
    test("a file is held only while its bytes hash to the recorded digest"):
      val path   = Path.of("a/src/A.scala")
      val ledger = FormatLedger(key, Map(path -> FormatLedger.digest("object A")))
      assertTrue(
        ledger.holds(path, FormatLedger.digest("object A")),
        !ledger.holds(path, FormatLedger.digest("object A ")),
        !ledger.holds(Path.of("a/src/B.scala"), FormatLedger.digest("object A")),
      )
    ,
    test("a run replaces the verdicts for the files it saw and keeps the rest"):
      val kept    = Path.of("kept/src/K.scala")
      val proved  = Path.of("seen/src/P.scala")
      val dropped = Path.of("seen/src/D.scala")
      val digest  = FormatLedger.digest("x")
      val before  = FormatLedger(key, Map(kept -> digest, proved -> digest, dropped -> digest))
      val fresh   = FormatLedger.digest("y")
      assertTrue(
        before.after(Set(proved, dropped), Map(proved -> fresh)) == FormatLedger(
          key,
          Map(kept -> digest, proved -> fresh),
        )
      )
    ,
    test("a bad row is dropped, and a missing ledger is empty"):
      val text =
        s"key ${key.value}\nnot-a-digest a/src/A.scala\n${FormatLedger.digest("ok").value} b/src/B.scala\nloose\n"
      Workspace
        .work("histos-format-ledger")
        .flatMap: repo =>
          for
            missing <- FormatLedger.read(repo, key)
            _       <- FormatLedger.write(repo, FormatLedger(key, Map(Path.of("x.scala") -> FormatLedger.digest("x"))))
            written <- FormatLedger.read(repo, key)
          yield assertTrue(
            FormatLedger.parse(key, text).formatted == Map(Path.of("b/src/B.scala") -> FormatLedger.digest("ok")),
            missing == FormatLedger(key, Map.empty),
            written.formatted.keySet == Set(Path.of("x.scala")),
          ),
  )
end FormatLedgerLawSpec
