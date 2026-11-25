package histos.doctor

import histos.cli.Part
import histos.suite.HistosSpec
import histos.theme.Role

import zio.Chunk
import zio.test.*

object RenderSpec extends HistosSpec:
  private val noted =
    Noted.error("ship.pin", "the pin trails", "move the pin", Noted.command("histos bump"))

  def spec = suite("doctor lines")(
    test("an error is a mark and the check id stays plain") {
      val report = DoctorReport(List(Outcome.Found(noted)), Nil)
      val rows   = Render.lines("repo", report, false)
      val text   = Render.text("repo", report, false)
      val marked = rows.exists: line =>
        line.exists:
          case Part.Mark("error", Role.Error) => true
          case _                              => false
      val idPlain = rows.exists: line =>
        line.exists:
          case Part.Plain(span) => span.contains("ship.pin")
          case _                => false
      val tally = rows.collect:
        case Chunk(Part.Mark(_, role)) => role
      assertTrue(
        marked,
        idPlain,
        text == Part.join(Chunk.fromIterable(rows)),
        !text.contains("\u001b"),
        !Render.json(report).contains("\u001b"),
        tally == List(Role.TallyBad),
      )
    },
    test("info has no role and stays off the default log") {
      val info = Noted(
        CheckId("ship.note"),
        Level.Info,
        "a note",
        "a note",
        "nothing to change",
        ::(Noted.command("true"), Nil),
      )
      val report   = DoctorReport(List(Outcome.Found(info)), Nil)
      val quiet    = Render.lines("repo", report, false)
      val verbose  = Render.lines("repo", report, true)
      val infoLine = verbose.find: line =>
        line.exists:
          case Part.Plain(span) => span.contains("ship.note")
          case _                => false
      val unmarked = infoLine match
        case Some(line) =>
          line.forall:
            case Part.Plain(_)   => true
            case Part.Mark(_, _) => false
        case None => false
      val named = quiet.exists: line =>
        line.exists:
          case Part.Plain(span) => span.contains("ship.note")
          case _                => false
      assertTrue(!named, unmarked)
    },
    test("a timed out check is a failed tally") {
      val report = DoctorReport(Nil, List(CheckId("ship.late")))
      val rows   = Render.lines("repo", report, false)
      val late   = rows.exists: line =>
        line.exists:
          case Part.Mark("timed out", Role.Failed) => true
          case _                                   => false
      val tally = rows.collect:
        case Chunk(Part.Mark(_, role)) => role
      assertTrue(late, tally == List(Role.TallyBad))
    },
  )
end RenderSpec
