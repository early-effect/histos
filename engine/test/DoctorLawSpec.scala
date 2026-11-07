package histos.doctor

import slayer.*

import zio.*
import zio.test.*

object DoctorLawSpec extends ZIOSpecDefault:
  private val mark = CheckId.trusted("demo.mark")

  def spec = suite("doctor laws")(
    test("exit is 1 exactly when an error or a timeout is present"):
      val levels = List(Level.Error, Level.Warning, Level.Info)
      assertTrue(
        levels.forall { level =>
          val failed = level == Level.Error
          report(level, timedOut = false).exit == (if failed then 1 else 0) &&
          report(level, timedOut = true).exit == 1
        },
        DoctorReport(List(Outcome.Ok(mark, "ok")), Nil).exit == 0,
      )
    ,
    test("a missing probe skips the check and does not ask it to run"):
      val program =
        for
          _      <- stub[DoctorCheck](_.id)(mark)
          _      <- stub[DoctorCheck](_.area)(Area.Toolchain)
          _      <- stub[DoctorCheck](_.needs)(Set(Probe.GitHubRead))
          _      <- stub[DoctorCheck](_.run(slayer.any[DoctorContext]))(ZIO.die(new IllegalStateException("ran")))
          check  <- ZIO.service[DoctorCheck]
          report <- Runner.run(List(check), DoctorContext(Nil, Set.empty), 1.second)
        yield assertTrue(report.exit == 0, report.skipped == 1, report.timedOut.isEmpty)
      program.provideSomeLayer[TestEnvironment](stubbed[DoctorCheck])
    ,
    test("a check that does not finish is a timeout"):
      val late    = CheckId.trusted("demo.late")
      val program =
        for
          _      <- stub[DoctorCheck](_.id)(late)
          _      <- stub[DoctorCheck](_.area)(Area.Toolchain)
          _      <- stub[DoctorCheck](_.needs)(Set.empty[Probe])
          _      <- stub[DoctorCheck](_.run(slayer.any[DoctorContext]))(ZIO.sleep(1.hour).as(Nil))
          check  <- ZIO.service[DoctorCheck]
          fiber  <- Runner.run(List(check), DoctorContext(Nil, Set.empty), 1.second).fork
          _      <- TestClock.adjust(1.second)
          report <- fiber.join
        yield assertTrue(report.exit == 1, report.timedOut == List(late), report.errors == 0)
      program.provideSomeLayer[TestEnvironment](stubbed[DoctorCheck]),
  )

  private def report(level: Level, timedOut: Boolean): DoctorReport =
    DoctorReport(
      List(Outcome.Found(Note(mark, level))),
      if timedOut then List(CheckId.trusted("demo.late")) else Nil,
    )

  private final case class Note(check: CheckId, level: Level) extends Diagnostic:
    def title: String   = "note"
    def summary: String = "note"
    def explain: String = "note"
    def fixes: ::[Fix]  = ::(Fix.Command(ShellLine.trusted("true")), Nil)
end DoctorLawSpec
