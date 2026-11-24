package histos.process

import zio.{UIO, ULayer, ZIO, ZLayer}

/** What remains after the leader has been reaped. */
private[process] enum Remainder:
  case Clear
  case Held

/** The two facts a finished leader leaves: whether its group still has a member, and whether its pipes have closed. The
  * live layer asks the operating system. A test provides both facts.
  */
private[process] trait Attendance:
  def remainder: UIO[Remainder]
  def closed: UIO[Unit]

private[process] object Attendance:
  /** A member still in the group is an orphan. An empty group waits until the pipes close. */
  def settle(code: ProcessExit): ZIO[Attendance, ChildError, ChildEvent] =
    ZIO.serviceWithZIO[Attendance]: attendance =>
      attendance.remainder.flatMap:
        case Remainder.Held  => ZIO.fail(ChildError.Orphan(code.value))
        case Remainder.Clear => attendance.closed.as(ChildEvent.Exited(code))

  def live(process: Process, closed: UIO[Unit]): ULayer[Attendance] =
    ZLayer.succeed[Attendance](Live(process, closed))

  def scripted(remainder: Remainder, closed: UIO[Unit]): ULayer[Attendance] =
    ZLayer.succeed[Attendance](Scripted(remainder, closed))

  val held: ULayer[Attendance] =
    scripted(Remainder.Held, ZIO.never)

  val clear: ULayer[Attendance] =
    scripted(Remainder.Clear, ZIO.unit)
end Attendance

private final class Live(process: Process, done: UIO[Unit]) extends Attendance:
  /** Signal 0 does not deliver a signal. The leader is already reaped, so a hit is a descendant. */
  def remainder: UIO[Remainder] =
    ZIO
      .attempt(process.pid())
      .orElseSucceed(-1L)
      .map: pid =>
        if pid > 0 && pid <= Int.MaxValue && Posix.occupied(pid.toInt) then Remainder.Held
        else Remainder.Clear

  def closed: UIO[Unit] = done
end Live

private final class Scripted(fixed: Remainder, done: UIO[Unit]) extends Attendance:
  def remainder: UIO[Remainder] = ZIO.succeed(fixed)
  def closed: UIO[Unit]         = done
