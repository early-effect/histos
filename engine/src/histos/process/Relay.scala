package histos.process

import histos.server.Attendance

import zio.{Chunk, UIO}

/** One thing the owning client asked of a running program. */
enum RelayCommand:
  case Stdin(bytes: Chunk[Byte])
  case End
  case Interrupt

/** `Closed` leaves the child's stdin open and empty. `Open` delivers frames until the source ends. */
enum RunRelay:
  case Closed
  case Open(take: UIO[Option[RelayCommand]])

/** Whether this process's standard input belongs to a program it launches.
  *
  * A detached caller, such as MCP, keeps its own stdin. The call can still be a program.
  */
enum Terminal:
  case Owned
  case Detached

/** `Live` copies this process's standard input. `Closed` leaves it alone. */
enum StdinFeed:
  case Live
  case Closed

object StdinFeed:
  def of(terminal: Terminal, attendance: Attendance): StdinFeed =
    (terminal, attendance) match
      case (Terminal.Owned, Attendance.Program) =>
        Live
      case (Terminal.Owned, Attendance.Held | Attendance.Quiet) =>
        Closed
      case (Terminal.Detached, Attendance.Held | Attendance.Program | Attendance.Quiet) =>
        Closed
