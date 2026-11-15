package histos.process

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
