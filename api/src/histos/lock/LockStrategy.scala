package histos.lock

import zio.IO

/** Chooses a disposition. It does not write the lock or adopt a generation. */
trait LockStrategy:
  def decide(context: LockContext): IO[LockStrategyError, LockDecision]
