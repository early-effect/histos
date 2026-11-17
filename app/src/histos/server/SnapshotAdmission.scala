package histos.server

import histos.lock.{LockBlocked, LockMode, LockRefusal, NecessaryScope}
import histos.resolve.ResolutionSnapshot

import zio.{IO, UIO, ZIO}

/** Frozen admits only a committed snapshot. An ephemeral snapshot does not solve for that caller. */
object SnapshotAdmission:
  def use[A](
      snapshot: ResolutionSnapshot,
      mode: LockMode,
      scope: NecessaryScope,
      solve: UIO[A],
  ): IO[LockBlocked, A] =
    if ResolutionSnapshot.admits(snapshot, mode) then solve
    else ZIO.fail(LockBlocked(LockRefusal.FrozenDrift, scope))
