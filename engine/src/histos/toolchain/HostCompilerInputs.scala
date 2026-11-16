package histos.toolchain

import zio.{Scope, ZIO}

/** Reads the checked host distribution. It does not solve, and it has no project lock. */
trait HostCompilerInputs:
  def materialize: ZIO[Scope, HostCompilerError, HostCompilerSession]
