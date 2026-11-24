package histos.load

import histos.ScalacFlags

/** Shared build-file policy, independent of module-specific flags. */
private[histos] object BuildScalac:
  val bootstrapFlags: ScalacFlags = ScalacFlags.empty
