package histos.plugin

import zio.{Scope, ZIO}

/** Implemented by the generated wiring. `Failure` is the layer's error. The engine maps it after the load. */
trait WiringBuilt:
  type Failure
  def build: ZIO[Scope, Failure, Plugins]
