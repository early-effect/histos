package histos.step

/** Evidence that a step of kind `Have` accepts an aspect that requires `Need`. */
@scala.annotation.implicitNotFound("this aspect decorates ${Need} steps, and the step is ${Have}")
final class Fits[-Have, +Need] private (val proof: Have <:< Need)

object Fits:
  given [Have <: Need, Need]: Fits[Have, Need] = new Fits(summon[Have <:< Need])
