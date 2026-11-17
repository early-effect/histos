package histos.plugin

import zio.{Tag, ZIO, ZLayer}

/** Plugins installed by explicit activation, in registration order. The same values are also services, under their own
  * types.
  */
final case class Plugins(values: zio.Chunk[HistosPlugin])

/** `ZLayer.succeed`. The type argument is the service key. */
def plugin[P <: HistosPlugin: Tag](value: P): ZLayer[Any, Nothing, P] =
  ZLayer.succeed(value)

/** Pure construction. The first type argument is the service. The rest are the environment, so the lambda parameters
  * are already typed.
  */
def pluginWith[P <: HistosPlugin: Tag, A: Tag](f: A => P): ZLayer[A, Nothing, P] =
  ZLayer.fromFunction(f)

def pluginWith[P <: HistosPlugin: Tag, A: Tag, B: Tag](f: (A, B) => P): ZLayer[A & B, Nothing, P] =
  ZLayer.fromFunction(f)

def pluginWith[P <: HistosPlugin: Tag, A: Tag, B: Tag, C: Tag](
    f: (A, B, C) => P
): ZLayer[A & B & C, Nothing, P] =
  ZLayer.fromFunction(f)

/** Effectful construction. `Scope` is in the environment only when the effect asks for it. */
def pluginWithZIO[P <: HistosPlugin: Tag, R, E](effect: ZIO[R, E, P]): ZLayer[R, E, P] =
  ZLayer.fromZIO(effect)

def pluginWithZIO[P <: HistosPlugin: Tag, A: Tag, R, E](f: A => ZIO[R, E, P]): ZLayer[A & R, E, P] =
  ZLayer.fromZIO(ZIO.serviceWithZIO[A](f))

def pluginWithZIO[P <: HistosPlugin: Tag, A: Tag, B: Tag, R, E](
    f: (A, B) => ZIO[R, E, P]
): ZLayer[A & B & R, E, P] =
  ZLayer.fromZIO(ZIO.environmentWithZIO[A & B](env => f(env.get[A], env.get[B])))
