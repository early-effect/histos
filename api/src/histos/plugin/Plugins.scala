package histos.plugin

import zio.{Tag, ZIO, ZLayer}

/** Every plugin the wiring built, in registration order. The same values are also services, under their own types.
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

/** One plugin, or several. Each type's companion `layer` is used. An object with no `layer` is `ZLayer.succeed`. The
  * result provides those services and [[Plugins]].
  */
transparent inline def wire[A <: HistosPlugin]: ZLayer[Nothing, Any, Plugins] =
  ${ BundleMacro.wire1[A] }

transparent inline def wire[A <: HistosPlugin, B <: HistosPlugin]: ZLayer[Nothing, Any, Plugins] =
  ${ BundleMacro.wire2[A, B] }

transparent inline def wire[A <: HistosPlugin, B <: HistosPlugin, C <: HistosPlugin]: ZLayer[Nothing, Any, Plugins] =
  ${ BundleMacro.wire3[A, B, C] }

transparent inline def wire[A <: HistosPlugin, B <: HistosPlugin, C <: HistosPlugin, D <: HistosPlugin]
    : ZLayer[Nothing, Any, Plugins] =
  ${ BundleMacro.wire4[A, B, C, D] }

/** Marker that an object is a plugin bundle. [[layer]] builds the `ZLayer`. */
trait Bundle[A]

object Bundle:
  /** The object must be a module. The vals are checked when [[layer]] is called, after the object exists. */
  inline def derived[A]: Bundle[A] = ${ BundleMacro.derived[A] }

extension [A](self: A)(using Bundle[A])
  /** Provides each plugin val and [[Plugins]]. A val with a `layer` member uses it. A plugin object uses
    * `ZLayer.succeed`.
    */
  transparent inline def layer: ZLayer[Nothing, Any, Plugins] =
    ${ BundleMacro.bundle[A]('self) }
