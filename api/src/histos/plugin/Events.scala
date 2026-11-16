package histos.plugin

import histos.doctor.Noted

import zio.{IO, ZIO}

/** A generation event has no caller and no invocation id. */
enum GenerationPluginEvent:
  case Diagnosis(noted: Noted)
  case Phase(key: ServiceKey)

/** One invocation's event. It cannot be written onto the generation channel. */
enum InvocationPluginEvent:
  case Diagnosis(noted: Noted)
  case Message(text: PluginDetail)

enum PluginEventError:
  case Closed

  def message: String = this match
    case Closed => "the plugin event handle is closed"
end PluginEventError

object PluginEvents:
  trait Generation:
    def emit(event: GenerationPluginEvent): IO[PluginEventError, Unit]

  trait Invocation:
    def emit(event: InvocationPluginEvent): IO[PluginEventError, Unit]

  /** Writes fail. This runner has no invocation lease. */
  val closedInvocation: Invocation = new Invocation:
    def emit(event: InvocationPluginEvent): IO[PluginEventError, Unit] =
      ZIO.fail(PluginEventError.Closed)
end PluginEvents
