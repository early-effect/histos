package histos.plugin

/** A plugin task is sendable only when it is hermetic and declares no need. */
enum Seal:
  case Hermetic
  case Open

/** A declared need that keeps the task on this machine. */
enum Need:
  case Network
  case Docker
