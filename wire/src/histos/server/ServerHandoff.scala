package histos.server

/** What a command does with the recorded server. The record and the process move together. A live server is stopped
  * before another starts, and a record with nobody listening is dropped in that same step.
  */
object ServerHandoff:
  enum Step:
    /** The process listening is this command's server. */
    case Use

    /** A different server is listening. Stop it, then start this command's. */
    case Stop

    /** Nobody is listening. A record here is stale and is removed before the start. */
    case Start
  end Step

  /** `same` is true when this command does not judge the server, or the recorded version is this command's. */
  def step(recorded: Boolean, listening: Boolean, same: Boolean): Step =
    if listening && same then Step.Use
    else if listening then Step.Stop
    else Step.Start
end ServerHandoff
