package histos.client

import histos.server.{Feed, ServerError}

/** One decoded daemon frame. The terminal frame is [[histos.server.Feed.Done]].
  */
object Remote:
  enum Event:
    case More(feed: Feed)
    case Stop(feed: Feed)

  private[histos] def event(payload: String): Either[ServerError, Event] =
    histos.server.CommandFrame
      .read(Some(payload))
      .map:
        case done @ Feed.Done(_) => Event.Stop(done)
        case feed                => Event.More(feed)
end Remote
