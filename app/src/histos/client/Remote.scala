package histos.client

import histos.server.{Feed, Message, ServerError}

import zio.json.*

/** One decoded daemon frame. The terminal frame is [[Feed.Done]].
  */
object Remote:
  enum Event:
    case More(feed: Feed)
    case Stop(feed: Feed)

  private[histos] def event(payload: String): Either[ServerError, Event] =
    payload.fromJson[Message] match
      case Right(Message.Event(event)) => Right(Event.More(Feed.Event(event)))
      case Right(Message.Done(result)) => Right(Event.Stop(Feed.Done(result)))
      case _                           => Left(ServerError.Protocol)
end Remote
