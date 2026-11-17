package histos.server

import histos.lock.LockReply

import zio.json.*

/** One socket frame after Ready. A consent reply is never [[RunInput]]. */
object ClientFrames:
  enum Decoded:
    case Program(input: RunInput)
    case Reply(reply: LockReply)
    case Invalid

  def classify(
      payload: String
  )(using JsonCodec[RunInput], JsonCodec[LockReply]): Decoded =
    payload.fromJson[LockReply] match
      case Right(reply) => Decoded.Reply(reply)
      case Left(_)      =>
        payload.fromJson[RunInput] match
          case Right(input) => Decoded.Program(input)
          case Left(_)      => Decoded.Invalid

  /** Program bytes stay on the program queue. A reply always stays on the consent queue. */
  def deliver[A](
      decoded: Decoded,
      programOpen: Boolean,
      program: RunInput => A,
      reply: LockReply => A,
      ignore: A,
  ): A =
    decoded match
      case Decoded.Reply(found)                  => reply(found)
      case Decoded.Program(found) if programOpen => program(found)
      case Decoded.Program(_)                    => ignore
      case Decoded.Invalid                       => ignore
end ClientFrames
