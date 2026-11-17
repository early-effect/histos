package histos.cli

import histos.lock.{LockOffer, LockReply}

import zio.{UIO, ZIO}

/** The CLI asks on its own terminal. A redirected CLI does not read. The daemon never calls this. */
object ConsentAsk:
  def ask(stdio: histos.cli.Stdio, offer: LockOffer, call: histos.server.Call, read: UIO[String]): UIO[Ask] =
    if stdio.prompts then read.map(line => Ask.Answered(ConsentRoute.reply(offer, line)))
    else ZIO.succeed(Ask.Blocked(ConsentRoute.required(offer, call)))

enum Ask:
  case Answered(reply: LockReply)
  case Blocked(required: ConsentDecision.Required)
