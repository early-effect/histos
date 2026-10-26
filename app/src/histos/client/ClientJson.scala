package histos.client

import heddle.endpoint.Schema
import histos.report.WireJson.given
import histos.server.{ServerError, ServerIo}

import zio.json.JsonCodec

/** Codecs for the tool error. They stay next to the client so the report package does not import the server. */
object ClientJson:
  given JsonCodec[ServerIo]             = JsonCodec.derived
  given Schema[ServerIo]                = Schema.derived
  given JsonCodec[ServerError]          = JsonCodec.derived
  given Schema[ServerError]             = Schema.derived
  given JsonCodec[Session.SessionError] = JsonCodec.derived
  given Schema[Session.SessionError]    = Schema.derived
  given JsonCodec[Session.CallProblem]  = JsonCodec.derived
  given Schema[Session.CallProblem]     = Schema.derived
end ClientJson
