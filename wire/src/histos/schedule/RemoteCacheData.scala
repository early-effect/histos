package histos.schedule

/** Why an exchange with the remote cache produced no response. The case never carries a throwable or its message. */
enum CacheExchange:
  case Target
  case Connect(host: String, port: Int)
  case ConnectTimeout(host: String, port: Int)
  case Tls(host: String, port: Int)
  case ReadTimeout(host: String, port: Int)
  case Io(host: String, port: Int)
  case Protocol(host: String, port: Int)
  case Pool(host: String, port: Int)
  case Trust
  case Body

  def message: String = this match
    case Target                     => "the remote cache target is not a url"
    case Connect(host, port)        => s"could not connect to the remote cache at $host:$port"
    case ConnectTimeout(host, port) => s"the remote cache at $host:$port did not accept a connection"
    case Tls(host, port)            => s"tls failed with the remote cache at $host:$port"
    case ReadTimeout(host, port)    => s"the remote cache at $host:$port stopped answering"
    case Io(host, port)             => s"the remote cache at $host:$port closed the exchange"
    case Protocol(host, port)       => s"the remote cache at $host:$port sent an invalid response"
    case Pool(host, port)           => s"no free connection to the remote cache at $host:$port"
    case Trust                      => "the remote cache trust material is unusable"
    case Body                       => "the remote cache response body failed"
end CacheExchange

enum RemoteError:
  case Off
  case ReadOnly
  case Dangling(action: Digest, output: Digest)
  case Text(action: Digest)
  case Exchange(fault: CacheExchange)
  case Status(code: Int)
  case Index(action: Digest)
  case Mismatch(action: Digest, output: Digest)
  case Catalog(line: String)
  case Scheme(raw: String)
  case Mode(raw: String)
  case Address(raw: String)
  case Namespace(error: NamespaceError)
  case Rpc(code: String)
  case HttpOnly(command: String)

  def message: String = this match
    case Off                      => "remote cache is not configured"
    case ReadOnly                 => "remote cache is read-only"
    case Dangling(action, output) =>
      s"remote cache action ${action.value} names missing output ${output.value}"
    case Text(action)             => s"remote cache output for ${action.value} is not utf-8"
    case Exchange(fault)          => fault.message
    case Status(code)             => s"remote cache answered $code"
    case Index(action)            => s"remote cache action ${action.value} is not an action result"
    case Mismatch(action, output) =>
      s"remote cache action ${action.value} returned bytes that are not ${output.value}"
    case Catalog(line)     => s"'$line' is not an action digest"
    case Scheme(raw)       => s"'$raw' is not a remote cache scheme. A scheme is http, https, grpc, or grpcs."
    case Mode(raw)         => s"'$raw' is not a remote cache mode. A mode is read or write."
    case Address(raw)      => s"'$raw' is not a remote cache address."
    case Namespace(error)  => error.message
    case Rpc(code)         => s"remote cache answered $code"
    case HttpOnly(command) => s"$command is an http cache operation"
end RemoteError
