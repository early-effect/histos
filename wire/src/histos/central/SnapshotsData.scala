package histos.central

/** Why no HTTP response arrived. The case carries the target, never a throwable or its message. */
enum TransportFault:
  case Target(raw: String)
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
    case Target(raw)                => s"could not reach the snapshot repository: '$raw' is not a target"
    case Connect(host, port)        => s"could not connect to the snapshot repository at $host:$port"
    case ConnectTimeout(host, port) => s"the snapshot repository at $host:$port did not accept a connection"
    case Tls(host, port)            => s"tls failed with the snapshot repository at $host:$port"
    case ReadTimeout(host, port)    => s"the snapshot repository at $host:$port stopped answering"
    case Io(host, port)             => s"the snapshot repository at $host:$port closed the exchange"
    case Protocol(host, port)       => s"the snapshot repository at $host:$port sent an invalid response"
    case Pool(host, port)           => s"no free connection to the snapshot repository at $host:$port"
    case Trust                      => "the snapshot repository trust material is unusable"
    case Body                       => "the snapshot repository response body failed"
end TransportFault

/** Why snapshot metadata could not name the next build. */
enum MetadataFault:
  case MissingBuild
  case BadBuild
  case Saturated
  case Unread

  def message: String = this match
    case MissingBuild => "snapshot metadata has no build number"
    case BadBuild     => "snapshot metadata build number is not a positive int"
    case Saturated    => "snapshot metadata build number cannot increase"
    case Unread       => "snapshot metadata was not a complete body"
end MetadataFault

/** A revision that is not a `-SNAPSHOT`. */
enum SnapshotRevisionError:
  case NotSnapshot(revision: String)

  def message: String = this match
    case NotSnapshot(revision) => s"'$revision' is not a snapshot"
end SnapshotRevisionError
