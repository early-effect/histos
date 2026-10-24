package histos.schedule

import histos.workspace.IoFailure

enum CacheError:
  case Io(failure: IoFailure)
  case Corrupt(digest: Digest)

  def message: String = this match
    case Io(failure)     => failure.message
    case Corrupt(digest) => s"cache entry ${digest.value} failed verification"
