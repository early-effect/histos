package histos.schedule

enum CacheError:
  case Io(kind: String)
  case Corrupt(digest: Digest)

  def message: String = this match
    case Io(kind)       => s"cache io failed ($kind)"
    case Corrupt(digest) => s"cache entry ${digest.value} failed verification"
