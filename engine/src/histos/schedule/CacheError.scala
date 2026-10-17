package histos.schedule

/** Which cache step failed. The exception text is not the error. */
enum CacheOp:
  case CreateTemp, Write, Read, Move, Stamp, Delete, List, Keep

enum CacheError:
  case Io(op: CacheOp)
  case Corrupt(digest: Digest)

  def message: String = this match
    case Io(op)          => s"cache failed while ${op.toString.toLowerCase}"
    case Corrupt(digest) => s"cache entry ${digest.value} failed verification"
