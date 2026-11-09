package histos.pack

enum LazyValFrame:
  case Rejected
  case MethodTooLarge(owner: String, method: String, descriptor: String, codeSize: Int)
  case ClassTooLarge(owner: String, pool: Int)
  case Limit
  case Bounds

  def message: String = this match
    case Rejected                                            => "the bytecode was rejected"
    case MethodTooLarge(owner, method, descriptor, codeSize) =>
      s"$owner.$method $descriptor is $codeSize bytes"
    case ClassTooLarge(owner, pool) => s"$owner has $pool constant pool entries"
    case Limit                      => "an ASM limit was exceeded"
    case Bounds                     => "the frame merge was out of bounds"
end LazyValFrame

enum LazyValError:
  case StillCalls(entry: String, method: String)
  case MissingHandle(entry: String, owner: String, offset: String)
  case OffsetShape(entry: String, method: String)
  case Frames(entry: String, failure: LazyValFrame)

  def message: String = this match
    case StillCalls(entry, method)           => s"$entry still calls Unsafe lazy vals from $method"
    case MissingHandle(entry, owner, offset) =>
      s"$entry has no VarHandle for $owner.$offset"
    case OffsetShape(entry, method) => s"$entry cannot rewrite the offset load in $method"
    case Frames(entry, failure)     => s"$entry could not compute stack map frames: ${failure.message}"
end LazyValError
