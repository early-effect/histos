package histos

/** A system library a Native module links. The name is the library, not a linker flag. */
enum NativeLibrary:
  case Ssl
  case Crypto
  case Gc
  case Unwind
  case Stdcxx

  def token: String = this match
    case Ssl    => "ssl"
    case Crypto => "crypto"
    case Gc     => "gc"
    case Unwind => "unwind"
    case Stdcxx => "stdcxx"
end NativeLibrary
