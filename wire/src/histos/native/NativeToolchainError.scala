package histos.native

import histos.RepositoryUrlError

enum NativeToolchainError:
  case Empty
  case Origin(raw: String)
  case Revision(raw: String)
  case Repository(error: RepositoryUrlError)

  def message: String = this match
    case Empty       => "HISTOS_NATIVE is empty"
    case Origin(raw) =>
      s"'$raw' is not HISTOS_NATIVE. Use ivy-local or file:/path"
    case Revision(raw) =>
      s"'$raw' puts a revision in HISTOS_NATIVE. Name nativeVersion on the module"
    case Repository(error) => error.message
end NativeToolchainError
