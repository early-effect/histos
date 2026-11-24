package histos.plugin

import _root_.scala.annotation.publicInBinary

/** A safe file or directory relative to the repository. Filename punctuation, spaces and Unicode are allowed.
  * `ProjectPath` retains its separate module-directory grammar.
  */
opaque type RepoPath = String

object RepoPath:
  given CanEqual[RepoPath, RepoPath] = CanEqual.derived

  inline def apply(inline raw: String): RepoPath = ${ PluginMacros.repoPath('raw) }

  def from(raw: String): Either[RepoPathError, RepoPath] =
    if raw.isEmpty then Left(RepoPathError.Empty)
    else if Tokens.filePath(raw) then Right(trusted(raw))
    else Left(RepoPathError.Escapes(raw))

  extension (path: RepoPath) def value: String = path

  @publicInBinary private[histos] def trusted(raw: String): RepoPath = raw
end RepoPath

enum RepoPathError:
  case Empty
  case Escapes(raw: String)

  def message: String = this match
    case Empty        => "a repository path is not empty"
    case Escapes(raw) => s"'$raw' is not a path under the repository"
end RepoPathError
