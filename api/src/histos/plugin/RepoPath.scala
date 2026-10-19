package histos.plugin

import _root_.scala.annotation.publicInBinary

/**
 * A file or directory relative to the repository.
 * A segment matches `[A-Za-z][A-Za-z0-9._-]*`, so `docs/index.html` is a path.
 * `ProjectPath` stays the module-directory grammar and rejects that dot.
 */
opaque type RepoPath = String

object RepoPath:
  given CanEqual[RepoPath, RepoPath] = CanEqual.derived

  inline def apply(inline raw: String): RepoPath = ${ PluginMacros.repoPath('raw) }

  def from(raw: String): Either[RepoPathError, RepoPath] =
    if raw.isEmpty then Left(RepoPathError.Empty)
    else if raw.startsWith("/") || raw.contains('\\') || raw.contains(':') then Left(RepoPathError.Escapes(raw))
    else
      val parts = raw.split("/", -1).toList
      if parts.forall(Tokens.repoSegment) then Right(trusted(raw))
      else Left(RepoPathError.Escapes(raw))

  extension (path: RepoPath) def value: String = path

  @publicInBinary private[histos] def trusted(raw: String): RepoPath = raw

enum RepoPathError:
  case Empty
  case Escapes(raw: String)

  def message: String = this match
    case Empty        => "a repository path is not empty"
    case Escapes(raw) => s"'$raw' is not a path under the repository"
