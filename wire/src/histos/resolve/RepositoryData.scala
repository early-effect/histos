package histos.resolve

import java.nio.file.{Path}

/** A `file:` or `https:` repository root. An empty string is not a url. */
opaque type RepoUrl = String

object RepoUrl:
  given CanEqual[RepoUrl, RepoUrl] = CanEqual.derived

  def from(raw: String): Either[LockError, RepoUrl] =
    histos.RepositoryUrl.from(raw) match
      case Right(url) => Right(url.value)
      case Left(_)    => Left(LockError.BadRepository(raw))

  def file(path: Path): RepoUrl =
    path.toAbsolutePath.normalize.toUri.toASCIIString

  /** The api already checked the text. */
  private[histos] def trusted(raw: String): RepoUrl = raw

  extension (url: RepoUrl) def value: String = url
end RepoUrl

enum Repository:
  case Central
  case IvyLocal
  case Maven(url: RepoUrl)

object Repository:
  def token(repository: Repository): String =
    repository match
      case Central    => "central"
      case IvyLocal   => "ivy-local"
      case Maven(url) => s"maven:${url.value}"

  def parse(raw: String): Either[LockError, Repository] =
    raw match
      case "central"                           => Right(Central)
      case "ivy-local"                         => Right(IvyLocal)
      case other if other.startsWith("maven:") =>
        RepoUrl.from(other.drop("maven:".length)).map(Maven(_))
      case other => Left(LockError.BadRepository(other))

  /** The repository whose root is a prefix of this artifact url. */
  def serves(repository: Repository, url: String): Boolean =
    repository match
      case Central =>
        canonical(url).startsWith("https://repo1.maven.org/maven2/")
      case IvyLocal =>
        canonical(url).contains("/.ivy2/local/")
      case Maven(root) =>
        val artifact = canonical(url)
        val base     = canonical(root.value)
        artifact == base || artifact.startsWith(base + "/")

  def recorded(
      url: String,
      repositories: List[Repository],
  ): Option[Repository] =
    repositories.find(repository => serves(repository, url))

  private def canonical(raw: String): String =
    val trimmed = raw.stripSuffix("/")
    if trimmed.startsWith("file:///") then "file:/" + trimmed.drop("file:///".length)
    else if trimmed.startsWith("file://") then "file:/" + trimmed.drop("file://".length)
    else trimmed
end Repository
