package histos.publish

import java.nio.charset.StandardCharsets
import java.nio.file.Path

import zio.{IO, UIO, ZIO}

/** GitHub release credentials. `HISTOS_GITHUB` and `HISTOS_GITHUB_UPLOAD` redirect the hosts. Unset is GitHub. */
private[publish] object AssetAccess:
  final case class Access(token: AssetToken, repository: AssetRepo)
  final case class Bases(api: AssetBase, uploads: AssetBase)

  def access(env: Map[String, String]): Either[AssetError, Access] =
    val token = env.get("GITHUB_TOKEN").filter(_.nonEmpty)
    val repo  = env.get("GITHUB_REPOSITORY").map(_.trim).filter(_.nonEmpty)
    (token, repo) match
      case (Some(secret), Some(name)) => AssetRepo.from(name).map(parsed => Access(AssetToken(secret), parsed))
      case _                          => Left(AssetError.Credentials)

  def bases(env: Map[String, String]): Either[AssetError, Bases] =
    val api    = text(env.get("HISTOS_GITHUB"))
    val upload = text(env.get("HISTOS_GITHUB_UPLOAD"))
    (api, upload) match
      case (None, None)      => Right(Bases(AssetBase.github, AssetBase.uploads))
      case (Some(raw), None) =>
        AssetBase.from(raw).map(base => Bases(base, base))
      case (None, Some(raw)) =>
        AssetBase.from(raw).map(base => Bases(AssetBase.github, base))
      case (Some(apiRaw), Some(uploadRaw)) =>
        for
          apiBase    <- AssetBase.from(apiRaw)
          uploadBase <- AssetBase.from(uploadRaw)
        yield Bases(apiBase, uploadBase)

  /** `GITHUB_TOKEN`, or `gh auth token`. `GITHUB_REPOSITORY`, or `owner/name` from `origin`. */
  def complete(
      repo: Path,
      env: Map[String, String],
      readToken: UIO[Option[String]] = ghToken,
      remote: Path => Option[String] = origin,
  ): IO[AssetError, Map[String, String]] =
    val secret = env.get("GITHUB_TOKEN").map(_.trim).filter(_.nonEmpty)
    val named  = env.get("GITHUB_REPOSITORY").map(_.trim).filter(_.nonEmpty)
    for
      token <- secret match
        case Some(value) => ZIO.succeed(value)
        case None        =>
          readToken.flatMap:
            case Some(value) if value.nonEmpty => ZIO.succeed(value)
            case _                             => ZIO.fail(AssetError.Credentials)
      repository <- named match
        case Some(value) => ZIO.succeed(value)
        case None        =>
          ZIO.attemptBlocking(remote(repo)).orDie.flatMap:
            case Some(value) => ZIO.succeed(value)
            case None        => ZIO.fail(AssetError.Credentials)
      filled = env.updated("GITHUB_TOKEN", token).updated("GITHUB_REPOSITORY", repository)
      _ <- ZIO.fromEither(access(filled))
    yield filled

  /** `owner/name` from an https or ssh origin URL. */
  def repositoryOf(raw: String): Option[String] =
    val text = raw.trim.stripSuffix("/").stripSuffix(".git")
    val body = after(text, "github.com/").orElse(after(text, "github.com:")).orElse(after(text, ":")).getOrElse(text)
    body.split("/").filter(_.nonEmpty) match
      case Array(owner, name) =>
        AssetRepo.from(s"$owner/$name") match
          case Right(_) => Some(s"$owner/$name")
          case Left(_)  => None
      case _ => None

  private def after(raw: String, mark: String): Option[String] =
    val at = raw.indexOf(mark)
    if at < 0 then None else Some(raw.substring(at + mark.length))

  def ghToken: UIO[Option[String]] =
    ZIO.attemptBlocking(runGh).orElseSucceed(None)

  def origin(repo: Path): Option[String] =
    try
      val process = new ProcessBuilder("git", "-C", repo.toString, "remote", "get-url", "origin")
        .redirectError(ProcessBuilder.Redirect.DISCARD)
        .start()
      val text = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      if process.waitFor() == 0 then repositoryOf(text) else None
    catch case _: java.io.IOException => None

  private def runGh: Option[String] =
    val process = new ProcessBuilder("gh", "auth", "token")
      .redirectError(ProcessBuilder.Redirect.DISCARD)
      .start()
    val text = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8).trim
    if process.waitFor() == 0 && text.nonEmpty then Some(text) else None

  private def text(value: Option[String]): Option[String] =
    value.map(_.trim).filter(_.nonEmpty)
end AssetAccess
