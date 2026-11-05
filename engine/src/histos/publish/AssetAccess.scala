package histos.publish

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

  private def text(value: Option[String]): Option[String] =
    value.map(_.trim).filter(_.nonEmpty)
end AssetAccess
