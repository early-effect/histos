package histos.doctor

import histos.plugin.JobRender
import histos.version.Catalog
import histos.ProjectGraph

/** Pure release and pages decisions. The probes that fill these values live beside this object. */
object HygieneChecks:
  val credentialsId = CheckId.trusted("central.credentials")
  val keyId         = CheckId.trusted("pgp.key")
  val pagesId       = CheckId.trusted("pages.enabled")

  def publishesPages(graph: ProjectGraph): Boolean =
    graph.plugins.exists: plugin =>
      plugin.contribution.jobs.exists: job =>
        job.render
          .exists:
            case JobRender.Site(_, _) => true
            case _                    => false

  def credentials(
      catalog: Catalog,
      env: Map[String, String],
      secrets: Option[Set[String]],
      auth: CentralProbe.Auth,
  ): Outcome =
    catalog match
      case Catalog.Empty    => Outcome.Ok(credentialsId, "no catalog")
      case Catalog.Lines(_) =>
        val user     = env.get("SONATYPE_USERNAME").map(_.trim).filter(_.nonEmpty)
        val password = env.get("SONATYPE_PASSWORD").filter(_.nonEmpty)
        (user, password) match
          case (Some(_), Some(_)) =>
            auth match
              case CentralProbe.Auth.Rejected(status) =>
                Outcome.Found(ReleaseDiagnostic.CredentialsRejected(status))
              case CentralProbe.Auth.Accepted =>
                Outcome.Ok(credentialsId, "the portal accepted the token")
              case CentralProbe.Auth.NotRun | CentralProbe.Auth.Unreachable =>
                Outcome.Skipped(credentialsId, Probe.JobEnv, Fix.Steps(::("retry when the portal answers", Nil)))
          case _ =>
            secrets match
              case Some(names) if missing(names) =>
                Outcome.Found(ReleaseDiagnostic.CredentialsMissing())
              case Some(_) =>
                Outcome.Ok(credentialsId, "repository secrets are set")
              case None =>
                Outcome.Skipped(
                  credentialsId,
                  Probe.JobEnv,
                  Fix.Steps(::("export SONATYPE_USERNAME and SONATYPE_PASSWORD, or list the repository secrets", Nil)),
                )

  def key(hit: PgpProbe.Hit): Outcome =
    hit match
      case PgpProbe.Hit.Unset =>
        Outcome.Ok(keyId, "no signing fingerprint")
      case PgpProbe.Hit.Bad =>
        Outcome.Found(ReleaseDiagnostic.KeyHex())
      case PgpProbe.Hit.Present(_) =>
        Outcome.Ok(keyId, "keyserver.ubuntu.com has the key")
      case PgpProbe.Hit.Missing(fingerprint) =>
        Outcome.Found(ReleaseDiagnostic.KeyMissing(fingerprint))
      case PgpProbe.Hit.Unreachable(fingerprint) =>
        Outcome.Skipped(
          keyId,
          Probe.JobEnv,
          Fix.Command(ShellLine.trusted(s"gpg --keyserver keyserver.ubuntu.com --recv-keys $fingerprint")),
        )

  def pages(site: Boolean, hit: PagesProbe.Hit): List[Outcome] =
    if !site then List(Outcome.Ok(pagesId, "no pages site"))
    else
      hit match
        case PagesProbe.Hit.Absent =>
          List(Outcome.Skipped(pagesId, Probe.GitHubRead, Fix.Command(ShellLine.trusted("gh auth login"))))
        case PagesProbe.Hit.Unreachable =>
          List(
            Outcome.Skipped(pagesId, Probe.GitHubRead, Fix.Steps(::("retry when the GitHub API answers", Nil)))
          )
        case PagesProbe.Hit.Ready(source, gate) =>
          val sourceOutcome = source match
            case PagesProbe.Source.Actions => None
            case PagesProbe.Source.Other   => Some(Outcome.Found(PagesDiagnostic.Source()))
          val gateOutcome = gate match
            case PagesProbe.Gate.Open | PagesProbe.Gate.Unread => None
            case PagesProbe.Gate.Blocks(branch)                => Some(Outcome.Found(PagesDiagnostic.Branch(branch)))
          sourceOutcome.toList ++ gateOutcome.toList match
            case Nil   => List(Outcome.Ok(pagesId, "Pages deploys from GitHub Actions"))
            case found => found

  private def missing(names: Set[String]): Boolean =
    !names.contains("SONATYPE_USERNAME") || !names.contains("SONATYPE_PASSWORD")
end HygieneChecks
