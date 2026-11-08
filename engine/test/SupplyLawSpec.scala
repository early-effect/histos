package histos.supply

import histos.doctor.{Level, Outcome}
import histos.plugin.SecretName
import histos.workflow.SupplyYaml

import zio.test.*

object SupplyLawSpec extends ZIOSpecDefault:
  private val id      = SecretName("HISTOS_APP_ID")
  private val key     = SecretName("HISTOS_APP_PRIVATE_KEY")
  private val authors = List(
    PrAuthor.Actions,
    PrAuthor.App(id, key),
    PrAuthor.OwnerOpens(id, key),
    PrAuthor.Pat(SecretName("HISTOS_TOKEN")),
  )
  private val appGate = Set(
    "supply.secret-missing",
    "supply.app-installed",
    "supply.app-permissions",
    "supply.app-token",
    "supply.required-check",
  )
  private val files = List(
    ".github/workflows/ci.yml" -> "workflow",
    ".github/workflows"        -> "dir",
    "dependencies.lock"        -> "lock",
  )

  def spec = suite("supply laws")(
    test("Actions drops workflow files and does not ask the app gate"):
      assertTrue:
        authors.forall: author =>
          val kept    = PrFlow.keep(author, files).map(_._1)
          val actions = author == PrAuthor.Actions
          val ran     = SupplyChecks.run(view(Sight.Known, allowPrs = true), policy(author))
          val unread  =
            SupplyChecks.blind(
              Some(RepoName.trusted("early-effect/histos")),
              policy(author),
              Blind.NoCredential,
              Venue.Actions,
            )
          val ids       = ran.map(checkId)
          val unreadIds = unread.map(checkId)
          kept.contains("dependencies.lock") &&
          kept.exists(PrFlow.workflowPath) == !actions &&
          ids.contains("supply.workflow-files") == actions &&
          appGate.forall(gate => ids.contains(gate) == !actions) &&
          unreadIds == (if actions then List("supply.workflow-files")
                        else
                          List(
                            "supply.secret-missing",
                            "supply.app-installed",
                            "supply.app-permissions",
                            "supply.required-check",
                          ))
    ,
    test("allow-prs follows the author only when the setting was read"):
      val cases =
        for
          author <- authors
          sight  <- Sight.values.toList
          allow  <- List(true, false)
        yield (author, sight, allow)
      assertTrue:
        cases.forall: (author, sight, allow) =>
          val got = SupplyChecks
            .run(view(sight, allow), policy(author))
            .collectFirst:
              case outcome if checkId(outcome) == "supply.allow-prs" => outcome
          expect(author, sight, allow, got)
    ,
    test("the doctor step mints a token only for an author with a credential"):
      assertTrue:
        authors.forall: author =>
          val yaml  = SupplyYaml.fullDoctor(author).mkString("\n")
          val mints = yaml.contains("actions/create-github-app-token@")
          mints == (author != PrAuthor.Actions) && yaml.contains("GH_TOKEN:"),
  )

  private def policy(author: PrAuthor): SupplyChain =
    SupplyChain(updates = UpdatePolicy(author = author))

  private def expect(author: PrAuthor, sight: Sight, allow: Boolean, got: Option[Outcome]): Boolean =
    (author, sight, allow) match
      case (_, Sight.Unseen | Sight.Unreadable, _) =>
        got.exists(found(Level.Warning))
      case (PrAuthor.Actions, Sight.Known, false) =>
        got.exists(found(Level.Error))
      case (PrAuthor.Actions, Sight.Known, true) =>
        got.contains(Outcome.Ok(histos.doctor.CheckId.trusted("supply.allow-prs"), "Actions can create pull requests"))
      case (_, Sight.Known, true) =>
        got.exists(found(Level.Info))
      case (_, Sight.Known, false) =>
        got.contains(
          Outcome.Ok(histos.doctor.CheckId.trusted("supply.allow-prs"), "Actions cannot create pull requests")
        )

  private def found(level: Level)(outcome: Outcome): Boolean =
    outcome match
      case Outcome.Found(diagnostic) => diagnostic.level == level
      case _                         => false

  private def checkId(outcome: Outcome): String =
    outcome match
      case Outcome.Ok(id, _)         => id.value
      case Outcome.Found(found)      => found.check.value
      case Outcome.Skipped(id, _, _) => id.value

  private def view(sight: Sight, allowPrs: Boolean): GitHubView =
    val branch = BranchName.trusted("main")
    GitHubView(
      repo = RepoName.trusted("early-effect/histos"),
      appInstalled = true,
      permissions = Map.empty,
      secrets = Set.empty,
      required = Nil,
      signedCommits = true,
      mergeMethods = Set(MergeMethod.Squash),
      rule = RuleSource.Absent,
      dependabot = Nil,
      securityPrs = Nil,
      graphOn = true,
      alertsOn = true,
      defaultBranch = branch,
      expectedBranch = branch,
      tokenWrite = false,
      allowPrs = allowPrs,
      shaPins = true,
      disabled = None,
      refusals = Nil,
      snapshot = false,
      owner = RepoOwner.Organization("early-effect"),
      secretsSight = Sight.Known,
      appSight = Sight.Known,
      rulesSight = Sight.Known,
      flowSight = sight,
      tokenStep = Some(StepOutcome.Success),
      venue = Venue.Actions,
    )
end SupplyLawSpec
