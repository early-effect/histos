package histos.supply

import histos.doctor.{
  Area,
  CheckId,
  DoctorCheck,
  DoctorContext,
  Fix,
  FixApply,
  FixMode,
  GhAuth,
  GitHubCredential,
  GitHubPut,
  HistosCommand,
  JobEnv,
  Outcome,
  Probe,
  Render,
  Runner,
}
import histos.central.{KeyServer, ReleaseFeed}
import histos.cli.Stdio
import histos.plugin.RepoPath
import histos.version.CommitSha
import histos.workflow.PinScan
import histos.workspace.{IoFailure, Workspace}
import histos.toolchain.Survey
import histos.ProjectGraph

import java.nio.file.{Files, Path}
import java.time.ZoneOffset

import zio.{durationInt, Clock, IO, UIO, ULayer, URIO, ZIO, ZLayer}

/** In-process doctor. GitHub settings are applied only after an explicit yes on a terminal. */
object DoctorSurface:
  /** A PATH survey and `clang --version` have to finish inside this. Past it, the check is named and the run fails. */
  private val checkLimit = 10.seconds

  final case class Request(
      area: Option[String],
      json: Boolean,
      preflight: Option[String],
      fix: Boolean,
      yes: Boolean,
      launch: List[histos.Module],
      verbose: Boolean,
  )

  /** What doctor reads from the host: the registry feed, git, the network, the token `gh` holds, and the keyserver. */
  type Reads = ReleaseFeed & GitRead & Net & GhAuth & KeyServer

  /** What a doctor run needs from its edge: the reads, whether a person is at the terminal, and the runner a fix's
    * histos command runs in.
    */
  type Needs = Reads & Stdio & CommandRunner

  /** The live reads. An edge adds its own [[Stdio]] and [[CommandRunner]]. */
  val reads: ULayer[Reads] =
    ReleaseFeed.live ++ GitRead.live ++ ZLayer.succeed(Net.live) ++ GhAuth.live ++ KeyServer.central

  /** The terminal's doctor. The environment comes from `zio.System`, a token from it or from `gh`, and every other read
    * from `Needs`. Only a `--fix` run asks the terminal, because only a fix can prompt.
    */
  def run(
      repo: Path,
      graph: ProjectGraph,
      request: Request,
      machine: histos.jdk.Machine,
  ): URIO[Needs, (Int, String)] =
    for
      env   <- zio.System.envs.orElseSucceed(Map.empty)
      token <- ZIO.serviceWithZIO[GhAuth](gh => GitHubCredential.resolve(env, gh.token))
      net   <- ZIO.service[Net]
      feed  <- ZIO.service[ReleaseFeed]
      job        = JobEnv.from(env)
      credential = if request.preflight.isDefined then None else token
      located            <- github(repo, net, env, credential, job, graph)
      (auth, pgp, pages) <- probes(repo, graph, request, net, env, token, located)
      report             <- Runner.run(
        selected(repo, graph, request, machine, env, feed, located, auth, pgp, pages),
        DoctorContext(Nil, Set(Probe.Local, Probe.JobEnv)),
        checkLimit,
      )
      written <- applyReported(repo, report.found.flatMap(_.fixes.toList), request, token)
      survey  <- toolchainLines(request, machine, graph)
    yield
      val body =
        if request.json then Render.json(report) else Render.text(repoName(repo), report, request.verbose)
      val note = if written.isEmpty then "" else written.mkString("\n", "\n", "")
      (report.exit, body + note + survey)

  /** The toolchain lines, when the text report is the whole doctor or the toolchain area, and no preflight is set. */
  private def toolchainLines(
      request: Request,
      machine: histos.jdk.Machine,
      graph: ProjectGraph,
  ): UIO[String] =
    val shown = !request.json && request.preflight.isEmpty && request.area.forall(_ == "toolchain")
    if shown then Survey.collect(machine, graph).map(found => s"\n${found.text}")
    else ZIO.succeed("")

  private def probes(
      repo: Path,
      graph: ProjectGraph,
      request: Request,
      net: Net,
      env: Map[String, String],
      token: Option[GitHubCredential.Token],
      located: Located,
  ): URIO[KeyServer, (histos.doctor.CentralProbe.Auth, histos.doctor.PgpProbe.Hit, histos.doctor.PagesProbe.Hit)] =
    if request.preflight.isDefined then
      ZIO.succeed(
        (histos.doctor.CentralProbe.Auth.NotRun, histos.doctor.PgpProbe.Hit.Unset, histos.doctor.PagesProbe.Hit.Absent)
      )
    else
      val auth  = histos.doctor.CentralProbe.read(net, env)
      val pgp   = histos.doctor.PgpProbe.read(env)
      val pages =
        if !histos.doctor.HygieneChecks.publishesPages(graph) then ZIO.succeed(histos.doctor.PagesProbe.Hit.Absent)
        else
          (token.map(_.value).filter(_.nonEmpty), located.name) match
            case (Some(value), Some(name)) =>
              val branch = located.hit match
                case histos.doctor.GitHubProbe.Hit.Ready(read) => read.view.defaultBranch.value
                case _                                         => "main"
              histos.doctor.PagesProbe.read(net, name, branch, value)
            case _ => ZIO.succeed(histos.doctor.PagesProbe.Hit.Absent)
      auth.zipPar(pgp).zipPar(pages)

  private def selected(
      repo: Path,
      graph: ProjectGraph,
      request: Request,
      machine: histos.jdk.Machine,
      env: Map[String, String],
      feed: ReleaseFeed,
      located: Located,
      auth: histos.doctor.CentralProbe.Auth,
      pgp: histos.doctor.PgpProbe.Hit,
      pages: histos.doctor.PagesProbe.Hit,
  ): List[DoctorCheck] =
    val pins   = pinCheck(repo, graph)
    val github = githubCheck(repo, graph, located, request.preflight.isDefined)
    val listed = located.hit match
      case histos.doctor.GitHubProbe.Hit.Ready(read)
          if read.covered.contains("supply.secret-missing") && read.view.secretsSight == Sight.Known =>
        Some(read.view.secrets)
      case _ => None
    val release = List(
      closed(
        "central.credentials",
        Area.Release,
        histos.doctor.HygieneChecks.credentials(graph.catalog, env, listed, auth),
      ),
      closed("pgp.key", Area.Release, histos.doctor.HygieneChecks.key(pgp)),
      closedPages(graph, pages),
    )
    val core =
      pins :: github :: release ++ histos.doctor.ShipChecks.checks(repo, graph, env, feed) ++
        histos.doctor.ToolchainChecks.checks(machine, graph, request.launch) ++
        histos.doctor.FormatChecks.checks(repo, graph)
    val all     = histos.doctor.Registry.collect(graph, core)
    val dropped = Set(histos.doctor.ShipChecks.bumpId.value, histos.doctor.ShipChecks.pinId.value)
    val kept    =
      if request.preflight.isDefined then all.filterNot(check => dropped.contains(check.id.value)) else all
    request.area.fold(kept)(name => kept.filter(check => areaName(check.area) == name))

  private def closed(raw: String, where: Area, outcome: Outcome): DoctorCheck =
    new DoctorCheck:
      def id                      = CheckId.trusted(raw)
      def area                    = where
      def needs                   = Set.empty
      def run(ctx: DoctorContext) = ZIO.succeed(List(outcome))

  private def closedPages(graph: ProjectGraph, hit: histos.doctor.PagesProbe.Hit): DoctorCheck =
    new DoctorCheck:
      def id                      = CheckId.trusted("pages.enabled")
      def area                    = Area.SitePublish
      def needs                   = Set.empty
      def run(ctx: DoctorContext) =
        ZIO.succeed(histos.doctor.HygieneChecks.pages(histos.doctor.HygieneChecks.publishesPages(graph), hit))

  private def pinCheck(repo: Path, graph: ProjectGraph): DoctorCheck =
    new DoctorCheck:
      def id                      = CheckId.trusted("supply.sha-pin-policy")
      def area                    = Area.SupplyChain
      def needs                   = Set(Probe.Local)
      def run(ctx: DoctorContext) =
        PinScan.faults(repo, graph) match
          case Nil => ZIO.succeed(List(Outcome.Ok(id, "every uses is pinned")))
          case _ => ZIO.succeed(List(Outcome.Found(SupplyDiagnostic.ShaPinPolicyOff(RepoName.trusted(repoName(repo))))))

  private def githubCheck(
      repo: Path,
      graph: ProjectGraph,
      located: Located,
      preflight: Boolean,
  ): DoctorCheck =
    new DoctorCheck:
      def id                      = CheckId.trusted("supply.app-installed")
      def area                    = Area.SupplyChain
      def needs                   = Set.empty
      def run(ctx: DoctorContext) =
        val outcomes = located.hit match
          case histos.doctor.GitHubProbe.Hit.Absent =>
            ZIO.succeed(SupplyChecks.blind(located.name, graph.supply, Blind.NoCredential, located.venue))
          case histos.doctor.GitHubProbe.Hit.Unreachable =>
            ZIO.succeed(SupplyChecks.blind(located.name, graph.supply, Blind.Unreachable, located.venue))
          case histos.doctor.GitHubProbe.Hit.Ready(read) =>
            readFile(repo.resolve(dependabot.value)).fold(
              _ =>
                Outcome.Found(SupplyDiagnostic.DependabotUnread(dependabot)) ::
                  SupplyChecks.run(read.view, graph.supply, read.covered - "supply.dependabot-overlap"),
              text =>
                val view = read.view.copy(dependabot = histos.doctor.GitHubProbe.ecosystems(text))
                SupplyChecks.run(view, graph.supply, read.covered),
            )
        outcomes.map(found => if preflight then SupplyChecks.unseenAsSkip(found) else found)

  /** Local edits and histos commands run when `--fix` is set, the commands in the given runner. A GitHub setting with a
    * call is sent on the net with `token` only after a person at the terminal says yes. A shell command and a step are
    * for a person, so nothing here runs them.
    */
  def applyReported(
      repo: Path,
      fixes: List[Fix],
      request: Request,
      token: Option[GitHubCredential.Token],
  ): URIO[Stdio & CommandRunner & Net, List[String]] =
    if !request.fix then ZIO.succeed(Nil)
    else
      val mode     = FixMode.from(request.fix, request.yes)
      val targets  = fixes.collect { case Fix.Config(file, _) => file.value }.distinct
      val settings = fixes.collect { case fix: Fix.Settings => fix }
      for
        stdio  <- ZIO.service[Stdio]
        runner <- ZIO.service[CommandRunner]
        net    <- ZIO.service[Net]
        read   <- ZIO.foreach(targets)(name => readFile(repo.resolve(name)).either.map(name -> _))
        files  = read.collect { case (name, Right(text)) => name -> text }.toMap
        unread = read.collect { case (_, Left(failure)) => s"not applied: ${failure.message}" }
        kept   = fixes.filter:
          case Fix.Config(file, _) => files.contains(file.value)
          case _                   => true
        (next, commands) = FixApply.applyAll(kept, mode, _ => FixApply.Answer.No, files)
        changed          = next.toList.filterNot((name, body) => files.get(name).contains(body))
        wrote <- ZIO.foreach(changed): (name, body) =>
          Workspace
            .writeString(repo.resolve(name), body)
            .fold(failure => s"not applied: ${failure.message}", _ => s"wrote $name")
        ran   <- ZIO.foreach(commands)(command => runner.run(repo, command).map(ranNotes(command, _)))
        asked <- ZIO.foreach(settings): fix =>
          confirmSetting(fix, request.yes, stdio.prompts, applySetting(net, token.map(_.value)))
      yield unread ++ wrote ++ ran.flatten ++ asked

  /** The command, then what it printed when the runner kept that, indented under it. */
  private def ranNotes(command: HistosCommand, ran: Ran): List[String] =
    ran match
      case Ran.Exited(0, output)      => s"ran: ${command.line}" :: output.map(line => s"  $line")
      case Ran.Exited(status, output) =>
        s"failed with status $status: ${command.line}" :: output.map(line => s"  $line")
      case Ran.Unstarted => List(s"not run (./histos did not start): ${command.line}")

  /** A page with no call is a person's to open. A call goes out only after a yes on a terminal, never on `--yes`. */
  private def confirmSetting(
      fix: Fix.Settings,
      yes: Boolean,
      person: Boolean,
      setting: GitHubPut => UIO[Unit],
  ): UIO[String] =
    val url = fix.url.value
    fix.call match
      case None               => ZIO.succeed(s"not applied (open it in a browser): $url")
      case Some(_) if !person => ZIO.succeed(s"not applied (no terminal): $url")
      case Some(_) if yes     => ZIO.succeed(s"not applied (--yes does not confirm GitHub settings): $url")
      case Some(put)          =>
        confirmed(url).flatMap: agreed =>
          if agreed then setting(put).as(s"applied: $url") else ZIO.succeed(s"skipped: $url")

  private final case class Located(name: Option[RepoName], hit: histos.doctor.GitHubProbe.Hit, venue: Venue)

  private def github(
      repo: Path,
      net: Net,
      env: Map[String, String],
      token: Option[GitHubCredential.Token],
      job: JobEnv,
      graph: ProjectGraph,
  ): URIO[GitRead, Located] =
    val needed = graph.supply.updates.author.secretNames.map(_.value)
    GitRead
      .github(repo, env.get("GITHUB_REPOSITORY"))
      .flatMap:
        case None       => ZIO.succeed(Located(None, histos.doctor.GitHubProbe.Hit.Absent, job.venue))
        case Some(name) =>
          val api = token.fold("https://api.github.com")(_.api)
          histos.doctor.GitHubProbe
            .read(net, name, BranchName.trusted("main"), token.map(_.value), api, job, needed)
            .map(hit => Located(Some(name), hit, job.venue))

  /** `y` or `yes` on the console. Any other line, or none, is no. */
  private def confirmed(url: String): UIO[Boolean] =
    zio.Console.printLine(s"Apply GitHub setting $url? [y/N] ").orDie *>
      zio.Console.readLine
        .map(line => line.trim.equalsIgnoreCase("y") || line.trim.equalsIgnoreCase("yes"))
        .orElseSucceed(false)

  private def applySetting(net: Net, token: Option[String])(put: GitHubPut): UIO[Unit] =
    token match
      case None        => ZIO.unit
      case Some(value) =>
        net
          .send(
            "PUT",
            put.url.value,
            put.body,
            List("Authorization" -> s"Bearer $value", "Accept" -> "application/vnd.github+json"),
          )
          .ignore

  /** The file's text, or no text when there is no file. */
  private def readFile(path: Path): IO[IoFailure, String] =
    Workspace.readIfPresent(path).map(_.getOrElse(""))

  private val dependabot: RepoPath = RepoPath.trusted(".github/dependabot.yml")

  private def areaName(area: Area): String =
    area match
      case Area.SupplyChain     => "supply-chain"
      case Area.Toolchain       => "toolchain"
      case Area.BuildDefinition => "build"
      case Area.Workflows       => "workflows"
      case Area.RepoSettings    => "repo"
      case Area.SitePublish     => "site"
      case Area.Release         => "release"
      case Area.Plugin(id)      => id.value

  private def repoName(repo: Path): String =
    repo.getFileName.toString
end DoctorSurface

/** Lock-backed deps commands. Network access goes through [[Net]]. */
object DepsSurface:
  final case class DepsResult(code: Int, text: String)

  /** Why a command ends before its own work: no lock, a file it could not read, or a supply fault. */
  private enum Stop:
    case NoLock
    case Unread(failure: IoFailure)
    case Fault(fault: SupplyFault)

    def result: DepsResult = this match
      case NoLock          => DepsResult(1, "dependencies.lock is missing\n")
      case Unread(failure) => DepsResult(1, failure.message + "\n")
      case Fault(fault)    => DepsResult(1, fault.message + "\n")
  end Stop

  /** A cooldown counts days to today in UTC on the caller's clock. */
  def check(repo: Path, policy: SupplyChain, net: Net): UIO[DepsResult] =
    propose(repo, policy, net, false).fold(_.result, (proposal, _) => DepsResult(0, proposal.text))

  def update(repo: Path, policy: SupplyChain, net: Net, yes: Boolean, security: Boolean): UIO[DepsResult] =
    propose(repo, policy, net, security).foldZIO(
      stop => ZIO.succeed(stop.result),
      (proposal, graph) =>
        if !yes then ZIO.succeed(DepsResult(0, proposal.text))
        else
          val wrote =
            for
              paths <- Plan.workflowFiles(repo)
              files <- ZIO.foreach(paths)(path => Workspace.readString(path).map(path -> _))
              names <- Plan.write(repo, proposal, graph, files)
            yield names
          wrote.fold(
            failure => DepsResult(1, failure.message + "\n"),
            names => DepsResult(0, proposal.text + names.map(name => s"wrote $name\n").mkString),
          ),
    )

  def audit(repo: Path, report: Boolean, net: Net): UIO[DepsResult] =
    lock(repo).foldZIO(
      stop => ZIO.succeed(stop.result),
      text =>
        query(net, purls(text, directOnly = false))
          .foldZIO(
            _ => ZIO.succeed(DepsResult(1, "osv is down\n")),
            body =>
              cache(body).as:
                val ids  = Scan.strings(body, "id").filter(_.startsWith("GHSA-")).distinct
                val head = if report then "deps audit --report" else "deps audit"
                DepsResult(0, s"$head\n${ids.map(id => s"  $id").mkString("\n")}\n"),
          ),
    )

  /** The branch commits the files `git diff` lists. A dry run, `HISTOS_DRY_RUN`, prints the calls and sends none. */
  def pullRequest(
      repo: Path,
      policy: SupplyChain,
      net: Net,
      security: Boolean,
      open: Boolean,
  ): URIO[GitRead, DepsResult] =
    (dryRun <*> githubToken).flatMap: (dry, token) =>
      if token.isEmpty && !dry then ZIO.succeed(DepsResult(1, "GH_TOKEN is not set\n"))
      else
        changed(repo).foldZIO(
          stop => ZIO.succeed(stop.result),
          files =>
            val present = PrFlow.keep(policy.updates.author, files)
            val branch  = if security then BranchName.trusted("histos/security") else policy.updates.branch
            if present.isEmpty then ZIO.succeed(DepsResult(0, "deps pr: nothing to commit\n"))
            else
              githubName(repo).flatMap:
                case None       => ZIO.succeed(unnamed)
                case Some(name) =>
                  for
                    git  <- ZIO.service[GitRead]
                    main <- git.head(repo).map(_.getOrElse(zero))
                    head <- git.tip(repo, branch).map(_.getOrElse(zero))
                    sent <- PrFlow.plan(
                      PrRequest(
                        policy.updates,
                        branch,
                        main,
                        if head == main then None else Some(head),
                        group(present),
                        Nil,
                        open,
                        None,
                      )
                    ) match
                      case Left(fault)  => ZIO.succeed(DepsResult(1, fault.message + "\n"))
                      case Right(calls) =>
                        val kept = calls.filter:
                          case GhCall.OpenOrUpdate(_, _) => opens(policy, open)
                          case _                         => true
                        GhRun
                          .execute(net, name, token.getOrElse(""), kept, dry)
                          .map(text => DepsResult(0, text))
                          .catchAll(fault => ZIO.succeed(DepsResult(1, fault.message + "\n")))
                  yield sent,
        )

  def alert(repo: Path, policy: SupplyChain, net: Net): URIO[GitRead, DepsResult] =
    (dryRun <*> githubToken).flatMap: (dry, token) =>
      if token.isEmpty && !dry then ZIO.succeed(DepsResult(1, "GH_TOKEN is not set\n"))
      else
        githubName(repo).flatMap:
          case None       => ZIO.succeed(unnamed)
          case Some(name) =>
            lock(repo)
              .map(text => purls(text, directOnly = false))
              .catchSome { case Stop.NoLock => ZIO.succeed(Nil) }
              .foldZIO(
                stop => ZIO.succeed(stop.result),
                packages =>
                  query(net, packages)
                    .orElseSucceed("")
                    .flatMap: body =>
                      GhRun
                        .alert(net, name, token.getOrElse(""), policy, findingsOf(body), dry)
                        .map(text => DepsResult(0, text))
                        .catchAll(fault => ZIO.succeed(DepsResult(1, fault.message + "\n"))),
              )

  private def findingsOf(body: String): List[Finding] =
    Scan
      .strings(body, "id")
      .filter(id => AdvisoryId.from(id).isRight)
      .distinct
      .map: id =>
        Finding(
          AdvisoryId.trusted(id),
          Severity.Unknown,
          None,
          Nil,
          Scan.strings(body, "fixed"),
          id,
          "",
        )

  /** The commit is `GITHUB_SHA`, else HEAD. The ref is `GITHUB_REF`, else `refs/heads/main`. */
  def snapshot(repo: Path, net: Net, submit: Boolean): URIO[GitRead, DepsResult] =
    lock(repo).foldZIO(
      stop => ZIO.succeed(stop.result),
      text =>
        for
          commit <- env("GITHUB_SHA").flatMap:
            case Some(named) => ZIO.succeed(named)
            case None        => ZIO.serviceWithZIO[GitRead](_.head(repo)).map(_.fold("")(_.value))
          ref   <- env("GITHUB_REF").map(_.getOrElse("refs/heads/main"))
          dry   <- dryRun
          token <- githubToken
          body = SnapshotSubmit.json(SnapshotSubmit.fromLock(text), commit, ref)
          result <-
            if !submit then ZIO.succeed(DepsResult(0, body + "\n"))
            else if token.isEmpty then ZIO.succeed(DepsResult(1, "GITHUB_TOKEN is not set\n"))
            else
              githubName(repo).flatMap:
                case None       => ZIO.succeed(unnamed)
                case Some(name) =>
                  GhRun
                    .snapshot(net, name, token.getOrElse(""), body, dry)
                    .map(text => DepsResult(0, text + body + "\n"))
                    .catchAll(fault => ZIO.succeed(DepsResult(1, fault.message + "\n")))
        yield result,
    )

  /** The plan the lock and the build file propose today, with the build file's text. */
  private def propose(repo: Path, policy: SupplyChain, net: Net, security: Boolean): IO[Stop, (Proposal, String)] =
    for
      text     <- lock(repo)
      graph    <- buildFile(repo)
      day      <- today
      proposal <- Plan
        .propose(text, graph, Plan.catalog(policy.extraActions), policy, day, security, net)
        .mapError(Stop.Fault(_))
    yield (proposal, graph)

  private def opens(policy: SupplyChain, open: Boolean): Boolean =
    policy.updates.author match
      case PrAuthor.OwnerOpens(_, _)                               => open
      case PrAuthor.Actions | PrAuthor.App(_, _) | PrAuthor.Pat(_) => true

  private def group(files: List[(String, String)]): List[KindChange] =
    files
      .groupBy((path, _) => kindOf(path))
      .toList
      .map: (kind, rows) =>
        KindChange(kind, rows)

  private def kindOf(path: String): BumpKind =
    if path.contains(".github") || path.endsWith("ActionPins.scala") then BumpKind.Actions
    else if path == "histos" || path.endsWith("build.properties") then BumpKind.Launcher
    else BumpKind.Libraries

  /** The files `git diff` lists that are still in the checkout, with their text. */
  private def changed(repo: Path): ZIO[GitRead, Stop, List[(String, String)]] =
    ZIO
      .serviceWithZIO[GitRead](_.changed(repo))
      .flatMap: names =>
        ZIO
          .foreach(names)(name => Workspace.readIfPresent(repo.resolve(name)).map(_.map(name -> _)))
          .mapBoth(Stop.Unread(_), _.flatten)

  private def lock(repo: Path): IO[Stop, String] =
    Workspace.readIfPresent(repo.resolve("dependencies.lock")).mapError(Stop.Unread(_)).someOrFail(Stop.NoLock)

  /** `histos.scala`, or no text when the checkout has none. */
  private def buildFile(repo: Path): IO[Stop, String] =
    Workspace.readIfPresent(repo.resolve("histos.scala")).mapError(Stop.Unread(_)).map(_.getOrElse(""))

  private def purls(text: String, directOnly: Boolean): List[String] =
    val rows = text.split('\n').toList.map(_.trim).filter(_.nonEmpty)
    rows
      .foldLeft((false, List.empty[String])):
        case ((_, seen), line) if line.startsWith("direct ")        => (true, seen)
        case ((direct, seen), line) if line.startsWith("artifact ") =>
          val parts = line.split(" ").toList
          parts match
            case _ :: org :: name :: version :: _ if !directOnly || direct =>
              (false, seen :+ s"pkg:maven/$org/$name@$version")
            case _ => (false, seen)
        case (state, _) => state
      ._2
      .distinct

  private def query(net: Net, packages: List[String]) =
    val queries = packages.map(purl => s"""{"package":{"purl":"$purl"}}""").mkString(",")
    net
      .send(
        "POST",
        "https://api.osv.dev/v1/querybatch",
        s"""{"queries":[$queries]}""",
        List("Content-Type" -> "application/json"),
      )
      .map(_.body)

  /** `~/.cache/histos/advisories`, the directory the generated supply workflow caches. A failed write is not an audit
    * failure.
    */
  private def cache(body: String): UIO[Unit] =
    zio.System
      .property("user.home")
      .orElseSucceed(None)
      .flatMap:
        case None       => ZIO.unit
        case Some(home) =>
          ZIO.attemptBlocking {
            val dir = Path.of(home, ".cache", "histos", "advisories")
            Files.createDirectories(dir)
            Files.writeString(dir.resolve("querybatch.json"), body)
          }.ignore

  private def env(name: String): UIO[Option[String]] =
    zio.System.env(name).orElseSucceed(None).map(_.filter(_.nonEmpty))

  private def dryRun: UIO[Boolean] =
    env("HISTOS_DRY_RUN").map(_.exists(value => value == "true" || value == "1"))

  private def today: UIO[Date] =
    Clock.instant.map(now => Date.trusted(now.atOffset(ZoneOffset.UTC).toLocalDate.toString))

  /** The github.com token `gh` reads: `GH_TOKEN`, then `GITHUB_TOKEN`. An empty one is unset. */
  private def githubToken: UIO[Option[String]] =
    zio.System.envs.orElseSucceed(Map.empty).map(GitHubCredential.fromEnv(_, "github.com"))

  private def githubName(repo: Path): URIO[GitRead, Option[RepoName]] =
    env("GITHUB_REPOSITORY").flatMap(GitRead.github(repo, _))

  /** The commit a pull request names when git has no answer. */
  private val zero: CommitSha = CommitSha.trusted("0" * 40)

  private val unnamed: DepsResult =
    DepsResult(1, "no GitHub repository: set GITHUB_REPOSITORY or add a github.com origin\n")
end DepsSurface

/** Where `setup supply-chain --from` reads a zipx tree. `Zipx` is this checkout. */
enum SetupSource:
  case Zipx
  case Directory(raw: String)

/** Whether `setup supply-chain` writes `histos.scala`. */
enum PolicyWrite:
  case Preview
  case Write

/** Whether a zipx import asks before the old workflows would be deleted. */
enum ZipxConfirm:
  case Ask
  case Delete

/** Inserts the default policy. A second run changes nothing. */
object SetupSurface:
  def run(repo: Path, from: Option[SetupSource], write: PolicyWrite, confirm: ZipxConfirm): (Int, String) =
    val path = repo.resolve("histos.scala")
    if !Files.isRegularFile(path) then (2, "histos.scala is missing\n")
    else
      val source = Files.readString(path)
      val next   = SetupPlan.insert(source)
      val zip    = from.map(readZipx(repo, _))
      val wrote  =
        if write == PolicyWrite.Write && next != source then
          Files.writeString(path, next)
          "wrote histos.scala\n"
        else if next == source then "histos.scala already has .supplyChain\n"
        else "would write .supplyChain(SupplyChain())\n"
      val extra = zip.fold("")(identity)
      val ask   =
        if from.isDefined && confirm == ZipxConfirm.Ask then "\nconfirm before deleting the zipx workflows\n" else ""
      (0, wrote + extra + ask)

  private def readZipx(repo: Path, source: SetupSource): String =
    val root = source match
      case SetupSource.Zipx           => repo
      case SetupSource.Directory(raw) => repo.resolve(raw)
    def text(name: String): String =
      val path = root.resolve(name)
      if Files.isRegularFile(path) then Files.readString(path) else ""
    val versions   = text("project/ZipxVersions.scala")
    val workflows  = text(".github/workflows/zipx-version-updates.yml") + text(".github/workflows/zipx-pin-check.yml")
    val dependabot = text(".github/dependabot.yml")
    val report     = ZipxImport.read(versions, workflows, dependabot)
    val rows       = report.rows.map(row => s"  ${row.kind} ${row.text}").mkString("\n")
    val todos      = report.todos.map(line => s"  $line").mkString("\n")
    s"zipx\n$rows\n$todos\n"
end SetupSurface
