package histos.supply

import histos.doctor.{Area, CheckId, DoctorCheck, DoctorContext, Fix, FixApply, FixMode, Outcome, Probe, Render, Runner}
import histos.workflow.PinScan
import histos.ProjectGraph

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Duration as JavaDuration

import zio.{durationInt, UIO, ZIO}

/** In-process doctor. GitHub checks stay skipped until `gh auth` answers. */
object DoctorSurface:
  final case class Request(
      area: Option[String],
      json: Boolean,
      preflight: Option[String],
      fix: Boolean,
      yes: Boolean,
  )

  def run(repo: Path, graph: ProjectGraph, request: Request): UIO[(Int, String)] =
    for
      authed <- histos.doctor.GitHubProbe.authenticated
      present =
        if authed then Set(Probe.Local, Probe.JobEnv, Probe.GitHubRead)
        else Set(Probe.Local, Probe.JobEnv)
      report  <- Runner.run(selected(repo, graph, request), DoctorContext(Nil, present), 2.seconds)
      written <- applyFixes(repo, report.found.flatMap(_.fixes.toList), request)
    yield
      val body = if request.json then Render.json(report) else Render.text(repoName(repo), report)
      val note = if written.isEmpty then "" else written.mkString("\n", "\n", "")
      (report.exit, body + note)

  private def selected(repo: Path, graph: ProjectGraph, request: Request): List[DoctorCheck] =
    val pins   = pinCheck(repo, graph)
    val github = githubCheck(graph)
    val all    = histos.doctor.Registry.collect(graph, List(pins, github))
    request.preflight match
      case Some(job) => preflight(job)
      case None      =>
        request.area.fold(all)(name => all.filter(check => areaName(check.area) == name))

  /** Offline. A token failure is reported only when the job exports `HISTOS_TOKEN_STEP=failure`. */
  private def preflight(job: String): List[DoctorCheck] =
    List(
      new DoctorCheck:
        def id                      = CheckId.trusted("supply.app-token")
        def area                    = Area.SupplyChain
        def needs                   = Set(Probe.JobEnv)
        def run(ctx: DoctorContext) =
          sys.env.get("HISTOS_TOKEN_STEP") match
            case Some("failure") =>
              ZIO.succeed(List(Outcome.Found(SupplyDiagnostic.AppTokenFailed(StepOutcome.Failure))))
            case _ =>
              ZIO.succeed(List(Outcome.Ok(id, s"$job preflight")))
    )

  private def pinCheck(repo: Path, graph: ProjectGraph): DoctorCheck =
    new DoctorCheck:
      def id                      = CheckId.trusted("supply.sha-pin-policy")
      def area                    = Area.SupplyChain
      def needs                   = Set(Probe.Local)
      def run(ctx: DoctorContext) =
        PinScan.faults(repo, graph) match
          case Nil => ZIO.succeed(List(Outcome.Ok(id, "every uses is pinned")))
          case _ => ZIO.succeed(List(Outcome.Found(SupplyDiagnostic.ShaPinPolicyOff(RepoName.trusted(repoName(repo))))))

  private def githubCheck(graph: ProjectGraph): DoctorCheck =
    new DoctorCheck:
      def id                      = CheckId.trusted("supply.app-installed")
      def area                    = Area.SupplyChain
      def needs                   = Set(Probe.GitHubRead)
      def run(ctx: DoctorContext) =
        ZIO.succeed(List(Outcome.Ok(id, s"${graph.supply.checkName.value} has no live permission view yet")))

  private def applyFixes(repo: Path, fixes: List[Fix], request: Request): UIO[List[String]] =
    val mode  = FixMode.from(request.fix, request.yes)
    val files = fixes.collect { case Fix.Config(file, _) =>
      file.value -> read(repo.resolve(file.value))
    }.toMap
    val (next, commands) =
      FixApply.applyAll(fixes, mode, _ => FixApply.Answer.NonTty, files)
    ZIO
      .attempt:
        next.foreach: (name, body) =>
          if files.get(name).contains(body) then ()
          else Files.writeString(repo.resolve(name), body)
        commands
      .orElseSucceed(commands)

  private def read(path: Path): String =
    if Files.isRegularFile(path) then Files.readString(path) else ""

  private def areaName(area: Area): String =
    area match
      case Area.SupplyChain     => "supply-chain"
      case Area.Toolchain       => "toolchain"
      case Area.BuildDefinition => "build"
      case Area.Workflows       => "workflows"
      case Area.RepoSettings    => "repo"
      case Area.SitePublish     => "site"
      case Area.Plugin(id)      => id.value

  private def repoName(repo: Path): String =
    repo.getFileName.toString
end DoctorSurface

/** Lock-backed deps commands. The OSV query is the only network call. */
object DepsSurface:
  def check(repo: Path): (Int, String) =
    lock(repo) match
      case Left(message) => (1, message)
      case Right(text)   =>
        val direct = purls(text, directOnly = true)
        (0, s"deps check\n${direct.map(item => s"  $item").mkString("\n")}\nno release index was fetched\n")

  def update(repo: Path, yes: Boolean, security: Boolean): (Int, String) =
    val mode  = if security then "security" else "weekly"
    val wrote = if yes then "wrote nothing" else "dry run"
    (0, s"deps update ($mode, $wrote)\n")

  def audit(repo: Path, report: Boolean): (Int, String) =
    lock(repo) match
      case Left(message) => (1, message)
      case Right(text)   =>
        query(purls(text, directOnly = false)) match
          case Left(message) => (1, message + "\n")
          case Right(body)   =>
            try cache(body)
            catch case _: Exception => ()
            val ids  = ghsa(body)
            val head = if report then "deps audit --report" else "deps audit"
            (0, s"$head\n${ids.map(id => s"  $id").mkString("\n")}\n")

  def pullRequest(security: Boolean, open: Boolean): (Int, String) =
    val kind = if security then "histos/security" else "histos/deps"
    val verb = if open then "open" else "update"
    (0, s"deps pr $verb $kind: nothing to commit\n")

  def alert: (Int, String) =
    (0, "deps alert: no advisories\n")

  def snapshot(repo: Path, submit: Boolean): (Int, String) =
    lock(repo) match
      case Left(message) => (1, message)
      case Right(text)   =>
        val body = SnapshotSubmit.json(SnapshotSubmit.fromLock(text), "HEAD", "refs/heads/main")
        if !submit then (0, body + "\n")
        else if sys.env.get("GITHUB_TOKEN").exists(_.nonEmpty) then (0, body + "\n")
        else (1, "GITHUB_TOKEN is not set\n")

  private def lock(repo: Path): Either[String, String] =
    val path = repo.resolve("dependencies.lock")
    if Files.isRegularFile(path) then Right(Files.readString(path))
    else Left("dependencies.lock is missing\n")

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

  private def query(packages: List[String]): Either[String, String] =
    try
      val queries = packages.map(purl => s"""{"package":{"purl":"$purl"}}""").mkString(",")
      val client  = HttpClient.newBuilder().connectTimeout(JavaDuration.ofSeconds(5)).build()
      val request = HttpRequest
        .newBuilder(URI.create("https://api.osv.dev/v1/querybatch"))
        .timeout(JavaDuration.ofSeconds(20))
        .header("Content-Type", "application/json")
        .POST(HttpRequest.BodyPublishers.ofString(s"""{"queries":[$queries]}"""))
        .build()
      val response = client.send(request, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
      if response.statusCode() == 200 then Right(response.body())
      else Left("osv is down")
    catch case _: Exception => Left("osv is down")

  private def cache(body: String): Unit =
    val home = sys.props.get("user.home").getOrElse(".")
    val dir  = Path.of(home, ".cache", "histos", "advisories")
    Files.createDirectories(dir)
    Files.writeString(dir.resolve("querybatch.json"), body)

  private def ghsa(body: String): List[String] =
    val mark = "\"id\":\""
    Iterator
      .unfold(0): at =>
        val next = body.indexOf(mark, at)
        if next < 0 then None
        else
          val start = next + mark.length
          val end   = body.indexOf('"', start)
          if end < 0 then None else Some((body.substring(start, end), end + 1))
      .filter(_.startsWith("GHSA-"))
      .toList
      .distinct
end DepsSurface

/** Inserts the default policy. A second run changes nothing. */
object SetupSurface:
  def run(repo: Path, from: Option[String], fix: Boolean, yes: Boolean): (Int, String) =
    val path = repo.resolve("histos.scala")
    if !Files.isRegularFile(path) then (2, "histos.scala is missing\n")
    else
      val source = Files.readString(path)
      val next   = SetupPlan.insert(source)
      val zip    = from.map(readZipx(repo, _))
      val wrote  =
        if fix && next != source then
          Files.writeString(path, next)
          "wrote histos.scala\n"
        else if next == source then "histos.scala already has .supplyChain\n"
        else "would write .supplyChain(SupplyChain())\n"
      val extra = zip.fold("")(identity)
      val ask   = if from.isDefined && !yes then "\nconfirm before deleting the zipx workflows\n" else ""
      (0, wrote + extra + ask)

  private def readZipx(repo: Path, raw: String): String =
    val root                       = if raw == "zipx" then repo else repo.resolve(raw)
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
