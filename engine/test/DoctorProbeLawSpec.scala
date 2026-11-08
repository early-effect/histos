package histos.doctor

import histos.jdk.{JdkRead, JdkSource, Machine, Toolchain, ToolchainError}
import histos.native.NativeOrigin
import histos.plugin.RepoPath
import histos.supply.{ApiCall, Net, OutageStrategy, Reply, RepoName, SourceName, SupplyDiagnostic, SupplyFault}
import histos.toolchain.Architecture
import histos.central.FeedError
import histos.publish.{LauncherChannel, LauncherChoice}
import histos.version.{
  BumpReason,
  Catalog,
  PinLag,
  Registry,
  RegistryError,
  Ship,
  ShipDiagnostic,
  ShipName,
  ShipNumber,
  SnapshotPointer,
  TagRelation,
}
import histos.workspace.Workspace
import histos.{HttpUrl, Module, Platform, Platforms, ProjectGraph, ProjectName, ScalaVersion, ScalaVersions, SourcePos}

import slayer.{any, stub, stubbed}

import zio.*
import zio.test.*

object DoctorProbeLawSpec extends ZIOSpecDefault:
  private val user     = "ada"
  private val password = "secret"
  private val repo     = RepoName.trusted("early-effect/histos")
  private val ship     = ShipName.trusted("core")
  private val number   = ShipNumber.trusted(1, 2, 3)
  private val module   = ProjectName.trusted("core")
  private val line     = row(ship, number, ::(module, Nil))

  private def row(name: ShipName, number: ShipNumber, modules: ::[ProjectName]): Ship.Line =
    Ship.Line(name, number, modules) match
      case line: Ship.Line => line
  private val catalog = Catalog.Lines(::(line, Nil))
  private val creds   = Map("SONATYPE_USERNAME" -> user, "SONATYPE_PASSWORD" -> password)

  def spec = suite("doctor probe laws")(
    test("central stays unread until both credentials exist, and a portal answer is not a pass by default"):
      val die = ZIO.dieMessage("sent"): IO[SupplyFault, Reply]
      for
        absent <- central(Map.empty, die)
        blank  <- central(Map("SONATYPE_USERNAME" -> "  ", "SONATYPE_PASSWORD" -> password), die)
        bad    <- central(creds + ("HISTOS_PORTAL" -> "notaurl"), die)
        named  <- central(creds + ("HISTOS_PORTAL" -> "https://portal.example.test/"), ZIO.succeed(Reply(200, "")))
      yield assertTrue(
        absent._1 == CentralProbe.Auth.NotRun && absent._2.isEmpty,
        blank._1 == CentralProbe.Auth.NotRun && blank._2.isEmpty,
        bad._1 == CentralProbe.Auth.NotRun && bad._2.isEmpty,
        named._2.exists(_.startsWith("https://portal.example.test/api/v1/publisher/status")),
      )
    ,
    test("central 401 and 403 are rejected, 404 is accepted, and 500 or a failed send is unreachable"):
      val codes = List(200, 404, 499, 401, 403, 500)
      for
        hits <- ZIO.foreach(codes)(code => central(creds, ZIO.succeed(Reply(code, ""))).map(_._1))
        down <- central(
          creds,
          ZIO.fail(SupplyFault.ServiceDown(SourceName.trusted("portal"), OutageStrategy.FailClosed)),
        )
      yield assertTrue(
        hits == List(
          CentralProbe.Auth.Accepted,
          CentralProbe.Auth.Accepted,
          CentralProbe.Auth.Accepted,
          CentralProbe.Auth.Rejected(401),
          CentralProbe.Auth.Rejected(403),
          CentralProbe.Auth.Unreachable,
        ),
        down._1 == CentralProbe.Auth.Unreachable,
        down._2.nonEmpty,
      )
    ,
    test("pages 401, 403, and a failed send are unreachable, and a workflow build with a missing environment is open"):
      for
        denied <- ZIO.foreach(List(401, 403))(code =>
          pages(Reply(code, ""), Reply(200, ""), Reply(200, ""), Reply(200, ""))
        )
        down  <- pagesFail
        ready <- pages(Reply(200, """{"build_type":"workflow"}"""), Reply(404, ""), Reply(200, ""), Reply(200, ""))
        other <- pages(Reply(200, "nope"), Reply(500, ""), Reply(200, ""), Reply(200, ""))
      yield assertTrue(
        denied.forall(_ == PagesProbe.Hit.Unreachable),
        down == PagesProbe.Hit.Unreachable,
        ready == PagesProbe.Hit.Ready(PagesProbe.Source.Actions, PagesProbe.Gate.Open),
        other == PagesProbe.Hit.Ready(PagesProbe.Source.Other, PagesProbe.Gate.Unread),
      )
    ,
    test("a null pages policy is open, a named branch policy opens only that branch, and protection follows the flag"):
      val openPolicy = Reply(200, """{"deployment_branch_policy":null}""")
      val custom     = Reply(200, """{"custom_branch_policies":true}""")
      val listed     = Reply(200, """{"branch_policies":[{"name":"main"}]}""")
      val other      = Reply(200, """{"branch_policies":[{"name":"docs"}]}""")
      val guard      = Reply(200, """{"protected_branches":true}""")
      for
        opened  <- pages(Reply(200, """{"build_type":"workflow"}"""), openPolicy, Reply(200, ""), Reply(200, ""))
        allowed <- pages(Reply(200, """{"build_type":"workflow"}"""), custom, listed, Reply(200, ""))
        blocked <- pages(Reply(200, """{"build_type":"workflow"}"""), custom, other, Reply(200, ""))
        unread  <- pages(Reply(200, """{"build_type":"workflow"}"""), custom, Reply(500, ""), Reply(200, ""))
        safe    <- pages(
          Reply(200, """{"build_type":"workflow"}"""),
          guard,
          Reply(200, ""),
          Reply(200, """{"protected":true}"""),
        )
        closed <- pages(
          Reply(200, """{"build_type":"workflow"}"""),
          guard,
          Reply(200, ""),
          Reply(200, """{"protected":false}"""),
        )
      yield assertTrue(
        opened == PagesProbe.Hit.Ready(PagesProbe.Source.Actions, PagesProbe.Gate.Open),
        allowed == PagesProbe.Hit.Ready(PagesProbe.Source.Actions, PagesProbe.Gate.Open),
        blocked == PagesProbe.Hit.Ready(PagesProbe.Source.Actions, PagesProbe.Gate.Blocks("main")),
        unread == PagesProbe.Hit.Ready(PagesProbe.Source.Actions, PagesProbe.Gate.Unread),
        safe == PagesProbe.Hit.Ready(PagesProbe.Source.Actions, PagesProbe.Gate.Open),
        closed == PagesProbe.Hit.Ready(PagesProbe.Source.Actions, PagesProbe.Gate.Blocks("main")),
      )
    ,
    test("a bad signing hex is bad and an empty env is unset, and neither asks a keyserver"):
      for
        bad   <- PgpProbe.read(Map("PGP_KEY_HEX" -> "nope"))
        unset <- PgpProbe.read(Map.empty)
      yield assertTrue(bad == PgpProbe.Hit.Bad, unset == PgpProbe.Hit.Unset)
    ,
    test("an unset signing hit is ok and a bad hex is found"):
      val unset = HygieneChecks.key(PgpProbe.Hit.Unset)
      val bad   = HygieneChecks.key(PgpProbe.Hit.Bad)
      assertTrue(
        unset == Outcome.Ok(HygieneChecks.keyId, "no signing fingerprint"),
        found(bad),
      )
    ,
    test("central credentials are not a pass when the portal was not asked"):
      val skipped  = HygieneChecks.credentials(catalog, Map.empty, None, CentralProbe.Auth.NotRun)
      val unread   = HygieneChecks.credentials(catalog, creds, None, CentralProbe.Auth.NotRun)
      val accepted = HygieneChecks.credentials(catalog, creds, None, CentralProbe.Auth.Accepted)
      val rejected = HygieneChecks.credentials(catalog, creds, None, CentralProbe.Auth.Rejected(401))
      val empty    = HygieneChecks.credentials(Catalog.Empty, Map.empty, None, CentralProbe.Auth.Unreachable)
      assertTrue(
        skipped == Outcome.Skipped(
          HygieneChecks.credentialsId,
          Probe.JobEnv,
          Fix.Command(
            ShellLine.trusted("export SONATYPE_USERNAME and SONATYPE_PASSWORD, or list the repository secrets")
          ),
        ),
        unread == Outcome.Skipped(
          HygieneChecks.credentialsId,
          Probe.JobEnv,
          Fix.Command(ShellLine.trusted("retry when the portal answers")),
        ),
        accepted == Outcome.Ok(HygieneChecks.credentialsId, "the portal accepted the token"),
        found(rejected),
        empty == Outcome.Ok(HygieneChecks.credentialsId, "no catalog"),
      )
    ,
    test("pages without a site is ok, and an unreachable site is not a pass"):
      val off   = HygieneChecks.pages(false, PagesProbe.Hit.Unreachable)
      val down  = HygieneChecks.pages(true, PagesProbe.Hit.Unreachable)
      val open  = HygieneChecks.pages(true, PagesProbe.Hit.Ready(PagesProbe.Source.Actions, PagesProbe.Gate.Open))
      val other = HygieneChecks.pages(true, PagesProbe.Hit.Ready(PagesProbe.Source.Other, PagesProbe.Gate.Open))
      assertTrue(
        off == List(Outcome.Ok(HygieneChecks.pagesId, "no pages site")),
        down == List(
          Outcome.Skipped(
            HygieneChecks.pagesId,
            Probe.GitHubRead,
            Fix.Command(ShellLine.trusted("retry when the GitHub API answers")),
          )
        ),
        open == List(Outcome.Ok(HygieneChecks.pagesId, "Pages deploys from GitHub Actions")),
        other.exists(found),
      )
    ,
    test("a refusal is 401, 403, 404, 422, or the integration sentence, and 200 or 500 is not"):
      val call   = ApiCall.Workflows
      val phrase = "Resource not accessible by integration"
      val known  = List(401, 403, 404, 422).forall(status =>
        RefusalMap.refused(call, status, "no").contains(SupplyDiagnostic.GitHubRefused(call, status, "no"))
      )
      assertTrue(
        known,
        RefusalMap.refused(call, 200, phrase).contains(SupplyDiagnostic.GitHubRefused(call, 200, phrase)),
        RefusalMap.refused(call, 200, "ok").isEmpty,
        RefusalMap.refused(call, 500, "down").isEmpty,
      )
    ,
    test("print changes nothing, yes-mode skips a GitHub setting, and a local edit replaces the file text"):
      val file    = RepoPath.trusted("histos.scala")
      val files   = Map(file.value -> "before")
      val edit    = Fix.Config(file, Edit("before", "after"))
      val command = Fix.Command(ShellLine.trusted("echo ok"))
      val setting = Fix.Settings(HttpUrl.trusted("https://example.test/settings"), "click")
      val printed = FixApply.applyAll(List(edit, command, setting), FixMode.Print, _ => FixApply.Answer.Yes, files)
      val yes     = FixApply.applyAll(
        List(edit, command, setting),
        FixMode.from(fix = true, yes = true),
        _ => FixApply.Answer.Yes,
        files,
      )
      val denied =
        FixApply.applyAll(List(setting), FixMode.from(fix = true, yes = false), _ => FixApply.Answer.No, files)
      assertTrue(
        printed == (files, Nil),
        yes._1.get(file.value).contains("after"),
        yes._2 == List("echo ok"),
        denied == (files, Nil),
      )
    ,
    test("fix mode prints unless fix is set, and yes excludes the GitHub setting"):
      val modes = List(true, false).flatMap(fix => List(true, false).map(yes => (fix, yes)))
      assertTrue:
        modes.forall: (fix, yes) =>
          FixMode.from(fix, yes) match
            case FixMode.Print        => !fix
            case FixMode.Apply(kinds) =>
              fix &&
              kinds.contains(FixKind.LocalEdit) &&
              kinds.contains(FixKind.HistosCommand) &&
              kinds.contains(FixKind.GitHubSetting) == !yes
    ,
    test("job env reads secret presence and the app step, and a token follows the host"):
      val env = JobEnv.from(
        Map(
          "HISTOS_SECRET_PRESENT_HISTOS_APP_ID" -> "true",
          "HISTOS_SECRET_PRESENT_OTHER"         -> "false",
          "HISTOS_STEP_APP"                     -> "cancelled",
          "GITHUB_ACTIONS"                      -> "true",
          "GITHUB_TOKEN"                        -> "",
        )
      )
      val hosts = List("github.com", "localhost", "acme.ghe.com")
      assertTrue(
        env.present.get("HISTOS_APP_ID").contains(true),
        env.present.get("OTHER").contains(false),
        env.step.contains(histos.supply.StepOutcome.Failure),
        env.actions,
        env.githubToken.isEmpty,
        hosts.forall(host =>
          GitHubCredential.fromEnv(Map("GH_TOKEN" -> "", "GITHUB_TOKEN" -> "hub"), host).contains("hub")
        ),
        GitHubCredential.fromEnv(Map("GH_TOKEN" -> "nope"), "example.com").isEmpty,
        GitHubCredential.fromEnv(Map("GH_ENTERPRISE_TOKEN" -> "ent"), "example.com").contains("ent"),
        GitHubCredential.githubHost("github.com"),
      )
    ,
    test("an empty catalog is ok, a missing releases file is found, and a matches line is a bump"):
      val empty   = ShipChecks.outcomes(Catalog.Empty, Map.empty, ReleasesFile.Missing, Map.empty)
      val missing = ShipChecks.releases(catalog, ReleasesFile.Missing)
      val garbage = ShipChecks.releases(catalog, ReleasesFile.Text("not a line"))
      val read    = ShipChecks.releases(catalog, ReleasesFile.Text("core matches"))
      val bump    = ShipChecks.bumps(catalog, ReleasesFile.Text("core matches"), Map.empty)
      val moved   = ShipChecks.bumps(catalog, ReleasesFile.Text("core moved"), Map.empty)
      val unknown = ShipChecks.bumps(catalog, ReleasesFile.Text("core unknown"), Map.empty)
      val session = ShipChecks.session(catalog, Map(histos.version.Session.Key -> "nope"))
      assertTrue(
        empty.forall(passed),
        found(missing),
        found(garbage),
        read == Outcome.Ok(ShipChecks.releasesId, "releases was read"),
        bump == List(Outcome.Found(ShipDiagnostic.ShouldBump(ship, number, BumpReason.Tagged))),
        moved == List(Outcome.Found(ShipDiagnostic.ShouldBump(ship, number, BumpReason.Shadowed))),
        unknown == List(Outcome.Ok(ShipChecks.bumpId, "catalog numbers are not released")),
        found(session),
        Registry.parse(List("core matches", "core moved")) == Left(RegistryError.Duplicate(ship)),
        TagRelation.from("matches") == Right(TagRelation.Matches),
      )
    ,
    test("a listed release at or above the catalog number is a bump, below is not, and no answer is skipped"):
      val part    = Gen.int(0, 3)
      val numbers = (part <*> part <*> part).map((major, minor, patch) => ShipNumber.trusted(major, minor, patch))
      val down    = FeedError.Unreachable("https://repo.test/maven-metadata.xml")
      check(numbers): listed =>
        val published = ShipChecks.bumps(catalog, ReleasesFile.Missing, Map(ship -> Listing.Newest(Some(listed))))
        val expected  =
          if Ordering[ShipNumber].gteq(listed, number) then
            List(Outcome.Found(ShipDiagnostic.ShouldBump(ship, number, BumpReason.Published(listed))))
          else List(Outcome.Ok(ShipChecks.bumpId, "releases was not read"))
        val tagged =
          ShipChecks.bumps(catalog, ReleasesFile.Text("core matches"), Map(ship -> Listing.Unreachable(down)))
        val none = ShipChecks.bumps(catalog, ReleasesFile.Missing, Map(ship -> Listing.Newest(None)))
        val gone = ShipChecks.bumps(catalog, ReleasesFile.Missing, Map(ship -> Listing.Unreachable(down)))
        assertTrue(
          published == expected,
          tagged == List(Outcome.Found(ShipDiagnostic.ShouldBump(ship, number, BumpReason.Tagged))),
          none == List(Outcome.Ok(ShipChecks.bumpId, "releases was not read")),
          gone.collect { case Outcome.Skipped(id, probe, _) => (id, probe) } ==
            List((ShipChecks.bumpId, Probe.Registry)),
        )
    ,
    test("a pin a published release passed is that lag, a current pin is ok, and no answer is skipped"):
      val newer   = ShipNumber.trusted(1, 3, 0)
      val down    = FeedError.Status("https://repo.test/maven-metadata.xml", 500)
      val release = LauncherChoice.Release(number)
      val line    = LauncherChoice.Pointer(LauncherChannel.Line(SnapshotPointer(number)))
      assertTrue(
        ShipChecks.pin(Some(release), Listing.Newest(Some(newer))) ==
          Outcome.Found(ShipDiagnostic.PinBehind(PinLag.NewerRelease(number, newer))),
        ShipChecks.pin(Some(line), Listing.Newest(Some(number))) ==
          Outcome.Found(ShipDiagnostic.PinBehind(PinLag.LineReleased(number, number))),
        passed(ShipChecks.pin(Some(release), Listing.Newest(Some(number)))),
        passed(ShipChecks.pin(Some(line), Listing.Newest(None))),
        passed(ShipChecks.pin(None, Listing.Newest(Some(newer)))),
        ShipChecks.pin(Some(release), Listing.Unreachable(down)) match
          case Outcome.Skipped(id, probe, _) => id == ShipChecks.pinId && probe == Probe.Registry
          case _                             => false,
      )
    ,
    test("json quotes a title, and checks sort by id"):
      val report =
        DoctorReport(List(Outcome.Found(Note("a \"quote\" and a \\ slash"))), List(CheckId.trusted("demo.late")))
      val graph  = ProjectGraph.trusted(Nil)
      val checks = histos.doctor.Registry.collect(graph, List(mark("b.b"), mark("a.a")))
      assertTrue(
        Render.json(report).contains("""a \"quote\" and a \\ slash"""),
        Render.json(report).contains(""""timedOut":["demo.late"]"""),
        checks.map(_.id.value) == List("a.a", "b.b"),
      )
    ,
    test("a jvm graph does not probe node or clang, and a missing jdk is not ok"):
      ZIO.scoped:
        Workspace
          .work("histos-doctor-jdk")
          .flatMap: repo =>
            val machine = Machine(
              Map.empty,
              "/home/demo",
              histos.jdk.Os.Mac,
              Architecture.Aarch64,
              repo,
              repo,
              None,
              NativeOrigin.Central,
            )
            val left  = native(histos.ClangPin.Major(histos.ClangMajor(21))).copy(name = ProjectName.trusted("left"))
            val right = native(histos.ClangPin.Major(histos.ClangMajor(22))).copy(name = ProjectName.trusted("right"))
            for
              jdk     <- ToolchainChecks.jdk(machine)
              read    <- Toolchain.read(machine)
              node    <- ToolchainChecks.nodeFloor(machine, ProjectGraph.trusted(List(jvm)))
              clang   <- ToolchainChecks.clang(machine, ProjectGraph.trusted(List(jvm)))
              missing <- ToolchainChecks.clang(machine, ProjectGraph.trusted(List(native(histos.ClangPin.Unused))))
              split   <- ToolchainChecks.clang(machine, ProjectGraph.trusted(List(left, right)))
            yield assertTrue(
              jdk.exists(found),
              read == JdkRead.Failed(ToolchainError.Absent(Toolchain.Floor), Some(JdkSource.Running)),
              node == List(Outcome.Ok(ToolchainChecks.nodeFloorId, "no Js module")),
              clang == List(Outcome.Ok(ToolchainChecks.clangId, "no Native module")),
              missing.exists(found),
              split.exists(found),
            ),
  )

  private def central(env: Map[String, String], reply: IO[SupplyFault, Reply]): UIO[(CentralProbe.Auth, List[String])] =
    val program =
      for
        seen <- Ref.make(List.empty[String])
        _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
          (_: String, url: String, _: String, _: List[(String, String)]) => seen.update(url :: _) *> reply
        )
        net  <- ZIO.service[Net]
        auth <- CentralProbe.read(net, env)
        urls <- seen.get
      yield (auth, urls)
    program.provide(stubbed[Net])

  private def pages(pages: Reply, env: Reply, policies: Reply, branch: Reply): UIO[PagesProbe.Hit] =
    val program =
      for
        _ <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
          (_: String, url: String, _: String, _: List[(String, String)]) =>
            ZIO.succeed(page(url, pages, env, policies, branch))
        )
        net <- ZIO.service[Net]
        hit <- PagesProbe.read(net, repo, "main", "token")
      yield hit
    program.provide(stubbed[Net])

  private def pagesFail: UIO[PagesProbe.Hit] =
    val down    = ZIO.fail(SupplyFault.ServiceDown(SourceName.trusted("github"), OutageStrategy.FailClosed))
    val program =
      for
        _ <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
          (_: String, _: String, _: String, _: List[(String, String)]) => down
        )
        net <- ZIO.service[Net]
        hit <- PagesProbe.read(net, repo, "main", "token")
      yield hit
    program.provide(stubbed[Net])

  private def page(url: String, pages: Reply, env: Reply, policies: Reply, branch: Reply): Reply =
    if url.endsWith("/pages") then pages
    else if url.endsWith("/deployment-branch-policies") then policies
    else if url.contains("/environments/github-pages") then env
    else if url.contains("/branches/") then branch
    else Reply(500, url)

  private def passed(outcome: Outcome): Boolean =
    outcome match
      case Outcome.Ok(_, _) => true
      case _                => false

  private def found(outcome: Outcome): Boolean =
    outcome match
      case Outcome.Found(_) => true
      case _                => false

  private def mark(raw: String): DoctorCheck =
    val checkId = CheckId.trusted(raw)
    new DoctorCheck:
      def id: CheckId                                 = checkId
      def area: Area                                  = Area.Toolchain
      def needs: Set[Probe]                           = Set.empty
      def run(ctx: DoctorContext): UIO[List[Outcome]] =
        ZIO.succeed(if ctx.jobs.isEmpty then Nil else Nil)

  private final case class Note(title: String) extends Diagnostic:
    def check: CheckId  = CheckId.trusted("demo.mark")
    def level: Level    = Level.Error
    def summary: String = "sum"
    def explain: String = "why"
    def fixes: ::[Fix]  = ::(Fix.Command(ShellLine.trusted("true")), Nil)

  private def jvm: Module =
    Module(
      module,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(ScalaVersion.trusted("3.3.7"), Nil),
      Nil,
      Nil,
      SourcePos("histos.scala", 1, 1),
    )

  private def native(pin: histos.ClangPin): Module =
    jvm.copy(platforms = Platforms(Platform.Native, Nil), clang = pin)
end DoctorProbeLawSpec
