package histos.doctor

import histos.jdk.{JdkRead, JdkSource, Machine, Toolchain, ToolchainError}
import histos.native.NativeOrigin
import histos.select.SelectError
import histos.supply.{
  ApiCall,
  DoctorSurface,
  GitAnswers,
  Net,
  OutageStrategy,
  Reply,
  RepoName,
  SourceName,
  SupplyDiagnostic,
  SupplyFault,
}
import histos.toolchain.Architecture
import histos.central.{FeedError, ReleaseFeed}
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
import histos.{
  Aggregate,
  Module,
  NodeFloor,
  NodePath,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  RunnerTools,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
  ToolRevision,
}

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

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
          Fix.Steps(::("export SONATYPE_USERNAME and SONATYPE_PASSWORD, or list the repository secrets", Nil)),
        ),
        unread == Outcome.Skipped(
          HygieneChecks.credentialsId,
          Probe.JobEnv,
          Fix.Steps(::("retry when the portal answers", Nil)),
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
          Outcome.Skipped(HygieneChecks.pagesId, Probe.GitHubRead, Fix.Steps(::("retry when the GitHub API answers", Nil)))
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
        env.venue == histos.supply.Venue.Actions,
        JobEnv.from(Map.empty).venue == histos.supply.Venue.Laptop,
        JobEnv.from(Map("GITHUB_ACTIONS" -> "false")).venue == histos.supply.Venue.Laptop,
        JobEnv.empty.venue == histos.supply.Venue.Laptop,
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
    test("checks sort by id"):
      val checks = histos.doctor.Registry.collect(ProjectGraph.trusted(Nil), List(mark("b.b"), mark("a.a")))
      assertTrue(checks.map(_.id.value) == List("a.a", "b.b"))
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
              node    <- ToolchainChecks.nodeFloor(machine, ProjectGraph.trusted(List(jvm)), List(jvm))
              clang   <- ToolchainChecks.clang(machine, List(jvm))
              missing <- ToolchainChecks.clang(machine, List(native(histos.ClangPin.Unused)))
              split   <- ToolchainChecks.clang(machine, List(left, right))
            yield assertTrue(
              jdk.exists(found),
              read == JdkRead.Failed(ToolchainError.Absent(Toolchain.Floor), Some(JdkSource.Running)),
              node == List(Outcome.Ok(ToolchainChecks.nodeFloorId, "no Js module")),
              clang == List(Outcome.Ok(ToolchainChecks.clangId, "no Native module")),
              missing.exists(found),
              split.exists(found),
            )
    ,
    test("node.floor probes only the modules this run launches"):
      val program =
        ZIO.scoped:
          Workspace
            .work("histos-doctor-node-floor")
            .flatMap: repo =>
              val binary = repo.resolve("bin").resolve("node")
              val core   = jvm
              val web    = core.copy(name = ProjectName.trusted("web"), platforms = Platforms(Platform.Js, Nil))
              val ui     = core.copy(name = ProjectName.trusted("ui"), platforms = Platforms(Platform.Js, Nil))
              val batch  = Aggregate(ProjectName.trusted("batch"), List(core.name), SourcePos("histos.scala", 4, 1))
              val site   = Aggregate(ProjectName.trusted("site"), List(web.name), SourcePos("histos.scala", 5, 1))
              val tools  = RunnerTools(Some(NodeFloor(25)), None, None)
              val mixed  = ProjectGraph.trusted(
                List(core, web, ui, batch, site),
                Splits.aggregate,
                Catalog.empty,
                None,
                Nil,
                runnerTools = tools,
              )
              val typo                                                  = ProjectName.trusted("webx")
              def launched(names: List[ProjectName]): UIO[List[Module]] =
                DoctorLaunch.modules(mixed, names) match
                  case Left(error)    => ZIO.dieMessage(error.message)
                  case Right(modules) => ZIO.succeed(modules)
              for
                _    <- script(binary)
                home <- ZIO
                  .attempt(Option(java.lang.System.getProperty("java.home")).filter(_.nonEmpty))
                  .orDie
                  .flatMap:
                    case Some(value) => ZIO.succeed(Path.of(value))
                    case None        => ZIO.dieMessage("java.home is unset")
                machine = Machine(
                  Map("HISTOS_NODE" -> binary.toAbsolutePath.normalize.toString),
                  "/home/demo",
                  histos.jdk.Os.Mac,
                  Architecture.Aarch64,
                  repo,
                  repo,
                  Some(home),
                  NativeOrigin.Central,
                )
                batchModules <- launched(List(batch.name))
                siteModules  <- launched(List(site.name))
                pairModules  <- launched(List(web.name, ui.name))
                jvmFloor     <- ToolchainChecks.nodeFloor(machine, mixed, batchModules)
                jsFloor      <- ToolchainChecks.nodeFloor(machine, mixed, siteModules)
                pairFloor    <- ToolchainChecks.nodeFloor(machine, mixed, pairModules)
                idle         <- ToolchainChecks.nodeFloor(machine, mixed, Nil)
                jvmDoctor    <- doctor(repo, mixed, batchModules, machine)
                jsDoctor     <- doctor(repo, mixed, siteModules, machine)
                open         <- doctor(repo, mixed, Nil, machine)
              yield
                val unknown = DoctorLaunch.modules(mixed, List(typo))
                assertTrue(
                  DoctorLaunch.modules(mixed, Nil) == Right(Nil),
                  batchModules == List(core),
                  siteModules == List(web),
                  pairModules == List(web, ui),
                  (
                    unknown match
                      case Left(SelectError.Unknown(name, near)) =>
                        name == typo && near.contains("web")
                      case _ => false
                  ),
                  jvmFloor == List(Outcome.Ok(ToolchainChecks.nodeFloorId, "no Js module")),
                  idle == List(Outcome.Ok(ToolchainChecks.nodeFloorId, "this run does not launch Node")),
                  jsFloor.length == 1,
                  pairFloor.length == 1,
                  floorText(jsFloor).contains("web launches on Node"),
                  floorText(jsFloor).contains("24.22.2"),
                  floorText(jsFloor).contains("older than 25"),
                  floorText(pairFloor).contains("web, ui launch on Node"),
                  floorText(pairFloor).contains("24.22.2"),
                  floorText(pairFloor).contains("older than 25"),
                  !floorText(jvmFloor).contains("24.22.2"),
                  !floorText(idle).contains("24.22.2"),
                  jvmDoctor._1 == 0,
                  jsDoctor._1 == 1,
                  jsDoctor._2.contains("web launches on Node"),
                  jsDoctor._2.contains("node.floor"),
                  open._1 == 0,
                  open._2.contains("this run does not launch Node"),
                  !open._2.contains("x error"),
                )
      program.provideSomeLayer[TestEnvironment](stubbed[ReleaseFeed])
    // The doctor bounds each real probe with a timeout, which needs the clock that real work runs on.
    @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock,
    test("node.shadowed walks PATH only when this run launches Node from PATH"):
      val program =
        ZIO.scoped:
          Workspace
            .work("histos-doctor-node-shadow")
            .flatMap: repo =>
              val oldDir = repo.resolve("old")
              val newDir = repo.resolve("new")
              val older  = oldDir.resolve("node")
              val newer  = newDir.resolve("node")
              val core   = jvm
              val web    = core.copy(name = ProjectName.trusted("web"), platforms = Platforms(Platform.Js, Nil))
              val tools  = RunnerTools(Some(NodeFloor(25)), None, None)
              val named  = tools.copy(nodePath = Some(NodePath("/usr/bin/node")))
              val plain  = ProjectGraph.trusted(
                List(core, web),
                Splits.aggregate,
                Catalog.empty,
                None,
                Nil,
                runnerTools = tools,
              )
              val pinned = ProjectGraph.trusted(
                List(core, web),
                Splits.aggregate,
                Catalog.empty,
                None,
                Nil,
                runnerTools = named,
              )
              val path =
                s"${oldDir.toAbsolutePath}${java.io.File.pathSeparator}${newDir.toAbsolutePath}"
              def box(env: Map[String, String]): Machine =
                Machine(
                  env,
                  "/home/demo",
                  histos.jdk.Os.Mac,
                  Architecture.Aarch64,
                  repo,
                  repo,
                  None,
                  NativeOrigin.Central,
                )
              def versioned(path: Path, version: String): UIO[Unit] =
                ZIO
                  .attempt:
                    Option(path.getParent).foreach(parent => Files.createDirectories(parent))
                    Files.writeString(path, s"#!/bin/sh\necho $version\n")
                    Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwxr-xr-x"))
                    ()
                  .orDie
              val shadowed = Map("PATH" -> path)
              for
                _ <- versioned(older, "v20.18.0")
                _ <- versioned(newer, "v26.1.0")
                _ <- ZIO.attempt {
                  Files.createDirectories(repo.resolve(".histos"))
                  Files.writeString(repo.resolve(".histos").resolve("node.local"), "/opt/node-home\n")
                }.orDie
                idle  <- ToolchainChecks.nodeShadow(box(shadowed), plain, Nil)
                jvmOn <- ToolchainChecks.nodeShadow(box(shadowed), plain, List(core))
                envOn <- ToolchainChecks.nodeShadow(box(shadowed + ("HISTOS_NODE" -> newer.toString)), plain, List(web))
                blank <- ToolchainChecks.nodeShadow(box(shadowed + ("HISTOS_NODE" -> "")), plain, List(web))
                local <- ToolchainChecks.nodeShadow(box(shadowed), plain, List(web))
                _   <- ZIO.attempt(Files.writeString(repo.resolve(".histos").resolve("node.local"), "relative\n")).orDie
                bad <- ToolchainChecks.nodeShadow(box(shadowed), plain, List(web))
                _   <- ZIO.attempt(Files.delete(repo.resolve(".histos").resolve("node.local"))).orDie
                config <- ToolchainChecks.nodeShadow(box(shadowed), pinned, List(web))
                walked <- ToolchainChecks.nodeShadow(box(shadowed), plain, List(web))
              yield assertTrue(
                idle == List(Outcome.Ok(ToolchainChecks.shadowId, "this run does not launch Node")),
                jvmOn == List(Outcome.Ok(ToolchainChecks.shadowId, "no Js module")),
                envOn == List(
                  Outcome.Ok(ToolchainChecks.shadowId, "the launch uses HISTOS_NODE and does not read PATH")
                ),
                blank == envOn,
                local == List(
                  Outcome.Ok(ToolchainChecks.shadowId, "the launch uses .histos/node.local and does not read PATH")
                ),
                bad == local,
                config == List(
                  Outcome.Ok(ToolchainChecks.shadowId, "the launch uses runnerTools.nodePath and does not read PATH")
                ),
                !floorText(idle).contains("26.1.0"),
                !floorText(envOn).contains("26.1.0"),
                !floorText(local).contains("26.1.0"),
                !floorText(config).contains("26.1.0"),
                (
                  walked match
                    case List(Outcome.Found(ToolchainDiagnostic.NodeShadowed(_, summary))) =>
                      summary.contains("web launches on Node") &&
                      summary.contains("hides") &&
                      summary.contains("20.18.0") &&
                      summary.contains("26.1.0")
                    case _ => false
                ),
              )
      program
    ,
    test("clang.source probes only the modules this run links"):
      val program =
        ZIO.scoped:
          Workspace
            .work("histos-doctor-clang")
            .flatMap: repo =>
              val core  = jvm
              val link  = native(histos.ClangPin.Major(histos.ClangMajor(21))).copy(name = ProjectName.trusted("link"))
              val other = link.copy(name = ProjectName.trusted("other"))
              val batch = Aggregate(ProjectName.trusted("batch"), List(link.name), SourcePos("histos.scala", 4, 1))
              val tools = RunnerTools(None, Some(ToolRevision("21.1.8")), None)
              val mixed = ProjectGraph.trusted(
                List(core, link, other, batch),
                Splits.aggregate,
                Catalog.empty,
                None,
                Nil,
                runnerTools = tools,
              )
              def launched(names: List[ProjectName]): UIO[List[Module]] =
                DoctorLaunch.modules(mixed, names) match
                  case Left(error)    => ZIO.dieMessage(error.message)
                  case Right(modules) => ZIO.succeed(modules)
              for
                home <- ZIO
                  .attempt(Option(java.lang.System.getProperty("java.home")).filter(_.nonEmpty))
                  .orDie
                  .flatMap:
                    case Some(value) => ZIO.succeed(Path.of(value))
                    case None        => ZIO.dieMessage("java.home is unset")
                machine = Machine(
                  Map.empty,
                  "/home/demo",
                  histos.jdk.Os.Mac,
                  Architecture.Aarch64,
                  repo,
                  repo,
                  Some(home),
                  NativeOrigin.Central,
                )
                batchModules <- launched(List(batch.name))
                idle         <- ToolchainChecks.clang(machine, Nil)
                jvmClang     <- ToolchainChecks.clang(machine, List(core))
                one          <- ToolchainChecks.clang(machine, List(link))
                pair         <- ToolchainChecks.clang(machine, List(link, other))
                fromBatch    <- ToolchainChecks.clang(machine, batchModules)
                open         <- doctor(repo, mixed, Nil, machine)
                jvmDoctor    <- doctor(repo, mixed, List(core), machine)
                nativeDoctor <- doctor(repo, mixed, List(link), machine)
              yield assertTrue(
                DoctorLaunch.modules(mixed, Nil) == Right(Nil),
                batchModules == List(link),
                idle == List(Outcome.Ok(ToolchainChecks.clangId, "this run does not link Native")),
                jvmClang == List(Outcome.Ok(ToolchainChecks.clangId, "no Native module")),
                one.length == 1,
                pair.length == 1,
                fromBatch.length == 1,
                floorText(one).contains("link links with clang"),
                floorText(one).contains("clang is not on PATH"),
                floorText(pair).contains("link, other link with clang"),
                floorText(fromBatch).contains("link links with clang"),
                !floorText(idle).contains("clang is not on PATH"),
                !floorText(jvmClang).contains("clang is not on PATH"),
                open._1 == 0,
                open._2.contains("this run does not link Native"),
                !open._2.contains("x error"),
                jvmDoctor._1 == 0,
                jvmDoctor._2.contains("no Native module"),
                nativeDoctor._1 == 1,
                nativeDoctor._2.contains("link links with clang"),
                nativeDoctor._2.contains("clang.source"),
              )
      program.provideSomeLayer[TestEnvironment](stubbed[ReleaseFeed])
    @@ TestAspect.withLiveClock,
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

  private def doctor(
      repo: Path,
      graph: ProjectGraph,
      launch: List[Module],
      machine: Machine,
  ): URIO[ReleaseFeed, (Int, String)] =
    DoctorSurface
      .runWith(
        repo,
        graph,
        DoctorSurface.Request(Some("toolchain"), false, None, false, false, launch, true),
        machine,
        Quiet,
        Map.empty,
        None,
        false,
        _ => ZIO.dieMessage("doctor asked"),
        line => ZIO.dieMessage(s"doctor ran $line"),
        url => ZIO.dieMessage(s"doctor wrote $url"),
      )
      .provideSomeLayer[ReleaseFeed](GitAnswers().layer)

  private def floorText(outcomes: List[Outcome]): String =
    outcomes match
      case Outcome.Found(diagnostic) :: Nil => diagnostic.summary
      case Outcome.Ok(_, text) :: Nil       => text
      case other                            => other.toString

  private def script(path: Path): UIO[Unit] =
    ZIO
      .attempt:
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        Files.writeString(path, "#!/bin/sh\necho v24.22.2\n")
        Files.setPosixFilePermissions(path, PosixFilePermissions.fromString("rwxr-xr-x"))
        ()
      .orDie
end DoctorProbeLawSpec

private object Quiet extends Net:
  def send(
      method: String,
      url: String,
      body: String,
      headers: List[(String, String)],
  ): zio.IO[SupplyFault, Reply] =
    ZIO.succeed(Reply(404, ""))
