package histos.supply

import histos.cli.{Attached, Stdio}
import histos.doctor.{Edit, Fix, GitHubCredential, GitHubPut, HistosCommand}
import histos.plugin.RepoPath
import histos.version.CommitSha
import histos.HttpUrl
import histos.workspace.Workspace

import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions

import slayer.{any, stub, stubbed}

import zio.*
import zio.test.*

object SupplySurfaceLawSpec extends ZIOSpecDefault:
  private val sha = CommitSha.trusted("a" * 40)
  private val hex = "ab" * 32

  /** Bytes that are not UTF-8, so `readString` refuses them. */
  private val notText = Array[Byte](0xc3.toByte, 0x28)

  def spec = suite("supply surface laws")(
    test("a fix runs only the histos commands it names, and prose or a person's command never reaches the shell"):
      val repoName = RepoName.trusted("early-effect/histos")
      val found    = List(
        SupplyDiagnostic.SignedCommitsConflict(
          RuleSource.Absent,
          CommitSigning.GitPush(Identity.FirstDeveloper),
          Set(MergeMethod.Rebase),
        ),
        SupplyDiagnostic.DependencyGraphOff(repoName),
        SupplyDiagnostic.DependabotAlertsOff(repoName),
        SupplyDiagnostic.DefaultTokenWrite(repoName),
        SupplyDiagnostic.ScheduleDisabled(RepoPath(".github/workflows/ci.yml"), repoName),
        histos.version.ShipDiagnostic.Releases("releases was not read"),
        histos.version.ShipDiagnostic.Session("HISTOS_SHA is missing"),
        histos.doctor.ToolchainDiagnostic.NodeShadowed("export HISTOS_NODE=/opt/node/bin/node", "node 22 is first"),
        SupplyDiagnostic.DependabotOverlap(::(Ecosystem.Maven, Nil), RepoPath(".github/dependabot.yml")),
        SupplyDiagnostic.WorkflowFiles,
      )
      val program =
        for
          repo <- Workspace.work("surface-prose")
          done <- fixed(repo, found.flatMap(_.fixes.toList), fixYes, Stdio.redirected)
        yield assertTrue(
          done.ran == List("./histos setup supply-chain --fix", "./histos workflow --update"),
          done.sent.isEmpty,
          done.notes == List(
            "ran: ./histos setup supply-chain --fix",
            "ran: ./histos workflow --update",
            "not applied (no terminal): https://github.com/early-effect/histos/settings/security_analysis",
            "not applied (open it in a browser): https://github.com/early-effect/histos/actions",
          ),
        )
      program
    ,
    test("fix off applies nothing, --yes does not confirm a GitHub setting, and a page with no call is a person's"):
      val program =
        for
          repo   <- Workspace.work("surface-fix")
          off    <- fixed(repo, List(command("workflow", "--update")), fixOff, person)
          yes    <- fixed(repo, List(setting("https://example.test/settings")), fixYes, person)
          open   <- fixed(repo, List(page("https://example.test/secrets")), fixOn, person)
          prompt <- TestConsole.output
        yield assertTrue(
          off == Applied(Nil, Nil, Nil),
          yes == Applied(
            List("not applied (--yes does not confirm GitHub settings): https://example.test/settings"),
            Nil,
            Nil,
          ),
          open == Applied(List("not applied (open it in a browser): https://example.test/secrets"), Nil, Nil),
          prompt.isEmpty,
        )
      program
    ,
    test("a person's no skips a setting, a yes sends its call, no terminal sends nothing, and an edit lands"):
      val program =
        for
          repo <- Workspace.work("surface-yes")
          path = repo.resolve("histos.scala")
          _       <- Workspace.writeString(path, "before")
          _       <- TestConsole.feedLines("n")
          skipped <- fixed(repo, List(setting("https://example.test/settings")), fixOn, person)
          _       <- TestConsole.feedLines("y")
          applied <- fixed(repo, List(setting("https://example.test/rules")), fixOn, person)
          absent  <- fixed(repo, List(setting("https://example.test/rules")), fixOn, Stdio.redirected)
          wrote   <- fixed(
            repo,
            List(Fix.Config(RepoPath.trusted("histos.scala"), Edit("before", "after"))),
            fixOn,
            Stdio.redirected,
          )
          text   <- Workspace.readString(path)
          prompt <- TestConsole.output
        yield assertTrue(
          skipped == Applied(List("skipped: https://example.test/settings"), Nil, Nil),
          applied == Applied(
            List("applied: https://example.test/rules"),
            Nil,
            List("PUT https://example.test/rules/api {} Bearer token"),
          ),
          absent == Applied(List("not applied (no terminal): https://example.test/rules"), Nil, Nil),
          wrote == Applied(List("wrote histos.scala"), Nil, Nil),
          text == "after",
          prompt == Vector(
            "Apply GitHub setting https://example.test/settings? [y/N] \n",
            "Apply GitHub setting https://example.test/rules? [y/N] \n",
          ),
        )
      program
    ,
    test("a local edit whose file is not text is that read failure, leaves the bytes, and the readable edit lands"):
      val program =
        for
          repo <- Workspace.work("surface-unread-fix")
          kept = repo.resolve("histos.scala")
          bad  = repo.resolve(".scalafmt.conf")
          _    <- Workspace.writeString(kept, "before")
          _    <- ZIO.attemptBlocking(Files.write(bad, notText)).orDie
          done <- fixed(
            repo,
            List(
              Fix.Config(RepoPath.trusted(".scalafmt.conf"), Edit("before", "after")),
              Fix.Config(RepoPath.trusted("histos.scala"), Edit("before", "after")),
            ),
            fixOn,
            Stdio.redirected,
          )
          text  <- Workspace.readString(kept)
          bytes <- ZIO.attemptBlocking(Files.readAllBytes(bad).toList).orDie
        yield assertTrue(
          done.notes == List(s"not applied: could not read $bad", "wrote histos.scala"),
          text == "after",
          bytes == notText.toList,
        )
      program
    ,
    test("a local edit whose path is a directory is that write failure, and the writable edit lands"):
      val program =
        for
          repo <- Workspace.work("surface-unwritten-fix")
          kept = repo.resolve("histos.scala")
          held = repo.resolve(".scalafmt.conf")
          _    <- Workspace.writeString(kept, "before")
          _    <- Workspace.createDirectories(held)
          done <- fixed(
            repo,
            List(
              Fix.Config(RepoPath.trusted(".scalafmt.conf"), Edit("", "version = 3.11.5\n")),
              Fix.Config(RepoPath.trusted("histos.scala"), Edit("before", "after")),
            ),
            fixOn,
            Stdio.redirected,
          )
          text <- Workspace.readString(kept)
          dir  <- ZIO.attemptBlocking(Files.isDirectory(held)).orDie
        yield assertTrue(
          done.notes == List(s"not applied: could not write $held", "wrote histos.scala"),
          text == "after",
          dir,
        )
      program
    ,
    test("a missing lock is a failure, and an audit that cannot reach osv does not pretend the scan passed"):
      val program =
        for
          repo <- Workspace.work("surface-deps")
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, _: String, _: String, _: List[(String, String)]) => ZIO.die(new IllegalStateException("sent"))
          )
          net     <- ZIO.service[Net]
          missing <- DepsSurface.check(repo, SupplyChain(), net)
          audit   <- DepsSurface.audit(repo, false, net)
          _       <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _       <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, _: String, _: String, _: List[(String, String)]) =>
              ZIO.fail(SupplyFault.ServiceDown(SourceName.trusted("osv"), OutageStrategy.FailClosed))
          )
          down <- DepsSurface.audit(repo, false, net)
        yield assertTrue(
          missing.code == 1 && missing.text.contains("dependencies.lock is missing"),
          audit.code == 1 && audit.text.contains("dependencies.lock is missing"),
          down.code == 1 && down.text.contains("osv is down"),
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net])
    ,
    test("a lock, a build file, or a changed file that is not text is that read failure, and nothing is sent"):
      val program =
        for
          repo <- Workspace.work("surface-unread")
          path  = repo.resolve("dependencies.lock")
          build = repo.resolve("histos.scala")
          _ <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) => ZIO.dieMessage(s"sent $url")
          )
          net      <- ZIO.service[Net]
          _        <- TestSystem.putEnv("GH_TOKEN", "token")
          _        <- ZIO.attemptBlocking(Files.write(path, notText)).orDie
          checked  <- DepsSurface.check(repo, SupplyChain(), net)
          updated  <- DepsSurface.update(repo, SupplyChain(), net, true, false)
          audited  <- DepsSurface.audit(repo, false, net)
          alerted  <- DepsSurface.alert(repo, SupplyChain(), net)
          snapshot <- DepsSurface.snapshot(repo, net, false)
          _        <- Workspace.writeString(path, lock("1.0.0"))
          _        <- ZIO.attemptBlocking(Files.write(build, notText)).orDie
          planned  <- DepsSurface.check(repo, SupplyChain(), net)
          pulled   <- DepsSurface.pullRequest(repo, SupplyChain(), net, false, true)
        yield
          val unread = DepsSurface.DepsResult(1, s"could not read $path\n")
          assertTrue(
            checked == unread,
            updated == unread,
            audited == unread,
            alerted == unread,
            snapshot == unread,
            planned == DepsSurface.DepsResult(1, s"could not read $build\n"),
            pulled == DepsSurface.DepsResult(1, s"could not read $build\n"),
          )
      program.provideSomeLayer[TestEnvironment & Scope](
        stubbed[Net] ++ GitAnswers(remote = Some(RepoName.trusted("acme/demo")), diff = List("histos.scala")).layer
      )
    ,
    test("an update with --yes names a workflow file it cannot read, and writes nothing"):
      val source  = "dep\"org.example::demo:1.0.0\"\n"
      val page    = """{"response":{"docs":[{"v":"1.2.3","timestamp":1600000000000}]}}"""
      val program =
        for
          repo <- Workspace.work("surface-update-unread")
          workflow = repo.resolve(".github").resolve("workflows").resolve("ci.yml")
          build    = repo.resolve("histos.scala")
          _       <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _       <- Workspace.writeString(build, source)
          _       <- Workspace.writeString(workflow, "name: ci\n")
          _       <- ZIO.attemptBlocking(Files.write(workflow, notText)).orDie
          _       <- TestClock.setTime(java.time.Instant.parse("2026-01-10T00:00:00Z"))
          updated <- DepsSurface.update(repo, SupplyChain(), reply(200, page), true, false)
          text    <- Workspace.readString(build)
        yield assertTrue(
          updated == DepsSurface.DepsResult(1, s"could not read $workflow\n"),
          text == source,
        )
      program
    ,
    test("an update with --yes names the build file it cannot write"):
      val source  = "dep\"org.example::demo:1.0.0\"\n"
      val page    = """{"response":{"docs":[{"v":"1.2.3","timestamp":1600000000000}]}}"""
      val program =
        for
          repo <- Workspace.work("surface-update-unwritten")
          build = repo.resolve("histos.scala")
          _ <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _ <- Workspace.writeString(build, source)
          _ <- ZIO
            .attemptBlocking(Files.setPosixFilePermissions(build, PosixFilePermissions.fromString("r--r--r--")))
            .orDie
          _       <- TestClock.setTime(java.time.Instant.parse("2026-01-10T00:00:00Z"))
          updated <- DepsSurface.update(repo, SupplyChain(), reply(200, page), true, false)
          text    <- Workspace.readString(build)
        yield assertTrue(
          updated == DepsSurface.DepsResult(1, s"could not write $build\n"),
          text == source,
        )
      program
    ,
    test("an empty token is not a pull request unless the run is dry, and Actions does not commit a workflow file"):
      val workflow = ".github/workflows/ci.yml"
      val nothing  = DepsSurface.DepsResult(0, "deps pr: nothing to commit\n")
      val program  =
        for
          repo <- Workspace.work("surface-pr")
          _    <- Workspace.writeString(repo.resolve(workflow), "name: ci")
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, _: String, _: String, _: List[(String, String)]) => ZIO.die(new IllegalStateException("sent"))
          )
          net <- ZIO.service[Net]
          policy = SupplyChain(updates = UpdatePolicy(author = PrAuthor.Actions))
          _     <- TestSystem.putEnv("GH_TOKEN", "")
          empty <- DepsSurface.pullRequest(repo, policy, net, false, true)
          _     <- TestSystem.putEnv("HISTOS_DRY_RUN", "true")
          dry   <- DepsSurface.pullRequest(repo, policy, net, false, true)
          _     <- TestSystem.clearEnv("HISTOS_DRY_RUN")
          _     <- TestSystem.putEnv("GH_TOKEN", "token")
          kept  <- DepsSurface.pullRequest(repo, policy, net, false, true)
        yield assertTrue(
          empty == DepsSurface.DepsResult(1, "GH_TOKEN is not set\n"),
          kept == nothing,
          dry == nothing,
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net] ++ GitAnswers(diff = List(workflow)).layer)
    ,
    test("an origin names its GitHub repository over https or ssh, and anything else names none"):
      assertTrue(
        GitRead.repository("https://github.com/acme/demo.git\n") == Some(RepoName.trusted("acme/demo")),
        GitRead.repository("git@github.com:acme/demo.git") == Some(RepoName.trusted("acme/demo")),
        GitRead.repository("acme/demo") == Some(RepoName.trusted("acme/demo")),
        GitRead.repository("https://github.com/acme") == None,
        GitRead.repository("") == None,
      )
    ,
    test("a snapshot that is not submitted renders the purl and does not send it"):
      val program =
        for
          repo <- Workspace.work("surface-snap")
          _    <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, _: String, _: String, _: List[(String, String)]) => ZIO.die(new IllegalStateException("sent"))
          )
          net    <- ZIO.service[Net]
          result <- DepsSurface.snapshot(repo, net, false)
        yield assertTrue(
          result.code == 0,
          result.text.contains(s""""sha":"${sha.value}","ref":"refs/heads/main""""),
          result.text.contains("pkg:maven/org.example/demo@1.0.0"),
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net] ++ GitAnswers(commit = Some(sha)).layer)
    ,
    test("setup writes the policy once, and a zipx import asks before it deletes"):
      val program =
        for
          repo <- Workspace.work("surface-setup")
          missing = SetupSurface.run(repo, None, PolicyWrite.Preview, ZipxConfirm.Ask)
          _ <- Workspace.writeString(repo.resolve("histos.scala"), "graph\n")
          would = SetupSurface.run(repo, None, PolicyWrite.Preview, ZipxConfirm.Ask)
          wrote = SetupSurface.run(
            repo,
            Some(SetupSource.Directory("no-such-zipx")),
            PolicyWrite.Write,
            ZipxConfirm.Ask,
          )
          again = SetupSurface.run(
            repo,
            Some(SetupSource.Directory("no-such-zipx")),
            PolicyWrite.Write,
            ZipxConfirm.Delete,
          )
          text <- Workspace.readString(repo.resolve("histos.scala"))
        yield assertTrue(
          missing._1 == 2 && missing._2.contains("histos.scala is missing"),
          would._1 == 0 && would._2.contains("would write"),
          wrote._1 == 0 && wrote._2.contains("wrote histos.scala") && wrote._2.contains("confirm before deleting"),
          again._2.contains("already has .supplyChain") && !again._2.contains("confirm before deleting"),
          text.contains(".supplyChain("),
          SetupPlan.idempotent(text),
          SetupPlan.insert(text) == text,
        )
      program
    ,
    test("a pin stays unmapped, a library maps, and the workflow gate is the word that was written"):
      val report = ZipxImport.read(
        "Lib(zio)\nPlugin(sbt)\nAction(checkout)\nPin(native)\n",
        "Introduced ZIPX_APP_ID",
        "npm",
      )
      val all  = ZipxImport.read("", "All", "")
      val none = ZipxImport.read("", "", "")
      assertTrue(
        report.unmapped.map(_.kind) == List("Pin"),
        report.mappedCount("Lib") == 1,
        report.mappedCount("Plugin") == 1,
        report.mappedCount("Action") == 1,
        report.gate.contains("Introduced"),
        report.secrets.contains("ZIPX_APP_ID"),
        report.secrets.contains("dependabot:npm"),
        all.gate.contains("All"),
        none.gate.isEmpty,
      )
    ,
    test("a plan pairs a direct with its artifact, reads a maven page, and rewrites the version it names"):
      val program =
        for
          repo <- Workspace.work("surface-plan")
          directs  = Plan.directs(lock("1.0.0"))
          page     = """{"response":{"docs":[{"v":"1.2.3","timestamp":1700000000000}]}}"""
          releases = Plan.releases(Coordinate.trusted("org.example:demo"), page, Map.empty)
          bump     = ChosenBump(
            BumpKind.Libraries,
            LockedDependency(
              Coordinate.trusted("dev.zio:zio"),
              "2.0.0",
              Purl.trusted("pkg:maven/dev.zio/zio@2.0.0"),
              true,
              false,
              "core",
            ),
            Release(Coordinate.trusted("dev.zio:zio"), "2.1.0", Date.trusted("2026-01-10"), None, false),
            false,
          )
          source    = "dev.zio::zio:2.0.0\ndev.zio:zio:2.0.0\n"
          action    = ActionChange("actions/checkout", "v4", "v5", "a" * 40, "b" * 40, false)
          rewritten = Plan.rewriteAction(s"uses: actions/checkout@${"a" * 40}\n# v4\n", action)
          proposal  = Proposal("deps plan\n", BumpPlan(List(bump), Nil), Nil)
          _     <- Workspace.writeString(repo.resolve("histos.scala"), source)
          wrote <- Plan.write(repo, proposal, source, Nil)
          next  <- Workspace.readString(repo.resolve("histos.scala"))
          again <- Plan.write(repo, proposal, next, Nil)
        yield assertTrue(
          directs.map(_.version) == List("1.0.0"),
          directs.map(_.testOnly) == List(false),
          releases.map(_.version) == List("1.2.3"),
          Plan.render(BumpPlan(List(bump), Nil), Nil, Nil, Map.empty).contains("apply Libraries"),
          Plan.rewriteDep(source, bump) == "dev.zio::zio:2.1.0\ndev.zio:zio:2.1.0\n",
          rewritten.contains(s"actions/checkout@${"b" * 40}"),
          rewritten.contains("# v5"),
          wrote == List("histos.scala"),
          again.isEmpty,
        )
      program
    ,
    test("a pinned action moves to its newest cooled tag, and the plan reads only the tags and their commits"):
      val older   = "a" * 40
      val newer   = "b" * 40
      val tags    = "https://api.github.com/repos/example/demo/tags?per_page=30"
      val commits = "https://api.github.com/repos/example/demo/commits/"
      val listed  = s"""[{"name":"v1.0.0","commit":{"sha":"$older"}},{"name":"v1.1.0","commit":{"sha":"$newer"}}]"""
      val program =
        for
          sent <- Ref.make(List.empty[String])
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              sent.update(_ :+ url) *>
                (if url == tags then ZIO.succeed(Reply(200, listed))
                 else if url.startsWith(commits) then ZIO.succeed(Reply(200, """{"date":"2026-01-01T00:00:00Z"}"""))
                 else ZIO.dieMessage(s"sent $url"))
          )
          net      <- ZIO.service[Net]
          proposal <- Plan.propose(
            "",
            "",
            Map("example/demo" -> (older, "v1.0.0")),
            SupplyChain(),
            Date("2026-01-10"),
            false,
            net,
          )
          urls <- sent.get
        yield assertTrue(
          proposal.actions == List(ActionChange("example/demo", "v1.0.0", "v1.1.0", older, newer, false)),
          urls.sorted == List(s"$commits$older", s"$commits$newer", tags).sorted,
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net])
    ,
    test("a family member is not its own bump, and a lock child keeps its direct"):
      val graph =
        """dep"org.example:demo:1.0.0".module("member")
          |dep"dev.zio::zio:2.1.27".module("zio-streams")
          |dep"org.example:::sbt-plugin:1.0.0"
          |dep"org.scala-lang::scala3-compiler:3.10.0"
          |""".stripMargin
      val family =
        s"""cell core 3.3.7 jvm
           |direct org.example demo 1.0.0 java
           |exclude org.codehaus.plexus plexus-archiver
           |series 2
           |artifact org.example demo 1.0.0 $hex central
           |direct org.example member 1.0.0 java
           |artifact org.example member 1.0.0 $hex central
           |direct dev.zio zio 2.1.27 scala
           |artifact dev.zio zio_3 2.1.27 $hex central
           |direct dev.zio zio-streams 2.1.27 scala
           |artifact dev.zio zio-streams_3 2.1.27 $hex central
           |direct org.example sbt-plugin 1.0.0 full
           |artifact org.example sbt-plugin_2.12_1.0 1.0.0 $hex central
           |direct org.scala-lang scala3-compiler 3.10 scala
           |artifact org.scala-lang scala3-compiler_3 3.10 $hex central
           |""".stripMargin
      val seen   = Plan.directs(family).map(row => s"${row.coordinate.value}:${row.version}")
      val kept   = Plan.roots(family, graph).map(row => s"${row.coordinate.value}:${row.version}")
      val zio    = bump("dev.zio:zio", "2.0.0", "2.1.0")
      val demo   = bump("org.example:demo", "1.0.0", "1.2.0")
      val member = """dep"dev.zio::zio:2.0.0".module("zio-streams")"""
      val plugin = """dep"org.example:::demo:1.0.0""""
      assertTrue(
        seen == List(
          "org.example:demo:1.0.0",
          "org.example:member:1.0.0",
          "dev.zio:zio:2.1.27",
          "dev.zio:zio-streams:2.1.27",
          "org.example:sbt-plugin:1.0.0",
          "org.scala-lang:scala3-compiler:3.10",
        ),
        kept == List(
          "org.example:demo:1.0.0",
          "dev.zio:zio:2.1.27",
          "org.example:sbt-plugin:1.0.0",
        ),
        Plan.roots(family, "").isEmpty,
        Plan.rewriteDep(member, zio) == """dep"dev.zio::zio:2.1.0".module("zio-streams")""",
        Plan.rewriteDep(plugin, demo) == plugin,
        Plan.rewriteDep(plugin, zio) == plugin,
      )
    ,
    test("the expected commit is the head already written, and a dry run does not send"):
      val planned = CommitSha.trusted("b" * 40)
      val program =
        for
          _ <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, _: String, _: String, _: List[(String, String)]) => ZIO.die(new IllegalStateException("sent"))
          )
          net  <- ZIO.service[Net]
          note <- GhRun.execute(
            net,
            RepoName.trusted("early-effect/histos"),
            "token",
            List(GhCall.CommitOnBranch(BranchName.trusted("histos/deps"), planned, "deps: Libraries", Nil)),
            true,
          )
          dry <- GhRun.snapshot(net, RepoName.trusted("early-effect/histos"), "token", "{}", true)
        yield assertTrue(
          GhRun.expected(GhRun.State(Some(sha), Nil), planned) == sha,
          GhRun.expected(GhRun.State(None, Nil), planned) == planned,
          note.contains(planned.value),
          dry.contains("dry-run POST"),
        )
      program.provideSomeLayer[TestEnvironment](stubbed[Net])
    ,
    test("a commit status of 400, or a 200 that carries errors, is a moved head, and an oid is the commit"):
      val branch  = BranchName.trusted("histos/deps")
      val calls   = List(GhCall.CommitOnBranch(branch, sha, "deps", Nil))
      val program =
        for
          bad       <- exchange(calls, 400, "no")
          erred     <- exchange(calls, 200, """{"errors":[]}""")
          committed <- exchange(calls, 200, s"""{"oid":"${"c" * 40}"}""")
          sent      <- postSnapshot(201, "ok")
          denied    <- postSnapshot(422, "nope")
        yield assertTrue(
          bad == Left(SupplyFault.HeadMoved(branch, sha)),
          erred == Left(SupplyFault.HeadMoved(branch, sha)),
          committed.exists(_.contains("c" * 40)),
          sent.exists(_.contains("submitted")),
          denied.exists(_.contains("nope")),
        )
      program
    ,
    test("a scanner reads the strings, numbers, and booleans it was shown, and an unclosed string stops"):
      val body = """{"v":"1.2.3","timestamp":1700000000000,"ok":true,"names":["a","b"]}"""
      assertTrue(
        Scan.strings(body, "v") == List("1.2.3"),
        Scan.longs(body, "timestamp") == List(1700000000000L),
        Scan.bool(body, "ok").contains(true),
        Scan.array(body, "names") == List("a", "b"),
        Scan.strings(body, "missing").isEmpty,
        Scan.strings("""{"v":"unterminated}""", "v").isEmpty,
      ),
  )

  private val fixOff = DoctorSurface.Request(None, false, None, false, false, Nil, false)
  private val fixOn  = DoctorSurface.Request(None, false, None, true, false, Nil, false)
  private val fixYes = DoctorSurface.Request(None, false, None, true, true, Nil, false)
  private val person = Stdio(Attached.Terminal, Attached.Terminal)

  /** What applying fixes did: the notes, the histos commands the runner was given, and the calls the net sent. */
  private final case class Applied(notes: List[String], ran: List[String], sent: List[String])

  /** `fixes` applied on `stdio` with a token, a runner and a net that record what they are given, and TestConsole. */
  private def fixed(repo: Path, fixes: List[Fix], request: DoctorSurface.Request, stdio: Stdio): UIO[Applied] =
    for
      ran  <- Ref.make(List.empty[String])
      sent <- Ref.make(List.empty[String])
      runner = new CommandRunner:
        def run(at: Path, command: HistosCommand): UIO[Ran] = ran.update(_ :+ command.line).as(Ran.Exited(0, Nil))
      net = new Net:
        def send(method: String, url: String, body: String, headers: List[(String, String)]) =
          val bearer = headers.collectFirst { case ("Authorization", value) => value }.getOrElse("")
          sent.update(_ :+ s"$method $url $body $bearer").as(Reply(204, ""))
      notes <- DoctorSurface
        .applyReported(repo, fixes, request, Some(GitHubCredential.Token("token", "github.com")))
        .provide(ZLayer.succeed(stdio), ZLayer.succeed(runner), ZLayer.succeed(net))
      commands <- ran.get
      calls    <- sent.get
    yield Applied(notes, commands, calls)

  private def command(head: String, tail: String*): Fix =
    Fix.Histos(HistosCommand.of(head, tail*))

  /** A page with the call that applies it. */
  private def setting(url: String): Fix =
    Fix.Settings(HttpUrl.trusted(url), "open", Some(GitHubPut(HttpUrl.trusted(s"$url/api"), "{}")))

  /** A page a person opens. */
  private def page(url: String): Fix =
    Fix.Settings(HttpUrl.trusted(url), "open", None)

  private def bump(coordinate: String, from: String, to: String): ChosenBump =
    val where = Coordinate.trusted(coordinate)
    val purl  = Purl.trusted(s"pkg:maven/${coordinate.replace(":", "/")}@$from")
    ChosenBump(
      BumpKind.Libraries,
      LockedDependency(where, from, purl, true, false, "core"),
      Release(where, to, Date.trusted("2026-01-10"), None, false),
      false,
    )

  private def lock(version: String): String =
    s"""cell core 3.3.7 jvm
       |direct org.example demo $version java
       |artifact org.example demo $version $hex central
       |""".stripMargin

  private def exchange(calls: List[GhCall], status: Int, body: String): UIO[Either[SupplyFault, String]] =
    GhRun
      .execute(reply(status, body), RepoName.trusted("early-effect/histos"), "token", calls, false)
      .either

  private def postSnapshot(status: Int, body: String): UIO[Either[SupplyFault, String]] =
    GhRun.snapshot(reply(status, body), RepoName.trusted("early-effect/histos"), "token", "{}", false).either

  private def reply(status: Int, body: String): Net =
    new Net:
      def send(
          method: String,
          url: String,
          payload: String,
          headers: List[(String, String)],
      ): IO[SupplyFault, Reply] =
        ZIO.succeed(Reply(status, body))
end SupplySurfaceLawSpec
