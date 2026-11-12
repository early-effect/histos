package histos.supply

import histos.version.CommitSha
import histos.workspace.Workspace

import slayer.{any, stub, stubbed}

import zio.*
import zio.test.*

/** Every law runs on TestSystem and TestClock, so the token, `GITHUB_REPOSITORY`, `GITHUB_SHA`, `HISTOS_DRY_RUN`, and
  * today are what the law puts.
  */
object SurfaceExchangeLawSpec extends ZIOSpecDefault:
  private val ghsa   = "GHSA-1234-abcd-ef09"
  private val oid    = "c" * 40
  private val head   = "0" * 40
  private val origin = RepoName.trusted("acme/demo")
  private val maven  =
    """{"response":{"docs":[{"v":"1.2.3","timestamp":1700000000000}]}}"""
  private val planned =
    """deps plan
      |apply Libraries org.example:demo 1.0.0 -> 1.2.3
      |note org.example:demo demo
      |""".stripMargin
  private val advisory = s"""{"id":"$ghsa","fixed":"9.9.9"}"""

  def spec = suite("surface exchange laws")(
    test("a maven 200 names the library the plan applies once the cooldown has passed on the caller's clock"):
      val program =
        for
          repo <- Workspace.work("surface-exchange-check")
          _    <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _    <- Workspace.writeString(repo.resolve("histos.scala"), "dep\"org.example:demo:1.0.0\"\n")
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, _: String, _: String, _: List[(String, String)]) => ZIO.succeed(Reply(200, maven))
          )
          net    <- ZIO.service[Net]
          _      <- TestClock.setTime(java.time.Instant.parse("2023-11-16T23:00:00Z"))
          early  <- DepsSurface.check(repo, SupplyChain(), net)
          _      <- TestClock.setTime(java.time.Instant.parse("2023-11-17T00:00:00Z"))
          cooled <- DepsSurface.check(repo, SupplyChain(), net)
        yield assertTrue(
          early == DepsSurface.DepsResult(0, "deps plan\nnote org.example:demo demo\n"),
          cooled == DepsSurface.DepsResult(0, planned),
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net])
    ,
    test("a confirmed update writes the version that maven 200 named, and an unconfirmed one does not"):
      val program =
        for
          repo <- Workspace.work("surface-exchange-update")
          path = repo.resolve("histos.scala")
          _ <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _ <- Workspace.writeString(path, "org.example:demo:1.0.0\n")
          _ <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, _: String, _: String, _: List[(String, String)]) => ZIO.succeed(Reply(200, maven))
          )
          net    <- ZIO.service[Net]
          _      <- TestClock.setTime(java.time.Instant.parse("2026-01-10T00:00:00Z"))
          shown  <- DepsSurface.update(repo, SupplyChain(), net, false, false)
          before <- Workspace.readString(path)
          wrote  <- DepsSurface.update(repo, SupplyChain(), net, true, false)
          after  <- Workspace.readString(path)
        yield assertTrue(
          shown.code == 0 && shown.text == planned,
          before == "org.example:demo:1.0.0\n",
          wrote.code == 0 && wrote.text == planned + "wrote histos.scala\n",
          after == "org.example:demo:1.2.3\n",
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net])
    ,
    test("an audit 200 prints the GHSA line, --report keeps that line, and the answer is cached under the home"):
      val program =
        for
          repo <- Workspace.work("surface-exchange-audit")
          _    <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _    <- TestSystem.putProperty("user.home", repo.resolve("home").toString)
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, _: String, _: String, _: List[(String, String)]) => ZIO.succeed(Reply(200, advisory))
          )
          net    <- ZIO.service[Net]
          plain  <- DepsSurface.audit(repo, false, net)
          report <- DepsSurface.audit(repo, true, net)
          cached <- Workspace.readString(
            repo.resolve("home").resolve(".cache").resolve("histos").resolve("advisories").resolve("querybatch.json")
          )
        yield assertTrue(
          plain.code == 0 && plain.text == s"deps audit\n  $ghsa\n",
          report.code == 0 && report.text == s"deps audit --report\n  $ghsa\n",
          cached == advisory,
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net])
    ,
    test(
      "a pull request 200 records the commit it opened, a 409 names the branch that moved, and a dry run sends none"
    ):
      val openedLive =
        sentence(s"POST https://api.github.com/repos/${origin.value}/git/refs", s"commit $oid", "opened")
      val updatedLive =
        sentence(s"POST https://api.github.com/repos/${origin.value}/git/refs", s"commit $oid", "updated 12")
      val dryText = sentence(
        s"dry-run POST https://api.github.com/repos/${origin.value}/git/refs",
        s"commit deps: Libraries expected $head",
        "pull deps: weekly updates",
      )
      val moved     = s"histos/deps moved from $head\n"
      val committed = s"""{"oid":"$oid"}"""
      val program   =
        for
          repo <- Workspace.work("surface-exchange-pr")
          _    <- Workspace.writeString(repo.resolve("histos.scala"), "graph\n")
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              if url == GhRun.graphql then ZIO.succeed(Reply(200, committed))
              else ZIO.succeed(Reply(200, "{}"))
          )
          net    <- ZIO.service[Net]
          _      <- TestSystem.putEnv("GH_TOKEN", "token")
          opened <- DepsSurface.pullRequest(repo, SupplyChain(), net, false, true)
          _      <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              if url == GhRun.graphql then ZIO.succeed(Reply(200, committed))
              else if url.contains("/pulls?") then ZIO.succeed(Reply(200, """{"number":12}"""))
              else ZIO.succeed(Reply(200, "{}"))
          )
          updated <- DepsSurface.pullRequest(repo, SupplyChain(), net, false, true)
          _       <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              if url == GhRun.graphql then ZIO.succeed(Reply(409, "conflict"))
              else ZIO.succeed(Reply(200, "{}"))
          )
          conflict <- DepsSurface.pullRequest(repo, SupplyChain(), net, false, true)
          _        <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) => ZIO.dieMessage(s"sent $url")
          )
          _   <- TestSystem.clearEnv("GH_TOKEN")
          _   <- TestSystem.putEnv("HISTOS_DRY_RUN", "1")
          dry <- DepsSurface.pullRequest(repo, SupplyChain(), net, false, true)
        yield assertTrue(
          opened == DepsSurface.DepsResult(0, openedLive),
          updated == DepsSurface.DepsResult(0, updatedLive),
          conflict == DepsSurface.DepsResult(1, moved),
          dry == DepsSurface.DepsResult(0, dryText),
        )
      program.provideSomeLayer[TestEnvironment & Scope](
        stubbed[Net] ++ GitAnswers(remote = Some(origin), diff = List("histos.scala")).layer
      )
    ,
    test("an alert 200 opens the advisory and the security pull on the repository origin names"):
      val live    = s"open $ghsa\nsecurity opened\n"
      val osv     = "https://api.osv.dev/v1/querybatch"
      val program =
        for
          repo <- Workspace.work("surface-exchange-alert")
          _    <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          sent <- Ref.make(List.empty[String])
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              sent.update(_ :+ url) *>
                (if url == osv then ZIO.succeed(Reply(200, advisory))
                 else if url.contains("/issues?") || url.contains("/pulls?") then ZIO.succeed(Reply(200, "[]"))
                 else ZIO.succeed(Reply(200, "{}")))
          )
          net    <- ZIO.service[Net]
          _      <- TestSystem.putEnv("GH_TOKEN", "token")
          result <- DepsSurface.alert(repo, SupplyChain(), net)
          github <- sent.get.map(_.filterNot(_ == osv))
        yield assertTrue(
          result == DepsSurface.DepsResult(0, live),
          github.nonEmpty,
          github.forall(_.startsWith(s"https://api.github.com/repos/${origin.value}/")),
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net] ++ GitAnswers(remote = Some(origin)).layer)
    ,
    test("a snapshot 200 submits the purl to the job's repository at the job's commit"):
      val declared = "acme/declared"
      val commit   = "d" * 40
      val program  =
        for
          repo <- Workspace.work("surface-exchange-snapshot")
          _    <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _    <- TestSystem.putEnv("GITHUB_REPOSITORY", declared)
          _    <- TestSystem.putEnv("GITHUB_SHA", commit)
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              if url == s"https://api.github.com/repos/$declared/dependency-graph/snapshots" then
                ZIO.succeed(Reply(200, "accepted"))
              else ZIO.dieMessage(s"sent $url")
          )
          net    <- ZIO.service[Net]
          _      <- TestSystem.putEnv("GITHUB_TOKEN", "token")
          result <- DepsSurface.snapshot(repo, net, true)
        yield assertTrue(
          result.code == 0,
          result.text.startsWith(s"submitted $declared\n"),
          result.text.contains(s""""sha":"$commit""""),
          result.text.contains("pkg:maven/org.example/demo@1.0.0"),
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net] ++ GitAnswers(remote = Some(origin)).layer)
    ,
    test("the bearer GitHub sees is GH_TOKEN, else GITHUB_TOKEN, and an empty one is not set"):
      val program =
        for
          repo    <- Workspace.work("surface-exchange-token")
          _       <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          bearers <- Ref.make(List.empty[String])
          _       <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, _: String, _: String, headers: List[(String, String)]) =>
              bearers.update(_ ++ headers.collect { case ("Authorization", value) => value }).as(Reply(200, "accepted"))
          )
          net     <- ZIO.service[Net]
          _       <- TestSystem.putEnv("GH_TOKEN", "")
          unset   <- DepsSurface.snapshot(repo, net, true)
          _       <- TestSystem.putEnv("GITHUB_TOKEN", "actions")
          actions <- DepsSurface.snapshot(repo, net, true)
          _       <- TestSystem.putEnv("GH_TOKEN", "person")
          person  <- DepsSurface.snapshot(repo, net, true)
          sent    <- bearers.get
        yield assertTrue(
          unset == DepsSurface.DepsResult(1, "GITHUB_TOKEN is not set\n"),
          actions.text.startsWith(s"submitted ${origin.value}\n"),
          person.text.startsWith(s"submitted ${origin.value}\n"),
          sent == List("Bearer actions", "Bearer person"),
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net] ++ GitAnswers(remote = Some(origin)).layer)
    ,
    test("a checkout that names no GitHub repository submits nothing"):
      val program =
        for
          repo <- Workspace.work("surface-exchange-unnamed")
          _    <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _    <- Workspace.writeString(repo.resolve("histos.scala"), "graph\n")
          sent <- Ref.make(List.empty[String])
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) => sent.update(_ :+ url).as(Reply(200, "[]"))
          )
          net      <- ZIO.service[Net]
          _        <- TestSystem.putEnv("GH_TOKEN", "token")
          snapshot <- DepsSurface.snapshot(repo, net, true)
          pull     <- DepsSurface.pullRequest(repo, SupplyChain(), net, false, true)
          urls     <- sent.get
        yield assertTrue(
          snapshot == unnamed,
          pull == unnamed,
          urls == Nil,
        )
      program.provideSomeLayer[TestEnvironment & Scope](
        stubbed[Net] ++ GitAnswers(commit = Some(CommitSha.trusted("e" * 40)), diff = List("histos.scala")).layer
      ),
  )

  private val unnamed =
    DepsSurface.DepsResult(1, "no GitHub repository: set GITHUB_REPOSITORY or add a github.com origin\n")

  private def sentence(lines: String*): String =
    lines.mkString("\n") + "\n"

  private def lock(version: String): String =
    s"""cell core 3.3.7 jvm
       |direct org.example demo $version java
       |artifact org.example demo $version ${"ab" * 32} central
       |""".stripMargin
end SurfaceExchangeLawSpec
