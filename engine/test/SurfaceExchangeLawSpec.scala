package histos.supply

import histos.version.CommitSha
import histos.workspace.Workspace

import slayer.{any, stub, stubbed}

import zio.*
import zio.test.*

/** Every law runs on TestSystem, so `GITHUB_REPOSITORY`, `GITHUB_SHA`, and `HISTOS_DRY_RUN` are what the law puts. */
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
    test("a maven 200 names the library the plan applies"):
      val program =
        for
          repo <- Workspace.work("surface-exchange-check")
          _    <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _    <- Workspace.writeString(repo.resolve("histos.scala"), "dep\"org.example:demo:1.0.0\"\n")
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, _: String, _: String, _: List[(String, String)]) => ZIO.succeed(Reply(200, maven))
          )
          net    <- ZIO.service[Net]
          result <- DepsSurface.check(repo, SupplyChain(), net, Date.trusted("2026-01-10"))
        yield assertTrue(result.code == 0, result.text == planned)
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
          shown  <- DepsSurface.update(repo, SupplyChain(), net, Date.trusted("2026-01-10"), false, false)
          before <- Workspace.readString(path)
          wrote  <- DepsSurface.update(repo, SupplyChain(), net, Date.trusted("2026-01-10"), true, false)
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
          opened <- DepsSurface.pullRequest(repo, SupplyChain(), net, Some("token"), false, true)
          _      <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              if url == GhRun.graphql then ZIO.succeed(Reply(200, committed))
              else if url.contains("/pulls?") then ZIO.succeed(Reply(200, """{"number":12}"""))
              else ZIO.succeed(Reply(200, "{}"))
          )
          updated <- DepsSurface.pullRequest(repo, SupplyChain(), net, Some("token"), false, true)
          _       <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              if url == GhRun.graphql then ZIO.succeed(Reply(409, "conflict"))
              else ZIO.succeed(Reply(200, "{}"))
          )
          conflict <- DepsSurface.pullRequest(repo, SupplyChain(), net, Some("token"), false, true)
          _        <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) => ZIO.dieMessage(s"sent $url")
          )
          _   <- TestSystem.putEnv("HISTOS_DRY_RUN", "1")
          dry <- DepsSurface.pullRequest(repo, SupplyChain(), net, None, false, true)
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
          result <- DepsSurface.alert(repo, SupplyChain(), net, Some("token"))
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
          result <- DepsSurface.snapshot(repo, net, Some("token"), true)
        yield assertTrue(
          result.code == 0,
          result.text.startsWith(s"submitted $declared\n"),
          result.text.contains(s""""sha":"$commit""""),
          result.text.contains("pkg:maven/org.example/demo@1.0.0"),
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
          snapshot <- DepsSurface.snapshot(repo, net, Some("token"), true)
          pull     <- DepsSurface.pullRequest(repo, SupplyChain(), net, Some("token"), false, true)
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
