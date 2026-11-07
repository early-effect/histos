package histos.supply

import histos.workspace.Workspace

import slayer.{any, stub, stubbed}

import zio.*
import zio.test.*

object SurfaceExchangeLawSpec extends ZIOSpecDefault:
  private val ghsa  = "GHSA-1234-abcd-ef09"
  private val oid   = "c" * 40
  private val head  = "0" * 40
  private val maven =
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
    test("an audit 200 prints the GHSA line, and --report keeps that line"):
      val program =
        for
          repo <- Workspace.work("surface-exchange-audit")
          _    <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, _: String, _: String, _: List[(String, String)]) => ZIO.succeed(Reply(200, advisory))
          )
          net    <- ZIO.service[Net]
          plain  <- DepsSurface.audit(repo, false, net)
          report <- DepsSurface.audit(repo, true, net)
        yield assertTrue(
          plain.code == 0 && plain.text == s"deps audit\n  $ghsa\n",
          report.code == 0 && report.text == s"deps audit --report\n  $ghsa\n",
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net])
    ,
    test("a pull request 200 records the commit it opened, and a 409 names the branch that moved"):
      val name       = repoName
      val files      = Some(List("histos.scala" -> "graph\n"))
      val policy     = SupplyChain()
      val openedLive =
        sentence(s"POST https://api.github.com/repos/$name/git/refs", s"commit $oid", "opened")
      val updatedLive =
        sentence(s"POST https://api.github.com/repos/$name/git/refs", s"commit $oid", "updated 12")
      val moved     = s"histos/deps moved from $head\n"
      val committed = s"""{"oid":"$oid"}"""
      val program   =
        for
          repo <- Workspace.work("surface-exchange-pr")
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              if url == GhRun.graphql then ZIO.succeed(Reply(200, committed))
              else ZIO.succeed(Reply(200, "{}"))
          )
          net    <- ZIO.service[Net]
          opened <- DepsSurface.pullRequest(repo, policy, net, Some("token"), false, true, files)
          _      <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              if url == GhRun.graphql then ZIO.succeed(Reply(200, committed))
              else if url.contains("/pulls?") then ZIO.succeed(Reply(200, """{"number":12}"""))
              else ZIO.succeed(Reply(200, "{}"))
          )
          updated <- DepsSurface.pullRequest(repo, policy, net, Some("token"), false, true, files)
          _       <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              if url == GhRun.graphql then ZIO.succeed(Reply(409, "conflict"))
              else ZIO.succeed(Reply(200, "{}"))
          )
          conflict <- DepsSurface.pullRequest(repo, policy, net, Some("token"), false, true, files)
        yield assertTrue(
          opened.code == 0,
          opened.text == openedLive,
          updated.code == 0,
          updated.text == updatedLive,
          conflict.code == 1,
          conflict.text == moved,
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net])
    ,
    test("an alert 200 opens the advisory and the security pull"):
      val live    = s"open $ghsa\nsecurity opened\n"
      val program =
        for
          repo <- Workspace.work("surface-exchange-alert")
          _    <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              if url == "https://api.osv.dev/v1/querybatch" then ZIO.succeed(Reply(200, advisory))
              else if url.contains("/issues?") || url.contains("/pulls?") then ZIO.succeed(Reply(200, "[]"))
              else ZIO.succeed(Reply(200, "{}"))
          )
          net    <- ZIO.service[Net]
          result <- DepsSurface.alert(repo, SupplyChain(), net, Some("token"))
        yield assertTrue(result.code == 0, result.text == live)
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net])
    ,
    test("a snapshot 200 submits the purl"):
      val name    = repoName
      val line    = s"submitted $name\n"
      val program =
        for
          repo <- Workspace.work("surface-exchange-snapshot")
          _    <- Workspace.writeString(repo.resolve("dependencies.lock"), lock("1.0.0"))
          _    <- stub[Net](_.send(any[String], any[String], any[String], any[List[(String, String)]]))(
            (_: String, url: String, _: String, _: List[(String, String)]) =>
              if url.contains("/dependency-graph/snapshots") then ZIO.succeed(Reply(200, "accepted"))
              else ZIO.dieMessage(s"sent $url")
          )
          net    <- ZIO.service[Net]
          result <- DepsSurface.snapshot(repo, net, Some("token"), true)
        yield assertTrue(
          result.code == 0,
          result.text.startsWith(line),
          result.text.contains("pkg:maven/org.example/demo@1.0.0"),
        )
      program.provideSomeLayer[TestEnvironment & Scope](stubbed[Net]),
  )

  private def repoName: String =
    sys.env
      .get("GITHUB_REPOSITORY")
      .flatMap(raw => RepoName.from(raw).toOption)
      .map(_.value)
      .getOrElse("early-effect/histos")

  private def sentence(lines: String*): String =
    lines.mkString("\n") + "\n"

  private def lock(version: String): String =
    s"""cell core 3.3.7 jvm
       |direct org.example demo $version java
       |artifact org.example demo $version ${"ab" * 32} central
       |""".stripMargin
end SurfaceExchangeLawSpec
