package histos

import histos.central.{MavenModule, ReleaseFeed}
import histos.doctor.{Fix, FixApply, GitHubCredential}
import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.plugin.SecretName
import histos.supply.{DoctorSurface, GitAnswers, GitRead, Net, PrAuthor, Reply, SupplyChain, Terminal, UpdatePolicy}
import histos.toolchain.Architecture
import histos.workspace.Workspace

import java.nio.file.Path

import slayer.{any, stub, stubbed}

import zio.*
import zio.test.*

/** The survey runs in process. The net answers 404 and is never a socket. Preflight dies if it opens one. The terminal
  * doctor's law sets no Central credential and names no repository, so its live net sends nothing.
  */
object DoctorSurfaceLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val token  = GitHubCredential.Token("token", "github.com")

  private val emptyJson = """{"exit":0,"timedOut":[],"findings":[]}"""

  def spec = suite("doctor surface laws")(
    test("preflight stays quiet, an unknown area is empty, and a named area keeps only that area"):
      ZIO
        .scoped:
          Workspace
            .work("histos-doctor-surface")
            .flatMap: repo =>
              ZIO
                .attempt(Option(java.lang.System.getProperty("java.home")).filter(_.nonEmpty))
                .orDie
                .flatMap:
                  case None       => ZIO.dieMessage("java.home is unset")
                  case Some(home) =>
                    val machine = Machine(
                      Map.empty,
                      "/home/demo",
                      histos.jdk.Os.Mac,
                      Architecture.Aarch64,
                      repo,
                      repo,
                      Some(Path.of(home)),
                      NativeOrigin.Central,
                    )
                    Projects(module("core", Jvm)(scala3)) match
                      case Left(_)      => ZIO.succeed(assertTrue(Projects(module("core", Jvm)(scala3)).isRight))
                      case Right(graph) =>
                        def ask(fix: Fix): UIO[FixApply.Answer]           = ZIO.dieMessage("doctor surface asked")
                        def exec(line: String): UIO[Unit]                 = ZIO.dieMessage(s"doctor surface ran $line")
                        def setting(url: String): UIO[Unit]               = ZIO.dieMessage(s"doctor surface wrote $url")
                        def run(request: DoctorSurface.Request, net: Net) =
                          DoctorSurface.runWith(
                            repo,
                            graph,
                            request,
                            machine,
                            net,
                            Map.empty,
                            None,
                            false,
                            ask,
                            exec,
                            setting,
                          )
                        for
                          preflight <- run(
                            DoctorSurface.Request(None, false, Some("nightly"), false, false, Nil, false),
                            Dying,
                          )
                          spoken <- run(
                            DoctorSurface.Request(None, false, Some("nightly"), false, false, Nil, true),
                            Dying,
                          )
                          toolchain <- run(
                            DoctorSurface.Request(Some("toolchain"), false, None, false, false, Nil, true),
                            Closed,
                          )
                          supply <- run(
                            DoctorSurface.Request(Some("supply-chain"), false, None, false, false, Nil, true),
                            Closed,
                          )
                          missing <- run(
                            DoctorSurface.Request(Some("missing"), true, None, false, false, Nil, false),
                            Closed,
                          )
                        yield assertTrue(
                          preflight._1 == 0,
                          preflight._2 == s"histos doctor  ${repo.getFileName}\n0 errors, 0 warnings",
                          !preflight._2.contains("nightly preflight"),
                          spoken._1 == 0,
                          spoken._2.contains("this run does not launch Node"),
                          spoken._2.contains("this run does not link Native"),
                          spoken._2.contains("every uses is pinned"),
                          !spoken._2.contains("nightly preflight"),
                          spoken._2.contains("0 errors, 0 warnings, "),
                          toolchain._2.contains("this run does not launch Node"),
                          toolchain._2.contains("this run does not link Native"),
                          !toolchain._2.contains("every uses is pinned"),
                          supply._2.contains("every uses is pinned"),
                          !supply._2.contains("this run does not launch Node"),
                          missing._2 == emptyJson,
                        )
        .provideSomeLayer[TestEnvironment](stubbed[ReleaseFeed] ++ GitAnswers().layer)
    ,
    test("preflight does not open a socket, and an unseen Actions fact is a skip"):
      ZIO
        .scoped:
          Workspace
            .work("histos-doctor-preflight")
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
              val author = PrAuthor.App(SecretName("HISTOS_APP_ID"), SecretName("HISTOS_APP_PRIVATE_KEY"))
              Projects
                .organization(organization"example.com")
                .supplyChain(SupplyChain(updates = UpdatePolicy(author = author)))(
                  module("core", Jvm)(scala3)
                ) match
                case Left(_)      => ZIO.dieMessage("the preflight graph did not build")
                case Right(graph) =>
                  val actions = Map(
                    "GITHUB_ACTIONS"    -> "true",
                    "GITHUB_REPOSITORY" -> "example/demo",
                    "GITHUB_TOKEN"      -> "token",
                  )
                  def run(env: Map[String, String], verbose: Boolean) =
                    DoctorSurface.runWith(
                      repo,
                      graph,
                      DoctorSurface.Request(Some("supply-chain"), false, Some("nightly"), false, false, Nil, verbose),
                      machine,
                      Dying,
                      env,
                      Some(token),
                      false,
                      _ => ZIO.dieMessage("doctor surface asked"),
                      line => ZIO.dieMessage(s"doctor surface ran $line"),
                      url => ZIO.dieMessage(s"doctor surface wrote $url"),
                    )
                  for
                    quiet  <- run(actions, false)
                    spoken <- run(actions, true)
                    failed <- run(actions + ("HISTOS_STEP_APP" -> "failure"), false)
                  yield assertTrue(
                    quiet._1 == 0,
                    quiet._2.contains("0 errors, 0 warnings"),
                    !quiet._2.contains("x error"),
                    spoken._1 == 0,
                    spoken._2.contains("skipped"),
                    spoken._2.contains("supply.required-check"),
                    !spoken._2.contains("x error"),
                    failed._1 == 1,
                    failed._2.contains("x error"),
                    failed._2.contains("supply.app-token"),
                    failed._2.contains("failure"),
                  )
        .provideSomeLayer[TestEnvironment](stubbed[ReleaseFeed] ++ GitAnswers().layer)
    ,
    test("the terminal doctor reads the feed it is given, and a preflight of a pinned ship never reads it"):
      ZIO
        .scoped:
          Workspace
            .work("histos-doctor-feed")
            .flatMap: repo =>
              val machine =
                Machine(
                  Map.empty,
                  "/home/demo",
                  histos.jdk.Os.Mac,
                  Architecture.Aarch64,
                  repo,
                  repo,
                  None,
                  NativeOrigin.Central,
                )
              val app = module("app", Jvm)(scala3)
              Projects.organization(organization"example.com")(ships(Ship("demo").next(1, 2, 3)(app)), app) match
                case Left(_)      => ZIO.dieMessage("the pinned graph did not build")
                case Right(graph) =>
                  def run(preflight: Option[String]) =
                    DoctorSurface.run(
                      repo,
                      graph,
                      DoctorSurface.Request(Some("release"), false, preflight, false, false, Nil, true),
                      machine,
                    )
                  for
                    _       <- TestSystem.putEnv("GH_TOKEN", "token")
                    _       <- Workspace.writeString(repo.resolve("build.properties"), "histos.version=0.2.0\n")
                    offline <- run(Some("nightly"))
                    _       <- stub[ReleaseFeed](_.newest(any[MavenModule]))(ZIO.succeed(Some(ship"9.9.9")))
                    asked   <- run(None)
                  yield assertTrue(
                    !offline._2.contains("ship.pin"),
                    !offline._2.contains("ship.should-bump"),
                    asked._2.contains("ship.pin"),
                    asked._2.contains("ship.should-bump"),
                    asked._2.contains("9.9.9"),
                  )
        .provideSomeLayer[TestEnvironment](stubbed[ReleaseFeed] ++ GitAnswers().layer ++ stubbed[Terminal])
    ,
    test("a fix asks the terminal it is given: none is not applied, and an answer of no or yes is that answer"):
      ZIO
        .scoped:
          Workspace
            .work("histos-doctor-terminal")
            .flatMap: repo =>
              val machine =
                Machine(
                  Map.empty,
                  "/home/demo",
                  histos.jdk.Os.Mac,
                  Architecture.Aarch64,
                  repo,
                  repo,
                  None,
                  NativeOrigin.Central,
                )
              val url    = s"https://github.com/${repo.getFileName}/settings/actions"
              val author = PrAuthor.App(SecretName("HISTOS_APP_ID"), SecretName("HISTOS_APP_PRIVATE_KEY"))
              // An App author has no workflow-files command, so no fix here runs a shell.
              Projects
                .organization(organization"example.com")
                .supplyChain(SupplyChain(updates = UpdatePolicy(author = author)))(module("core", Jvm)(scala3)) match
                case Left(_)      => ZIO.dieMessage("the terminal graph did not build")
                case Right(graph) =>
                  def fix(attached: Boolean) =
                    DoctorSurface
                      .run(
                        repo,
                        graph,
                        DoctorSurface.Request(Some("supply-chain"), false, None, true, false, Nil, false),
                        machine,
                      )
                      .provideSomeLayer[ReleaseFeed & GitRead](ZLayer.succeed(terminal(attached)))
                  for
                    _ <- TestSystem.putEnv("GH_TOKEN", "token")
                    _ <- Workspace.writeString(
                      repo.resolve(".github").resolve("workflows").resolve("ci.yml"),
                      "jobs:\n  a:\n    steps:\n      - uses: actions/checkout@v4\n",
                    )
                    none   <- fix(false)
                    _      <- TestConsole.feedLines("n")
                    no     <- fix(true)
                    _      <- TestConsole.feedLines("y")
                    yes    <- fix(true)
                    prompt <- TestConsole.output
                  yield assertTrue(
                    none._2.contains(s"not applied (no terminal): $url"),
                    no._2.contains(s"skipped: $url"),
                    yes._2.contains(s"applied: $url"),
                    prompt == Vector(s"Apply GitHub setting $url? [y/N] \n", s"Apply GitHub setting $url? [y/N] \n"),
                  )
        .provideSomeLayer[TestEnvironment](stubbed[ReleaseFeed] ++ GitAnswers().layer),
  ) @@ TestAspect.withLiveClock // the doctor bounds each real probe with a timeout

  private def terminal(answer: Boolean): Terminal =
    new Terminal:
      def attached: UIO[Boolean] = ZIO.succeed(answer)
end DoctorSurfaceLawSpec

private object Dying extends Net:
  def send(
      method: String,
      url: String,
      body: String,
      headers: List[(String, String)],
  ): zio.IO[histos.supply.SupplyFault, Reply] =
    ZIO.dieMessage(s"doctor opened $url")

private object Closed extends Net:
  def send(
      method: String,
      url: String,
      body: String,
      headers: List[(String, String)],
  ): zio.IO[histos.supply.SupplyFault, Reply] =
    ZIO.succeed(Reply(404, ""))
