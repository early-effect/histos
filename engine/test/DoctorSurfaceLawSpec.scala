package histos

import histos.central.ReleaseFeed
import histos.doctor.{Fix, FixApply, GitHubCredential}
import histos.jdk.Machine
import histos.native.NativeOrigin
import histos.plugin.SecretName
import histos.supply.{DoctorSurface, GitAnswers, Net, PrAuthor, Reply, SupplyChain, UpdatePolicy}
import histos.toolchain.Architecture
import histos.workspace.Workspace

import java.nio.file.Path

import slayer.stubbed

import zio.*
import zio.test.*

/** The survey runs in process. The net answers 404 and is never a socket. Preflight dies if it opens one. */
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
        .provideSomeLayer[TestEnvironment](stubbed[ReleaseFeed] ++ GitAnswers().layer),
  ) @@ TestAspect.withLiveClock // the doctor bounds each real probe with a timeout
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
