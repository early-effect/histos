package histos.supply

import histos.*
import histos.central.{FeedError, MavenModule, ReleaseFeed}
import histos.doctor.*
import histos.version.ShipNumber
import histos.workflow.SupplyYaml
import histos.workspace.Workspace
import java.nio.file.Path
import zio.{Duration, IO, Ref, UIO, ZIO, ZLayer}
import zio.test.*

object PreflightCompatibilitySpec extends ZIOSpecDefault:
  private val version   = ScalaVersion("3.10.1")
  private val published = module("published", Jvm)(version)
  private val built     = Projects.organization(organization"example.test")(
    ships(Ship("library").next(0, 15, 1)(published)),
    published,
  )

  def spec = suite("product-independent preflight")(
    test("cold preflight stays on session and releases while the full selection keeps bump and pin") {
      ZIO.scoped:
        for
          repo   <- Workspace.work("cold-preflight")
          graph  <- ZIO.fromEither(built)
          _      <- Workspace.writeString(repo.resolve("releases"), "")
          listed <- Ref.make(0)
          feed = new ReleaseFeed:
            def newest(module: MavenModule): IO[FeedError, Option[ShipNumber]] = listed.update(_ + 1).as(None)
          checks    = ShipChecks.checks(repo, graph, Map.empty, feed)
          preflight = DoctorSurface.selection(checks, Some("workflow"))
          full      = DoctorSurface.selection(checks, None)
          report <- Runner.run(preflight, DoctorContext(Nil, Set(Probe.Local)), Duration.Infinity)
          machine = histos.jdk.Machine(
            Map.empty,
            repo.toString,
            histos.jdk.Os.Linux,
            histos.toolchain.Architecture.X86_64,
            repo.resolve("cache"),
            repo,
            None,
            histos.native.NativeOrigin.Central,
          )
          request = DoctorSurface.Request(Some("release"), true, Some("workflow"), false, false, Nil, false)
          surface <- DoctorSurface
            .run(repo, graph, request, machine)
            .provideLayer(
              ZLayer.succeed(feed) ++ ZLayer.succeed(noGit) ++ ZLayer.succeed(noNet) ++
                ZLayer.succeed(noAuth) ++ histos.central.KeyServer.central ++
                ZLayer.succeed(histos.cli.Stdio.redirected) ++ ZLayer.succeed(noCommands)
            )
          reads <- listed.get
        yield assertTrue(
          preflight.map(_.id) == List(ShipChecks.sessionId, ShipChecks.releasesId),
          report.exit == 0,
          surface._1 == 0,
          !surface._2.contains("ship.binary"),
          reads == 0,
          full.map(_.id) == List(
            ShipChecks.sessionId,
            ShipChecks.releasesId,
            ShipChecks.bumpId,
            ShipChecks.pinId,
          ),
          SupplyYaml.preflight(graph, "workflow").exists(_.contains("doctor --preflight workflow")),
        )
    }
  )
  private val noGit = new GitRead:
    def origin(repo: Path): UIO[Option[RepoName]]                                  = ZIO.none
    def head(repo: Path): UIO[Option[histos.version.CommitSha]]                    = ZIO.none
    def tip(repo: Path, branch: BranchName): UIO[Option[histos.version.CommitSha]] = ZIO.none
    def changed(repo: Path): UIO[List[String]]                                     = ZIO.succeed(Nil)

  private val noNet = new Net:
    def send(method: String, url: String, body: String, headers: List[(String, String)]): IO[SupplyFault, Reply] =
      ZIO.succeed(Reply(503, ""))

  private val noAuth = new GhAuth:
    def token(hostname: Option[String]): UIO[Option[String]] = ZIO.none

  private val noCommands = new CommandRunner:
    def run(repo: Path, command: HistosCommand): UIO[Ran] = ZIO.succeed(Ran.Unstarted)
end PreflightCompatibilitySpec
