package histos.server

import histos.build.BuildEvent
import histos.doctor.DoctorPlacement
import histos.load.{LoadedBuild, PluginRuntime}
import histos.plugin.{ApiDigest, PluginArgs, PluginTaskId, SecretName}
import histos.report.CallResult
import histos.supply.{AdvisoryPolicy, DailyAudit, PrAuthor, SecurityFixes, SupplyChain, UpdatePolicy}
import histos.jdk.{Sha256, Toolchain}
import histos.version.CommitSha
import histos.workflow.{RenderingShip, SupplyYaml, Workflow, WorkflowAction}
import histos.workspace.Workspace
import histos.{HistosBuild, Jvm, LawGit, Projects, module, organization, `scala`}

import java.nio.file.{Files, Path}

import zio.{Chunk, Ref, ZIO}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Check refuses a missing workflow. Update writes the audit job. A file the graph does not render is drift. */
object CallsWorkflowLawSpec extends ZIOSpecDefault:
  private val id   = SecretName("HISTOS_APP_ID")
  private val key  = SecretName("HISTOS_APP_PRIVATE_KEY")
  private val ship = RenderingShip(CommitSha.trusted("0123456789abcdef0123456789abcdef01234567"))
  private val shim = Sha256.trusted("ab" * 32)

  def spec = suite("calls workflow laws")(
    test("a jar with no toolchain identity does not write a workflow"):
      Workspace
        .work("histos-calls-noship")
        .flatMap: dir =>
          val built = Projects
            .organization(organization"example.com")
            .supplyChain(chain)(module("wflowlaw", Jvm)(scala"3.10.0"))
          for
            graph <- ZIO.fromEither(built).catchAll(error => ZIO.dieMessage(error.message))
            loaded = LoadedBuild(graph, idle, HistosBuild.fixed(graph))
            (ran, heard) <- speak(say => Calls.workflow(dir, loaded, WorkflowAction.Update, say))
            ci           <- ZIO.attemptBlocking(Files.isRegularFile(Workflow.path(dir))).orDie
            script       <- ZIO.attemptBlocking(Files.isRegularFile(dir.resolve("histos"))).orDie
          yield assertTrue(
            ran == Right(()),
            done(heard).exists(faultHas("the rendering ship has no commit")),
            !ci,
            !script,
          )
    ,
    test("update writes the audit workflow, and check matches that file until an extra workflow appears"):
      Workspace
        .work("histos-calls-workflow")
        .flatMap: dir =>
          val built = Projects
            .organization(organization"example.com")
            .supplyChain(chain)(module("wflowlaw", Jvm)(scala"3.10.0"))
          for
            graph <- ZIO.fromEither(built).catchAll(error => ZIO.dieMessage(error.message))
            loaded = LoadedBuild(graph, idle, HistosBuild.fixed(graph))
            _                       <- origin(dir)
            (missing, missingHeard) <- speak(say => Calls.workflow(dir, loaded, WorkflowAction.Check, say, ship, shim))
            (updated, updatedHeard) <- speak(say => Calls.workflow(dir, loaded, WorkflowAction.Update, say, ship, shim))
            auditPath = Workflow.file(dir, SupplyYaml.auditName)
            ciPath    = Workflow.path(dir)
            auditFile               <- ZIO.attemptBlocking(Files.isRegularFile(auditPath)).orDie
            audit                   <- ZIO.attemptBlocking(Files.readString(auditPath)).orDie
            ciFile                  <- ZIO.attemptBlocking(Files.isRegularFile(ciPath)).orDie
            ci                      <- ZIO.attemptBlocking(Files.readString(ciPath)).orDie
            script                  <- ZIO.attemptBlocking(Files.readString(dir.resolve("histos"))).orDie
            (checked, checkedHeard) <- speak(say => Calls.workflow(dir, loaded, WorkflowAction.Check, say, ship, shim))
            _ <- ZIO.attemptBlocking(Files.writeString(Workflow.file(dir, "histos-extra.yml"), "name: extra\n")).orDie
            (drifted, driftedHeard) <- speak(say => Calls.workflow(dir, loaded, WorkflowAction.Check, say, ship, shim))
          yield
            val written = outs(updatedHeard)
            assertTrue(
              missing == Right(()),
              done(missingHeard).exists(faultHas("no workflow at")),
              done(missingHeard).exists(faultHas("ci.yml")),
              updated == Right(()),
              done(updatedHeard).contains(CallResult.reported()),
              written == ciPath.toString,
              auditFile,
              ciFile,
              audit.contains("0 6 * * *"),
              audit.contains("America/Chicago"),
              audit.contains("./histos deps audit"),
              audit.contains("security:"),
              audit.contains("needs: audit"),
              audit.contains("secrets.HISTOS_APP_ID"),
              audit.contains("secrets.HISTOS_APP_PRIVATE_KEY"),
              audit.contains("./histos deps update --security --yes"),
              audit.contains("./histos doctor --preflight audit"),
              ci.contains("./histos doctor --preflight"),
              ci.contains("./histos test --affected"),
              ci.contains(s"# ${ship.commit.value}"),
              script.contains(shim.value),
              script.contains(s"-ge ${Toolchain.Floor.value}"),
              !script.contains("HISTOS_VERSION"),
              checked == Right(()),
              done(checkedHeard).contains(CallResult.reported()),
              outs(checkedHeard) == ciPath.toString,
              drifted == Right(()),
              done(driftedHeard).exists(faultHas("histos-extra.yml")),
              done(driftedHeard).exists(faultHas("does not match the graph")),
            ),
  )

  private val chain = SupplyChain(
    advisories = AdvisoryPolicy(daily = DailyAudit.sixCentral),
    updates = UpdatePolicy(author = PrAuthor.App(id, key)),
    security = SecurityFixes.default,
    placement = DoctorPlacement.PreflightPlusWorkflowJob,
  )

  private val idle: PluginRuntime = new PluginRuntime:
    def launcher: ApiDigest = ApiDigest.trusted("a" * 64)

    def run(task: PluginTaskId, args: PluginArgs) =
      ZIO.dieMessage("plugin was not asked")
    def bind =
      ZIO.dieMessage("plugin was not asked")
    def links = Nil

  /** `origin/HEAD` names `main`, so the workflow's default branch is `main`. */
  private def origin(repo: Path): ZIO[Any, Nothing, Unit] =
    LawGit.git(repo, "init", "-b", "main") *>
      LawGit.git(repo, "symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/main").unit

  private def faultHas(text: String)(result: CallResult): Boolean =
    result match
      case CallResult.Fault(fault) => fault.message.contains(text)
      case _                       => false

  private def done(heard: Chunk[Message]): Option[CallResult] =
    heard.collect { case Message.Done(result) => result }.lastOption

  private def outs(heard: Chunk[Message]): String =
    heard.collect { case Message.Event(BuildEvent.Out(text)) => text }.mkString("\n")

  private def speak[R, E](use: Say => ZIO[R, E, Unit]): ZIO[R, Nothing, (Either[E, Unit], Chunk[Message])] =
    for
      messages <- Ref.make(Chunk.empty[Message])
      say = new Say(message => messages.update(_ :+ message))
      result <- use(say).either
      heard  <- messages.get
    yield (result, heard)
end CallsWorkflowLawSpec
