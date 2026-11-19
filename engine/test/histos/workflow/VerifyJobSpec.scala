package histos.workflow

import histos.plugin.{JobName, TaskName}
import histos.{
  BuildError,
  ClientLink,
  Jvm,
  ProjectGraph,
  Projects,
  RunnerTools,
  ScalaVersion,
  ToolRevision,
  VerifyRun,
  VerifyTask,
  module,
}

import zio.test.*

object VerifyJobSpec extends ZIOSpecDefault:
  private val ShipCommit = "0123456789abcdef0123456789abcdef01234567"
  private val Setup      = "early-effect/histos/.github/actions/setup@"
  private val NoClient   = "install/histos toolchain install --no-client"
  private val WithClient = "install/histos toolchain install --client"
  private val api        = module("api", Jvm)(ScalaVersion("3.10.1"))
  private val host       = VerifyRun.BuiltIn(TaskName("host"))

  private val llvm: RunnerTools = RunnerTools(None, Some(ToolRevision("21.1.8")), None)

  def spec = suite("verify jobs")(
    test("a pin with no verify task keeps today's test job") {
      val text = render(Projects(api))
      assertTrue(
        text.map(_.contains("    needs: affected")) == Right(true),
        text.map(_.contains(Setup)) == Right(true),
        text.map(_.contains("publish:")) == Right(false),
        text.map(_.contains("install/histos toolchain install")) == Right(false),
      )
    },
    test("a local suite publishes, and the suite installs that jar") {
      val task = VerifyTask(JobName("client"), host, ClientLink.Native)
      val text = render(
        Projects.runnerTools(llvm).docCheck.scalafmt.publishLocalSuite.verify(task)(api)
      )
      assertTrue(
        text.map(body => !body.contains("HISTOS_SESSION") && !body.contains("HISTOS_BRANCH")) == Right(true),
        text.map(body => publishHost(slice(body, "publish"))) == Right(true),
        text.map(body =>
          suiteInstall(slice(body, "test")) && slice(body, "test").contains(
            "    needs: [affected, publish]"
          ) && slice(body, "test").contains("*LockInvocationSpec*") && slice(body, "test").contains(
            "*InstalledCompilerPayloadSpec*"
          )
        ) == Right(true),
        text.map(body =>
          suiteInstall(slice(body, "scaladoc")) && slice(body, "scaladoc").contains(
            "    needs: [publish]"
          )
        ) == Right(true),
        text.map(body =>
          suiteInstall(slice(body, "format")) && slice(body, "format").contains(
            "    needs: [publish]"
          )
        ) == Right(true),
        text.map(body => clientJob(slice(body, "client"))) == Right(true),
        text.map(body => seedStays(slice(body, "cache-seed"))) == Right(true),
      )
    },
    test("a pin verify task runs on the setup action and does not publish") {
      val task = VerifyTask(JobName("client"), host, ClientLink.Skip)
      val text = render(Projects.verify(task)(api))
      assertTrue(
        text.map { body =>
          val job = slice(body, "client")
          job.contains("./histos run host") && job.contains(Setup)
        } == Right(true),
        text.map(_.contains("toolchain install --client")) == Right(false),
        text.map(_.contains("publish:")) == Right(false),
      )
    },
  )

  private def publishHost(job: String): Boolean =
    job.contains("./histos publish local") && job.contains("name: host") && job.contains(Setup) &&
      job.contains("id: llvm")

  private def suiteInstall(job: String): Boolean =
    job.contains(NoClient) && !job.contains(Setup) && pinsEnv(job)

  /** The job selects the jar it just installed. Later steps see `GITHUB_ENV`. The pin file stays. */
  private def pinsEnv(job: String): Boolean =
    job.contains("printf 'HISTOS_PIN=%s\\n' \"$version\" >> \"$GITHUB_ENV\"") &&
      job.contains("export HISTOS_PIN=\"$version\"") &&
      !job.contains("mkdir -p .histos") &&
      !job.contains("> .histos/toolchain")

  private def clientJob(job: String): Boolean =
    job.contains(WithClient) && pinsEnv(job) && job.contains("./histos run host") && job.contains("id: llvm") &&
      job.contains("path: \"~/.cache/coursier/v1\"") && !job.contains(Setup) && !job.contains("setup-node") &&
      !job.contains("playwright")

  private def seedStays(job: String): Boolean =
    job.contains(Setup) && job.contains("github.event_name == 'push'") && !job.contains("needs:")

  private def render(built: Either[BuildError, ProjectGraph]): Either[CiError | BuildError, String] =
    built match
      case Left(error)  => Left(error)
      case Right(graph) =>
        RenderingShip.from(ShipCommit) match
          case Left(error) => Left(error)
          case Right(ship) =>
            given RenderingShip = ship
            given CiBranch      = CiBranch("main")
            Workflow.render(graph)

  private def slice(text: String, name: String): String =
    val lines = text.split("\n", -1).toVector
    val start = lines.indexWhere(_ == s"  $name:")
    lines.lift(start) match
      case None       => ""
      case Some(head) =>
        val rest = lines.drop(start + 1).takeWhile(line => !jobHead(line))
        (head +: rest).mkString("\n")

  private def jobHead(line: String): Boolean =
    val body = line.dropWhile(_ == ' ')
    line.startsWith("  ") && !line.startsWith("   ") && body.endsWith(":") && body.indexOf(' ') < 0 && body.length > 1
end VerifyJobSpec
