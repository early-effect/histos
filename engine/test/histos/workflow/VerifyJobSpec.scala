package histos.workflow

import histos.plugin.{JobName, TaskName}
import histos.{
  Axis,
  BuildError,
  ClientLink,
  Js,
  JsEnv,
  Jvm,
  NodeFloor,
  ProjectGraph,
  Projects,
  RunnerTools,
  ScalaVersion,
  Ship,
  Splits,
  ToolRevision,
  VerifyRun,
  VerifyTask,
  module,
  ships,
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
  private val node: RunnerTools = RunnerTools(Some(NodeFloor(25)), None, None)

  private val browser: RunnerTools =
    RunnerTools(Some(NodeFloor(25)), None, Some(ToolRevision("1.62.1")))

  def spec = suite("verify jobs")(
    test("a pin with no verify task keeps today's test job") {
      val text = render(Projects(api), NpmLock.Absent)
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
        Projects.runnerTools(llvm).docCheck.scalafmt.publishLocalSuite.verify(task)(api),
        NpmLock.Absent,
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
      val text = render(Projects.verify(task)(api), NpmLock.Absent)
      assertTrue(
        text.map { body =>
          val job = slice(body, "client")
          job.contains("./histos run host") && job.contains(Setup)
        } == Right(true),
        text.map(_.contains("toolchain install --client")) == Right(false),
        text.map(_.contains("publish:")) == Right(false),
      )
    },
    test("jsdom with a lockfile runs npm ci after node and before the suite") {
      val web  = module("web", Js)(ScalaVersion("3.10.1")).jsEnv(JsEnv.Jsdom)
      val text = render(Projects.runnerTools(node)(web), NpmLock.Present)
      assertTrue(text.map(body => lockedJs(slice(body, "test"))) == Right(true))
    },
    test("node with a lockfile runs npm ci after node and before the suite") {
      val web  = module("web", Js)(ScalaVersion("3.10.1"))
      val text = render(Projects.runnerTools(node)(web), NpmLock.Present)
      assertTrue(text.map(body => lockedJs(slice(body, "test"))) == Right(true))
    },
    test("a jvm module installs neither node nor npm ci") {
      val absent  = render(Projects.runnerTools(node)(api), NpmLock.Absent)
      val present = render(Projects.runnerTools(node)(api), NpmLock.Present)
      assertTrue(
        absent.map(body => bare(slice(body, "test"))) == Right(true),
        present.map(body => bare(slice(body, "test"))) == Right(true),
      )
    },
    test("javascript without a lockfile installs node and does not run npm ci") {
      val web  = module("web", Js)(ScalaVersion("3.10.1"))
      val text = render(Projects.runnerTools(node)(web), NpmLock.Absent)
      assertTrue(
        text.map { body =>
          val job = slice(body, "test")
          job.contains("setup-node") && !job.contains("npm ci")
        } == Right(true)
      )
    },
    test("playwright on a jvm module does not run npm ci") {
      val text = render(Projects.runnerTools(browser)(api), NpmLock.Present)
      assertTrue(
        text.map { body =>
          val job = slice(body, "test")
          job.contains("setup-node") && job.contains("npm install --prefix") && job.contains("chekhov") &&
          !job.contains("npm ci")
        } == Right(true)
      )
    },
    test("a module split guards npm ci and marks only the javascript cell") {
      val web  = module("web", Js)(ScalaVersion("3.10.1"))
      val text = render(Projects.runnerTools(node)(Splits.of(Axis.Module), web, api), NpmLock.Present)
      assertTrue(
        text.map { body =>
          val job = slice(body, "test")
          job.contains("matrix.npm == 'true'") && job.contains("run: \"npm ci\"") &&
          matrixRow(job, "web").contains("npm: \"true\"") && matrixRow(job, "api").contains("npm: \"false\"")
        } == Right(true)
      )
    },
    test("scaladoc and cache-seed install node and do not run npm ci") {
      val web  = module("web", Js)(ScalaVersion("3.10.1"))
      val text = render(
        Projects.runnerTools(node).docCheck(ships(Ship("docs").next(0, 1, 0)(web)), web),
        NpmLock.Present,
      )
      assertTrue(
        text.map { body =>
          val suite = slice(body, "test")
          val seed  = slice(body, "cache-seed")
          val doc   = slice(body, "scaladoc")
          suite.contains("npm ci") && !seed.contains("npm ci") && !doc.contains("npm ci") &&
          seed.contains("setup-node") && doc.contains("setup-node")
        } == Right(true)
      )
    },
  )

  /** `npm ci` follows `setup-node`, precedes the suite, and is not the Chekhov prefix install. */
  private def lockedJs(job: String): Boolean =
    val nodeAt = job.indexOf("setup-node")
    val ciAt   = job.indexOf("run: \"npm ci\"")
    val testAt = job.indexOf("./histos test")
    nodeAt >= 0 && ciAt > nodeAt && testAt > ciAt && !job.contains("npm install --prefix") && !job.contains(
      "chekhov"
    ) &&
    !job.contains("matrix.npm")

  private def bare(job: String): Boolean =
    !job.contains("setup-node") && !job.contains("npm ci")

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

  private def render(
      built: Either[BuildError, ProjectGraph],
      lock: NpmLock,
  ): Either[CiError | BuildError, String] =
    built match
      case Left(error)  => Left(error)
      case Right(graph) =>
        RenderingShip.from(ShipCommit) match
          case Left(error) => Left(error)
          case Right(ship) =>
            given RenderingShip = ship
            given CiBranch      = CiBranch("main")
            Workflow.render(graph, lock)

  private def slice(text: String, name: String): String =
    val lines = text.split("\n", -1).toVector
    val start = lines.indexWhere(_ == s"  $name:")
    lines.lift(start) match
      case None       => ""
      case Some(head) =>
        val rest = lines.drop(start + 1).takeWhile(line => !jobHead(line))
        (head +: rest).mkString("\n")

  private def matrixRow(job: String, name: String): String =
    val lines = job.split("\n", -1).toVector
    val start = lines.indexWhere(_.contains(s"- name: \"$name\""))
    lines.lift(start) match
      case None       => ""
      case Some(head) =>
        val rest = lines.drop(start + 1).takeWhile(line => !line.contains("- name:"))
        (head +: rest).mkString("\n")

  private def jobHead(line: String): Boolean =
    val body = line.dropWhile(_ == ' ')
    line.startsWith("  ") && !line.startsWith("   ") && body.endsWith(":") && body.indexOf(' ') < 0 && body.length > 1
end VerifyJobSpec
