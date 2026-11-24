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
    test("review OpenSSL: matrix libraries do not depend on a pinned LLVM requirement") {
      val native = histos
        .module("native", histos.Native)(ScalaVersion("3.10.1"))
        .clang(histos.ClangMajor(21))
        .link(histos.NativeLibrary.Ssl)
      val text = render(Projects(Splits.of(Axis.Platform), api, native), NpmLock.Absent)
      assertTrue(
        text.exists(body =>
          slice(body, "test").contains("matrix.openssl == 'true'") && matrixRow(slice(body, "test"), "native").contains(
            "openssl: \"true\""
          )
        )
      )
    },
    test("typed JVM verify tests run test, install the JDK floor and all selected browsers") {
      val tools = browser.copy(
        jdk = Some(histos.JdkFloor(25)),
        browsers = Set(histos.Browser.Chromium, histos.Browser.Firefox, histos.Browser.WebKit),
      )
      val selected = histos.TestSelection(
        histos.TestTargets.Named(histos.ProjectName("api"), Nil),
        histos.PlatformSelection.only(Jvm),
      )
      val task = VerifyTask(JobName("browsers"), VerifyRun.Tests(selected))
      val text = render(Projects.runnerTools(tools).verify(task)(api), NpmLock.Absent)
      assertTrue(text.exists: body =>
        val job = slice(body, "browsers")
        job.contains("./histos test api --platform jvm") && !job.contains("./histos run test") &&
        job.contains("setup-java") && job.contains("java-version: \"25\"") &&
        job.contains("setup-node") && job.contains("install chromium firefox webkit") &&
        job.contains("install-deps chromium firefox webkit") && !job.contains("id: llvm"))
    },
    test("selected JVM cells on a cross module do not install Native or Node tools") {
      val cross = histos
        .module("cross", Jvm, Js, histos.Native)(ScalaVersion("3.10.1"))
        .clang(histos.ClangMajor(21))
        .link(histos.NativeLibrary.Ssl)
      val selected = histos.TestSelection(
        histos.TestTargets.Named(histos.ProjectName("cross"), Nil),
        histos.PlatformSelection.only(Jvm),
      )
      val task  = VerifyTask(JobName("jvm-check"), VerifyRun.Tests(selected))
      val built = Projects.runnerTools(llvm.copy(node = Some(NodeFloor(25)))).verify(task)(cross)
      val text  = render(built, NpmLock.Absent)
      assertTrue(text.exists: body =>
        val job = slice(body, "jvm-check")
        job.contains("./histos test cross --platform jvm") && !job.contains("setup-node") && !job.contains(
          "id: llvm"
        ) && !job.contains("libssl-dev"))
    },
    test("Native verify tests install LLVM and OpenSSL from the requested cells") {
      val native = histos
        .module("native", histos.Native)(ScalaVersion("3.10.1"))
        .clang(histos.ClangMajor(21))
        .link(histos.NativeLibrary.Ssl)
      val selected = histos.TestSelection(
        histos.TestTargets.Named(histos.ProjectName("native"), Nil),
        histos.PlatformSelection.only(histos.Native),
      )
      val task = VerifyTask(JobName("native-check"), VerifyRun.Tests(selected))
      val text = render(Projects.runnerTools(llvm).verify(task)(native), NpmLock.Absent)
      assertTrue(text.exists: body =>
        val job = slice(body, "native-check")
        job.contains("./histos test native --platform native") && job.contains("id: llvm") && job.contains(
          "libssl-dev"
        ) && !job.contains("setup-node"))
    },
    test("a pin compatibility job runs publish check against the base") {
      val other = module("other", Jvm)(ScalaVersion("3.10.1"))
      val text  = render(Projects(ships(Ship("libs").next(0, 1, 0)(api, other)), api, other), NpmLock.Absent)
      assertTrue(text.exists: body =>
        val job = slice(body, "compatibility")
        job.contains("./histos publish check --base \"$base\"") && job.contains("base=all") &&
        job.contains("PR_BASE") && !job.contains("doctor --area release") && !job.contains("./histos compile") &&
        !job.contains("continue-on-error") && !job.contains("--preflight") &&
        !job.contains("HISTOS_CACHE_MODE") &&
        slice(body, "supply-chain").contains("./histos compile --platform jvm"))
    },
    test("a local suite does not reserve the compatibility job name") {
      val task = VerifyTask(JobName("compatibility"), host)
      val text = render(
        Projects.publishLocalSuite.verify(task)(ships(Ship("libs").next(0, 1, 0)(api)), api),
        NpmLock.Absent,
      )
      assertTrue(text.exists: body =>
        slice(body, "compatibility").contains("./histos run host") &&
          !body.contains("./histos publish check") &&
          slice(body, "publish").contains("./histos publish local --base \"$base\""))
    },
    test("a user job cannot collide with the complete compatibility check") {
      val task = VerifyTask(JobName("compatibility"), host)
      val text = render(Projects.verify(task)(ships(Ship("libs").next(0, 1, 0)(api)), api), NpmLock.Absent)
      assertTrue(text == Left(CiError.Fault(histos.CiFault.RenderedJob(JobName("compatibility")))))
    },
    test("main CI runs required verification instead of publishing from skipped PR-only checks") {
      val text = render(Projects.docCheck.scalafmt(api), NpmLock.Absent)
      assertTrue(text.exists: body =>
        List("test", "scaladoc", "format", "workflow").forall(name =>
          slice(body, name).contains("github.event_name == 'push'")
        ))
    },
    test("affected installs the toolchain and every later job restores it") {
      val text = render(Projects(api), NpmLock.Absent)
      assertTrue(
        text.map { body =>
          val first = slice(body, "affected")
          !first.contains("needs:") && first.contains(".github/actions/setup@") &&
          slice(body, "workflow").contains("needs: [affected]") &&
          slice(body, "supply-chain").contains("needs: [affected]") &&
          slice(body, "dependency-snapshot").contains("needs: [affected]") &&
          slice(body, "cache-seed").contains("needs: [affected]")
        } == Right(true)
      )
    },
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
        text.map(body =>
          !body.contains("HISTOS_SESSION") && !body.contains("HISTOS_BRANCH") &&
            !body.contains("\n  compatibility:\n") &&
            slice(body, "publish").contains("./histos publish local --base \"$base\"") &&
            slice(body, "publish").contains("base=all") &&
            slice(body, "publish").contains("PR_BASE") &&
            !slice(body, "publish").contains("doctor --area release") &&
            !slice(body, "workflow").contains("doctor --area release")
        ) == Right(true),
        text.map(body => publishHost(slice(body, "publish")) && publishStores(slice(body, "publish"))) == Right(
          true
        ),
        text.map(body =>
          suiteInstall(slice(body, "test")) && restoresStore(slice(body, "test")) && slice(body, "test").contains(
            "    needs: [affected, publish]"
          ) && slice(body, "test").contains("*LockInvocationSpec*") && slice(body, "test").contains(
            "*InstalledCompilerPayloadSpec*"
          )
        ) == Right(true),
        text.map(body =>
          suiteInstall(slice(body, "scaladoc")) && restoresStore(slice(body, "scaladoc")) &&
            slice(body, "scaladoc").contains("    needs: [affected, publish]") &&
            !slice(body, "scaladoc").contains("actions/cache/save@")
        ) == Right(true),
        text.map(body =>
          suiteInstall(slice(body, "format")) && slice(body, "format").contains(
            "    needs: [affected, publish]"
          ) && !slice(body, "format").contains("~/.cache/histos")
        ) == Right(true),
        text.map(body =>
          clientJob(slice(body, "client")) && !slice(body, "client").contains("~/.cache/histos")
        ) == Right(true),
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

  private def publishStores(job: String): Boolean =
    val restoreAt = job.indexOf("actions/cache/restore@")
    val publishAt = job.indexOf("./histos publish local")
    val saveAt    = job.indexOf("actions/cache/save@")
    restoreAt >= 0 && publishAt > restoreAt && saveAt > publishAt &&
    job.contains("path: \"~/.cache/histos\"") &&
    job.contains("histos-store-") &&
    job.contains("HISTOS_CACHE_NAMESPACE:") &&
    job.contains("HISTOS_CACHE_MODE: \"write\"")

  private def restoresStore(job: String): Boolean =
    job.contains("actions/cache/restore@") &&
      job.contains("path: \"~/.cache/histos\"") &&
      job.contains("histos-store-")

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
    job.contains(Setup) && job.contains("github.event_name == 'push'") && job.contains("needs: [affected]")

  private def render(
      built: Either[BuildError, ProjectGraph],
      lock: NpmLock,
  ): Either[CiError | BuildError | histos.testrun.TestSelectionError, String] =
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
