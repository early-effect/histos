package histos.workflow

import histos.doctor.DoctorPlacement
import histos.supply.SupplyChain
import histos.plugin.{
  ActionName,
  ActionPin,
  AwsOidc,
  CiJob,
  CiSettings,
  Cond,
  Contribution,
  Cancel,
  Concurrency,
  Cron,
  DispatchInput,
  EntryName,
  EnvName,
  EnvValue,
  EventKind,
  Grant,
  GraphSecrets,
  GroupName,
  Guard,
  InputKey,
  InputKind,
  JobName,
  JobPermissions,
  JobRender,
  On,
  PluginId,
  RegisteredPlugin,
  RepoPath,
  Runner,
  RunnerName,
  SecretName,
  Site,
  SiteArrange,
  SiteJob,
  SitePublish,
  SiteShape,
  Target,
  TargetName,
  TaskJob,
  TaskName,
  Trigger,
}
import histos.version.CommitSha
import histos.schedule.CacheNamespace
import histos.version.{Catalog, Ship, ShipName, ShipNumber}
import histos.{
  Axis,
  HttpUrl,
  MainClass,
  Module,
  NodeFloor,
  OrganizationFacts,
  Platform,
  Platforms,
  ProjectDep,
  ProjectGraph,
  ProjectName,
  Publishing,
  ReleaseMode,
  Resolution,
  RunnerTools,
  ScalaVersion,
  ScalaVersions,
  Scm,
  ScmConnection,
  Sona,
  SourcePos,
  Splits,
  ToolRevision,
}
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{Scope, System, ZIO}
import zio.test.*

object WorkflowSpec extends ZIOSpecDefault:
  private given CiBranch     = CiBranch("main")
  private val v390           = ScalaVersion.trusted("3.9.0")
  private val core           = ProjectName("core")
  private val tests          = ProjectName("tests")
  private val nodeOnly       = RunnerTools(Some(NodeFloor(22)), None, None, None)
  private val llvmOnly       = RunnerTools(None, Some(ToolRevision("20.1.8")), None, None)
  private val playwrightOnly = RunnerTools(None, None, Some(ToolRevision("1.49.0")), None)
  private val automatic      = Sona.described(ReleaseMode.Automatic)
  private val hold           = Sona.described(ReleaseMode.Hold)

  private val render = suite("render")(
    test("the push filter is the branch render was given"):
      val built = graph(Splits.aggregate)
      val main  = Workflow.render(built)(using CiBranch("main"))
      val trunk = Workflow.render(built)(using CiBranch("trunk"))
      assertTrue(
        main.exists(_.contains("push:\n    branches:\n      - \"main\"")),
        trunk.exists(_.contains("push:\n    branches:\n      - \"trunk\"")),
        trunk.exists(text => !text.contains("branches:\n      - \"main\"")),
        trunk.exists(_.contains("github.event.repository.default_branch")),
        trunk.exists(text => !text.contains("${{ github.event.repository.default_branch }}")),
      )
    ,
    test("origin HEAD is the name written into push"):
      ZIO.scoped:
        for
          root <- temp
          _    <- ZIO.attempt(pointOrigin(root, "develop"))
          found <- CiBranch.read(root)
          rendered = Workflow.render(graph(Splits.aggregate))(using found)
        yield assertTrue(
          found.name == "develop",
          rendered.exists(_.contains("push:\n    branches:\n      - \"develop\"")),
        )
    ,
    test("the empty split set is one aggregate test job"):
      for
        golden  <- text("aggregate.yml")
        drifted <- text("drifted.yml")
        rendered = Workflow.render(graph(Splits.aggregate))
      yield assertTrue(
        rendered == Right(golden),
        golden.contains("./histos test"),
        golden.contains("contents: read"),
        golden.contains("workflow --check"),
        !golden.contains("setup-java"),
        !golden.contains("setup-node"),
        !golden.contains("llvm.tar.xz"),
        !golden.contains("playwright"),
        !golden.contains("format:"),
        !golden.contains("publish"),
        golden.contains("push:\n    branches:\n      - \"main\""),
        golden.contains("pull_request:"),
        golden.contains("github.event.repository.default_branch"),
        !golden.contains("branches:\n      - \"${{"),
        !golden.contains("branches:\n      - main"),
        !golden.contains("\"**\""),
        !golden.contains("tags:"),
        Workflow.differs(golden, drifted) == Left(CiError.Drift),
        Workflow.argv(TestJob.Aggregate(TestTarget.Entire)) == List(
          "./histos",
          "test",
        ),
      )
    ,
    test("every generated uses is a full sha or a local path, and both placements render"):
      val pinned = "uses: \\S+@[0-9a-f]{40} # \\S+".r
      val lean   = graph(
        Splits.aggregate,
        supply = SupplyChain(placement = DoctorPlacement.WorkflowJobOnly),
      )
      val daily = graph(
        Splits.aggregate,
        supply = SupplyChain(advisories = histos.supply.AdvisoryPolicy(daily = histos.supply.DailyAudit.sixCentral)),
      )
      val all                               = Workflow.renderAll(graph(Splits.aggregate), automatic)
      val bare                              = Workflow.render(lean)
      val audit                             = Workflow.renderAll(daily, automatic)
      def pinnedUses(body: String): Boolean =
        body.split('\n').forall { line =>
          !line.contains("uses:") || line.contains("uses: ./") || pinned.findFirstIn(line).isDefined
        }
      val pinsOk = all match
        case Right(files) =>
          files.forall(file => pinnedUses(file.body)) &&
          files.exists(_.name == SupplyYaml.depsName) &&
          files.exists(_.body.contains("timezone: \"America/Chicago\""))
        case Left(_) => false
      val leanOk = bare match
        case Right(yaml) => !yaml.contains("doctor --preflight") && yaml.contains("run: ./histos doctor")
        case Left(_)     => false
      val auditOk = audit match
        case Right(files) => files.exists(_.name == SupplyYaml.auditName)
        case Left(_)      => false
      assertTrue(pinsOk, leanOk, auditOk)
    ,
    test("a module split is a matrix, one cell per module"):
      val rendered = Workflow.render(graph(Splits.of(Axis.Module)))
      assertTrue:
        rendered match
          case Right(yaml) =>
            val test = slice(yaml, "  test:", "  cache-seed:").getOrElse("")
            test.contains("fail-fast: false") &&
            test.contains("\"./histos test core\"") &&
            test.contains("\"./histos test tests\"") &&
            !test.contains("setup-java") && !test.contains("setup-node") &&
            !test.contains("llvm.tar.xz") && !test.contains("playwright") &&
            !test.contains("~/.cache/histos")
          case Left(_) => false
    ,
    test("a catalog adds the versions command and still does not publish"):
      val rendered = (ShipName.from("libs"), ShipNumber.from("1.4.2")) match
        case (Right(name), Right(number)) =>
          Catalog.from(List(Ship.Line(name, number, ::(core, Nil)))) match
            case Right(catalog) =>
              Workflow.render(
                ProjectGraph.trusted(
                  List(module(ProjectName("core"), Platform.Jvm, Nil)),
                  Splits.aggregate,
                  catalog,
                )
              ) match
                case Right(yaml) =>
                  yaml.contains("./histos run versions") && !yaml.contains(
                    "publish"
                  ) && !yaml.contains("format:")
                case Left(_) => false
            case Left(_) => false
        case _ => false
      assertTrue(rendered)
    ,
    test("update writes the render and check rejects a drifted file"):
      ZIO.scoped:
        for
          root  <- temp
          file  <- Workflow.update(root, graph(Splits.aggregate), automatic)
          read  <- ZIO.attempt(Files.readString(file))
          again <- Workflow.check(root, graph(Splits.aggregate), automatic)
          wrote = Files.isRegularFile(file)
          _       <- ZIO.attempt(Files.writeString(file, read + "\n"))
          drifted <- Workflow.check(root, graph(Splits.aggregate), automatic).either
          gone    <- Workflow
            .check(root.resolve("absent"), graph(Splits.aggregate), automatic)
            .either
        yield assertTrue(
          file == Workflow.path(root) && wrote,
          again == file,
          drifted == Left(CiError.Drift),
          gone match
            case Left(CiError.Missing(path)) =>
              path == Workflow.path(root.resolve("absent"))
            case _ => false,
        )
    ,
    test("one tool renders that tool only"):
      for
        node       <- text("node.yml")
        llvm       <- text("llvm.yml")
        playwright <- text("playwright.yml")
      yield assertTrue(
        Workflow.render(graph(Splits.aggregate, nodeOnly, Platform.Js)) == Right(node),
        Workflow.render(graph(Splits.aggregate, llvmOnly, Platform.Native)) == Right(llvm),
        Workflow.render(graph(Splits.aggregate, playwrightOnly)) == Right(playwright),
        node.contains("restore-keys: histos-osv-") &&
          !node.replace("restore-keys: histos-osv-", "").contains("restore-keys") &&
          llvm.contains("restore-keys: histos-osv-") &&
          playwright.contains("restore-keys: histos-osv-"),
        !node.contains("cache: npm") && !node.contains("setup-java"),
        node.contains("hashFiles('histos.scala')") &&
          node.contains("${{ runner.os }}") && node.contains("${{ runner.arch }}"),
        llvm.contains("llvm-20.1.8-${{ runner.os }}-${{ runner.arch }}") &&
          llvm.contains("steps.llvm.outputs.cache-hit") &&
          llvm.contains("test -x \"$dir/bin/clang\"") &&
          !llvm.contains("if: steps.llvm.outputs.cache-hit"),
        playwright.contains("id: playwright") &&
          playwright.contains("steps.playwright.outputs.cache-hit") &&
          !playwright.contains("if: steps.playwright.outputs.cache-hit") &&
          playwright.contains("install-deps firefox") &&
          playwright.contains("playwright-1.49.0-${{ runner.os }}-${{ runner.arch }}") &&
          playwright.contains("~/.cache/chekhov/playwright/1.49.0") &&
          playwright.contains("~/.cache/ms-playwright"),
        Workflow.render(
          graph(
            Splits.aggregate,
            RunnerTools(Some(NodeFloor(25)), Some(ToolRevision("21.1.8")), None, None),
          )
        ) match
          case Right(copied) =>
            !copied.contains("setup-node") && !copied.contains("llvm.tar.xz") &&
            !copied.contains("playwright")
          case Left(_) => false,
      )
    ,
    test("a jvm cell does not install llvm while the native cell does"):
      val jvm    = module(ProjectName("core"), Platform.Jvm, Nil)
      val native = module(ProjectName("native"), Platform.Native, Nil)
      val built  = ProjectGraph.trusted(
        List(jvm, native),
        Splits.of(Axis.Platform),
        Catalog.empty,
        None,
        Nil,
        runnerTools = RunnerTools(None, Some(ToolRevision("21.1.8")), None, None),
      )
      assertTrue:
        Workflow.render(built) match
          case Right(yaml) =>
            yaml.contains("if: matrix.llvm == 'true'") &&
            yaml.contains("LLVM-21.1.8-Linux-X64.tar.xz") &&
            yaml.contains("steps.llvm.outputs.cache-hit") &&
            !yaml.contains("if: steps.llvm.outputs.cache-hit") &&
            yaml.contains("llvm: \"false\"") &&
            yaml.contains("llvm: \"true\"") &&
            !yaml.contains("setup-node")
          case Left(_) => false
    ,
    test("a locked build keys Coursier on the pin"):
      val open = graph(Splits.aggregate, nodeOnly)
      assertTrue:
        Workflow.render(
          ProjectGraph.trusted(
            open.projects,
            open.splits,
            Catalog.empty,
            None,
            Nil,
            runnerTools = nodeOnly,
            resolution = Resolution.Lock,
          )
        ) match
          case Right(yaml) => yaml.contains("hashFiles('dependencies.lock')")
          case Left(_)     => false
    ,
    test("a doc check runs beside the suite and does not save the store"):
      val checked = histos.Projects.docCheck(
        histos.module("core", histos.Jvm)(histos.ScalaVersion("3.10.0"))
      )
      assertTrue:
        checked match
          case Right(built) =>
            Workflow.render(built) match
              case Right(yaml) =>
                slice(yaml, "  doc:", "  cache-seed:") match
                  case Some(doc) =>
                    doc.contains("./histos run doc") &&
                    doc.contains("actions/cache/restore@") &&
                    !doc.contains("actions/cache/save@") &&
                    !doc.contains("needs:") &&
                    !doc.contains("actions: write")
                  case None => false
              case Left(_) => false
          case Left(_) => false
    ,
    test("this repository asks the runner for node, llvm, and playwright"):
      for golden <- text("histos.yml")
      yield assertTrue:
        repository match
          case Right(built) => Workflow.render(built) == Right(golden)
          case Left(_)      => false
    ,
    test("this repository builds one assembly and other builds stay on the shim"):
      val base = graph(Splits.aggregate)
      val api  = histos.module("api", Platform.Jvm)(ScalaVersion("3.10.0"))
      def hosted(site: histos.HttpUrl, remote: histos.ScmConnection) =
        histos.Projects
          .organization(histos.Organization("rocks.earlyeffect"))
          .scm(histos.Scm(site, remote))(
            histos.ships(histos.ship("lib", histos.version.ShipNumber("0.2.0"))(api)),
            api,
          )
      val own = hosted(
        histos.HttpUrl("https://github.com/early-effect/histos"),
        histos.ScmConnection("scm:git@github.com:early-effect/histos.git"),
      )
      val other = hosted(
        histos.HttpUrl("https://github.com/early-effect/other"),
        histos.ScmConnection("scm:git@github.com:early-effect/other.git"),
      )
      def ciOf(project: Either[histos.BuildError, ProjectGraph]) =
        project match
          case Right(built) =>
            Workflow.renderAll(built, automatic) match
              case Right(files) => files.find(_.name == "ci.yml").map(_.body)
              case Left(_)      => None
          case Left(_) => None
      def depsOf(project: Either[histos.BuildError, ProjectGraph]) =
        project match
          case Right(built) =>
            Workflow.renderAll(built, automatic) match
              case Right(files) => files.find(_.name == "histos-deps.yml").map(_.body)
              case Left(_)      => None
          case Left(_) => None
      val ownCi = ciOf(own).getOrElse("")
      val workflowAt = ownCi.indexOf("\n  workflow:\n")
      val supplyAt   = ownCi.indexOf("\n  supply-chain:\n")
      val before     = if workflowAt > 0 then ownCi.substring(0, workflowAt) else ownCi
      val workflow   =
        if workflowAt >= 0 && supplyAt > workflowAt then ownCi.substring(workflowAt, supplyAt) else ""
      val supply = if supplyAt >= 0 then ownCi.substring(supplyAt) else ""
      assertTrue(
        ownCi.nonEmpty,
        ownCi.contains("\n  toolchain:\n"),
        ownCi.contains("doctor --preflight workflow"),
        ownCi.contains("./histos workflow --check"),
        ownCi.split("cp .histos/assembly.sha256").length == 2,
        ownCi.contains("needs: [affected, toolchain]"),
        ownCi.contains("\n    needs: toolchain\n"),
        !before.contains("if [ ! -f dist/histos.jar ]"),
        workflow.contains("./histos assembly"),
        supply.contains("if [ ! -f dist/histos.jar ]; then"),
        supply.contains("./histos workflow --check"),
        depsOf(own) match
          case Some(deps) => deps.contains("case \"$usage\" in") && !deps.contains("needs: toolchain")
          case None       => false
        ,
        ciOf(other) match
          case Some(yaml) => !yaml.contains("toolchain:")
          case None       => false
        ,
        Workflow.render(base) match
          case Right(yaml) => !yaml.contains("histos-bootstrap")
          case _           => false,
      )
    ,
    test("a native client module is linked before the suite"):
      val api    = histos.module("api", Platform.Jvm)(ScalaVersion("3.10.0"))
      val client = histos
        .module("client", Platform.Native)(ScalaVersion("3.10.0"))
        .clang(histos.ClangMajor(21))
        .main(histos.MainClass("histos.client.Main"))
      val built = histos.Projects
        .organization(histos.Organization("rocks.earlyeffect"))
        .runnerTools(
          RunnerTools(
            Some(NodeFloor(25)),
            Some(ToolRevision("21.1.8")),
            Some(ToolRevision("1.62.1")),
            None,
          )
        )(
          histos.ships(histos.ship("histos", histos.version.ShipNumber("0.0.1"))(api)),
          api,
          client,
        )
      assertTrue:
        built match
          case Right(graph) =>
            Workflow.render(graph) match
              case Right(yaml) =>
                val linked = yaml.indexOf("./histos client")
                val suite  = yaml.indexOf("./histos test")
                linked >= 0 && suite > linked
              case Left(_) => false
          case Left(_) => false,
  )

  private val affected = suite("affected")(
    test(
      "a pull request runs the changed module and its dependents, not its dependencies"
    ):
      val built = graph(Splits.aggregate)
      val plan  = Workflow.plan(built, CiEvent.PullRequest(Set(core)))
      val leaf  = Workflow.plan(built, CiEvent.PullRequest(Set(tests)))
      val none  = Workflow.plan(built, CiEvent.PullRequest(Set.empty))
      val full  = Workflow.plan(built, CiEvent.Other)
      assertTrue(
        plan == Right(
          List(TestJob.Aggregate(TestTarget.Named(core, List(tests))))
        ),
        leaf == Right(List(TestJob.Aggregate(TestTarget.Named(tests, Nil)))),
        none == Right(Nil),
        full == Right(List(TestJob.Aggregate(TestTarget.Entire))),
      )
    ,
    test("names select modules and do not pull in dependents"):
      val built = graph(Splits.aggregate)
      assertTrue(
        Workflow.target(built, Nil) == Right(TestTarget.Entire),
        Workflow.target(built, List("core")) == Right(
          TestTarget.Named(core, Nil)
        ),
        Workflow.target(built, List("core", "core")) == Left(
          CiError.Duplicate(core)
        ),
        Workflow.target(built, List("1core")) == Left(CiError.BadName("1core")),
        Workflow.target(built, List("missing")) == Left(
          CiError.Unknown(ProjectName("missing"))
        ),
      )
    ,
    test("an unknown project is not dropped"):
      val missing = ProjectName("missing")
      assertTrue(
        Workflow.plan(
          graph(Splits.aggregate),
          CiEvent.PullRequest(Set(missing)),
        ) == Left(CiError.Unknown(missing))
      )
    ,
    test("a module matrix drops the unchanged module"):
      val built = graph(Splits.of(Axis.Module))
      val plan  = Workflow.plan(built, CiEvent.PullRequest(Set(tests)))
      assertTrue(
        plan == Right(
          List(
            TestJob.Split(ModuleRef.One(tests), PlatformRef.All, VersionRef.All)
          )
        )
      )
    ,
    test("a platform cell stays only when an affected module targets it"):
      val js    = module(ProjectName("jsapp"), Platform.Js, Nil)
      val jvm   = module(ProjectName("core"), Platform.Jvm, Nil)
      val built = ProjectGraph.trusted(List(jvm, js), Splits.of(Axis.Platform))
      val plan  =
        Workflow.plan(built, CiEvent.PullRequest(Set(ProjectName("jsapp"))))
      assertTrue(
        plan == Right(
          List(
            TestJob.Split(
              ModuleRef.All,
              PlatformRef.One(Platform.Js),
              VersionRef.All,
            )
          )
        ),
        Workflow.argv(
          TestJob.Split(
            ModuleRef.All,
            PlatformRef.One(Platform.Js),
            VersionRef.All,
          )
        ) ==
          List("./histos", "test", "--platform", "js"),
      ),
  )

  private def graph(
      splits: Splits,
      tools: RunnerTools = RunnerTools.empty,
      platform: Platform = Platform.Jvm,
      supply: SupplyChain = SupplyChain(),
  ): ProjectGraph =
    val library = module(ProjectName("core"), platform, Nil)
    val suite   = module(ProjectName("tests"), platform, List(core))
    ProjectGraph.trusted(
      List(library, suite),
      splits,
      Catalog.empty,
      None,
      Nil,
      runnerTools = tools,
      supply = supply,
    )

  /** The tools and the manual ship this repository names. The module list does not appear in an aggregate render. */
  private def repository: Either[histos.BuildError, ProjectGraph] =
    val api    = histos.module("api", Platform.Jvm)(ScalaVersion("3.10.0"))
    val native = histos
      .module("native", Platform.Native)(ScalaVersion("3.10.0"))
      .clang(histos.ClangMajor(21))
    histos.Projects
      .organization(histos.Organization("rocks.earlyeffect"))
      .runnerTools(
        RunnerTools(
          Some(NodeFloor(25)),
          Some(ToolRevision("21.1.8")),
          Some(ToolRevision("1.62.1")),
          None,
        )
      )(
        histos.ships(histos.ship("histos", histos.version.ShipNumber("0.0.1"))(api)),
        api,
        native,
      )

  private def module(
      name: ProjectName,
      platform: Platform,
      depends: List[ProjectName],
  ): Module =
    Module(
      name,
      Platforms(platform, Nil),
      ScalaVersions(v390, Nil),
      Nil,
      depends.map(name => ProjectDep(name, None)),
      SourcePos("histos.scala", 1, 1),
    )

  private enum FixtureFault:
    case Missing(name: String)
    case Unreadable(name: String, detail: String)

  private def text(name: String): ZIO[Any, FixtureFault, String] =
    val relative = List(
      Path.of("engine/test/fixtures/ci").resolve(name),
      Path.of("test/fixtures/ci").resolve(name),
    )
    def unreadable(error: Throwable): FixtureFault =
      FixtureFault.Unreadable(name, error.toString)
    System
      .env("HISTOS_REPO")
      .mapError(unreadable)
      .flatMap: root =>
        val candidates = root
          .filter(_.nonEmpty)
          .toList
          .map(
            Path.of(_).resolve("engine/test/fixtures/ci").resolve(name)
          ) ++ relative
        ZIO
          .attempt(candidates.find(Files.isRegularFile(_)))
          .mapError(unreadable)
          .flatMap:
            case Some(file) => ZIO.attempt(Files.readString(file)).mapError(unreadable)
            case None       => ZIO.fail(FixtureFault.Missing(name))

  private def temp: ZIO[Scope, Throwable, Path] =
    Workspace
      .work("histos-workflow")
      .mapError(error => new java.io.IOException(error.message))

  private def pointOrigin(repo: Path, branch: String): Unit =
    def git(args: String*): Unit =
      val process = new ProcessBuilder((Array("git", "-C", repo.toString) ++ args)*)
        .redirectErrorStream(true)
        .start()
      val text = String(process.getInputStream.readAllBytes)
      if process.waitFor() != 0 then throw new java.io.IOException(text)
    git("init")
    git("symbolic-ref", "refs/remotes/origin/HEAD", s"refs/remotes/origin/$branch")

  private val plugins = suite("plugins")(
    test("a job that names a site directory uploads on the default branch and deploys from a pages job"):
      val rendered = Workflow.render(
        withJobs(
          List(
            SiteJob(JobName("docs"), TaskName("site"), Site(RepoPath("target/site")))
          )
        )
      )
      for golden <- text("plugin-pages-ci.yml")
      yield assertTrue(rendered == Right(golden))
    ,
    test("manual builds on the default branch, and both matches the automatic ci file"):
      val manual                   = Workflow.render(withJobs(List(siteJob(SitePublish.Manual))))
      val both                     = Workflow.render(withJobs(List(siteJob(SitePublish.Both))))
      val automaticMode            = Workflow.render(withJobs(List(siteJob(SitePublish.Automatic))))
      def files(mode: SitePublish) = Workflow.renderAll(withJobs(List(siteJob(mode))), automatic)
      for
        plain    <- text("plugin-manual.yml")
        pages    <- text("plugin-pages-ci.yml")
        dispatch <- text("plugin-pages-dispatch.yml")
      yield assertTrue(
        manual == Right(plain),
        both == Right(pages),
        automaticMode == Right(pages),
        files(SitePublish.Automatic).map(_.map(_.name)) == Right(List(Workflow.fileName, SupplyYaml.depsName)),
        files(SitePublish.Manual).map(_.map(_.name)) == Right(
          List(Workflow.fileName, SupplyYaml.depsName, Workflow.pagesName)
        ),
        files(SitePublish.Both).map(_.map(_.name)) == Right(
          List(Workflow.fileName, SupplyYaml.depsName, Workflow.pagesName)
        ),
        files(SitePublish.Manual).map(pagesBody) == Right(List(dispatch)),
        files(SitePublish.Both).map(pagesBody) == Right(List(dispatch)),
      )
    ,
    test("two sites and a scheduled site are refused at render"):
      val twice = withJobs(
        List(
          siteJob(SitePublish.Both),
          SiteJob(JobName("deploy"), TaskName("ship"), Site(RepoPath("other"))),
        )
      )
      val nightly = withJobs(
        List(
          Twice(JobName("nightly"), TaskName("site"), Site(RepoPath("target/site")))
        )
      )
      assertTrue(
        Workflow.render(twice) == Left(CiError.DuplicateSite(JobName("docs"), JobName("deploy"))),
        Workflow.render(nightly) == Left(CiError.IllegalSite(JobName("nightly"))),
        Workflow.renderAll(twice, automatic) == Left(CiError.DuplicateSite(JobName("docs"), JobName("deploy"))),
        Workflow.renderAll(nightly, automatic) == Left(CiError.IllegalSite(JobName("nightly"))),
      )
    ,
    test("a need names a generated job and never pages or itself"):
      val present = withJobs(List(TaskJob(JobName("docs"), TaskName("site"), needs = List(JobName("test")))))
      val missing = withJobs(List(TaskJob(JobName("docs"), TaskName("site"), needs = List(JobName("absent")))))
      val pages   = withJobs(List(TaskJob(JobName("docs"), TaskName("site"), needs = List(JobName("pages")))))
      val self    = withJobs(List(TaskJob(JobName("docs"), TaskName("site"), needs = List(JobName("docs")))))
      val twice   = withJobs(
        List(TaskJob(JobName("docs"), TaskName("site"), needs = List(JobName("test"), JobName("test"))))
      )
      assertTrue(
        Workflow.render(present).isRight,
        Workflow.render(missing) == Left(CiError.UnknownNeed(JobName("docs"), JobName("absent"))),
        Workflow.render(pages) == Left(CiError.UnknownNeed(JobName("docs"), JobName("pages"))),
        Workflow.render(self) == Left(CiError.UnknownNeed(JobName("docs"), JobName("docs"))),
        Workflow.render(twice) == Left(CiError.DuplicateNeed(JobName("docs"), JobName("test"))),
      )
    ,
    test("an open job that delegates to SiteJob is still one site"):
      val shared = withJobs(
        List(
          Guide(TaskName("site")),
          SiteJob(JobName("docs"), TaskName("site"), Site(RepoPath("docs"))),
        )
      )
      assertTrue(
        Workflow.render(withJobs(List(Guide(TaskName("site"))))).isRight,
        Workflow.render(shared) == Left(CiError.DuplicateSite(JobName("guide"), JobName("docs"))),
      )
    ,
    test("a fan is one job and one matrix row per target"):
      val rendered = Workflow.render(withJobs(List(Matrix(TaskName("ship")))))
      assertTrue:
        rendered match
          case Right(yaml) =>
            yaml.contains("  deploy:") &&
            yaml.contains("name: \"us\"") &&
            yaml.contains("name: \"eu\"") &&
            yaml.contains("REGION: \"us-east-1\"") &&
            yaml.contains("REGION: \"eu-west-1\"") &&
            yaml.contains("contains(fromJSON(needs.affected.outputs.modules), 'core')") &&
            yaml.contains("contains(fromJSON(needs.affected.outputs.modules), 'all')")
          case Left(_) => false
    ,
    test("a third-party action needs a pin and its grant, and a graph secret stays off it"):
      val action = AwsOidc(EnvValue.Plain("role"), EnvValue.Plain("us-east-1"))
      val sha    = CommitSha("0123456789abcdef0123456789abcdef01234567")
      val pinned = CiSettings.empty.copy(
        actions = Map(ActionName("aws-actions/configure-aws-credentials") -> ActionPin(sha, "v6")),
        secrets = Set(SecretName("TOKEN")),
        env = List(EnvName("TOKEN") -> EnvValue.Secret(SecretName("TOKEN"))),
      )
      val bare    = withJobs(List(TaskJob(JobName("ship"), TaskName("echo"), pre = List(action))), pinned)
      val granted = TaskJob(
        JobName("ship"),
        TaskName("echo"),
        permissions = JobPermissions(Set(Grant.ContentsRead, Grant.IdTokenWrite)),
        pre = List(action),
      )
      val opened = withJobs(List(granted), pinned)
      val opted  = withJobs(
        List(granted.copy(graphSecrets = GraphSecrets.Include)),
        pinned,
      )
      val unpinned = withJobs(List(granted))
      assertTrue(
        Workflow.render(bare) == Left(CiError.GrantMissing(JobName("ship"), action.name)),
        Workflow.render(unpinned) == Left(CiError.UnpinnedAction(JobName("ship"), action.name)),
        Workflow.render(opened) match
          case Right(yaml) =>
            val job = slice(yaml, "  ship:", "  cache-seed:").getOrElse("")
            job.contains(s"uses: aws-actions/configure-aws-credentials@${sha.value} # v6") &&
            job.contains("id-token: write") &&
            !job.contains("secrets.TOKEN")
          case Left(_) => false
        ,
        Workflow.render(opted) match
          case Right(yaml) =>
            slice(yaml, "  ship:", "  cache-seed:").getOrElse("").contains("secrets.TOKEN")
          case Left(_) => false,
      )
    ,
    test("a runner is a catalog name"):
      val named   = TaskJob(JobName("ship"), TaskName("echo"), runner = Runner.Named(RunnerName("build")))
      val missing = withJobs(List(named))
      val known   = withJobs(
        List(named),
        CiSettings.empty.copy(runners = Map(RunnerName("build") -> "depot-ubuntu")),
      )
      assertTrue(
        Workflow.render(missing) == Left(CiError.UnknownRunner(JobName("ship"), RunnerName("build"))),
        Workflow.render(known) match
          case Right(yaml) => yaml.contains("    runs-on: depot-ubuntu")
          case Left(_)     => false,
      )
    ,
    test("a secret is declared before a job may read it"):
      val job = TaskJob(
        JobName("ship"),
        TaskName("echo"),
        env = List(EnvName("TOKEN") -> EnvValue.Secret(SecretName("TOKEN"))),
      )
      val declared = withJobs(
        List(job),
        CiSettings.empty.copy(secrets = Set(SecretName("TOKEN"))),
      )
      assertTrue(
        Workflow.render(withJobs(List(job))) == Left(CiError.UnknownSecret(SecretName("TOKEN"))),
        Workflow.render(declared) match
          case Right(yaml) => yaml.contains("TOKEN: ${{ secrets.TOKEN }}")
          case Left(_)     => false,
      )
    ,
    test("a label, a path, a cron, and workflow_run leave ci.yml"):
      val label   = TaskJob(JobName("triage"), TaskName("echo"), trigger = Trigger.Label("bug"))
      val paths   = TaskJob(JobName("api"), TaskName("echo"), trigger = Trigger.Paths(::("api/**", Nil)))
      val nightly = TaskJob.nightly(JobName("nightly"), TaskName("echo"), Cron("0 6 * * 1"))
      val after   = TaskJob(JobName("after"), TaskName("echo"), trigger = AfterCi)
      def names(jobs: List[CiJob]) =
        Workflow.renderAll(withJobs(jobs), automatic).map(_.map(_.name))
      val labeled = Workflow.renderAll(withJobs(List(label)), automatic)
      assertTrue(
        names(List(label)) == Right(List(Workflow.fileName, SupplyYaml.depsName, "histos-triage.yml")),
        names(List(paths)) == Right(List(Workflow.fileName, SupplyYaml.depsName, "histos-api.yml")),
        names(List(nightly)) == Right(List(Workflow.fileName, SupplyYaml.depsName, "histos-nightly.yml")),
        names(List(after)) == Right(List(Workflow.fileName, SupplyYaml.depsName, "histos-after.yml")),
        labeled match
          case Right(files) =>
            val ci  = files.find(_.name == Workflow.fileName).map(_.body).getOrElse("")
            val own = files.find(_.name == "histos-triage.yml").map(_.body).getOrElse("")
            !ci.contains("labeled") && own.contains("labeled") && own.contains("bug")
          case Left(_) => false
        ,
        Workflow.renderAll(withJobs(List(nightly)), automatic) match
          case Right(files) =>
            val own = files.find(_.name == "histos-nightly.yml").map(_.body).getOrElse("")
            own.contains("schedule:") && own.contains("workflow_dispatch:") && !own.contains("pull_request:")
          case Left(_) => false,
        Workflow.renderAll(withJobs(List(paths)), automatic) match
          case Right(files) =>
            val own = files.find(_.name == "histos-api.yml").map(_.body).getOrElse("")
            val ci  = files.find(_.name == Workflow.fileName).map(_.body).getOrElse("")
            own.contains("push:\n    branches:\n      - \"main\"") &&
            own.contains("paths:\n      - \"api/**\"") &&
            !own.contains("\"**\"") &&
            !own.contains("branches:\n      - \"${{") &&
            ci.contains("pull_request:") &&
            ci.contains("push:\n    branches:\n      - \"main\"") &&
            !ci.contains("branches:\n      - \"${{") &&
            !ci.contains("\"**\"")
          case Left(_) => false,
      )
    ,
    test("a tag stays in ci.yml and does not widen verify"):
      val tagged   = TaskJob(JobName("ship"), TaskName("echo"), trigger = Trigger.Tag("v*"))
      val rendered = Workflow.render(withJobs(List(tagged)))
      assertTrue:
        rendered match
          case Right(yaml) =>
            yaml.contains("tags:") &&
            yaml.contains("startsWith(github.ref, 'refs/tags/v')") &&
            slice(yaml, "  test:", "  ship:").getOrElse("").contains("github.event_name == 'pull_request'") &&
            !slice(yaml, "  test:", "  ship:").getOrElse("").contains("refs/tags")
          case Left(_) => false
    ,
    test("a condition the file cannot reach is refused"):
      val hidden  = TaskJob(JobName("triage"), TaskName("echo"), trigger = LabeledOnCi)
      val missing = TaskJob(JobName("after"), TaskName("echo"), trigger = AfterMissing)
      assertTrue(
        Workflow.render(withJobs(List(hidden))) == Left(CiError.UnreachableCond(JobName("triage"))),
        Workflow.render(withJobs(List(missing))) == Left(CiError.UnpinnedWorkflowRun("missing.yml")),
      )
    ,
    test("one file has one concurrency group and one declaration of a dispatch input"):
      val left = TaskJob(
        JobName("docs"),
        TaskName("echo"),
        concurrency = Some(Concurrency(GroupName("docs"), Cancel.InProgress)),
      )
      val right = TaskJob(
        JobName("ship"),
        TaskName("echo"),
        concurrency = Some(Concurrency(GroupName("ship"), Cancel.Finish)),
      )
      val ask   = TaskJob(JobName("docs"), TaskName("echo"), trigger = Ask("no"))
      val again = TaskJob(JobName("ship"), TaskName("echo"), trigger = Ask("yes"))
      assertTrue(
        Workflow.render(withJobs(List(left, right))) == Left(CiError.ConcurrencyConflict(Workflow.fileName)),
        Workflow.render(withJobs(List(ask, again))) == Left(CiError.DispatchInputConflict("ship")),
      )
    ,
    test("update writes the dispatch file and a later update deletes it"):
      ZIO.scoped:
        for
          root <- temp
          _    <- Workflow.update(root, withJobs(List(siteJob(SitePublish.Manual))), automatic)
          page  = Workflow.file(root, Workflow.pagesName)
          other = Workflow.file(root, "release.yml")
          _       <- ZIO.attempt(Files.writeString(other, "release\n"))
          removed <- ZIO.attempt(Files.deleteIfExists(page))
          missing <- Workflow.check(root, withJobs(List(siteJob(SitePublish.Manual))), automatic).either
          _       <- Workflow.update(root, withJobs(List(siteJob(SitePublish.Both))), automatic)
          _       <- Workflow.update(root, graph(Splits.aggregate), automatic)
          gone = !Files.exists(page)
          kept <- ZIO.attempt(Files.readString(other))
        yield assertTrue(
          removed,
          missing == Left(CiError.Drift),
          gone,
          kept == "release\n",
        )
    ,
    test("the test job passes the event namespace and caches coursier, and docs and deploy do not"):
      val rendered = Workflow.render(
        withJobs(
          List(
            TaskJob(JobName("docs"), TaskName("site")),
            TaskJob(JobName("deploy"), TaskName("ship"), permissions = JobPermissions.write),
          )
        )
      )
      val pull = CacheNamespace.from("pr/8")
      val main = CacheNamespace.from("main")
      assertTrue:
        rendered match
          case Right(yaml) =>
            (
              slice(yaml, "  test:", "  docs:"),
              slice(yaml, "  docs:", "  deploy:"),
              slice(yaml, "  deploy:", "  cache-seed:"),
              slice(yaml, "  supply-chain:", ""),
            ) match
              case (Some(test), Some(docs), Some(deploy), Some(workflow)) =>
                test.contains(CacheNamespace.Key) &&
                test.contains(
                  "case(github.event_name == 'pull_request', format('pr/{0}', github.event.pull_request.number), 'main')"
                ) &&
                test.contains("./histos test") &&
                test.contains("actions/cache@") &&
                test.contains("actions/cache/restore@") &&
                test.contains("actions/cache/save@") &&
                test.contains("~/.cache/histos") &&
                test.contains("format('pr-{0}', github.event.pull_request.number)") &&
                test.contains("steps.store.outputs.cache-hit") &&
                test.contains("actions: write") &&
                test.contains("gh api --method DELETE") &&
                test.contains("if: \"always()\"") &&
                !test.contains("if: \"always() &&") &&
                test.contains("hashFiles('histos.scala')") &&
                !test.contains("restore-keys") &&
                !test.contains("cache: npm") &&
                !test.contains("setup-java") &&
                occurrences(yaml, CacheNamespace.Key) == 1 &&
                occurrences(test, "actions/cache") == 4 &&
                !docs.contains(CacheNamespace.Key) &&
                !deploy.contains(CacheNamespace.Key) &&
                !workflow.contains(CacheNamespace.Key) &&
                !docs.contains("actions/cache") &&
                !deploy.contains("actions/cache") &&
                workflow.contains("deps audit") &&
                !docs.contains("needs:") &&
                !deploy.contains("needs:") &&
                test.contains("needs: affected") &&
                !test.contains("restore-keys") &&
                pull.map(ns => (ns.token, ns.reads.map(_.token))) == Right(("pr/8", List("pr/8", "main"))) &&
                main.map(ns => (ns.token, ns.reads.map(_.token))) == Right(("main", List("main")))
              case _ => false
          case Left(_) => false
    ,
    test(
      "an always job is rendered after test and before workflow, and the plugin supplies no yaml"
    ):
      for
        golden <- text("plugin-always.yml")
        rendered = Workflow.render(
          withJobs(
            List(TaskJob(JobName("docs"), TaskName("site")))
          )
        )
      yield assertTrue(rendered == Right(golden))
    ,
    test("render stays the ci.yml body and a schedule is a separate file"):
      val nightly = TaskJob(JobName("nightly"), TaskName("site"), cron = Some(Cron("0 6 * * 1")))
      val built   = withJobs(List(nightly))
      val all     = Workflow.renderAll(built, automatic)
      val one     = Workflow.render(built)
      val plain   = Workflow.render(graph(Splits.aggregate))
      (all, one, plain) match
        case (
              Right(
                RenderedWorkflow("ci.yml", body) :: RenderedWorkflow("histos-deps.yml", _) :: RenderedWorkflow(
                  name,
                  scheduled,
                ) :: Nil
              ),
              Right(text),
              Right(untouched),
            ) =>
          assertTrue(
            body == text,
            body == untouched,
            name == "histos-nightly.yml",
            scheduled ==
              """name: histos-nightly
                  |on:
                  |  schedule:
                  |    - cron: "0 6 * * 1"
                  |permissions:
                  |  contents: read
                  |jobs:
                  |  nightly:
                  |    runs-on: ubuntu-26.04
                  |    permissions:
                  |      contents: read
                  |    steps:
                  |      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
                  |      - uses: ./.github/actions/setup
                  |      - run: ./histos doctor --preflight nightly
                  |      - run: "./histos run dev.example/site"
                  |""".stripMargin,
            !body.contains("schedule:"),
            !body.contains("cron:"),
          )
        case _ => assertTrue(false)
    ,
    test(
      "an always job sits after versions and contents write stays on the job"
    ):
      val job  = TaskJob(JobName("docs"), TaskName("site"), permissions = JobPermissions.write)
      val yaml = (ShipName.from("libs"), ShipNumber.from("1.4.2")) match
        case (Right(name), Right(number)) =>
          Catalog.from(List(Ship.Line(name, number, ::(core, Nil)))) match
            case Right(catalog) =>
              val built = ProjectGraph.trusted(
                List(module(ProjectName("core"), Platform.Jvm, Nil)),
                Splits.aggregate,
                catalog,
                None,
                Nil,
                List(registered(List(job))),
              )
              Workflow.render(built).toOption
            case Left(_) => None
        case _ => None
      val versionsAt = yaml.fold(-1)(_.indexOf("  versions:"))
      val docsAt     = yaml.fold(-1)(_.indexOf("  docs:"))
      val workflowAt = yaml.fold(-1)(_.indexOf("  supply-chain:"))
      assertTrue(
        yaml.isDefined,
        versionsAt >= 0,
        docsAt > versionsAt,
        workflowAt > docsAt,
        yaml.exists(_.contains("    permissions:\n      contents: write\n")),
        yaml.exists(_.contains("permissions:\n  contents: read\n")),
        yaml.exists(_.contains("\"./histos run dev.example/site\"")),
        yaml.forall(!_.contains("id-token")),
      )
    ,
    test(
      "a scheduled contents write overrides the job and leaves the workflow header read"
    ):
      val job = TaskJob(
        JobName("nightly"),
        TaskName("site"),
        cron = Some(Cron("0 6 * * 1")),
        permissions = JobPermissions.write,
      )
      val rendered = Workflow.renderAll(withJobs(List(job)), automatic)
      rendered match
        case Right(
              _ :: RenderedWorkflow("histos-deps.yml", _) :: RenderedWorkflow("histos-nightly.yml", body) :: Nil
            ) =>
          assertTrue(
            body.contains("permissions:\n  contents: read\n"),
            body.contains("    permissions:\n      contents: write\n"),
            !body.contains("id-token"),
          )
        case _ => assertTrue(false)
    ,
    test("a missing scheduled file is drift and check does not rewrite ci.yml"):
      ZIO.scoped:
        for
          root <- temp
          _    <- Workflow.update(root, graph(Splits.aggregate), automatic)
          nightly = TaskJob(JobName("nightly"), TaskName("site"), cron = Some(Cron("0 6 * * 1")))
          checked <- Workflow.check(root, withJobs(List(nightly)), automatic).either
          ci      <- ZIO.attempt(Files.readString(Workflow.path(root)))
          plain   <- ZIO.fromEither(Workflow.render(graph(Splits.aggregate)))
        yield assertTrue(checked == Left(CiError.Drift), ci == plain)
    ,
    test(
      "update deletes an orphan histos workflow and leaves an unrelated file"
    ):
      ZIO.scoped:
        for
          root <- temp
          nightly = TaskJob(JobName("nightly"), TaskName("site"), cron = Some(Cron("0 6 * * 1")))
          _ <- Workflow.update(root, withJobs(List(nightly)), automatic)
          orphan = Workflow.file(root, "histos-retired.yml")
          other  = Workflow.file(root, "release.yml")
          _       <- ZIO.attempt(Files.writeString(orphan, "orphan\n"))
          _       <- ZIO.attempt(Files.writeString(other, "release\n"))
          drifted <- Workflow.check(root, withJobs(List(nightly)), automatic).either
          _       <- Workflow.update(root, graph(Splits.aggregate), automatic)
          gone = !Files.exists(
            Workflow.file(root, "histos-nightly.yml")
          ) && !Files.exists(orphan)
          kept  <- ZIO.attempt(Files.readString(other))
          clean <- Workflow.check(root, graph(Splits.aggregate), automatic)
        yield assertTrue(
          drifted == Left(CiError.Drift),
          gone && kept == "release\n",
          clean == Workflow.path(root),
        ),
  )

  private def slice(yaml: String, start: String, end: String): Option[String] =
    val from = yaml.indexOf(start)
    if from < 0 then None
    else if end.isEmpty then Some(yaml.substring(from))
    else
      val to = yaml.indexOf(end, from + start.length)
      if to < 0 then None else Some(yaml.substring(from, to))

  private def occurrences(text: String, needle: String): Int =
    def go(from: Int, seen: Int): Int =
      val at = text.indexOf(needle, from)
      if at < 0 then seen else go(at + needle.length, seen + 1)
    go(0, 0)

  private final case class Guide(taskName: TaskName) extends CiJob:
    def name: JobName                             = JobName("guide")
    def task: TaskName                            = taskName
    def permissions: JobPermissions               = JobPermissions.read
    def needs: List[JobName]                      = Nil
    def render(plugin: PluginId): List[JobRender] =
      SiteJob(name, taskName, Site(RepoPath("guide"))).render(plugin)

  private final case class Matrix(taskName: TaskName) extends CiJob:
    def name: JobName                             = JobName("deploy")
    def task: TaskName                            = taskName
    def permissions: JobPermissions               = JobPermissions.read
    def needs: List[JobName]                      = Nil
    def render(plugin: PluginId): List[JobRender] =
      TaskJob(
        name,
        taskName,
        guard = Some(Guard.Affected(::(ProjectName("core"), Nil), Guard.OnCi)),
      ).render(plugin) match
        case JobRender.Task(arranged) :: Nil =>
          val us = Target(TargetName("us"), env = List(EnvName("REGION") -> EnvValue.Plain("us-east-1")))
          val eu = Target(TargetName("eu"), env = List(EnvName("REGION") -> EnvValue.Plain("eu-west-1")))
          List(JobRender.Fan(arranged, ::(us, ::(eu, Nil))))
        case _ => Nil

  private object AfterCi extends Trigger:
    def on: List[On] = List(On.WorkflowRun(List("ci.yml")))
    def cond: Cond   = Cond.Event(EventKind.WorkflowRun)

  private object AfterMissing extends Trigger:
    def on: List[On] = List(On.WorkflowRun(List("missing.yml")))
    def cond: Cond   = Cond.Event(EventKind.WorkflowRun)

  private object LabeledOnCi extends Trigger:
    def on: List[On] = List(On.pullRequest)
    def cond: Cond   = Cond.HasLabel("bug")

  private final case class Ask(value: String) extends Trigger:
    def on: List[On] = List(On.Dispatch(List(DispatchInput(InputKey("ship"), InputKind.Text, Some(value)))))
    def cond: Cond   = Cond.True

  private final case class Twice(name: JobName, task: TaskName, site: Site) extends CiJob:
    def permissions: JobPermissions               = JobPermissions.read
    def needs: List[JobName]                      = Nil
    def render(plugin: PluginId): List[JobRender] =
      val arranged = SiteArrange(
        name,
        task,
        permissions,
        Nil,
        site,
        Trigger.DefaultPush.cond,
        Trigger.DefaultPush.on,
      )
      List(JobRender.Site(arranged, SiteShape.DeployOnPush), JobRender.Site(arranged, SiteShape.DeployOnPush))

  private def siteJob(publish: SitePublish): CiJob =
    SiteJob(JobName("docs"), TaskName("site"), Site(RepoPath("target/site"), publish))

  private def pagesBody(files: List[RenderedWorkflow]): List[String] =
    files.collect:
      case RenderedWorkflow(Workflow.pagesName, body) => body

  private def withJobs(jobs: List[CiJob], settings: CiSettings = CiSettings.empty): ProjectGraph =
    ProjectGraph.trusted(
      List(
        module(ProjectName("core"), Platform.Jvm, Nil),
        module(ProjectName("tests"), Platform.Jvm, List(core)),
      ),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      List(registered(jobs)),
      ci = settings,
    )

  private def registered(jobs: List[CiJob]): RegisteredPlugin =
    RegisteredPlugin(
      PluginId("dev.example"),
      EntryName.trusted("dev.example.Echo$"),
      Contribution(Nil, Nil, jobs),
    )

  private val release = suite("release")(
    test("one ship publishes on dispatch and does not upload to GitHub"):
      val body = renderRelease(
        List(line("histos", "0.0.1", "app")),
        Set("app"),
        automatic,
      ).flatMap:
        case RenderedWorkflow("ci.yml", ci) :: RenderedWorkflow(name, text) :: RenderedWorkflow(
              snapshots,
              snap,
            ) :: RenderedWorkflow(
              "histos-deps.yml",
              _,
            ) :: Nil
            if name == Workflow.releaseName && snapshots == Workflow.snapshotsName && snap == snapshotWorkflow &&
              !ci.contains("publish") && !ci.contains("tags:") =>
          Right(text)
        case other => Left(other.toString)
      assertTrue(body == Right(oneShipRelease))
    ,
    test("hold renders a promote file and does not pass --automatic"):
      val rendered = renderRelease(
        List(line("histos", "0.0.1", "app")),
        Set("app"),
        hold,
      )
      assertTrue:
        rendered match
          case Right(
                RenderedWorkflow("ci.yml", _) :: RenderedWorkflow(releaseFile, releaseBody) :: RenderedWorkflow(
                  promoteFile,
                  promoteBody,
                ) :: RenderedWorkflow(snapshots, snap) :: RenderedWorkflow("histos-deps.yml", _) :: Nil
              ) =>
            releaseFile == Workflow.releaseName &&
            releaseBody == oneShipRelease &&
            !releaseBody.contains("--automatic") &&
            promoteFile == "histos-promote.yml" &&
            promoteBody == holdPromote &&
            snapshots == Workflow.snapshotsName &&
            snap == snapshotWorkflow &&
            !promoteBody.contains("publish release") &&
            !promoteBody.contains("--automatic")
          case _ => false
    ,
    test("a release links the native client after sona and publishes the install files"):
      val tools    = RunnerTools(None, Some(ToolRevision("21.1.8")), None, None)
      val rendered = renderRelease(
        List(line("histos", "1.2.3", "app")),
        Set("app"),
        automatic,
        tools = tools,
        client = true,
      )
      assertTrue:
        rendered match
          case Right(
                _ :: RenderedWorkflow("histos-release.yml", body) :: RenderedWorkflow(
                  "histos-snapshots.yml",
                  _,
                ) :: RenderedWorkflow("histos-deps.yml", _) :: Nil
              ) =>
            val releaseAt = body.indexOf("  release-histos:")
            val clientAt  = body.indexOf("\n  client:\n")
            val installAt = body.indexOf("\n  install:\n")
            val release   = if releaseAt >= 0 && clientAt > releaseAt then body.substring(releaseAt, clientAt) else ""
            releaseAt >= 0 && clientAt > releaseAt && installAt > clientAt &&
            release.contains("\"./histos publish release histos\"") &&
            release.contains("\"./histos assembly\"") &&
            !release.contains("contents: write") &&
            body.contains("\"./histos client\"") &&
            body.contains("needs: [release-histos]") &&
            body.contains("needs: client") &&
            body.contains("macos-26") &&
            body.contains("linux-x86_64") &&
            body.contains("macos-aarch64") &&
            body.contains("LLVM-21.1.8-Linux-X64.tar.xz") &&
            body.contains("LLVM-21.1.8-macOS-ARM64.tar.xz") &&
            body.contains("actions/upload-artifact@") &&
            body.contains("actions/download-artifact@") &&
            body.contains("gh release upload dist") &&
            body.contains("shim.sha256") &&
            body.contains("install/histos") &&
            body.contains("--repo \"$GITHUB_REPOSITORY\"") &&
            body.contains("GH_TOKEN: ${{ github.token }}") &&
            body.contains("version=1.2.3") &&
            body.contains("java-version: \"25\"") &&
            body.contains("actions/setup-java@") &&
            body.contains("distribution: temurin") &&
            body.contains("xcrun --show-sdk-path") &&
            body.contains("-isysroot") &&
            !release.contains("java:") &&
            !release.contains("xcrun") &&
            body.contains("contents: write")
          case _ => false
    ,
    test("two ships release by dispatch, and neither uploads to GitHub"):
      val rendered = renderRelease(
        List(line("libs", "1.0.0", "core"), line("tool", "2.0.0", "app")),
        Set("app"),
        automatic,
      )
      assertTrue:
        rendered match
          case Right(
                _ :: RenderedWorkflow("histos-release.yml", body) :: RenderedWorkflow(
                  "histos-snapshots.yml",
                  snap,
                ) :: RenderedWorkflow("histos-deps.yml", _) :: Nil
              ) =>
            val libsAt  = body.indexOf("  release-libs:")
            val toolAt  = body.indexOf("  release-tool:")
            val libsJob = if libsAt >= 0 && toolAt > libsAt then body.substring(libsAt, toolAt) else ""
            val toolJob = if toolAt >= 0 then body.substring(toolAt) else ""
            !body.contains("tags:") &&
            !body.contains("inputs:") &&
            !body.contains("if:") &&
            !body.contains("publish promote") &&
            !body.contains("--automatic") &&
            !body.contains(CacheNamespace.Key) &&
            !body.contains("actions/cache") &&
            !body.contains("needs:") &&
            libsJob.contains("\"./histos publish release libs\"") &&
            toolJob.contains("\"./histos publish release tool\"") &&
            !body.contains("publish assets") &&
            !body.contains("assembly") &&
            !body.contains("contents: write") &&
            !body.contains("GITHUB_TOKEN") &&
            snap == snapshotWorkflow
          case _ => false
    ,
    test("a schedule is rendered after the release file"):
      val nightly  = TaskJob(JobName("nightly"), TaskName("site"), cron = Some(Cron("0 6 * * 1")))
      val rendered = renderRelease(
        List(line("histos", "0.0.1", "app")),
        Set("app"),
        automatic,
        List(registered(List(nightly))),
      )
      assertTrue:
        rendered match
          case Right(
                RenderedWorkflow("ci.yml", _) :: RenderedWorkflow("histos-release.yml", _) :: RenderedWorkflow(
                  "histos-snapshots.yml",
                  _,
                ) :: RenderedWorkflow(
                  "histos-deps.yml",
                  _,
                ) :: RenderedWorkflow(
                  "histos-nightly.yml",
                  _,
                ) :: Nil
              ) =>
            true
          case _ => false
    ,
    test("a ship with no asset does not render the snapshots workflow"):
      val rendered = renderRelease(List(line("libs", "1.0.0", "core")), Set.empty, automatic)
      assertTrue:
        rendered match
          case Right(files) => !files.exists(_.name == Workflow.snapshotsName)
          case _            => false
    ,
    test("this repository's scm renders the snapshots workflow with the bootstrap assembly"):
      val rendered = renderRelease(
        List(line("histos", "0.2.0", "app")),
        Set("app"),
        automatic,
        facts = OrganizationFacts(scm =
          Some(
            Scm(
              HttpUrl.trusted("https://github.com/early-effect/histos"),
              ScmConnection.trusted("scm:git@github.com:early-effect/histos.git"),
            )
          )
        ),
      )
      assertTrue:
        rendered match
          case Right(_ :: _ :: RenderedWorkflow(name, body) :: _) =>
            name == Workflow.snapshotsName && body == ownedSnapshots
          case _ => false
    ,
    test("update writes the release file and an empty catalog deletes it"):
      ZIO.scoped:
        for
          root    <- temp
          built   <- ZIO.fromEither(releaseGraph(List(line("histos", "0.0.1", "app")), Set("app")))
          _       <- Workflow.update(root, built, automatic)
          text    <- ZIO.attempt(Files.readString(Workflow.file(root, Workflow.releaseName)))
          again   <- Workflow.check(root, built, automatic)
          _       <- ZIO.attempt(Files.delete(Workflow.file(root, Workflow.releaseName)))
          missing <- Workflow.check(root, built, automatic).either
          stale   = Workflow.file(root, Workflow.releaseName)
          promote = Workflow.file(root, "histos-promote.yml")
          _ <- ZIO.attempt(Files.writeString(stale, "stale\n"))
          _ <- ZIO.attempt(Files.writeString(promote, "stale promote\n"))
          _ <- Workflow.update(root, graph(Splits.aggregate), automatic)
          gone = !Files.exists(stale) && !Files.exists(promote)
          clean       <- Workflow.check(root, graph(Splits.aggregate), automatic)
          _           <- Workflow.update(root, built, hold)
          heldRelease <- ZIO.attempt(Files.readString(Workflow.file(root, Workflow.releaseName)))
          heldPromote <- ZIO.attempt(Files.readString(promote))
          heldCheck   <- Workflow.check(root, built, hold)
          early       <- Workflow.check(root, built, automatic).either
          _           <- Workflow.update(root, built, automatic)
          dropped = !Files.exists(promote)
          kept  <- ZIO.attempt(Files.readString(Workflow.file(root, Workflow.releaseName)))
          after <- Workflow.check(root, built, automatic)
        yield assertTrue(
          text == oneShipRelease,
          again == Workflow.path(root),
          missing == Left(CiError.Drift),
          gone,
          clean == Workflow.path(root),
          heldRelease == oneShipRelease,
          heldPromote == holdPromote,
          heldCheck == Workflow.path(root),
          early == Left(CiError.Drift),
          dropped,
          kept == oneShipRelease,
          after == Workflow.path(root),
        ),
  )

  private val snapshotWorkflow: String =
    """name: snapshots
      |on:
      |  workflow_dispatch:
      |permissions:
      |  contents: read
      |jobs:
      |  snapshots:
      |    runs-on: ubuntu-26.04
      |    permissions:
      |      contents: write
      |    steps:
      |      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
      |      - uses: ./.github/actions/setup
      |      - run: ./histos doctor --preflight snapshots
      |      - if: "github.ref == format('refs/heads/{0}', github.event.repository.default_branch)"
      |        run: "./histos assembly"
      |      - run: "./histos publish snapshots"
      |        env:
      |          HISTOS_SESSION: snapshot
      |          HISTOS_SHA: ${{ github.sha }}
      |          HISTOS_BRANCH: ${{ github.ref == format('refs/heads/{0}', github.event.repository.default_branch) && 'default' || 'other' }}
      |          SONATYPE_USERNAME: ${{ secrets.SONATYPE_USERNAME }}
      |          SONATYPE_PASSWORD: ${{ secrets.SONATYPE_PASSWORD }}
      |          GITHUB_TOKEN: ${{ github.token }}
      |          GITHUB_REPOSITORY: ${{ github.repository }}
      |""".stripMargin

  private val ownedSnapshots: String =
    """name: snapshots
      |on:
      |  workflow_dispatch:
      |permissions:
      |  contents: read
      |jobs:
      |  snapshots:
      |    runs-on: ubuntu-26.04
      |    permissions:
      |      contents: write
      |    steps:
      |      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
      |      - uses: ./.github/actions/setup
      |      - run: |
      |          usage=$(./histos 2>&1 || true)
      |          case "$usage" in
      |            *" doctor "*) ;;
      |            *) ./histos assembly ;;
      |          esac
      |      - run: ./histos doctor --preflight snapshots
      |      - if: "github.ref == format('refs/heads/{0}', github.event.repository.default_branch)"
      |        run: "./histos assembly"
      |      - run: "./histos publish snapshots"
      |        env:
      |          HISTOS_SESSION: snapshot
      |          HISTOS_SHA: ${{ github.sha }}
      |          HISTOS_BRANCH: ${{ github.ref == format('refs/heads/{0}', github.event.repository.default_branch) && 'default' || 'other' }}
      |          SONATYPE_USERNAME: ${{ secrets.SONATYPE_USERNAME }}
      |          SONATYPE_PASSWORD: ${{ secrets.SONATYPE_PASSWORD }}
      |          GITHUB_TOKEN: ${{ github.token }}
      |          GITHUB_REPOSITORY: ${{ github.repository }}
      |""".stripMargin

  private val oneShipRelease: String =
    """name: release
      |on:
      |  workflow_dispatch:
      |permissions:
      |  contents: read
      |jobs:
      |  release-histos:
      |    runs-on: ubuntu-26.04
      |    permissions:
      |      contents: read
      |    steps:
      |      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
      |      - uses: ./.github/actions/setup
      |      - run: ./histos doctor --preflight release-histos
      |      - run: "./histos publish release histos"
      |        env:
      |          SONATYPE_USERNAME: ${{ secrets.SONATYPE_USERNAME }}
      |          SONATYPE_PASSWORD: ${{ secrets.SONATYPE_PASSWORD }}
      |          PGP_SECRET: ${{ secrets.PGP_SECRET }}
      |          PGP_PASSPHRASE: ${{ secrets.PGP_PASSPHRASE }}
      |          PGP_KEY_HEX: ${{ secrets.PGP_KEY_HEX }}
      |""".stripMargin

  private val holdPromote: String =
    """name: promote
      |on:
      |  workflow_dispatch:
      |    inputs:
      |      deployment:
      |        description: deployment id
      |        required: true
      |        type: string
      |permissions:
      |  contents: read
      |jobs:
      |  promote:
      |    runs-on: ubuntu-26.04
      |    steps:
      |      - uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1 # v7.0.1
      |      - uses: ./.github/actions/setup
      |      - run: ./histos doctor --preflight promote
      |      - run: "./histos publish promote ${{ inputs.deployment }}"
      |        env:
      |          SONATYPE_USERNAME: ${{ secrets.SONATYPE_USERNAME }}
      |          SONATYPE_PASSWORD: ${{ secrets.SONATYPE_PASSWORD }}
      |""".stripMargin

  private def renderRelease(
      rows: List[Either[String, Ship]],
      assets: Set[String],
      sona: Publishing,
      plugins: List[RegisteredPlugin] = Nil,
      tools: RunnerTools = RunnerTools.empty,
      client: Boolean = false,
      facts: OrganizationFacts = OrganizationFacts.empty,
  ): Either[String, List[RenderedWorkflow]] =
    releaseGraph(rows, assets, plugins, tools, client, facts).flatMap(graph =>
      Workflow.renderAll(graph, sona).left.map(_.message)
    )

  private def releaseGraph(
      rows: List[Either[String, Ship]],
      assets: Set[String],
      plugins: List[RegisteredPlugin] = Nil,
      tools: RunnerTools = RunnerTools.empty,
      client: Boolean = false,
      facts: OrganizationFacts = OrganizationFacts.empty,
  ): Either[String, ProjectGraph] =
    rows
      .foldLeft[Either[String, List[Ship]]](Right(Nil)): (acc, row) =>
        acc.flatMap(seen => row.map(ship => seen :+ ship))
      .flatMap: ships =>
        Catalog
          .from(ships)
          .left
          .map(_.message)
          .map: catalog =>
            val modules = ships
              .flatMap(_.members.toList)
              .distinct
              .map: name =>
                val base = module(name, Platform.Jvm, Nil)
                if assets.contains(name.value) then base.copy(asset = Some(RepoPath.trusted("dist/histos.jar")))
                else base
            val withClient =
              if client then
                modules :+ module(ProjectName("client"), Platform.Native, Nil)
                  .copy(application = Some(MainClass("histos.client.Main")))
              else modules
            ProjectGraph.trusted(
              withClient,
              Splits.aggregate,
              catalog,
              None,
              Nil,
              plugins,
              runnerTools = tools,
              facts = facts,
            )

  private def line(name: String, number: String, member: String): Either[String, Ship] =
    for
      ship    <- ShipName.from(name).left.map(_.message)
      parsed  <- ShipNumber.from(number).left.map(_.message)
      project <- ProjectName.from(member).left.map(_.message)
    yield Ship.Line(ship, parsed, ::(project, Nil))

  def spec =
    suite("workflow")(render, affected, plugins, release) @@ TestAspect.withLiveSystem
end WorkflowSpec
