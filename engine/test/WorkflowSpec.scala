package histos.workflow

import histos.plugin.{
  CiJob,
  Contribution,
  Cron,
  EntryName,
  JobName,
  JobPermissions,
  JobWhen,
  PluginId,
  RegisteredPlugin,
  RepoPath,
  TaskName,
}
import histos.version.{Catalog, ManualVersion, Ship, ShipName, ShipNumber}
import histos.{
  Axis,
  Module,
  NodeFloor,
  Platform,
  Platforms,
  ProjectDep,
  ProjectGraph,
  ProjectName,
  RunnerTools,
  ScalaVersion,
  ScalaVersions,
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
  private val v390           = ScalaVersion.trusted("3.9.0")
  private val core           = project("core")
  private val tests          = project("tests")
  private val nodeOnly       = RunnerTools(Some(NodeFloor(22)), None, None)
  private val llvmOnly       = RunnerTools(None, Some(ToolRevision("20.1.8")), None)
  private val playwrightOnly = RunnerTools(None, None, Some(ToolRevision("1.49.0")))

  private val render = suite("render")(
    test("the empty split set is one aggregate test job"):
      for
        golden  <- text("aggregate.yml")
        drifted <- text("drifted.yml")
        rendered = Workflow.render(graph(Splits.aggregate))
      yield assertTrue(
        rendered == Right(golden),
        golden.contains("./histos test") && golden.contains(
          "contents: read"
        ) && golden.contains("workflow --check"),
        !golden.contains("setup-java") && !golden.contains("setup-node") && !golden
          .contains("install-llvm-action") && !golden.contains("playwright") && !golden
          .contains("format") && !golden.contains("publish") &&
          golden.contains("push:\n    branches:\n      - main") && !golden
            .contains("tags:"),
        Workflow.differs(golden, drifted) == Left(CiError.Drift),
        Workflow.argv(TestJob.Aggregate(TestTarget.Entire)) == List(
          "./histos",
          "test",
        ),
      )
    ,
    test("a module split is a matrix, one cell per module"):
      val rendered = Workflow.render(graph(Splits.of(Axis.Module)))
      assertTrue:
        rendered match
          case Right(yaml) =>
            yaml.contains("fail-fast: false") &&
            yaml.contains("\"./histos test core\"") &&
            yaml.contains("\"./histos test tests\"") &&
            !yaml.contains("setup-java") && !yaml.contains("setup-node") &&
            !yaml.contains("install-llvm-action") && !yaml.contains("playwright")
          case Left(_) => false
    ,
    test("a catalog adds the versions command and still does not publish"):
      val rendered = (ShipName.from("libs"), ShipNumber.from("1.4.2")) match
        case (Right(name), Right(number)) =>
          Catalog.from(List(Ship.Line(name, number, ::(core, Nil)))) match
            case Right(catalog) =>
              Workflow.render(
                ProjectGraph.trusted(
                  List(module("core", Platform.Jvm, Nil)),
                  Splits.aggregate,
                  catalog,
                )
              ) match
                case Right(yaml) =>
                  yaml.contains("./histos run versions") && !yaml.contains(
                    "publish"
                  ) && !yaml.contains("format")
                case Left(_) => false
            case Left(_) => false
        case _ => false
      assertTrue(rendered)
    ,
    test("update writes the render and check rejects a drifted file"):
      ZIO.scoped:
        for
          root  <- temp
          file  <- Workflow.update(root, graph(Splits.aggregate))
          read  <- ZIO.attempt(Files.readString(file))
          again <- Workflow.check(root, graph(Splits.aggregate))
          wrote = Files.isRegularFile(file)
          _       <- ZIO.attempt(Files.writeString(file, read + "\n"))
          drifted <- Workflow.check(root, graph(Splits.aggregate)).either
          gone    <- Workflow
            .check(root.resolve("absent"), graph(Splits.aggregate))
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
        Workflow.render(graph(Splits.aggregate, nodeOnly)) == Right(node),
        Workflow.render(graph(Splits.aggregate, llvmOnly)) == Right(llvm),
        Workflow.render(graph(Splits.aggregate, playwrightOnly)) == Right(playwright),
      )
    ,
    test("this repository asks the runner for node, llvm, and playwright"):
      for golden <- text("histos.yml")
      yield assertTrue:
        repository match
          case Right(built) => Workflow.render(built) == Right(golden)
          case Left(_)      => false,
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
          CiError.Unknown(project("missing"))
        ),
      )
    ,
    test("an unknown project is not dropped"):
      val missing = project("missing")
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
      val js    = module("jsapp", Platform.Js, Nil)
      val jvm   = module("core", Platform.Jvm, Nil)
      val built = ProjectGraph.trusted(List(jvm, js), Splits.of(Axis.Platform))
      val plan  =
        Workflow.plan(built, CiEvent.PullRequest(Set(project("jsapp"))))
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

  private def graph(splits: Splits, tools: RunnerTools = RunnerTools.empty): ProjectGraph =
    val library = module("core", Platform.Jvm, Nil)
    val suite   = module("tests", Platform.Jvm, List(core))
    ProjectGraph.trusted(
      List(library, suite),
      splits,
      Catalog.empty,
      None,
      Nil,
      runnerTools = tools,
    )

  /** The tools and the manual ship this repository names. The module list does not appear in an aggregate render. */
  private def repository: Either[histos.BuildError, ProjectGraph] =
    val api = histos.module("api", Platform.Jvm)(ScalaVersion("3.10.0"))
    histos.Projects
      .organization(histos.Organization("rocks.earlyeffect"))
      .runnerTools(
        RunnerTools(
          Some(NodeFloor(25)),
          Some(ToolRevision("21.1.8")),
          Some(ToolRevision("1.62.1")),
        )
      )(
        histos.ships(histos.manual("histos", ManualVersion("0.0.1-SNAPSHOT"))(api)),
        api,
      )

  private def module(
      name: String,
      platform: Platform,
      depends: List[ProjectName],
  ): Module =
    Module(
      project(name),
      Platforms(platform, Nil),
      ScalaVersions(v390, Nil),
      Nil,
      depends.map(name => ProjectDep(name, None)),
      SourcePos("histos.scala", 1, 1),
    )

  private def project(raw: String): ProjectName =
    ProjectName.from(raw) match
      case Right(name) => name
      case Left(_)     => project("core")

  private def text(name: String): ZIO[Any, Throwable, String] =
    val relative = List(
      Path.of("engine/test/fixtures/ci").resolve(name),
      Path.of("test/fixtures/ci").resolve(name),
    )
    System
      .env("HISTOS_REPO")
      .flatMap: root =>
        val candidates = root
          .filter(_.nonEmpty)
          .toList
          .map(
            Path.of(_).resolve("engine/test/fixtures/ci").resolve(name)
          ) ++ relative
        ZIO
          .attempt(candidates.find(Files.isRegularFile(_)))
          .flatMap:
            case Some(file) => ZIO.attempt(Files.readString(file))
            case None       => ZIO.fail(new java.io.FileNotFoundException(name))

  private def temp: ZIO[Scope, Throwable, Path] =
    Workspace
      .work("histos-workflow")
      .mapError(error => new java.io.IOException(error.message))

  private val plugins = suite("plugins")(
    test(
      "an always job is rendered after test and before workflow, and the plugin supplies no yaml"
    ):
      for
        golden <- text("plugin-always.yml")
        rendered = Workflow.render(
          withJobs(
            List(CiJob(JobName("docs"), TaskName("site"), JobWhen.Always))
          )
        )
      yield assertTrue(rendered == Right(golden))
    ,
    test("render stays the ci.yml body and a schedule is a separate file"):
      val nightly = CiJob(
        JobName("nightly"),
        TaskName("site"),
        JobWhen.Schedule(Cron("0 6 * * 1")),
      )
      val built = withJobs(List(nightly))
      val all   = Workflow.renderAll(built)
      val one   = Workflow.render(built)
      val plain = Workflow.render(graph(Splits.aggregate))
      assertTrue:
        (all, one, plain) match
          case (
                Right(
                  RenderedWorkflow("ci.yml", body) :: RenderedWorkflow(
                    name,
                    scheduled,
                  ) :: Nil
                ),
                Right(text),
                Right(untouched),
              ) =>
            body == text && body == untouched && name == "histos-nightly.yml" &&
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
                  |      - uses: actions/checkout@v7
                  |      - run: "./histos run dev.example/site"
                  |""".stripMargin &&
              !body.contains("schedule:") && !body.contains("cron:")
          case _ => false
    ,
    test(
      "an always job sits after versions and contents write stays on the job"
    ):
      val job = CiJob(
        JobName("docs"),
        TaskName("site"),
        JobWhen.Always,
        JobPermissions.ContentsWrite,
      )
      val rendered = (ShipName.from("libs"), ShipNumber.from("1.4.2")) match
        case (Right(name), Right(number)) =>
          Catalog.from(List(Ship.Line(name, number, ::(core, Nil)))) match
            case Right(catalog) =>
              val built = ProjectGraph.trusted(
                List(module("core", Platform.Jvm, Nil)),
                Splits.aggregate,
                catalog,
                None,
                Nil,
                List(registered(List(job))),
              )
              Workflow.render(built) match
                case Right(yaml) =>
                  val versionsAt = yaml.indexOf("  versions:")
                  val docsAt     = yaml.indexOf("  docs:")
                  val workflowAt = yaml.indexOf("  workflow:")
                  versionsAt >= 0 && docsAt > versionsAt && workflowAt > docsAt &&
                  yaml.contains("    permissions:\n      contents: write\n") &&
                  yaml.contains("permissions:\n  contents: read\n") &&
                  yaml.contains("\"./histos run dev.example/site\"") &&
                  !yaml.contains("id-token")
                case Left(_) => false
            case Left(_) => false
        case _ => false
      assertTrue(rendered)
    ,
    test(
      "a scheduled contents write overrides the job and leaves the workflow header read"
    ):
      val job = CiJob(
        JobName("nightly"),
        TaskName("site"),
        JobWhen.Schedule(Cron("0 6 * * 1")),
        JobPermissions.ContentsWrite,
      )
      val rendered = Workflow.renderAll(withJobs(List(job)))
      assertTrue:
        rendered match
          case Right(
                _ :: RenderedWorkflow("histos-nightly.yml", body) :: Nil
              ) =>
            body.contains("permissions:\n  contents: read\n") &&
            body.contains("    permissions:\n      contents: write\n") &&
            !body.contains("id-token")
          case _ => false
    ,
    test("a missing scheduled file is drift and check does not rewrite ci.yml"):
      ZIO.scoped:
        for
          root <- temp
          _    <- Workflow.update(root, graph(Splits.aggregate))
          nightly = CiJob(
            JobName("nightly"),
            TaskName("site"),
            JobWhen.Schedule(Cron("0 6 * * 1")),
          )
          checked <- Workflow.check(root, withJobs(List(nightly))).either
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
          nightly = CiJob(
            JobName("nightly"),
            TaskName("site"),
            JobWhen.Schedule(Cron("0 6 * * 1")),
          )
          _ <- Workflow.update(root, withJobs(List(nightly)))
          orphan = Workflow.file(root, "histos-retired.yml")
          other  = Workflow.file(root, "release.yml")
          _       <- ZIO.attempt(Files.writeString(orphan, "orphan\n"))
          _       <- ZIO.attempt(Files.writeString(other, "release\n"))
          drifted <- Workflow.check(root, withJobs(List(nightly))).either
          _       <- Workflow.update(root, graph(Splits.aggregate))
          gone = !Files.exists(
            Workflow.file(root, "histos-nightly.yml")
          ) && !Files.exists(orphan)
          kept  <- ZIO.attempt(Files.readString(other))
          clean <- Workflow.check(root, graph(Splits.aggregate))
        yield assertTrue(
          drifted == Left(CiError.Drift),
          gone && kept == "release\n",
          clean == Workflow.path(root),
        ),
  )

  private def withJobs(jobs: List[CiJob]): ProjectGraph =
    ProjectGraph.trusted(
      List(
        module("core", Platform.Jvm, Nil),
        module("tests", Platform.Jvm, List(core)),
      ),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      List(registered(jobs)),
    )

  private def registered(jobs: List[CiJob]): RegisteredPlugin =
    RegisteredPlugin(
      PluginId("dev.example"),
      EntryName.trusted("dev.example.Echo$"),
      Contribution(Nil, Nil, jobs),
    )

  private val release = suite("release")(
    test("one ship holds, uploads the named jar, and promote is the other dispatch"):
      val body = renderRelease(List(manual("histos", "0.0.1-SNAPSHOT", "app")), Set("app")).flatMap:
        case RenderedWorkflow("ci.yml", ci) :: RenderedWorkflow(name, text) :: Nil
            if name == Workflow.releaseName && !ci.contains("publish") && !ci.contains("tags:") =>
          Right(text)
        case other => Left(other.toString)
      assertTrue(body == Right(oneShipRelease))
    ,
    test("two ships prefix the tag, and a library ship does not upload"):
      val rendered = renderRelease(
        List(line("libs", "1.0.0", "core"), line("tool", "2.0.0", "app")),
        Set("app"),
      )
      assertTrue:
        rendered match
          case Right(_ :: RenderedWorkflow("histos-release.yml", body) :: Nil) =>
            val libsAt    = body.indexOf("  release-libs:")
            val toolAt    = body.indexOf("  release-tool:")
            val promoteAt = body.indexOf("  promote:")
            val libsJob   = if libsAt >= 0 && toolAt > libsAt then body.substring(libsAt, toolAt) else ""
            val toolJob   = if toolAt >= 0 && promoteAt > toolAt then body.substring(toolAt, promoteAt) else ""
            body.contains("- \"libs/v1.0.0\"") &&
            body.contains("- \"tool/v2.0.0\"") &&
            !body.contains("- \"v1.0.0\"") &&
            !body.contains("- \"v2.0.0\"") &&
            libsJob.contains("\"./histos publish release libs\"") &&
            !libsJob.contains("publish assets") &&
            !libsJob.contains("contents: write") &&
            toolJob.contains("\"./histos publish release tool\"") &&
            toolJob.contains("\"./histos publish assets tool\"") &&
            toolJob.contains("contents: write") &&
            body.contains("\"./histos publish promote ${{ inputs.deployment }}\"") &&
            !body.contains("--automatic")
          case _ => false
    ,
    test("a schedule is rendered after the release file"):
      val nightly = CiJob(
        JobName("nightly"),
        TaskName("site"),
        JobWhen.Schedule(Cron("0 6 * * 1")),
      )
      val rendered = renderRelease(
        List(manual("histos", "0.0.1-SNAPSHOT", "app")),
        Set("app"),
        List(registered(List(nightly))),
      )
      assertTrue:
        rendered match
          case Right(
                RenderedWorkflow("ci.yml", _) :: RenderedWorkflow("histos-release.yml", _) :: RenderedWorkflow(
                  "histos-nightly.yml",
                  _,
                ) :: Nil
              ) =>
            true
          case _ => false
    ,
    test("update writes the release file and an empty catalog deletes it"):
      ZIO.scoped:
        for
          root    <- temp
          built   <- ZIO.fromEither(releaseGraph(List(manual("histos", "0.0.1-SNAPSHOT", "app")), Set("app")))
          _       <- Workflow.update(root, built)
          text    <- ZIO.attempt(Files.readString(Workflow.file(root, Workflow.releaseName)))
          again   <- Workflow.check(root, built)
          _       <- ZIO.attempt(Files.delete(Workflow.file(root, Workflow.releaseName)))
          missing <- Workflow.check(root, built).either
          stale = Workflow.file(root, Workflow.releaseName)
          _ <- ZIO.attempt(Files.writeString(stale, "stale\n"))
          _ <- Workflow.update(root, graph(Splits.aggregate))
          gone = !Files.exists(stale)
          clean <- Workflow.check(root, graph(Splits.aggregate))
        yield assertTrue(
          text == oneShipRelease,
          again == Workflow.path(root),
          missing == Left(CiError.Drift),
          gone,
          clean == Workflow.path(root),
        ),
  )

  private val oneShipRelease: String =
    """name: release
      |on:
      |  push:
      |    tags:
      |      - "v0.0.1-SNAPSHOT"
      |  workflow_dispatch:
      |    inputs:
      |      command:
      |        description: release or promote
      |        required: true
      |        type: choice
      |        options:
      |          - release
      |          - promote
      |      ship:
      |        description: ship
      |        required: true
      |        type: choice
      |        options:
      |          - "histos"
      |      deployment:
      |        description: deployment id
      |        required: false
      |        type: string
      |permissions:
      |  contents: read
      |jobs:
      |  release-histos:
      |    if: github.ref == 'refs/tags/v0.0.1-SNAPSHOT' || (inputs.command == 'release' && inputs.ship == 'histos')
      |    runs-on: ubuntu-26.04
      |    permissions:
      |      contents: write
      |    steps:
      |      - uses: actions/checkout@v7
      |      - run: "./histos publish release histos"
      |        env:
      |          SONATYPE_USERNAME: ${{ secrets.SONATYPE_USERNAME }}
      |          SONATYPE_PASSWORD: ${{ secrets.SONATYPE_PASSWORD }}
      |          PGP_SECRET: ${{ secrets.PGP_SECRET }}
      |          PGP_PASSPHRASE: ${{ secrets.PGP_PASSPHRASE }}
      |          PGP_KEY_HEX: ${{ secrets.PGP_KEY_HEX }}
      |      - run: "./histos publish assets histos"
      |        env:
      |          GITHUB_TOKEN: ${{ github.token }}
      |          GITHUB_REPOSITORY: ${{ github.repository }}
      |  promote:
      |    if: github.event_name == 'workflow_dispatch' && inputs.command == 'promote'
      |    runs-on: ubuntu-26.04
      |    steps:
      |      - uses: actions/checkout@v7
      |      - run: "./histos publish promote ${{ inputs.deployment }}"
      |        env:
      |          SONATYPE_USERNAME: ${{ secrets.SONATYPE_USERNAME }}
      |          SONATYPE_PASSWORD: ${{ secrets.SONATYPE_PASSWORD }}
      |""".stripMargin

  private def renderRelease(
      rows: List[Either[String, Ship]],
      assets: Set[String],
      plugins: List[RegisteredPlugin] = Nil,
  ): Either[String, List[RenderedWorkflow]] =
    releaseGraph(rows, assets, plugins).flatMap(graph => Workflow.renderAll(graph).left.map(_.message))

  private def releaseGraph(
      rows: List[Either[String, Ship]],
      assets: Set[String],
      plugins: List[RegisteredPlugin] = Nil,
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
                val base = module(name.value, Platform.Jvm, Nil)
                if assets.contains(name.value) then base.copy(asset = Some(RepoPath.trusted("dist/histos.jar")))
                else base
            ProjectGraph.trusted(modules, Splits.aggregate, catalog, None, Nil, plugins)

  private def line(name: String, number: String, member: String): Either[String, Ship] =
    for
      ship    <- ShipName.from(name).left.map(_.message)
      parsed  <- ShipNumber.from(number).left.map(_.message)
      project <- ProjectName.from(member).left.map(_.message)
    yield Ship.Line(ship, parsed, ::(project, Nil))

  private def manual(name: String, version: String, member: String): Either[String, Ship] =
    for
      ship    <- ShipName.from(name).left.map(_.message)
      parsed  <- ManualVersion.from(version).left.map(_.message)
      project <- ProjectName.from(member).left.map(_.message)
    yield Ship.Manual(ship, parsed, ::(project, Nil))

  def spec =
    suite("workflow")(render, affected, plugins, release) @@ TestAspect.withLiveSystem
end WorkflowSpec
