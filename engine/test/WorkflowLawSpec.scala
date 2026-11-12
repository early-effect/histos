package histos

import histos.plugin.JobName
import histos.supply.{Snapshot, SupplyChain, SupplyFault}
import histos.workflow.*

import java.nio.file.{Files, Path}

import _root_.scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object WorkflowLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  given CiBranch = CiBranch("main")

  private val finished = SonaProbe(ReleaseMode.Automatic)

  private val deployment =
    Publishing.fromEffect(
      request => ZIO.fail(PublishFault.Version(request.revision)),
      Sona.secrets,
      FollowUp.Deployment,
    )

  private val onDefault =
    "github.ref == format('refs/heads/{0}', github.event.repository.default_branch)"

  def spec = suite("workflow laws")(
    test("a scaladoc job is present exactly when the graph asks, and it does not wait for the suite"):
      val plain = Projects.organization(organization"example.com")(module("core", Jvm)(scala3))
      val asked = Projects.organization(organization"example.com").docCheck(module("core", Jvm)(scala3))
      (plain.flatMap(Workflow.render), asked.flatMap(Workflow.render)) match
        case (Right(off), Right(on)) =>
          val doc = job(on, "scaladoc")
          assertTrue(
            job(off, "scaladoc").isEmpty,
            job(on, "doc").isEmpty,
            doc.nonEmpty,
            doc.contains("""- run: "./histos run doc""""),
            !doc.contains("needs:"),
          )
        case (_, _) => assertTrue(false)
    ,
    test("workflow and supply-chain run doctor without installing Node, and the test job installs 25"):
      val built = Projects
        .organization(organization"example.com")
        .runnerTools(RunnerTools(Some(NodeFloor(25)), None, None))(
          module("core", Jvm)(scala3),
          module("web", Js)(scala3),
        )
      built.flatMap(Workflow.render) match
        case Right(text) =>
          val workflow = job(text, "workflow")
          val supply   = job(text, "supply-chain")
          val suite    = job(text, "test")
          assertTrue(
            workflow.contains("./histos doctor"),
            supply.contains("./histos doctor"),
            !workflow.contains("node-version"),
            !supply.contains("node-version"),
            suite.contains("""node-version: "25""""),
          )
        case Left(error) => assertTrue(error.toString.isEmpty)
    ,
    test("a versions job is present exactly when the catalog names a ship"):
      val open    = Projects.organization(organization"example.com")(module("core", Jvm)(scala3))
      val shipped = demo(module("core", Jvm)(scala3))
      (open.flatMap(Workflow.render), shipped.flatMap(Workflow.render)) match
        case (Right(off), Right(on)) =>
          val versions = job(on, "versions")
          assertTrue(
            job(off, "versions").isEmpty,
            versions.contains("""- run: "./histos run versions""""),
            !versions.contains("needs: test"),
          )
        case (_, _) => assertTrue(false)
    ,
    test("a release file exists exactly when a ship exists, and it publishes that ship"):
      val open    = Projects.organization(organization"example.com")(module("core", Jvm)(scala3))
      val shipped = demo(module("app", Jvm)(scala3))
      (open.flatMap(Workflow.renderAll(_, finished)), shipped.flatMap(Workflow.renderAll(_, finished))) match
        case (Right(off), Right(on)) =>
          named(on, Workflow.releaseName) match
            case None       => assertTrue(on.exists(_.name == Workflow.releaseName))
            case Some(file) =>
              val order = Sona.secrets.map(secret => file.body.indexOf(s"          $secret:"))
              assertTrue(
                off.forall(_.name != Workflow.releaseName),
                file.body.contains("  release-demo:"),
                file.body.contains("""- run: "./histos publish release demo""""),
                file.body.contains("          HISTOS_SESSION: release"),
                order.forall(_ >= 0),
                order == order.sorted,
                file.body.contains("      contents: read"),
                !file.body.contains("actions: write"),
                on.forall(_.name != Workflow.promoteName),
              )
        case (_, _) => assertTrue(false)
    ,
    test("a promote file exists exactly when follow up is a deployment"):
      val shipped = demo(module("app", Jvm)(scala3))
      shipped.flatMap(Workflow.renderAll(_, deployment)) match
        case Right(files) =>
          named(files, Workflow.promoteName) match
            case None       => assertTrue(files.exists(_.name == Workflow.promoteName))
            case Some(file) =>
              assertTrue(
                file.body.contains("name: promote"),
                !file.body.contains("PGP_SECRET"),
                !file.body.contains("publish release"),
              )
        case _ => assertTrue(false)
    ,
    test("a snapshots file exists exactly when a ship names an asset"):
      val plain  = demo(module("app", Jvm)(scala3))
      val packed =
        demo(module("app", Jvm)(scala3).assembly(MainClass("demo.Main")).asset(asset"dist/app.jar"))
      (plain.flatMap(Workflow.renderAll(_, finished)), packed.flatMap(Workflow.renderAll(_, finished))) match
        case (Right(off), Right(on)) =>
          named(on, Workflow.snapshotsName) match
            case None       => assertTrue(on.exists(_.name == Workflow.snapshotsName))
            case Some(file) =>
              assertTrue(
                off.forall(_.name != Workflow.snapshotsName),
                file.body.contains("""- run: "./histos assembly""""),
                !file.body.contains(s"""- if: "$onDefault""""),
                file.body.contains("""- run: "./histos publish snapshots""""),
                file.body.contains("          HISTOS_SESSION: snapshot"),
                file.body.contains("          HISTOS_BRANCH: default"),
              )
        case (_, _) => assertTrue(false)
    ,
    test("a dependency snapshot job is present exactly when the supply policy asks"):
      val on      = Projects.organization(organization"example.com")(module("core", Jvm)(scala3))
      val renamed = Projects
        .organization(organization"example.com")
        .supplyChain(SupplyChain(snapshot = Snapshot.OnPushToDefault(JobName("ship-stamp"))))(
          module("core", Jvm)(scala3)
        )
      val off = Projects
        .organization(organization"example.com")
        .supplyChain(SupplyChain(snapshot = Snapshot.Off))(module("core", Jvm)(scala3))
      (on.flatMap(Workflow.render), renamed.flatMap(Workflow.render), off.flatMap(Workflow.render)) match
        case (Right(asked), Right(other), Right(quiet)) =>
          assertTrue(
            job(asked, "dependency-snapshot").contains("- run: ./histos deps snapshot --submit"),
            job(asked, "dependency-snapshot").contains("github.event_name == 'push'"),
            job(other, "ship-stamp").contains("- run: ./histos deps snapshot --submit"),
            job(other, "dependency-snapshot").isEmpty,
            !quiet.contains("deps snapshot --submit"),
          )
        case (left, middle, right) =>
          assertTrue(false)
    ,
    test("an empty target is the whole suite, and a pull request plan closes dependents"):
      val core  = module("core", Jvm)(scala3)
      val built = Projects(core, module("app", Jvm)(scala3).dependsOn(core))
      built match
        case Left(_)      => assertTrue(false)
        case Right(graph) =>
          moduleNamed(graph, "core") match
            case None           => assertTrue(moduleNamed(graph, "core").isDefined)
            case Some(coreName) =>
              val planned = Workflow.plan(graph, CiEvent.PullRequest(Set(coreName)))
              val named   = Workflow.target(graph, List("core"))
              (planned, named, Workflow.target(graph, Nil), Workflow.plan(graph, CiEvent.Other)) match
                case (
                      Right(List(TestJob.Aggregate(TestTarget.Named(first, rest)))),
                      Right(TestTarget.Named(asked, tail)),
                      Right(TestTarget.Entire),
                      Right(List(TestJob.Aggregate(TestTarget.Entire))),
                    ) =>
                  assertTrue(
                    first == coreName,
                    rest.map(_.value) == List("app"),
                    asked == coreName,
                    tail.isEmpty,
                    Workflow.argv(TestJob.Aggregate(TestTarget.Entire)) == List("./histos", "test"),
                  )
                case (planLeft, targetLeft, emptyLeft, otherLeft) =>
                  assertTrue(
                    planLeft == Right(Nil),
                    targetLeft == Right(TestTarget.Entire),
                    emptyLeft == Right(TestTarget.Entire),
                    otherLeft == Right(Nil),
                  )
    ,
    test("a target name is a module, once"):
      val core  = module("core", Jvm)(scala3)
      val built = Projects(core, aggregate("all", core))
      (built, ProjectName.from("core"), ProjectName.from("all"), ProjectName.from("missing")) match
        case (Right(graph), Right(coreName), Right(all), Right(missingName)) =>
          assertTrue(
            Workflow.target(graph, List("1core")) == Left(CiError.BadName("1core")),
            Workflow.target(graph, List("core", "core")) == Left(CiError.Duplicate(coreName)),
            Workflow.target(graph, List("all")) == Left(CiError.NotModule(all)),
            Workflow.target(graph, List("missing")) == Left(CiError.Unknown(missingName)),
          )
        case _ => assertTrue(false)
    ,
    test("a split renders the cell command in quotes"):
      val built = Projects(Splits.of(Axis.Module, Axis.Platform, Axis.ScalaVersion), module("core", Jvm)(scala3))
      built.flatMap(Workflow.render) match
        case Right(text) =>
          assertTrue(
            text.contains("""command: "./histos test core --platform jvm --scala 3.10.0""""),
            text.contains("    runs-on: ubuntu-26.04"),
          )
        case _ => assertTrue(false)
    ,
    test("equal workflow text matches, and the first different line is the drift"):
      assertTrue(
        Workflow.differs("same\n", "same\n") == Right(()),
        Workflow.differs("same\n", "other\n") ==
          Left(CiError.Drift(".github/workflows/ci.yml", "line 1\n- other\n+ same")),
      )
    ,
    test("a job timeout outside 1 to 360 fails the render, and an unbounded job omits the cap"):
      val zero = Projects
        .organization(organization"example.com")
        .jobTimeout(JobTimeout.Minutes(0))(
          module("core", Jvm)(scala3)
        )
      val over = Projects
        .organization(organization"example.com")
        .jobTimeout(JobTimeout.Minutes(361))(
          module("core", Jvm)(scala3)
        )
      val open = Projects
        .organization(organization"example.com")
        .jobTimeout(JobTimeout.Unbounded)(
          module("core", Jvm)(scala3)
        )
      val capped = Projects
        .organization(organization"example.com")
        .jobTimeout(JobTimeout.Minutes(30))(
          module("core", Jvm)(scala3)
        )
      (
        zero.flatMap(Workflow.render),
        over.flatMap(Workflow.render),
        open.flatMap(Workflow.render),
        capped.flatMap(Workflow.render),
      ) match
        case (Left(CiError.JobTimeout(low)), Left(CiError.JobTimeout(high)), Right(unbounded), Right(minutes)) =>
          assertTrue(
            low == 0,
            high == 361,
            !unbounded.contains("timeout-minutes:"),
            minutes.contains("    timeout-minutes: 30"),
          )
        case _ => assertTrue(false)
    ,
    test("a native job quotes the pinned llvm digest, and an unknown archive fails before text"):
      val known   = native(ToolRevision("21.1.8"))
      val unknown = native(ToolRevision("9.9.9"))
      val bare    = Projects(module("core", Native)(scala3).clang(ClangMajor(21)))
      (known.flatMap(Workflow.render), unknown.flatMap(Workflow.render), bare.flatMap(Workflow.render)) match
        case (Right(text), Left(CiError.LlvmChecksum(archive)), Right(plain)) =>
          LlvmDigest.of("LLVM-21.1.8-Linux-X64.tar.xz") match
            case Right(hex) =>
              assertTrue(
                text.contains(s"""expected="$hex""""),
                archive == "LLVM-9.9.9-Linux-X64.tar.xz",
                !plain.contains("llvm.tar.xz"),
              )
            case _ => assertTrue(false)
        case _ => assertTrue(false)
    ,
    test("a build file selects everything, a module path selects its dependents, and an unknown path selects nothing"):
      val core  = module("core", Jvm)(scala3)
      val built = Projects(core, module("app", Jvm)(scala3).dependsOn(core), module("extra", Jvm)(scala3))
      built match
        case Left(_)      => assertTrue(false)
        case Right(graph) =>
          val owned = Affected.decide(graph, List("core/src/A.scala"))
          (moduleNamed(graph, "core"), moduleNamed(graph, "app")) match
            case (Some(coreName), Some(appName)) =>
              assertTrue(
                Affected.decide(graph, List("histos.scala")) == Affected.Selection.Everything,
                Affected.decide(graph, List("nested/histos.scala")) == Affected.Selection.Everything,
                owned == Affected.Selection.Modules(List(coreName, appName)),
                Affected.decide(graph, List("notes/readme.md")) == Affected.Selection.Modules(Nil),
                Affected.report(graph, "all", Some(Nil)) == """["all"]""",
                Affected.report(graph, "main", None) == """["all"]""",
                Affected.report(graph, "main", Some(Nil)) == "[]",
              )
            case _ => assertTrue(moduleNamed(graph, "core").isDefined, moduleNamed(graph, "app").isDefined)
    ,
    test("a ci fault lifts to that ci error"):
      val doc  = JobName.trusted("doc")
      val test = JobName.trusted("test")
      assertTrue(
        JobYaml.lift(BuildError.Ci(CiFault.UnknownNeed(doc, test))) == CiError.UnknownNeed(doc, test),
        JobYaml.lift(BuildError.Ci(CiFault.UnpinnedWorkflowRun("other.yml"))) ==
          CiError.UnpinnedWorkflowRun("other.yml"),
        JobYaml.lift(BuildError.PluginGraph(PluginGraphFault.DuplicateSite(doc, test))) ==
          CiError.DuplicateSite(doc, test),
      )
    ,
    test("an unpinned action is a fault, a local action is not, and a catalog miss is not a mismatch"):
      val built = Projects(module("core", Jvm)(scala3))
      built match
        case Left(_)      => ZIO.succeed(assertTrue(false))
        case Right(graph) =>
          ZIO.acquireRelease(ZIO.attemptBlocking(Files.createTempDirectory("histos-pins")).orDie)(delete).flatMap {
            repo =>
              val file = repo.resolve(".github").resolve("workflows").resolve("ci.yml")
              ZIO
                .attemptBlocking {
                  Option(file.getParent).foreach(Files.createDirectories(_))
                  Files.writeString(file, uses)
                }
                .orDie
                .as {
                  val found = PinScan.faults(repo, graph)
                  assertTrue(
                    found.length == 3,
                    found.contains(SupplyFault.UnpinnedUses(pathOf(repo, file), 1, "actions/checkout@v4")),
                    found.exists {
                      case SupplyFault.PinMismatch(_, line, action, foundPin, expected) =>
                        line == 3 && action.value == "actions/checkout" && expected == ActionPins.checkout.pin &&
                        foundPin.sha.value == "0000000000000000000000000000000000000000"
                      case _ => false
                    },
                    found.exists {
                      case SupplyFault.PinWithoutTag(_, line, action) =>
                        line == 4 && action.value == "actions/checkout"
                      case _ => false
                    },
                  )
                }
          }
    ,
    test("lookup prefers an exact action, then the longest prefix"):
      val catalog = ActionPins.catalog(Map.empty)
      assertTrue(
        ActionPins.lookup(catalog, "actions/checkout") == Some(ActionPins.checkout.pin),
        ActionPins.lookup(catalog, "actions/cache/restore") == Some(ActionPins.cache.pin),
        ActionPins.lookup(catalog, "actions/unknown") == None,
      ),
  )

  private val uses =
    """uses: actions/checkout@v4
      |uses: ./.github/actions/setup
      |uses: actions/checkout@0000000000000000000000000000000000000000 # v7.0.1
      |uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1
      |uses: docker://ghcr.io/example/tool@sha256:abc
      |uses: actions/unknown@0123456789abcdef0123456789abcdef01234567 # v1
      |""".stripMargin

  private def demo(member: ProjectDef): Either[BuildError, ProjectGraph] =
    Projects.organization(organization"example.com")(ships(Ship("demo").next(1, 2, 3)(member)), member)

  private def native(llvm: ToolRevision): Either[BuildError, ProjectGraph] =
    Projects.runnerTools(RunnerTools(None, Some(llvm), None))(
      module("core", Native)(scala3).clang(ClangMajor(21))
    )

  private def job(text: String, name: String): String =
    val lines = text.linesIterator.toList
    val start = lines.indexWhere(_ == s"  $name:")
    if start < 0 then ""
    else
      val rest = lines.drop(start + 1)
      val end  = rest.indexWhere(line => line.startsWith("  ") && !line.startsWith("   ") && line.endsWith(":"))
      val body = if end < 0 then rest else rest.take(end)
      (lines(start) :: body).mkString("\n")

  private def named(files: List[RenderedWorkflow], name: String): Option[RenderedWorkflow] =
    files.find(_.name == name)

  private def moduleNamed(graph: ProjectGraph, raw: String): Option[ProjectName] =
    graph.projects.collectFirst:
      case module: Module if module.name.value == raw => module.name

  private def pathOf(repo: Path, file: Path): histos.plugin.RepoPath =
    val relative = repo.relativize(file).toString.replace('\\', '/')
    histos.plugin.RepoPath.from(relative) match
      case Right(path) => path
      case Left(_)     => histos.plugin.RepoPath.trusted(relative)

  private def delete(root: Path): UIO[Unit] =
    ZIO.attemptBlocking {
      if Files.exists(root) then
        val walk  = Files.walk(root)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    }.orDie

end WorkflowLawSpec
