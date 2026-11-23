package histos

import histos.plugin.*
import java.nio.file.Path
import _root_.scala.compiletime.testing.typeChecks
import zio.test.*

object MigrationContractsSpec extends ZIOSpecDefault:
  private val scala3     = scala"3.10.1"
  private val core       = module("core", Jvm)(scala3)
  private val page       = OutputName("page")
  private val transcript = OutputName("transcript")
  private val generate   = TaskName("generate")

  private def plugin(tasks: List[DeclaredTask]): HistosPlugin = new HistosPlugin:
    val id                       = PluginId("migration")
    def contribute               = Contribution(tasks, Nil, Nil)
    def run(request: RunRequest) = zio.ZIO.succeed(PluginOutput("", Nil))

  private def task(name: TaskName, deps: List[TaskDep], outputs: List[DeclaredOutput] = Nil): DeclaredTask =
    DeclaredTask(name, Seal.Open, Set.empty, Nil, outputs, deps)

  private def contractFault(graph: Either[BuildError, ProjectGraph]): Option[ContractFault] =
    graph.swap.toOption.collect { case BuildError.Contract(fault) => fault }

  private def isCycle(graph: Either[BuildError, ProjectGraph]): Boolean =
    contractFault(graph).exists:
      case ContractFault.Cycle(path) => path.nonEmpty
      case _                         => false

  private val directory = DeclaredOutput.Directory(transcript, RepoPath("target/transcripts"))

  def spec = suite("migration contracts")(
    test("generation task cycles are refused when the graph is loaded") {
      val first  = task(TaskName("first"), List(TaskDep.Generate(TaskName("second"))))
      val second = task(TaskName("second"), List(TaskDep.Generate(TaskName("first"))))
      assertTrue(isCycle(Projects(use(plugin(List(first, second))), core)))
    },
    test("two tasks cannot own the same output") {
      val output = DeclaredOutput.File(page, RepoPath("target/page.html"))
      val first  = task(TaskName("first"), Nil, List(output))
      val second = task(TaskName("second"), Nil, List(output))
      assertTrue(contractFault(Projects(use(plugin(List(first, second))), core)).exists {
        case ContractFault.OutputOverlap(_, _, path) => path == RepoPath("target/page.html")
        case _                                       => false
      })
    },
    test("ownership includes subtrees, rejects duplicate names, and permits sibling prefixes") {
      val parent   = task(TaskName("parent"), Nil, List(DeclaredOutput.Directory(page, RepoPath("target/site"))))
      val child    = task(TaskName("child"), Nil, List(DeclaredOutput.File(page, RepoPath("target/site/index.html"))))
      val sibling  = child.copy(outputs = List(DeclaredOutput.File(page, RepoPath("target/site-other/index.html"))))
      val repeated = parent.copy(outputs = parent.outputs ++ List(DeclaredOutput.File(page, RepoPath("target/other"))))
      assertTrue(
        contractFault(Projects(use(plugin(List(parent, child))), core)).exists {
          case ContractFault.OutputOverlap(_, _, _) => true
          case _                                    => false
        },
        contractFault(Projects(use(plugin(List(repeated))), core)) ==
          Some(ContractFault.DuplicateOutput(PluginTaskId(PluginId("migration"), TaskName("parent")), page)),
        Projects(use(plugin(List(parent, sibling))), core).isRight,
      )
    },
    test("JVM Main can emit JVM Test and JS Main in the same docs module") {
      val docs     = module("docs", Jvm, Js)(scala3)
      val producer = task(
        generate,
        List(
          TaskDep.CompileScoped(ProjectName("docs"), None, Some(Jvm), SourceSet.Main),
          TaskDep
            .EmitScoped(ProjectName("docs"), SourceSet.Test, PlatformSelection.only(Jvm), RootKind.Sources, transcript),
          TaskDep
            .EmitScoped(ProjectName("docs"), SourceSet.Main, PlatformSelection.only(Js), RootKind.Sources, transcript),
        ),
        List(directory),
      )
      val site = task(
        TaskName("site"),
        List(TaskDep.Compile(ProjectName("docs"), None, Some(Jvm)), TaskDep.Link(ProjectName("docs"), None)),
      )
      assertTrue(Projects(use(plugin(List(producer, site))), docs).isRight)
    },
    test("test-complete Compile still waits for tests and therefore detects a test producer cycle") {
      val producer = task(
        generate,
        List(
          TaskDep.Compile(ProjectName("core"), None, Some(Jvm)),
          TaskDep.EmitScoped(
            ProjectName("core"),
            SourceSet.Test,
            PlatformSelection.only(Jvm),
            RootKind.Resources,
            transcript,
          ),
        ),
        List(directory),
      )
      assertTrue(isCycle(Projects(use(plugin(List(producer))), core)))
    },
    test("indirect compile and link generation cycles are rejected") {
      val downstream = module("downstream", Jvm)(scala3).dependsOn(core)
      val producer   = task(
        generate,
        List(
          TaskDep.CompileScoped(ProjectName("downstream"), None, Some(Jvm), SourceSet.Main),
          TaskDep
            .EmitScoped(ProjectName("core"), SourceSet.Main, PlatformSelection.only(Jvm), RootKind.Sources, transcript),
        ),
        List(directory),
      )
      val js           = module("client", Js)(scala3)
      val linkProducer = producer.copy(dependsOn =
        List(
          TaskDep.Link(ProjectName("client"), None),
          TaskDep.EmitScoped(
            ProjectName("client"),
            SourceSet.Main,
            PlatformSelection.only(Js),
            RootKind.Resources,
            transcript,
          ),
        )
      )
      assertTrue(
        isCycle(Projects(use(plugin(List(producer))), core, downstream)),
        isCycle(Projects(use(plugin(List(linkProducer))), js)),
      )
    },
    test("scoped emission accepts named files or directories and validates names and platforms") {
      val dep = TaskDep.EmitScoped(
        ProjectName("core"),
        SourceSet.Test,
        PlatformSelection.only(Jvm),
        RootKind.Resources,
        transcript,
      )
      val file   = task(generate, List(dep), List(DeclaredOutput.File(transcript, RepoPath("target/transcripts.txt"))))
      val absent = file.copy(outputs = Nil)
      val unsupported = file.copy(
        outputs = List(directory),
        dependsOn = List(
          TaskDep.EmitScoped(
            ProjectName("core"),
            SourceSet.Test,
            PlatformSelection.only(Js),
            RootKind.Resources,
            transcript,
          )
        ),
      )
      assertTrue(
        Projects(use(plugin(List(file))), core).isRight,
        Projects(use(plugin(List(file.copy(outputs = List(directory))))), core).isRight,
        contractFault(Projects(use(plugin(List(absent))), core)) == Some(
          ContractFault.EmittedOutput(PluginTaskId(PluginId("migration"), generate), transcript)
        ),
        Projects(use(plugin(List(unsupported))), core).swap.exists {
          case BuildError.PluginGraph(PluginGraphFault.PluginMissingCell(_, _, _, Some(Js))) => true
          case _                                                                             => false
        },
      )
    },
    test("test edges expose upstream tests without entering the main chain") {
      val client   = module("client", Jvm)(scala3).dependsOn(core).testDependsOn(core)
      val graph    = Projects(core, client)
      val upstream = graph.map(_.projects.collect {
        case module: Module if module.name == ProjectName("client") =>
          (module.upstream(Jvm), module.upstream(Jvm, SourceSet.Test), module.dependsOn.map(_.scope))
      })
      assertTrue(
        upstream == Right(
          List((List(ProjectName("core")), List(ProjectName("core")), List(SourceSet.Main, SourceSet.Test)))
        )
      )
    },
    test("same-scope dependency platform overlaps are duplicates but disjoint edges are valid") {
      val cross       = module("cross", Jvm, Js)(scala3)
      val client      = module("client", Jvm, Js)(scala3)
      val overlapping = client.dependsOn(cross).dependsOn(Jvm)(cross)
      val disjoint    = client.dependsOn(Jvm)(cross).dependsOn(Js)(cross)
      assertTrue(
        Projects(cross, overlapping).swap.exists {
          case BuildError.DuplicateDependency(module, upstream, _) =>
            module == ProjectName("client") && upstream == ProjectName("cross")
          case _ => false
        },
        Projects(cross, disjoint).isRight,
      )
    },
    test("test dependency cycles are validated without rejecting cross-version upstreams") {
      val client     = module("client", Jvm)(scala3).testDependsOn(core)
      val cyclicCore = core.testDependsOn(client)
      val old        = module("old", Jvm)(scala"3.9.3")
      val mismatch   = core.testDependsOn(old)
      val producer   = task(
        generate,
        List(
          TaskDep.CompileScoped(ProjectName("core"), None, Some(Jvm), SourceSet.Test),
          TaskDep
            .EmitScoped(ProjectName("old"), SourceSet.Main, PlatformSelection.only(Jvm), RootKind.Sources, transcript),
        ),
        List(directory),
      )
      assertTrue(
        isCycle(Projects(cyclicCore, client)),
        Projects(old, mismatch).isRight,
        isCycle(Projects(use(plugin(List(producer))), old, mismatch)),
      )
    },
    test("scoped roots and run defaults are validated and persisted") {
      val root = ScopedRoot(RepoPath("shared/scalajvmnative"), SourceSet.Test, ScopedRoot.jvmNative, RootKind.Resources)
      val shared  = module("shared", Jvm, Native)(scala3).clang(ClangMajor(21)).roots(root, root).runPlatform(Jvm)
      val modules = Projects(shared).map(_.projects.collect { case module: Module => module })
      assertTrue(
        modules.exists(_.exists(module => module.roots == List(root) && module.runPlatform.contains(Jvm))),
        root.applies(SourceSet.Test, Native),
        !root.applies(SourceSet.Main, Native),
        !root.applies(SourceSet.Test, Js),
        contractFault(Projects(core.roots(root))).contains(ContractFault.RootPlatform(ProjectName("core"), Native)),
        contractFault(Projects(core.runPlatform(Js))).contains(ContractFault.RunDefault(ProjectName("core"), Js)),
      )
    },
    test("run platform precedence, ambiguity, and explicit unsupported selections are typed") {
      val cross   = module("cross", Jvm, Js)(scala3)
      val modules = Projects(core, cross.runPlatform(Jvm)).map(_.projects.collect { case module: Module => module })
      val results = modules.map(
        _.map(module =>
          (
            RunPlatform.resolve(module, None),
            RunPlatform.resolve(module, Some(PlatformSelection.only(Js))),
            RunPlatform.resolve(module, Some(PlatformSelection.only(Jvm, Js))),
          )
        )
      )
      val ambiguous = Projects(cross).map(_.projects.collect { case module: Module =>
        RunPlatform.resolve(module, None)
      })
      assertTrue(
        results.exists(_.contains((Right(Jvm), Right(Js), Left(SelectionError.RunMany(Platforms(Jvm, List(Js))))))),
        results.exists(_.exists(_._2 == Left(SelectionError.Unsupported(ProjectName("core"), Js)))),
        ambiguous.exists(_.contains(Left(SelectionError.RunAmbiguous(ProjectName("cross"), Platforms(Jvm, List(Js)))))),
      )
    },
    test("text lists merge repeats, preserve escaped delimiters, and reject malformed entries") {
      assertTrue(
        TextList.parse(List("a,b", "b,c")) == Right(List("a", "b", "c")),
        TextList.parse(List("a\\,b,c\\\\d")) == Right(List("a,b", "c\\d")),
        TextList.parse(List("a,,b")) == Left(TextListError.EmptyEntry("a,,b")),
        TextList.parse(List("a,")) == Left(TextListError.EmptyEntry("a,")),
        TextList.parse(List("")) == Left(TextListError.EmptyEntry("")),
        TextList.parse(List("a\\")) == Left(TextListError.TrailingEscape("a\\")),
        TextList.parse(List("a\\q")) == Left(TextListError.InvalidEscape("a\\q", 'q')),
        PlatformSelection.from(List("jvm,js", "native,jvm")) == Right(PlatformSelection.only(Jvm, Js, Native)),
        PlatformSelection.from(Nil) == Left(SelectionError.Empty),
        PlatformSelection.from(List("unknown")) == Left(SelectionError.UnknownPlatform("unknown")),
      )
    },
    test("staging lookup returns only the requested kind and typed missing errors") {
      val stage        = Path.of("staging", "page")
      val destinations = OutputDestinations(
        List(
          OutputDestination.File(page, stage),
          OutputDestination.Directory(transcript, Path.of("staging", "transcripts")),
        )
      )
      assertTrue(
        destinations.file(page) == Right(stage),
        destinations.directory(transcript) == Right(Path.of("staging", "transcripts")),
        destinations.directory(page) == Left(
          PluginRunError.OutputKindMismatch(page, OutputKind.Directory, OutputKind.File)
        ),
        destinations.file(transcript) == Left(
          PluginRunError.OutputKindMismatch(transcript, OutputKind.File, OutputKind.Directory)
        ),
        destinations.directory(OutputName("missing")) == Left(PluginRunError.MissingDestination(OutputName("missing"))),
      )
    },
    test("link optimization cascades to one effective mode and is stored on declarations") {
      val mode   = JsOptimize.Full
      val client = module("client", Js)(scala3).jsOptimize(mode)
      val link   = TaskDep.Link(ProjectName("client"), None, Some(JsOptimize.Fast))
      assertTrue(
        JsOptimize.effective(JsOptimize.Fast, Some(mode), None) == mode,
        JsOptimize.effective(JsOptimize.Fast, Some(mode), Some(mode)) == mode,
        JsOptimize.effective(mode, None, Some(JsOptimize.Fast)) == JsOptimize.Fast,
        JsOptimize.effective(mode, None, None) == mode,
        Projects(client).map(_.projects.collect { case module: Module => module.jsOptimize }) == Right(
          List(Some(mode))
        ),
        link == TaskDep.Link(ProjectName("client"), None, Some(JsOptimize.Fast)),
      )
    },
    test("VM options and properties are distinct from program arguments and persist in the module") {
      val options = JvmOptions(List(JvmFlag("-Xmx2g")), List(SystemProperty("example.mode=checked")))
      assertTrue(
        options.arguments == List("-Xmx2g", "-Dexample.mode=checked"),
        Projects(core.jvmOptions(options)).map(_.projects.collect { case module: Module =>
          module.jvmOptions
        }) == Right(List(options)),
        JvmFlag.from("main.Class").isLeft,
        JvmFlag.from("-cp").isLeft,
        JvmFlag.from("-Dexample.mode=checked").isLeft,
      )
    },
    test("verify test selections and JDK 25 runner requirements are typed and validated") {
      val tools       = RunnerTools.empty.copy(jdk = Some(JdkFloor(25)))
      val selection   = TestSelection(TestTargets.Named(ProjectName("core"), Nil), PlatformSelection.only(Jvm))
      val verify      = VerifyTask(JobName("browser"), VerifyRun.Tests(selection))
      val graph       = Projects.runnerTools(tools).verify(verify)(core)
      val unsupported = verify.copy(run = VerifyRun.Tests(selection.copy(platforms = PlatformSelection.only(Js))))
      assertTrue(
        graph.map(_.runnerTools.jdk.map(_.value)) == Right(Some(25)),
        graph.map(_.verify) == Right(List(verify)),
        verify.run.command == "test core --platform jvm",
        contractFault(Projects.verify(unsupported)(core)).contains(ContractFault.TestPlatform(ProjectName("core"), Js)),
        JdkFloor.from(0) == Left(JdkFloorError.NotPositive(0)),
      )
    },
    test("overlapping native aggregate groups are refused") {
      val member = module("member", Native)(scala3).clang(ClangMajor(21))
      val first  = module("first", Native)(scala3).clang(ClangMajor(21)).testAggregate(member)
      val second = module("second", Native)(scala3).clang(ClangMajor(21)).testAggregate(member)
      assertTrue(
        contractFault(Projects(member, first, second)).contains(
          ContractFault.AggregateOverlap(ProjectName("first"), ProjectName("second"), ProjectName("member"))
        )
      )
    },
    test("invalid scopes, empty selections, platform settings, flags, and escaping paths do not compile") {
      assertTrue(
        typeChecks(
          """import histos.*; import histos.plugin.*; val m = module("client", Jvm, Js)(scala"3.10.1").jsOptimize(JsOptimize.Full).runPlatform(Jvm); val d = TaskDep.CompileScoped(ProjectName("client"), None, Some(Jvm), SourceSet.Main)"""
        ),
        !typeChecks(
          """import histos.*; import histos.plugin.*; TaskDep.CompileScoped(ProjectName("client"), None, Some(Jvm), "main")"""
        ),
        !typeChecks("""import histos.*; PlatformSelection.only()"""),
        !typeChecks("""import histos.*; module("client", Jvm)(scala"3.10.1").jsOptimize(JsOptimize.Full)"""),
        !typeChecks("""import histos.*; module("client", Js)(scala"3.10.1").jvmOptions(JvmOptions.empty)"""),
        !typeChecks("""import histos.*; JvmFlag("-cp")"""),
        !typeChecks("""import histos.*; JdkFloor(0)"""),
        !typeChecks("""import histos.plugin.*; DeclaredOutput.File(OutputName("page"), RepoPath("../page"))"""),
      )
    },
  )
end MigrationContractsSpec
