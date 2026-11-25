package histos

import histos.plugin.*
import histos.suite.HistosSpec
import java.nio.file.Path
import _root_.scala.compiletime.testing.typeChecks
import zio.test.*

object MigrationContractsSpec extends HistosSpec:
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
    test("JVM Main emits one source File to JVM Test and JS Main without owning its sibling") {
      val docs    = module("docs", Jvm, Js)(scala3)
      val file    = DeclaredOutput.File(transcript, RepoPath("target/transcripts/GeneratedTranscript.scala"))
      val sibling = task(
        TaskName("sibling"),
        Nil,
        List(DeclaredOutput.File(OutputName("sibling"), RepoPath("target/transcripts/Independent.scala"))),
      )
      val producer = task(
        generate,
        List(
          TaskDep.CompileScoped(ProjectName("docs"), None, Some(Jvm), SourceSet.Main),
          TaskDep.EmitScoped(
            ProjectName("docs"),
            SourceSet.Test,
            PlatformSelection.only(Jvm),
            EmittedContent.SourceFile(ArtifactPath("dev/earlyeffect/docs/GeneratedTranscript.scala")),
            transcript,
          ),
          TaskDep.EmitScoped(
            ProjectName("docs"),
            SourceSet.Main,
            PlatformSelection.only(Js),
            EmittedContent.SourceFile(ArtifactPath("dev/earlyeffect/docs/GeneratedTranscript.scala")),
            transcript,
          ),
        ),
        List(file),
      )
      val site = task(
        TaskName("site"),
        List(TaskDep.Compile(ProjectName("docs"), None, Some(Jvm)), TaskDep.Link(ProjectName("docs"), None)),
      )
      val widened   = producer.copy(outputs = List(directory))
      val collision = sibling.copy(outputs = List(file))
      assertTrue(
        Projects(use(plugin(List(producer, sibling, site))), docs).isRight,
        contractFault(Projects(use(plugin(List(widened, sibling, site))), docs)).exists {
          case ContractFault.OutputOverlap(_, _, _) => true
          case _                                    => false
        },
        contractFault(Projects(use(plugin(List(producer, collision, site))), docs)).exists {
          case ContractFault.OutputOverlap(_, _, _) => true
          case _                                    => false
        },
      )
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
            EmittedContent.ResourceTree,
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
            .EmitScoped(
              ProjectName("core"),
              SourceSet.Main,
              PlatformSelection.only(Jvm),
              EmittedContent.SourceTree,
              transcript,
            ),
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
            EmittedContent.ResourceTree,
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
        EmittedContent.ResourceFile(ResourceName("transcripts.txt")),
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
            EmittedContent.ResourceTree,
            transcript,
          )
        ),
      )
      assertTrue(
        Projects(use(plugin(List(file))), core).isRight,
        Projects(
          use(
            plugin(
              List(
                file.copy(
                  outputs = List(directory),
                  dependsOn = List(
                    TaskDep.EmitScoped(
                      ProjectName("core"),
                      SourceSet.Test,
                      PlatformSelection.only(Jvm),
                      EmittedContent.ResourceTree,
                      transcript,
                    )
                  ),
                )
              )
            )
          ),
          core,
        ).isRight,
        contractFault(Projects(use(plugin(List(absent))), core)) == Some(
          ContractFault.EmittedOutput(PluginTaskId(PluginId("migration"), generate), transcript)
        ),
        Projects(use(plugin(List(unsupported))), core).swap.exists {
          case BuildError.PluginGraph(PluginGraphFault.PluginMissingCell(_, _, _, Some(Js))) => true
          case _                                                                             => false
        },
      )
    },
    test("browser resource Files declare exact classpath names independent of generated paths") {
      val browser = module("appsBrowser", Jvm)(scala3)
      val counter = task(
        TaskName("counter"),
        List(
          TaskDep.EmitScoped(
            ProjectName("appsBrowser"),
            SourceSet.Test,
            PlatformSelection.only(Jvm),
            EmittedContent.ResourceFile(ResourceName("apps-browser/counter-view.js")),
            OutputName("counter"),
          )
        ),
        List(DeclaredOutput.File(OutputName("counter"), RepoPath("target/browser-assets/counter-view.js"))),
      )
      val reference = task(
        TaskName("reference"),
        List(
          TaskDep.EmitScoped(
            ProjectName("appsBrowser"),
            SourceSet.Test,
            PlatformSelection.only(Jvm),
            EmittedContent.ResourceFile(ResourceName("apps-browser/reference-host.js")),
            OutputName("reference"),
          )
        ),
        List(DeclaredOutput.File(OutputName("reference"), RepoPath("target/browser-assets/reference-host.js"))),
      )
      val graph = Projects(use(plugin(List(counter, reference))), browser)
      val names = graph.map(_.plugins.flatMap(_.contribution.tasks.flatMap(_.dependsOn.collect {
        case TaskDep.EmitScoped(_, SourceSet.Test, _, EmittedContent.ResourceFile(name), _) => name
      })))
      assertTrue(
        names == Right(
          List(ResourceName("apps-browser/counter-view.js"), ResourceName("apps-browser/reference-host.js"))
        ),
        EmittedContent.ResourceFile(ResourceName("apps-browser/counter-view.js")).outputKind == OutputKind.File,
        EmittedContent.ResourceFile(ResourceName("apps-browser/counter-view.js")).rootKind == RootKind.Resources,
        EmittedContent.ResourceTree.outputKind == OutputKind.Directory,
        !typeChecks("""import histos.plugin.*; EmittedContent.ResourceFile()"""),
        !typeChecks("""import histos.plugin.*; EmittedContent.ResourceFile("apps-browser/counter-view.js")"""),
        !typeChecks(
          """import histos.*; import histos.plugin.*; EmittedContent.ResourceFile(ResourceName("../counter-view.js"))"""
        ),
        !typeChecks(
          """import histos.*; import histos.plugin.*; TaskDep.EmitScoped(ProjectName("appsBrowser"), SourceSet.Test, PlatformSelection.only(Jvm), RootKind.Resources, OutputName("counter"))"""
        ),
      )
    },
    test("emission shape mismatches are refused with typed expected and actual kinds") {
      val file = DeclaredOutput.File(transcript, RepoPath("target/transcripts.txt"))
      val id   = PluginTaskId(PluginId("migration"), generate)
      def graph(output: DeclaredOutput, content: EmittedContent): Either[BuildError, ProjectGraph] =
        Projects(
          use(
            plugin(
              List(
                task(
                  generate,
                  List(
                    TaskDep
                      .EmitScoped(ProjectName("core"), SourceSet.Test, PlatformSelection.only(Jvm), content, transcript)
                  ),
                  List(output),
                )
              )
            )
          ),
          core,
        )
      assertTrue(
        contractFault(graph(directory, EmittedContent.ResourceFile(ResourceName("transcripts.txt")))) ==
          Some(ContractFault.EmittedOutputKind(id, transcript, OutputKind.File, OutputKind.Directory)),
        contractFault(graph(file, EmittedContent.ResourceTree)) ==
          Some(ContractFault.EmittedOutputKind(id, transcript, OutputKind.Directory, OutputKind.File)),
        contractFault(graph(directory, EmittedContent.SourceFile(ArtifactPath("docs/Generated.scala")))) ==
          Some(ContractFault.EmittedOutputKind(id, transcript, OutputKind.File, OutputKind.Directory)),
        contractFault(graph(file, EmittedContent.SourceTree)) ==
          Some(ContractFault.EmittedOutputKind(id, transcript, OutputKind.Directory, OutputKind.File)),
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
            .EmitScoped(
              ProjectName("old"),
              SourceSet.Main,
              PlatformSelection.only(Jvm),
              EmittedContent.SourceTree,
              transcript,
            ),
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
    test("scala tokens select a series or one version, and run starts one") {
      val older                                        = scala"3.3.6"
      val newer                                        = scala"3.7.1"
      val scala2                                       = scala"2.13.18"
      val cross                                        = module("cross", Jvm)(older, newer)
      val both                                         = module("both", Jvm)(scala2, older)
      val only                                         = module("only", Jvm)(scala2)
      val series                                       = ScalaSelection.Only(ScalaToken.Series3, Nil)
      val exact                                        = ScalaSelection.Only(ScalaToken.Exact(older), Nil)
      def built(project: ModuleDef[?]): Option[Module] =
        Projects(project).toOption.flatMap(_.projects.collect { case module: Module => module }.headOption)
      assertTrue(
        ScalaSelection.from(List("2.13")) == Right(ScalaSelection.Only(ScalaToken.Series2(13), Nil)),
        ScalaSelection.from(List("3")) == Right(series),
        ScalaSelection.from(List("2.13.18")) == Right(ScalaSelection.Only(ScalaToken.Exact(scala2), Nil)),
        ScalaSelection.from(List("wasm")) == Left(SelectionError.UnknownScala("wasm")),
        ScalaSelection.from(List("3.3")) == Left(SelectionError.UnknownScala("3.3")),
        ScalaSelection.from(List("2")) == Left(SelectionError.UnknownScala("2")),
        series.filter(List(older, newer)) == List(older, newer),
        exact.filter(List(older, newer)) == List(older),
        ScalaSelection.Only(ScalaToken.Series2(13), Nil).filter(List(older, newer)).isEmpty,
        series.require(ProjectName("cross"), List(older, newer)) == Left(
          SelectionError.AmbiguousScala(ProjectName("cross"), ScalaToken.Series3, List(older, newer))
        ),
        exact.require(ProjectName("cross"), List(older, newer)) == Right(List(older)),
        ScalaSelection.Only(ScalaToken.Series2(13), Nil).require(ProjectName("cross"), List(older, newer)) == Left(
          SelectionError.UnsupportedScala(ProjectName("cross"), ScalaToken.Series2(13))
        ),
        SelectionError.UnknownScala("wasm").message == "'wasm' is not a Scala series or a full version",
        SelectionError.UnsupportedScala(ProjectName("core"), ScalaToken.Series2(13)).message ==
          "core does not build 2.13",
        SelectionError.AmbiguousScala(ProjectName("core"), ScalaToken.Series3, List(older, newer)).message ==
          "core 3 matches 3.3.6 and 3.7.1",
        SelectionError.RunScalaMany(List(ScalaToken.Series2(13), ScalaToken.Series3)).message ==
          "run requires one Scala version, not 2.13,3",
        SelectionError.RunScalaAmbiguous(ProjectName("core"), List(scala2, scala3)).message ==
          "run core requires --scala 2.13.18|3.10.1",
        built(only).map(module => RunScala.resolve(module, None)) == Some(Right(scala2)),
        built(both).map(module => RunScala.resolve(module, None)) == Some(
          Left(SelectionError.RunScalaAmbiguous(ProjectName("both"), List(scala2, older)))
        ),
        built(cross).map(module => RunScala.resolve(module, Some(series))) == Some(
          Left(SelectionError.RunScalaAmbiguous(ProjectName("cross"), List(older, newer)))
        ),
        built(both).map(module =>
          RunScala.resolve(module, Some(ScalaSelection.Only(ScalaToken.Series2(13), List(ScalaToken.Series3))))
        ) == Some(
          Left(SelectionError.RunScalaMany(List(ScalaToken.Series2(13), ScalaToken.Series3)))
        ),
        built(only).map(module => RunScala.resolve(module, Some(series))) == Some(
          Left(SelectionError.UnsupportedScala(ProjectName("only"), ScalaToken.Series3))
        ),
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
    test("observed result constructors require completed scope and effective link mode") {
      assertTrue(
        !typeChecks(
          """import histos.*; import histos.plugin.*; CellClasspath(ProjectName("core"), scala"3.10.1", Jvm, Nil, Nil)"""
        ),
        !typeChecks(
          """import histos.*; import histos.plugin.*; import java.nio.file.Path; LinkedScript(ProjectName("core"), scala"3.10.1", Path.of("script.js"))"""
        ),
        typeChecks(
          """import histos.*; import histos.plugin.*; CellClasspath(ProjectName("core"), scala"3.10.1", Jvm, Nil, Nil, SourceSet.Main)"""
        ),
        typeChecks(
          """import histos.*; import histos.plugin.*; import java.nio.file.Path; LinkedScript(ProjectName("core"), scala"3.10.1", Path.of("script.js"), JsOptimize.Full)"""
        ),
      )
    },
    test("named classpath factories and omission overloads state their fixed semantics") {
      val mainPath = Path.of("main-classes")
      val testPath = Path.of("test-classes")
      val main     = CellClasspath.mainOnly(ProjectName("core"), scala3, Jvm, List(mainPath))
      val tests = CellClasspath.testComplete(ProjectName("core"), scala3, Jvm, List(mainPath), List(testPath, mainPath))
      val script = LinkedScript(
        ProjectName("core"),
        scala3,
        Path.of("script.js"),
        JsOptimize.effective(JsOptimize.Fast, Some(JsOptimize.Full), None),
      )
      assertTrue(
        main.scope == SourceSet.Main,
        main.test.isEmpty,
        tests.scope == SourceSet.Test,
        tests.test == List(testPath, mainPath),
        main.stage == CompilationStage(Cell(ProjectName("core"), scala3, Jvm), SourceSet.Main),
        script.optimize == JsOptimize.Full,
        PluginOutput("", Nil) == PluginOutput("", Nil, Nil),
        TaskDep.Link(ProjectName("core"), None) == TaskDep.Link(ProjectName("core"), None, None),
        typeChecks(
          """import histos.plugin.*; import java.nio.file.Path; def make(invocation: InvocationContext, events: PluginEvents.Invocation) = RunRequest(PluginTaskId(PluginId("review"), TaskName("task")), Path.of("repo"), Nil, PluginArgs.empty, Nil, invocation, events)"""
        ),
        typeChecks(
          """import histos.plugin.*; import java.nio.file.Path; def make(invocation: InvocationContext, events: PluginEvents.Invocation) = RunRequest(PluginTaskId(PluginId("review"), TaskName("task")), Path.of("repo"), Nil, PluginArgs.empty, Nil, invocation, events, destinations = OutputDestinations.empty)"""
        ),
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
        JvmFlag.from("-Xbootclasspath/a:/tmp/extra.jar") ==
          Left(JvmFlagError.Malformed("-Xbootclasspath/a:/tmp/extra.jar")),
        JvmFlag.from("-Xbootclasspath:/tmp/extra.jar") ==
          Left(JvmFlagError.Malformed("-Xbootclasspath:/tmp/extra.jar")),
        JvmFlag.from("-Xbootclasspath/p:/tmp/extra.jar") ==
          Left(JvmFlagError.Malformed("-Xbootclasspath/p:/tmp/extra.jar")),
        JvmFlag.from("-Xmx2g").map(_.value) == Right("-Xmx2g"),
        JvmFlag.from("-XX:+UnlockDiagnosticVMOptions").map(_.value) == Right("-XX:+UnlockDiagnosticVMOptions"),
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
        VerifyRun
          .Tests(
            selection.copy(scala = ScalaSelection.Only(ScalaToken.Series2(13), Nil))
          )
          .command == "test core --platform jvm --scala 2.13",
        VerifyRun.Tests(TestSelection(platforms = PlatformSelection.only(Jvm, Js, Native))).command ==
          "test --platform jvm,js,native",
        contractFault(Projects.verify(unsupported)(core)).contains(ContractFault.TestPlatform(ProjectName("core"), Js)),
        contractFault(
          Projects.verify(
            verify.copy(run = VerifyRun.Tests(selection.copy(scala = ScalaSelection.Only(ScalaToken.Series2(13), Nil))))
          )(core)
        ).contains(
          ContractFault.TestSelection(SelectionError.UnsupportedScala(ProjectName("core"), ScalaToken.Series2(13)))
        ),
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
        !typeChecks("""import histos.*; JvmFlag("-Xbootclasspath/a:/tmp/extra.jar")"""),
        !typeChecks("""import histos.*; JvmFlag("-Xbootclasspath:/tmp/extra.jar")"""),
        !typeChecks("""import histos.*; JvmFlag("-Xbootclasspath/p:/tmp/extra.jar")"""),
        !typeChecks("""import histos.*; JdkFloor(0)"""),
        !typeChecks("""import histos.plugin.*; DeclaredOutput.File(OutputName("page"), RepoPath("../page"))"""),
      )
    },
  )
end MigrationContractsSpec
