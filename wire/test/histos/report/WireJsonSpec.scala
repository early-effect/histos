package histos.report

import histos.*
import histos.load.LoadError
import histos.plugin.{
  ArtifactPath,
  EmittedContent,
  OutputKind,
  OutputName,
  PluginId,
  PluginTaskId,
  RepoPath,
  SourceSet,
  TaskName,
}
import histos.report.WireJson.given

import zio.json.*
import zio.test.*

object WireJsonSpec extends ZIOSpecDefault:
  private val module    = ProjectName("docs")
  private val other     = ProjectName("client")
  private val member    = ProjectName("core")
  private val task      = PluginTaskId(PluginId("docs"), TaskName("generate"))
  private val next      = PluginTaskId(PluginId("docs"), TaskName("site"))
  private val output    = OutputName("transcript")
  private val path      = RepoPath("target/site")
  private val platforms = Platforms(Platform.Native, List(Platform.Js, Platform.Jvm))

  private val selections: List[SelectionError] = List(
    SelectionError.Empty,
    SelectionError.Text(TextListError.EmptyEntry("jvm,,native")),
    SelectionError.Text(TextListError.TrailingEscape("native\\")),
    SelectionError.Text(TextListError.InvalidEscape("jvm\\x", 'x')),
    SelectionError.UnknownPlatform("wasm"),
    SelectionError.Unsupported(module, Platform.Js),
    SelectionError.RunMany(platforms),
    SelectionError.RunAmbiguous(module, platforms),
  )

  private val stages = for
    platform <- Platform.values.toList
    scope    <- List(SourceSet.Main, SourceSet.Test)
  yield CompilationStage(Cell(module, ScalaVersion("3.10.1"), platform), scope)

  private val faults: List[ContractFault] = List(
    ContractFault.RunDefault(module, Platform.Native),
    ContractFault.RootPlatform(module, Platform.Js),
    ContractFault.AggregateOverlap(module, other, member),
    ContractFault.DuplicateOutput(task, output),
    ContractFault.OutputOverlap(task, next, path),
    ContractFault.EmittedOutput(task, output),
    ContractFault.Cycle(
      stages.map(BuildNode.Compile(_)) ++ List(
        BuildNode.Link(Cell(other, ScalaVersion("3.9.2"), Platform.Js)),
        BuildNode.Task(task),
      )
    ),
    ContractFault.TestModule(other),
    ContractFault.TestPlatform(module, Platform.Native),
  ) ++ selections.map(ContractFault.TestSelection(_)) ++ (for
    expected <- OutputKind.values.toList
    actual   <- OutputKind.values.toList
  yield ContractFault.EmittedOutputKind(task, output, expected, actual))

  def spec = suite("wire scoped contracts")(
    test("every contract fault retains its data through the enclosing socket result") {
      val results =
        faults.map(fault => CallResult.Fault(BuildFault.Load(LoadError.BrokenGraph(BuildError.Contract(fault)))))
      assertTrue(results.forall(result => result.toJson.fromJson[CallResult] == Right(result)))
    },
    test("compilation stages retain main/test scope, platform, module, and version") {
      assertTrue(stages.forall(stage => stage.toJson.fromJson[CompilationStage] == Right(stage)))
    },
    test("platform and test selections retain nonempty ordering and named targets") {
      val all            = TestSelection()
      val selected       = TestSelection(TestTargets.Named(module, List(other)), PlatformSelection.Only(platforms))
      val emptyPlatforms = """{"first":"jvm","rest":[]}"""
      assertTrue(
        all.toJson.fromJson[TestSelection] == Right(all),
        selected.toJson.fromJson[TestSelection] == Right(selected),
        emptyPlatforms.fromJson[Platforms] == Right(Platforms(Platform.Jvm, Nil)),
        """{"rest":["native"]}""".fromJson[Platforms].isLeft,
      )
    },
    test("scoped roots retain exact path, kind, scope, and platform selection") {
      val roots = for
        scope <- List(SourceSet.Main, SourceSet.Test)
        kind  <- RootKind.values.toList
      yield ScopedRoot(path, scope, PlatformSelection.Only(platforms), kind)
      assertTrue(roots.forall(root => root.toJson.fromJson[ScopedRoot] == Right(root)))
    },
    test("optimization uses explicit fast/full tokens and refuses unknown modes") {
      assertTrue(
        JsOptimize.Fast.toJson == "\"fast\"",
        JsOptimize.Full.toJson == "\"full\"",
        "\"fast\"".fromJson[JsOptimize] == Right(JsOptimize.Fast),
        "\"full\"".fromJson[JsOptimize] == Right(JsOptimize.Full),
        "\"unknown\"".fromJson[JsOptimize].isLeft,
      )
    },
    test("emitted file names and tree kinds retain their typed wire shapes") {
      val source                         = ArtifactPath("dev/earlyeffect/docs/GeneratedTranscript.scala")
      val resource                       = ResourceName("apps-browser/counter-view.js")
      val contents: List[EmittedContent] = List(
        EmittedContent.SourceFile(source),
        EmittedContent.SourceTree,
        EmittedContent.ResourceFile(resource),
        EmittedContent.ResourceTree,
      )
      assertTrue(
        contents.forall(content => content.toJson.fromJson[EmittedContent] == Right(content)),
        "\"../Generated.scala\"".fromJson[ArtifactPath].isLeft,
        "\"../counter-view.js\"".fromJson[ResourceName].isLeft,
        source.toJson == "\"dev/earlyeffect/docs/GeneratedTranscript.scala\"",
        resource.toJson == "\"apps-browser/counter-view.js\"",
      )
    },
    test("newtype codecs validate input at the wire edge") {
      val flag  = JvmFlag("-Xmx2G")
      val floor = JdkFloor(25)
      assertTrue(
        flag.toJson.fromJson[JvmFlag] == Right(flag),
        floor.toJson.fromJson[JdkFloor] == Right(floor),
        "\"-Dkey=value\"".fromJson[JvmFlag].isLeft,
        "0".fromJson[JdkFloor].isLeft,
      )
    },
  )
end WireJsonSpec
