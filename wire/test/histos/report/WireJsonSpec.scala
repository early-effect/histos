package histos.report

import histos.*
import histos.load.{LoadError, NamedProduct, OutputManifest, PluginFault, ProductEntry, ProductEntryPath, ProductError}
import histos.schedule.{CacheError, Digest, ScheduleError}
import histos.workspace.{IoFailure, IoFault}
import java.nio.file.Path
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
import histos.suite.HistosSpec

import zio.json.*
import zio.test.*

object WireJsonSpec extends HistosSpec:
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
    test("product failures preserve their ADT data through direct and scheduled plugin faults") {
      val faults = List(
        ProductError.Io(IoFailure(Path.of("site"), IoFault.Read)),
        ProductError.Cache(CacheError.Corrupt(Digest.trusted("ab" * 32))),
        ProductError.Report(
          histos.plugin.PluginRunError.OutputKindMismatch(output, OutputKind.File, OutputKind.Directory)
        ),
        ProductError.InvalidManifest,
        ProductError.InvalidEntry("../page"),
        ProductError.UnsafePath(Path.of("unsafe")),
        ProductError.Link(Path.of("link")),
        ProductError.Missing(Path.of("missing")),
        ProductError.Kind(Path.of("kind"), OutputKind.Directory),
        ProductError.Unmanaged(Path.of("existing")),
        ProductError.ActiveReaders,
        ProductError.Changed,
      )
      val results = faults.flatMap(error =>
        List(
          CallResult.Fault(BuildFault.Plugin(PluginFault.Products(error))),
          CallResult.Fault(BuildFault.Plugin(PluginFault.Scheduled(ScheduleError.Products(error)))),
        )
      ) :+ CallResult.Fault(BuildFault.Plugin(PluginFault.MissingProduct(task)))
      assertTrue(results.forall(result => result.toJson.fromJson[CallResult] == Right(result)))
    },
    test("scoped execution faults retain data through test, plugin, run, preview and publication boundaries") {
      val scoped    = histos.testrun.ScopedTestFailure.MissingGenerated(task, output)
      val tested    = histos.testrun.ModuleTestError.Plugin(PluginFault.Scoped(scoped))
      val scheduled = histos.schedule.ActionId
        .from("docs/generate")
        .map(action =>
          CallResult.Fault(
            BuildFault.Plugin(PluginFault.Scheduled(ScheduleError.Failed(action, PluginFault.Tested(tested))))
          )
        )
      val results = List(
        CallResult.Fault(BuildFault.Test(tested)),
        CallResult.Fault(BuildFault.Plugin(PluginFault.Tested(tested))),
        CallResult.Fault(BuildFault.Program(histos.program.ProgramError.Scoped(tested))),
        CallResult.Fault(BuildFault.Preview(histos.preview.PreviewError.Test(tested))),
        CallResult.Fault(BuildFault.Publish(histos.publish.LocalPublishError.Build(tested))),
      )
      assertTrue(
        results.forall(result => result.toJson.fromJson[CallResult] == Right(result)),
        scheduled.exists(result => result.toJson.fromJson[CallResult] == Right(result)),
      )
    },
    test("generic product entry paths validate at the wire edge and round-trip inside manifests") {
      val entry    = ProductEntryPath("assets/@scope/client+v1.js")
      val manifest = OutputManifest(
        "site",
        List(
          NamedProduct(
            histos.plugin.DeclaredOutput.Directory(OutputName("site"), RepoPath("site")),
            List(ProductEntry.RootDirectory, ProductEntry.Directory(entry)),
          )
        ),
      )
      val invalid = List("", "../page", "assets/../page", "/page", "C:/page", "a\\b", "nul\u0000page")
      assertTrue(
        entry.toJson == "\"assets/@scope/client+v1.js\"",
        entry.toJson.fromJson[ProductEntryPath] == Right(entry),
        manifest.toJson.fromJson[OutputManifest] == Right(manifest),
        manifest.toJson.replace(entry.value, "../escape").fromJson[OutputManifest].isLeft,
        invalid.forall(raw => raw.toJson.fromJson[ProductEntryPath].isLeft),
      )
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
