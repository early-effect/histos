package histos

import histos.jdk.{JdkMajor, JdkVersion, Sha256}
import histos.node.{NodeMajor, NodeVersion}
import histos.pack.MainClass as PackMain
import histos.plugin.*
import histos.resolve.ResolveError
import histos.version.{CommitSha, ShipName, ShipNumber}
import histos.workspace.{IoFailure, IoFault}
import histos.zinc.GraphCompileError

import java.nio.file.Path

/** Literals the wire laws build on. Each one is a value `from` accepts. */
object WireSamples:
  val hex: String   = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
  val sha40: String = "0123456789abcdef0123456789abcdef01234567"

  val core: ProjectName            = ProjectName("core")
  val api: ProjectName             = ProjectName("api")
  val scala3: ScalaVersion         = ScalaVersion("3.10.0")
  val scala213: ScalaVersion       = ScalaVersion("2.13.16")
  val path: Path                   = Path.of("/work/repo")
  val other: Path                  = Path.of("/other/A.scala")
  val sha: Sha256                  = Sha256("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
  val dep: Dep                     = Dep("dev.zio:zio:2.1.27")
  val binaryDep: Dep               = Dep("dev.zio::zio:2.1.27")
  val fullDep: Dep                 = Dep("dev.zio:::zio:2.1.27")
  val plugin: PluginId             = PluginId("demo")
  val taskName: TaskName           = TaskName("site")
  val command: CommandName         = CommandName("site")
  val job: JobName                 = JobName("docs")
  val actionName: ActionName       = ActionName("actions/checkout")
  val runner: RunnerName           = RunnerName("ubuntu")
  val secret: SecretName           = SecretName("HISTOS_TOKEN")
  val target: TargetName           = TargetName("site")
  val ship: ShipName               = ShipName("histos")
  val number: ShipNumber           = ShipNumber("1.2.3")
  val commit: CommitSha            = CommitSha("0123456789abcdef0123456789abcdef01234567")
  val org: Organization            = Organization("rocks.earlyeffect")
  val main: MainClass              = MainClass("demo.Exit")
  val packed: PackMain             = PackMain.trusted("demo.Exit")
  val testMain: TestMain           = TestMain("demo.TestMain")
  val framework: FrameworkClass    = FrameworkClass("zio.test.ZTestFramework")
  val library: ProviderLibrary     = ProviderLibrary("dev.zio", "zio-test")
  val provider: TestProvider       = TestProvider.trusted(framework, library)
  val param: ParamName             = ParamName("out")
  val flag: FlagName               = FlagName("force")
  val note: NoteKey                = NoteKey("k")
  val output: OutputName           = OutputName("out")
  val entry: EntryName             = EntryName("demo.Plugin$")
  val projectPath: ProjectPath     = ProjectPath("modules/core")
  val repoPath: RepoPath           = RepoPath("docs/index.html")
  val jsVersion: JsVersion         = JsVersion("1.19.0")
  val nativeVersion: NativeVersion = NativeVersion("0.5.9")
  val revision: ToolRevision       = ToolRevision("21.1.8")
  val clang: ClangMajor            = ClangMajor(21)
  val digest: ApiDigest            = ApiDigest("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
  val hash: ContentHash            = ContentHash("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
  val detail: PluginDetail         = PluginDetail("broke")
  val jdk: JdkVersion              = JdkVersion.trusted("21.0.2", JdkMajor.trusted(21))
  val node: NodeVersion            = NodeVersion.trusted("22.14.0", NodeMajor.trusted(22))
  val pos: SourcePos               = SourcePos("histos.scala", 1, 1)
  val taskId: PluginTaskId         = PluginTaskId(plugin, taskName)
  val io: IoFailure                = IoFailure(path, IoFault.Read)
  val missing: ResolveError        = ResolveError.NotFound("dev.zio", "zio", "0")
  val noRoot: GraphCompileError    = GraphCompileError.NoRoot(core)
end WireSamples
