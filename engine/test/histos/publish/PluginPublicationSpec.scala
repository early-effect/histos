package histos.publish

import scala.jdk.CollectionConverters.*

import histos.*
import histos.cache.ContentStore
import histos.central.{Artifact, Layout, PublishError, Published, Publisher, Revision}
import histos.contracts.ContractFixtures
import histos.jdk.{JdkMajor, JdkVersion, Machine}
import histos.plugin.{
  ArtifactFormat,
  ArtifactIdentity,
  BindingClass,
  BindingKind,
  ClosureArtifact,
  ClosureRole,
  CompilerConstraint,
  Digests,
  EntryName,
  InputKey,
  InputRecord,
  RecipeError,
  RecipeNodeId,
  StageAssurance,
  StageBinding,
  StageContract,
  StageContractError,
  StageInputs,
  StageRestrictions,
}
import histos.resolve.{
  BuildResolvers,
  Classpath,
  ClasspathScope,
  LockedArtifact,
  LockedTools,
  Repositories,
  Repository,
  ResolutionService,
  Resolve,
  ResolveCell,
  ResolveError,
  ResolvedArtifact,
  ToolRequest,
}
import histos.schedule.RemoteCache
import histos.version.{ReleaseVersion, ShipNumber}
import histos.workspace.Workspace
import histos.zinc.{
  CellCapture,
  CompilationCapture,
  CompilationSnapshot,
  CompileError,
  CompileReplay,
  CompilerPart,
  GraphCompile,
  GraphCompileError,
  ScalacClasspath,
  Sources,
  ToolchainMiss,
}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import zio.{Chunk, Ref, ZIO, ZLayer, durationInt}
import zio.test.*

object PluginPublicationSpec extends ZIOSpecDefault:
  private val version  = ScalaVersion("3.10.1")
  private val revision = Revision.Release(ReleaseVersion(ShipNumber("0.1.0")))
  private val node     = RecipeNodeId("plug")

  def spec = suite("plugin publication")(
    test("a generated replacement is the rebuild payload") {
      ZIO.scoped {
        Workspace
          .work("plugin-prefer")
          .flatMap: repo =>
            val committed = "package demo\n\nobject Mark:\n  def token = \"committed-mark\"\n"
            val generated = "package demo\n\nobject Mark:\n  def token = \"generated-mark\"\n"
            val source    =
              repo.resolve("plug").resolve("src").resolve("main").resolve("scala").resolve("demo").resolve("Mark.scala")
            val unmanaged =
              Sources.unmanagedRoot(repo, "plug").resolve("main").resolve("scala").resolve("demo").resolve("Mark.scala")
            for
              _        <- write(source, committed)
              _        <- write(unmanaged, generated)
              selected <- PluginBundle.preferred(
                repo,
                repo.resolve("plug").resolve("src"),
                Sources.unmanagedRoot(repo, "plug"),
                version,
                Platform.Jvm,
              )
              current <- ZIO.foreach(selected): input =>
                read(repo.resolve(input.path.value)).map(bytes => SourceBytes(input.path, Chunk.fromArray(bytes)))
              capture <- ZIO.fromEither(captured(selected, Nil))
              staged  <- ZIO.fromEither(stage(PluginPublication.Rebuildable, capture, current, Nil))
              packed  <- publish(staged)
            yield
              val payload = packed.entries.find: (name, _) =>
                name.contains("META-INF/histos/rebuild/") && name.endsWith("Mark.scala")
              val text = payload.map: (_, bytes) =>
                new String(bytes, StandardCharsets.UTF_8)
              assertTrue(
                selected.exists(_.provenance == histos.plugin.SourceProvenance.Generated),
                text.contains(generated),
                !text.exists(_.contains("committed-mark")),
                packed.uploads == 1,
              )
      }
    },
    test("a compile classpath member is in the recipe and a compile-only coordinate is off the pom") {
      val only    = Dep("com.example:only:1.2.3").compileOnly
      val kept    = Dep("com.example:kept:1.2.3")
      val digest  = Digests.content("compile-only-member".getBytes(StandardCharsets.UTF_8))
      val closure = List(ClosureArtifact(ArtifactIdentity(ArtifactFormat.Jar, digest), ClosureRole.Compile))
      for
        dependencies <- ZIO.fromEither(PluginBundle.dependencies(List(only, kept), version, Platform.Jvm))
        capture      <- ZIO.fromEither(captured(Nil, closure))
        staged       <- ZIO.fromEither(stage(PluginPublication.Rebuildable, capture, Nil, dependencies))
        packed       <- publish(staged)
      yield
        val names  = dependencies.map(_.name.value)
        val frozen = staged.recipe.toList
          .flatMap(_.nodes)
          .flatMap: item =>
            item.sources match
              case InputRecord.Frozen(body) => body.closure
              case InputRecord.Unrecorded   => Nil
        val pom  = Layout.pom(artifactOf(staged, dependencies))
        val json = packed.text(PluginBundle.Descriptor)
        assertTrue(
          names.contains("kept"),
          !names.contains("only"),
          frozen.exists(item => item.role == ClosureRole.Compile && item.artifact.digest == digest),
          json.exists(_.contains(digest.value)),
          json.exists(_.contains("\"role\":\"compile\"")),
          pom.contains("<artifactId>kept</artifactId>"),
          !pom.contains("<artifactId>only</artifactId>"),
          packed.uploads == 1,
        )
    },
    test("an unknown stage refuses publication before an upload") {
      refuse(
        hostKnown(EntryName("demo.NotKnown")),
        RecipeError.Unsupported(histos.plugin.RecipeFeature("stage"), stageId),
      )
    },
    test("an empty stage entrypoint is incomplete and uploads nothing") {
      refuse(hostKnownEmpty, RecipeError.Incomplete(histos.plugin.RecipeInput("stage"), stageId))
    },
    test("a missing public binding is incomplete and uploads nothing") {
      val binding = StageBinding(InputKey("src/Need.scala"), BindingClass.Public, BindingKind.File)
      refuse(declared(List(binding)), RecipeError.Incomplete(histos.plugin.RecipeInput("binding"), stageId))
    },
    test("a secret stage input uploads nothing") {
      val binding = StageBinding(InputKey("TOKEN"), BindingClass.Secret, BindingKind.Env)
      refuse(declared(List(binding)), RecipeError.Stage(StageContractError.SecretInput))
    },
    test("an unclassified stage input uploads nothing") {
      val binding = StageBinding(InputKey("TOKEN"), BindingClass.Unclassified, BindingKind.Env)
      refuse(declared(List(binding)), RecipeError.Stage(StageContractError.UnclassifiedInput))
    },
    test("an unrecorded rebuildable capture uploads nothing") {
      val built = ask(PluginPublication.Rebuildable, CompilationCapture.Unrecorded, Nil, Nil)
      ZIO
        .fromEither(built)
        .flatMap: request =>
          counted(PluginBundle.publish(request)) { (uploads, result) =>
            ZIO.succeed(
              assertTrue(uploads == 0, result == Left(PluginPublishFault.Recipe(RecipeError.Unrecorded(node))))
            )
          }
    },
    test("binary-only keeps binding metadata and makes no rebuild claim") {
      val built = ask(PluginPublication.BinaryOnly, CompilationCapture.Unrecorded, Nil, Nil)
      ZIO
        .fromEither(built)
        .flatMap: request =>
          counted(PluginBundle.publish(request)) { (uploads, result) =>
            readPublished(uploads, result).map: packed =>
              val json = packed.text(PluginBundle.Descriptor)
              assertTrue(
                packed.uploads == 1,
                json.exists(_.contains("\"mode\":\"binary-only\"")),
                json.exists(_.contains("\"binding\":null")),
                json.forall(text => !text.contains("\"closure\"")),
                !packed.entries.keys.exists(_.startsWith("META-INF/histos/rebuild/")),
              )
          }
    },
    test("bytes that differ from the snapshot refuse publication") {
      val current = List(
        SourceBytes(histos.plugin.RepoPath("src/A.scala"), Chunk.fromArray("changed".getBytes(StandardCharsets.UTF_8)))
      )
      for
        snapshot <- ZIO.fromEither(fixture)
        request  <- ZIO.fromEither(
          ask(PluginPublication.Rebuildable, CompilationCapture.Recorded(snapshot), current, Nil)
        )
        outcome <- counted(PluginBundle.publish(request))((uploads, result) => ZIO.succeed((uploads, result)))
      yield
        val (uploads, result) = outcome
        val changed           = result match
          case Left(PluginPublishFault.Changed(path)) => path.value == "src/A.scala"
          case _                                      => false
        assertTrue(uploads == 0, changed)
    },
    test("an empty toolchain list fails capture") {
      val jdk     = JdkVersion.trusted("21", JdkMajor.trusted(21))
      val bridge  = Path.of("bridge.jar")
      val jar     = Path.of("compiler.jar")
      val library = Path.of("library.jar")
      val capture = histos.zinc.CaptureRequest(
        Path.of("repo"),
        CompilationStage(Cell(ProjectName("plug"), version, Platform.Jvm), histos.plugin.SourceSet.Main),
        histos.zinc.StageFiles(Nil, Nil, Nil),
        histos.zinc.CaptureProduct(Path.of("classes"), Nil),
        histos.zinc.CaptureTools(jdk, Nil, List(jar), bridge),
      )
      val runtimeMiss  = CellCapture.record(capture).either
      val compilerMiss = CellCapture
        .record(
          capture.copy(
            tools = histos.zinc.CaptureTools(jdk, List(library), Nil, bridge)
          )
        )
        .either
      runtimeMiss
        .zip(compilerMiss)
        .map: (runtime, compiler) =>
          assertTrue(
            runtime == Left(missing(CompilerPart.Library)),
            compiler == Left(missing(CompilerPart.Scala3Compiler)),
          )
    },
    test("a resident GraphCompile snapshot publishes the jar it compiled") {
      ZIO.scoped {
        Workspace
          .work("plugin-graph")
          .flatMap: repo =>
            val source =
              repo.resolve("plug").resolve("src").resolve("main").resolve("scala").resolve("demo").resolve("Mark.scala")
            val built = Projects(module("plug", Jvm)(version).pluginLibrary)
            Ref
              .make(0)
              .flatMap: fetches =>
                val compiled = for
                  _          <- write(source, "package demo\n\nobject Mark:\n  def token = \"from-graph\"\n")
                  _          <- hostFacts
                  graph      <- ZIO.fromEither(built).mapError(Gate.Build(_))
                  machine    <- Machine.load(repo).mapError(Gate.Tool(_))
                  jdk        <- GraphCompile.jdk(machine).mapError(Gate.Compile(_))
                  resolution <- ResolutionService.open(repo, graph).mapError(Gate.Lock(_))
                  compile = compileCell(repo, graph, jdk, machine.native).provideSomeLayer(ZLayer.succeed(resolution))
                  missed   <- compile
                  _        <- fetches.set(0)
                  hit      <- compile
                  resolved <- fetches.get
                  lock     <- resolution.current
                  tools    <- LockedTools.make(lock, graph)
                  module   <- graph.projects.collect { case item: Module => item } match
                    case item :: Nil => ZIO.succeed(item)
                    case _           => ZIO.fail(Gate.Module)
                  classpath <- ScalacClasspath
                    .of(
                      repo,
                      module,
                      Nil,
                      version,
                      Platform.Jvm,
                      lock,
                      BuildResolvers(graph.resolvers),
                      ClasspathScope.Compile,
                    )
                    .provideSomeLayer(ZLayer.succeed(tools))
                    .mapError(error => Gate.Compile(error))
                  tasty    <- ZIO.attemptBlocking(hasTasty(missed.classes)).orDie
                  analysis <- GraphCompile.analysisDirOf(missed.classes) match
                    case Some(path) => ZIO.succeed(path)
                    case None       => ZIO.fail(Gate.Cell)
                  loaded <- ZIO
                    .attemptBlocking(CompileReplay.recordedToolchain(analysis))
                    .mapError(_ => Gate.Disk(histos.workspace.IoFailure(analysis, histos.workspace.IoFault.Read)))
                  toolsJars <- loaded match
                    case Some(recorded) => ZIO.succeed(recorded)
                    case None           => ZIO.fail(Gate.Cell)
                  compilerIds <- identities(toolsJars.bridge :: toolsJars.compiler)
                  runtimeIds  <- identities(toolsJars.runtime)
                  staged      <- PluginBundle
                    .library(
                      PluginPublication.Rebuildable,
                      node,
                      hit.capture,
                      repo,
                      hit.classes,
                      Nil,
                      Nil,
                      Organization("com.example"),
                      histos.central.ArtifactId.trusted("plug"),
                      revision,
                    )
                    .mapError(Gate.Bundle(_))
                  upload = ZIO
                    .serviceWithZIO[Publisher](_.publish(artifactOf(staged, Nil)))
                    .mapError(PluginPublishFault.Publish(_))
                  packed <- counted(upload)((uploads, result) => readPublished(uploads, result))
                    .mapError(Gate.Bundle(_))
                yield
                  val uploads = packed.uploads
                  val entries = packed.entries
                  val payload = entries.collect:
                    case (name, bytes) if name.startsWith("META-INF/histos/rebuild/") && name.endsWith("Mark.scala") =>
                      new String(bytes, StandardCharsets.UTF_8)
                  val json = packed.text(PluginBundle.Descriptor)
                  assertTrue(
                    classpath.paths.nonEmpty,
                    missed.closure.length == classpath.paths.length,
                    missed.closure.forall(_.role == ClosureRole.Compile),
                    missed.compiler == compilerIds,
                    missed.runtime == runtimeIds,
                    hit.compiler == compilerIds,
                    hit.runtime == runtimeIds,
                    missed.compiler.nonEmpty,
                    missed.runtime.nonEmpty,
                    missed.compiler.forall(_.format == ArtifactFormat.Jar),
                    missed.runtime.forall(_.format == ArtifactFormat.Jar),
                    resolved == 0,
                    hit.changed == false,
                    hit.classesWritten.contains(0),
                    missed.sources.nonEmpty,
                    tasty,
                    uploads == 1,
                    entries.contains(PluginBundle.Descriptor),
                    payload.exists(_.contains("from-graph")),
                    entries.keys.exists(_.endsWith(".tasty")),
                    json.exists(text => compilerIds.forall(item => text.contains(item.digest.value))),
                    json.exists(text => runtimeIds.forall(item => text.contains(item.digest.value))),
                  )
                compiled.provideSome[zio.Scope](
                  Resolve.coursier >>> ZLayer.fromFunction(counting(fetches)),
                  ZLayer.succeed(ContentStore.in(repo.resolve("store"))),
                  RemoteCache.off,
                  histos.zinc.Compilers.live,
                  histos.zinc.CompileMemo.live,
                )
      }
    },
  ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(8.minutes)

  private final case class Shot(
      classes: Path,
      capture: CompilationCapture,
      closure: List[ClosureArtifact],
      compiler: List[ArtifactIdentity],
      runtime: List[ArtifactIdentity],
      sources: List[histos.plugin.SelectedInput],
      changed: Boolean,
      classesWritten: Option[Int],
  )

  private def compileCell(
      repo: Path,
      graph: ProjectGraph,
      jdk: histos.jdk.JdkRuntime,
      native: histos.native.NativeOrigin,
  ): ZIO[
    Resolve & ContentStore & RemoteCache & histos.zinc.Compilers & histos.zinc.CompileMemo & ResolutionService,
    Gate,
    Shot,
  ] =
    GraphCompile
      .strict(GraphCompile.resident(repo, graph, List(ProjectName("plug")), jdk, native, Some(Platform.Jvm)))
      .runCollect
      .map(_.collect { case histos.zinc.CompileFeed.Done(cell) => cell }.toList)
      .mapError(Gate.Compile(_))
      .flatMap:
        case cell :: Nil =>
          cell.capture match
            case CompilationCapture.Recorded(shot) =>
              ZIO.succeed(
                Shot(
                  cell.classes,
                  cell.capture,
                  shot.closure,
                  shot.compiler,
                  shot.runtime,
                  shot.sources,
                  cell.changed,
                  cell.classesWritten,
                )
              )
            case CompilationCapture.Unrecorded => ZIO.fail(Gate.Cell)
        case _ => ZIO.fail(Gate.Cell)

  private def identities(paths: List[Path]): ZIO[Any, Gate, List[ArtifactIdentity]] =
    ZIO.foreach(paths): path =>
      ZIO
        .attemptBlocking(ArtifactIdentity(ArtifactFormat.Jar, Digests.content(Files.readAllBytes(path))))
        .mapError(_ => Gate.Disk(histos.workspace.IoFailure(path, histos.workspace.IoFault.Read)))

  private def missing(part: CompilerPart): GraphCompileError =
    GraphCompileError.Failed(
      ProjectName("plug"),
      version,
      Platform.Jvm,
      CompileError.Toolchain(ToolchainMiss.Missing(version, part)),
    )

  private def counting(hits: Ref[Int])(inner: Resolve): Resolve =
    CountingResolve(inner, hits)

  private final class CountingResolve(inner: Resolve, hits: Ref[Int]) extends Resolve:
    def resolveTool(request: ToolRequest): zio.IO[ResolveError, Classpath] =
      inner.resolveTool(request)

    def resolve(cell: ResolveCell): zio.IO[ResolveError, Classpath] =
      inner.resolve(cell)

    def fetch(artifact: LockedArtifact): zio.IO[ResolveError, ResolvedArtifact] =
      hits.update(_ + 1) *> inner.fetch(artifact)

    def resolvePlugin(dependency: Dep, repositories: Repositories): zio.IO[ResolveError, Classpath] =
      inner.resolvePlugin(dependency, repositories)

    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository,
    ): zio.IO[ResolveError, ResolvedArtifact] =
      inner.sources(organization, name, revision, repository)
  end CountingResolve

  private enum Gate:
    case Build(error: BuildError)
    case Tool(error: histos.jdk.ToolchainError)
    case Compile(error: GraphCompileError)
    case Lock(error: histos.resolve.ResolveError)
    case Bundle(error: PluginPublishFault)
    case Disk(error: histos.workspace.IoFailure)
    case Host(name: String)
    case Cell
    case Module

  private final class Counting(inner: Publisher, hits: Ref[Int]) extends Publisher:
    def publish(artifact: Artifact): zio.IO[PublishError, Published] =
      hits.update(_ + 1) *> inner.publish(artifact)

  private final case class Packed(uploads: Int, entries: Map[String, Array[Byte]]):
    def text(name: String): Option[String] =
      entries.get(name).map(bytes => new String(bytes, StandardCharsets.UTF_8))

  private def publish(staged: StagedPlugin): ZIO[Any, PluginPublishFault, Packed] =
    val upload = ZIO
      .serviceWithZIO[Publisher](_.publish(artifactOf(staged, staged.dependencies)))
      .mapError(PluginPublishFault.Publish(_))
    counted(upload)((uploads, result) => readPublished(uploads, result))

  private def readPublished(
      uploads: Int,
      result: Either[PluginPublishFault, Published],
  ): ZIO[Any, PluginPublishFault, Packed] =
    result match
      case Left(fault)                             => ZIO.fail(fault)
      case Right(Published.Snapshot(_, _))         => ZIO.succeed(Packed(uploads, Map.empty))
      case Right(Published.Local(jar, _, _, _, _)) =>
        ZIO
          .attemptBlocking(Packed(uploads, unzip(jar)))
          .mapError(_ => PluginPublishFault.Io(histos.workspace.IoFailure(jar, histos.workspace.IoFault.Read)))

  /** The published jar is deleted with the workspace, so callers read it before `use` returns. */
  private def counted[A, B](
      effect: ZIO[Publisher, PluginPublishFault, A]
  )(
      use: (Int, Either[PluginPublishFault, A]) => ZIO[Any, PluginPublishFault, B]
  ): ZIO[Any, PluginPublishFault, B] =
    ZIO
      .scoped {
        Workspace
          .work("plugin-upload")
          .mapError(PluginPublishFault.Io(_))
          .flatMap: root =>
            for
              hits <- Ref.make(0)
              layer = Publisher.local(root) >>> ZLayer
                .fromFunction((inner: Publisher) => Counting(inner, hits): Publisher)
              result <- effect.either.provide(layer)
              count  <- hits.get
              used   <- use(count, result)
            yield used
      }

  private def hostFacts: ZIO[Any, Gate, Unit] =
    ZIO.foreachDiscard(List("user.home", "os.name", "os.arch", "java.home")): key =>
      Option(System.getProperty(key)) match
        case Some(value) if value.nonEmpty => TestSystem.putProperty(key, value)
        case _                             => ZIO.fail(Gate.Host(key))

  private def refuse(contract: StageContract, expected: RecipeError): ZIO[Any, PluginPublishFault, TestResult] =
    ZIO
      .fromEither(fixture)
      .flatMap: snapshot =>
        val capture = CompilationCapture.Recorded(snapshot.copy(stages = List(contract)))
        ZIO
          .fromEither(ask(PluginPublication.Rebuildable, capture, currentBlob, Nil))
          .flatMap: request =>
            counted(PluginBundle.publish(request)) { (uploads, result) =>
              ZIO.succeed(assertTrue(uploads == 0, result == Left(PluginPublishFault.Recipe(expected))))
            }

  private def stage(
      mode: PluginPublication,
      capture: CompilationCapture,
      current: List[SourceBytes],
      dependencies: List[histos.central.MavenDependency],
  ): Either[PluginPublishFault, StagedPlugin] =
    ask(mode, capture, current, dependencies).flatMap(PluginBundle.stage)

  private def ask(
      mode: PluginPublication,
      capture: CompilationCapture,
      current: List[SourceBytes],
      dependencies: List[histos.central.MavenDependency],
  ): Either[PluginPublishFault, PluginPublishRequest] =
    histos.central.ArtifactId.from("plug") match
      case Left(_)         => Left(PluginPublishFault.Dependency("com.example", "plug", "0.1.0"))
      case Right(artifact) =>
        Right(
          PluginPublishRequest(
            mode,
            node,
            capture,
            current,
            List(EntryBytes("pkg/A.class", Chunk.fromArray(ContractFixtures.blob))),
            None,
            dependencies,
            Organization("com.example"),
            artifact,
            revision,
          )
        )

  private def artifactOf(staged: StagedPlugin, dependencies: List[histos.central.MavenDependency]): Artifact =
    Artifact(
      Organization("com.example"),
      histos.central.ArtifactId.trusted("plug"),
      revision,
      staged.bytes,
      dependencies,
      histos.central.PomDocument(),
      None,
      None,
    )

  private def fixture: Either[PluginPublishFault, CompilationSnapshot] =
    ContractFixtures.snapshot(ContractFixtures.blob) match
      case Left(error)     => Left(PluginPublishFault.Seal(error))
      case Right(snapshot) =>
        Right(
          CompilationSnapshot(
            ProjectName("plug"),
            snapshot.version,
            snapshot.platform,
            snapshot.sources,
            snapshot.resources,
            snapshot.settings,
            snapshot.stages,
            snapshot.closure,
            snapshot.stageArtifacts,
            snapshot.host,
            snapshot.compiler,
            snapshot.runtime,
            snapshot.jdk,
            snapshot.outputs,
            snapshot.knownStages,
          )
        )

  private def captured(
      sources: List[histos.plugin.SelectedInput],
      closure: List[ClosureArtifact],
  ): Either[PluginPublishFault, CompilationCapture] =
    fixture.map: snapshot =>
      CompilationCapture.Recorded(snapshot.copy(sources = sources, closure = closure))

  private def currentBlob: List[SourceBytes] =
    List(SourceBytes(histos.plugin.RepoPath("src/A.scala"), Chunk.fromArray(ContractFixtures.blob)))

  private val stageId: RecipeNodeId = RecipeNodeId("stage0")

  private def hostKnown(name: EntryName): StageContract =
    contract(StageAssurance.HostKnown, List(name), Nil)

  private def hostKnownEmpty: StageContract =
    contract(StageAssurance.HostKnown, Nil, Nil)

  private def declared(required: List[StageBinding]): StageContract =
    contract(StageAssurance.ProducerDeclared, List(EntryName("demo.Stage")), required)

  private def contract(
      assurance: StageAssurance,
      entrypoints: List[EntryName],
      required: List[StageBinding],
  ): StageContract =
    StageContract(
      assurance,
      entrypoints,
      Nil,
      CompilerConstraint.unconstrained,
      required,
      StageInputs(Nil, Nil, Nil),
      StageRestrictions.v1,
    )

  private def unzip(path: Path): Map[String, Array[Byte]] =
    val zip = new ZipFile(path.toFile)
    try
      zip.entries.asScala
        .filterNot(_.isDirectory)
        .map: entry =>
          val stream = zip.getInputStream(entry)
          try entry.getName -> stream.readAllBytes()
          finally stream.close()
        .toMap
    finally zip.close()

  private def hasTasty(root: Path): Boolean =
    if !Files.isDirectory(root) then false
    else
      val walk = Files.walk(root)
      try
        walk.iterator.asScala.exists(path => Files.isRegularFile(path) && path.getFileName.toString.endsWith(".tasty"))
      finally walk.close()

  private def write(path: Path, text: String): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, text)
      ()
    }.orDie

  private def read(path: Path): ZIO[Any, histos.workspace.IoFailure, Array[Byte]] =
    ZIO
      .attemptBlocking(Option.when(Files.isRegularFile(path))(Files.readAllBytes(path)))
      .mapError(_ => histos.workspace.IoFailure(path, histos.workspace.IoFault.Read))
      .flatMap:
        case Some(bytes) => ZIO.succeed(bytes)
        case None        => ZIO.fail(histos.workspace.IoFailure(path, histos.workspace.IoFault.Stat))
end PluginPublicationSpec
