package histos.resolve

import histos.jdk.JdkRuntime
import histos.*
import histos.jdk.{JdkMajor, JdkVersion, Sha256}
import histos.toolchain.{CompilerIdentity, RuntimeIdentity}
import histos.zinc.{
  CompileKey,
  CompileRequest,
  CompileToolchain,

  ScalacFlags
}
import java.nio.file.Path
import histos.workspace.Workspace
import histos.native.{NativeClasspath, NativeOrigin, NativeToolchain}
import zio.{Chunk, IO, ZIO, ZLayer}
import zio.test.*

object SelectedToolsSpec extends ZIOSpecDefault:
  private val version = ScalaVersion("3.10.0")
  private val digest = Sha256.of(Chunk.fromArray(Array[Byte](1)))
  private val other = Sha256.of(Chunk.fromArray(Array[Byte](2)))
  private val jdk = JdkRuntime(
    Path.of("jdk"),
    JdkVersion.trusted("25.0.5", JdkMajor.trusted(25)),
    RuntimeIdentity.Local(Path.of("jdk"), digest)
  )
  private val request = CompileRequest(
    CompileToolchain(version, Platform.Js, jdk, ScalacFlags.empty),
    Nil,
    Nil,
    Path.of("classes"),
    Path.of("analysis"),
    Nil
  )

  private def held(request: ToolRequest): LockedTool = LockedTool(
    request.id,
    request.roots,
    request.repositories.all,
    request.roots.map(root =>
      LockedArtifact(
        root.organization,
        root.name,
        root.revision,
        digest,
        request.repositories.first
      )
    ),
    Nil
  )
  private val resolver = new Resolve:
    def resolveTool(request: ToolRequest): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.ToolDrift(request.id))
    def resolve(cell: ResolveCell): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.NoLock(Path.of("unexpected-solve")))
    def fetch(artifact: LockedArtifact): IO[ResolveError, ResolvedArtifact] =
      ZIO.succeed(
        ResolvedArtifact(
          artifact.organization,
          artifact.name,
          artifact.revision,
          Path.of(artifact.name),
          artifact.sha256,
          artifact.repository,
          artifact.classifier
        )
      )
    def resolvePlugin(
        dependency: Dep,
        repositories: Repositories
    ): IO[ResolveError, Classpath] =
      ZIO.fail(ResolveError.NoLock(Path.of("unexpected-plugin")))
    def sources(
        organization: String,
        name: String,
        revision: String,
        repository: Repository
    ): IO[ResolveError, ResolvedArtifact] =
      ZIO.fail(ResolveError.NotFound(organization, name, revision))

  def spec = suite("selected tool identities")(
    test("quick execution context preserves a failed Node selection"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-test-context-node")
          graph <- ZIO.fromEither(
            Projects(
              module("web", Js)(version).test(TestMain("dev.fixture.Check"))
            )
          )
          name <- ZIO.fromEither(ProjectName.from("web"))
          module <- ZIO
            .fromOption(graph.projects.collectFirst { case module: Module =>
              module
            })
            .orElseFail(histos.testrun.ModuleTestError.Absent(name))
          plans <- LockedTools.plan(graph, NativeOrigin.Central)
          view = LockedTools(
            DependencyLock(Nil, Nil, plans.map(held)),
            graph.tools,
            Repositories.central,
            resolver
          )
          runtimes <- histos.toolchain.Runtimes.read(
            TestMachine.at(root, Map.empty)
          )
          result <- histos.testrun.TestContext
            .make(
              module,
              histos.zinc
                .CompiledCell(name, version, Js, root.resolve("classes"), 0),
              jdk,
              runtimes
            )
            .provide(ZLayer.succeed(view))
            .either
        yield assertTrue(
          result == Left(
            histos.testrun.ModuleTestError.NoNode(
              name,
              version,
              histos.node.NodeMiss.Absent(histos.node.NodeToolchain.Floor)
            )
          )
        )
    ,
    test(
      "actual embedded compiler provenance includes supplied boundary bytes"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-embedded-boundary")
          support = root.resolve("support.jar")
          _ <- Workspace.writeString(support, "first boundary bytes")
          before <- histos.toolchain.BuildCompiler.identity(List(support))
          _ <- Workspace.writeString(support, "different boundary bytes")
          after <- histos.toolchain.BuildCompiler.identity(List(support))
        yield assertTrue(before != after)
    ,
    test(
      "Native planning and materialization keep application Scala separate from bridge Scala"
    ):
      ZIO.scoped:
        for
          graph <- ZIO.fromEither(
            Projects(
              module("one", Native)(ScalaVersion("3.10.0"))
                .clang(ClangMajor(21)),
              module("two", Native)(ScalaVersion("3.9.0"))
                .clang(ClangMajor(21))
                .nativeVersion(NativeVersion("0.5.11"))
            )
          )
          plans <- LockedTools.plan(graph, NativeOrigin.Central)
          view = LockedTools(
            DependencyLock(Nil, Nil, plans.map(held)),
            graph.tools,
            Repositories.central,
            resolver
          )
          toolchain <- ZIO.fromEither(
            NativeToolchain.of(
              graph.tools.native.revision,
              NativeOrigin.Central
            )
          )
          libraries <- NativeClasspath
            .fetch(toolchain, Some(ScalaVersion("3.10.0")))
            .provide(ZLayer.succeed(view))
          requests = plans.filter(_.id.role == ToolRole.NativeLibraries)
        yield assertTrue(
          requests.map(_.id).toSet == Set(
            ToolId(
              ToolRole.NativeLibraries,
              ScalaVersion("3.10.0"),
              ToolVariant.Native(NativeVersion("0.5.12"))
            ),
            ToolId(
              ToolRole.NativeLibraries,
              ScalaVersion("3.9.0"),
              ToolVariant.Native(NativeVersion("0.5.11"))
            )
          ),
          plans
            .filter(_.id.role == ToolRole.NativeLinker)
            .forall(_.id.scalaVersion == graph.tools.native.bridgeScala),
          libraries.input.id.scalaVersion == ScalaVersion("3.10.0"),
          libraries.input.artifacts
            .filter(artifact =>
              artifact.name.startsWith("scalalib_") || artifact.name.startsWith(
                "scala3lib_"
              )
            )
            .map(_.revision)
            .toSet == Set("3.10.0+0.5.12"),
          libraries.entries.forall(path => !path.toString.contains("compiler"))
        )
    ,
    test(
      "empty graph verify and check require complete verified compiler cells"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-empty-tool-graph")
          graph = ProjectGraph.trusted(Nil)
          planned <- LockedTools.plan(graph, NativeOrigin.Central)
          cells = planned.map(held)
          _ <- Workspace.writeString(
            Resolve.lockPath(root),
            DependencyLock.render(DependencyLock(Nil, Nil, cells))
          )
          goodVerify <- Resolve
            .verify(root, graph, Repositories.central)
            .provide(ZLayer.succeed(resolver))
          goodCheck <- Resolve
            .check(root, graph, Repositories.central)
            .provide(ZLayer.succeed(resolver))
          _ <- Workspace.writeString(
            Resolve.lockPath(root),
            DependencyLock.render(
              DependencyLock(
                Nil,
                Nil,
                List(
                  held(
                    histos.js.JsRequests.environment(
                      JsEnv.Node,
                      ToolCatalog.default,
                      Repositories.central
                    )
                  )
                )
              )
            )
          )
          missingVerify <- Resolve
            .verify(root, graph, Repositories.central)
            .provide(ZLayer.succeed(resolver))
            .either
          missingCheck <- Resolve
            .check(root, graph, Repositories.central)
            .provide(ZLayer.succeed(resolver))
            .either
          _ <- Workspace.writeString(
            Resolve.lockPath(root),
            DependencyLock.render(
              DependencyLock(Nil, Nil, cells.map(_.copy(artifacts = Nil)))
            )
          )
          incompleteVerify <- Resolve
            .verify(root, graph, Repositories.central)
            .provide(ZLayer.succeed(resolver))
            .either
          incompleteCheck <- Resolve
            .check(root, graph, Repositories.central)
            .provide(ZLayer.succeed(resolver))
            .either
        yield assertTrue(
          goodVerify.isEmpty,
          goodCheck.isEmpty,
          planned.headOption.exists(request =>
            missingVerify == Left(ResolveError.NoTool(request.id))
          ),
          missingVerify == missingCheck,
          incompleteVerify.left.toOption.exists {
            case ResolveError.ToolRoot(_, _, ToolRootFault.Missing) => true;
            case _                                                  => false
          },
          incompleteVerify == incompleteCheck
        )
    ,
    test(
      "captured tool views remain immutable after a complete file replacement"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-tool-view-generation")
          graph = ProjectGraph.trusted(Nil)
          compiler = histos.zinc.CompilerRequests
            .compiler(version, Repositories.central)
          before = DependencyLock(Nil, Nil, List(held(compiler)))
          _ <- Workspace.writeString(
            Resolve.lockPath(root),
            DependencyLock.render(before)
          )
          captured <- Resolve.readLock(root)
          view <- LockedTools
            .make(captured, graph)
            .provide(ZLayer.succeed(resolver))
          after = before.copy(tools =
            before.tools.map(cell =>
              cell.copy(artifacts = cell.artifacts.map(_.copy(sha256 = other)))
            )
          )
          _ <- Workspace.writeString(
            Resolve.lockPath(root),
            DependencyLock.render(after)
          )
          inFlight <- view.selected(compiler)
          nextLock <- Resolve.readLock(root)
          nextView <- LockedTools
            .make(nextLock, graph)
            .provide(ZLayer.succeed(resolver))
          next <- nextView.selected(compiler)
        yield assertTrue(
          inFlight.artifacts.forall(_.sha256 == digest),
          next.artifacts.forall(_.sha256 == other),
          inFlight.fingerprint != next.fingerprint
        )
    ,

    test(
      "coexisting platform revisions select distinct production compilation keys"
    ):
      val compiler =
        histos.zinc.CompilerRequests.compiler(version, Repositories.central)
      val first = histos.js.JsRequests.libraries(
        version,
        JsVersion("1.21.0"),
        Repositories.central,
        ToolCatalog.default
      )
      val second = histos.js.JsRequests.libraries(
        version,
        JsVersion("1.22.0"),
        Repositories.central,
        ToolCatalog.default
      )
      val view = LockedTools(
        DependencyLock(
          Nil,
          Nil,
          List(held(compiler), held(first), held(second))
        ),
        ToolCatalog.default,
        Repositories.central,
        resolver
      )
      for
        name <- ZIO.fromEither(ProjectName.from("web"))
        module = Module(
          name,
          Platforms(Js, Nil),
          ScalaVersions(version, Nil),
          Nil,
          Nil,
          SourcePos("fixture", 1, 1),
          jsVersion = Some(JsVersion("1.21.0"))
        )
        one <- view.compilation(module, version, Js, NativeOrigin.Central)
        two <- view.compilation(
          module.copy(jsVersion = Some(JsVersion("1.22.0"))),
          version,
          Js,
          NativeOrigin.Central
        )
      yield assertTrue(
        one.map(_.id).contains(first.id),
        !one.map(_.id).contains(second.id),
        two.map(_.id).contains(second.id),
        !two.map(_.id).contains(first.id),
        CompileKey.of(
          request.copy(compilationTools = one),
          digest
        ) != CompileKey.of(request.copy(compilationTools = two), digest)
      )
    ,
    test(
      "an unrelated environment does not change selected compilation inputs"
    ):
      val compiler =
        histos.zinc.CompilerRequests.compiler(version, Repositories.central)
      val library = histos.js.JsRequests.libraries(
        version,
        JsVersion("1.22.0"),
        Repositories.central,
        ToolCatalog.default
      )
      val environment = histos.js.JsRequests.environment(
        JsEnv.Firefox,
        ToolCatalog.default,
        Repositories.central
      )
      val lock = DependencyLock(
        Nil,
        Nil,
        List(held(compiler), held(library), held(environment))
      )
      val view =
        LockedTools(lock, ToolCatalog.default, Repositories.central, resolver)
      for
        name <- ZIO.fromEither(ProjectName.from("web"))
        module = Module(
          name,
          Platforms(Platform.Js, Nil),
          ScalaVersions(version, Nil),
          Nil,
          Nil,
          SourcePos("fixture", 1, 1)
        )
        before <- view.compilation(
          module,
          version,
          Platform.Js,
          histos.native.NativeOrigin.Central
        )
        after <- view
          .copy(lock =
            lock.copy(tools =
              lock.tools.map(tool =>
                if tool.id == environment.id then
                  tool.copy(artifacts =
                    tool.artifacts.map(_.copy(sha256 = other))
                  )
                else tool
              )
            )
          )
          .compilation(
            module,
            version,
            Platform.Js,
            histos.native.NativeOrigin.Central
          )
      yield assertTrue(before.map(_.fingerprint) == after.map(_.fingerprint))
    ,
    test(
      "missing, duplicated, wrong-organization, or classified root coverage is rejected"
    ):
      val planned =
        histos.zinc.CompilerRequests.compiler(version, Repositories.central)
      val valid = held(planned)
      val root = planned.roots.headOption
      val missing = LockedTools.validate(planned, valid.copy(artifacts = Nil))
      val wrong = LockedTools.validate(
        planned,
        valid.copy(artifacts =
          valid.artifacts.map(_.copy(organization = "wrong"))
        )
      )
      val classified = LockedTools.validate(
        planned,
        valid.copy(artifacts =
          valid.artifacts.map(_.copy(classifier = Some(JarClassifier("jdk8"))))
        )
      )
      assertTrue(
        root.exists(value =>
          missing == Left(
            ResolveError.ToolRoot(planned.id, value, ToolRootFault.Missing)
          )
        ),
        wrong == missing,
        classified == missing,
        root.exists(value =>
          LockedTools.validate(
            planned,
            valid.copy(artifacts = valid.artifacts ++ valid.artifacts)
          ) ==
            Left(
              ResolveError.ToolRoot(planned.id, value, ToolRootFault.Duplicated)
            )
        )
      )
    ,
    test(
      "source compiler cache identity includes actual compiler and runtime bytes"
    ):
      val one = CompilerIdentity(
        version,
        digest,
        RuntimeIdentity.Local(Path.of("jdk"), digest)
      )
      assertTrue(
        one.fingerprint != one.copy(embedded = other).fingerprint,
        one.fingerprint != one
          .copy(runtime = RuntimeIdentity.Local(Path.of("jdk"), other))
          .fingerprint
      )
  )
