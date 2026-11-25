package histos.publish

import histos.*
import histos.build.{BuildNode, BuildDagError}
import histos.central.{Artifact, ArtifactId, Publisher, Published, Revision}
import histos.clean.Products
import histos.plugin.*
import histos.preview.Plan
import histos.suite.HistosSpec
import histos.watch.SourceWatch
import histos.workflow.Affected
import histos.workspace.Workspace
import histos.zinc.{GeneratedLocation, GeneratedRootId, GeneratedRoots, StageRootError}
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile
import _root_.scala.jdk.CollectionConverters.*
import zio.*
import zio.test.*

object ProjectionSpec extends HistosSpec:
  private val version   = ScalaVersion("3.10.1")
  private val app       = ProjectName("app")
  private val generator = new HistosPlugin:
    val id                       = PluginId("projection")
    def contribute: Contribution = Contribution(
      List(
        DeclaredTask(
          TaskName("input"),
          Seal.Hermetic,
          Set.empty,
          List(DeclaredInput.Directory(RepoPath("templates")), DeclaredInput.File(RepoPath("config/input.txt"))),
          Nil,
          Nil,
        ),
        DeclaredTask(
          TaskName("source"),
          Seal.Hermetic,
          Set.empty,
          Nil,
          List(
            DeclaredOutput.File(OutputName("source"), RepoPath("templates/generated/Physical.scala")),
            DeclaredOutput.Directory(OutputName("assets"), RepoPath("templates/generated/assets")),
          ),
          List(
            TaskDep.Generate(TaskName("input")),
            TaskDep.EmitScoped(
              app,
              SourceSet.Main,
              ScopedRoot.jvmNative,
              EmittedContent.SourceFile(ArtifactPath("demo/Generated.scala")),
              OutputName("source"),
            ),
            TaskDep.EmitScoped(
              app,
              SourceSet.Test,
              PlatformSelection.All,
              EmittedContent.ResourceTree,
              OutputName("assets"),
            ),
          ),
        ),
      ),
      Nil,
      Nil,
    )
    def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      ZIO.fail(PluginRunError.MissingDestination(OutputName("source")))

  private val task = PluginTaskId(generator.id, TaskName("source"))
  private def node(platform: Platform = Platform.Jvm, scope: SourceSet = SourceSet.Main): BuildNode =
    BuildNode.Compile(CompilationStage(Cell(app, version, platform), scope))

  def spec = suite("stage projections")(
    test("source jar includes extra and shared roots, excludes Test and other platforms") {
      ZIO.scoped {
        for
          repo  <- Workspace.work("histos-publish-projection")
          graph <- ZIO.fromEither(
            Projects(
              module("app", Jvm, Js)(version)
                .sources(ProjectPath("extra"))
                .roots(
                  ScopedRoot(RepoPath("shared"), SourceSet.Main, PlatformSelection.All, RootKind.Sources)
                )
            )
          )
          _   <- Workspace.writeString(repo.resolve("app/src/Main.scala"), "object Main")
          _   <- Workspace.writeString(repo.resolve("app/src/scalajvmnative/Union.scala"), "object Union")
          _   <- Workspace.writeString(repo.resolve("app/src/scalajs/Client.scala"), "object Client")
          _   <- Workspace.writeString(repo.resolve("app/src/test/Fixture.scala"), "object Fixture")
          _   <- Workspace.writeString(repo.resolve("extra/Extra.scala"), "object Extra")
          _   <- Workspace.writeString(repo.resolve("shared/pkg/Shared.scala"), "package pkg\nobject Shared")
          jar <- LocalPublish.sourceJar(
            repo,
            graph,
            Cell(app, version, Platform.Jvm),
            GeneratedRoots.Existing,
            repo.resolve("sources.jar"),
          )
          names <- entries(jar)
          js    <- LocalPublish.sourceJar(
            repo,
            graph,
            Cell(app, version, Platform.Js),
            GeneratedRoots.Existing,
            repo.resolve("js-sources.jar"),
          )
          jsNames <- entries(js)
        yield assertTrue(
          names == Set("Main.scala", "Union.scala", "Extra.scala", "pkg/Shared.scala"),
          jsNames == Set("Main.scala", "Client.scala", "Extra.scala", "pkg/Shared.scala"),
        )
      }
    },
    test("a shared root selects every owning module and its typed dependent") {
      for
        one = module("one", Jvm)(version).sources(ProjectPath("shared"))
        two = module("two", Jvm)(version).sources(ProjectPath("shared"))
        graph <- ZIO.fromEither(Projects(one, two, module("three", Jvm)(version).testDependsOn(two)))
      yield assertTrue(
        Affected.project(graph, List("shared/Shared.scala")) ==
          Right(Affected.Selection.Modules(List(ProjectName("one"), ProjectName("two"), ProjectName("three"))))
      )
    },
    test("source jar reads a leased File with its explicit artifact entry, never its siblings or Test resources") {
      ZIO.scoped {
        for
          repo <- Workspace.work("histos-generated-source-jar")
          base <- ZIO.fromEither(Projects(module("app", Jvm, Js)(version)))
          graph    = base.withPlugins(List(RegisteredPlugin(generator.id, generator.contribute)(generator)))
          physical = repo.resolve(".histos/views/physical.scala")
          _ <- Workspace.writeString(physical, "package demo\nobject Generated")
          _ <- Workspace.writeString(repo.resolve(".histos/views/Sibling.scala"), "object Sibling")
          _ <- Workspace.writeString(repo.resolve("app/src/Main.scala"), "object Main")
          _ <- Workspace.createDirectories(repo.resolve("templates/generated"))
          _ <- ZIO.attemptBlocking(
            Files.createSymbolicLink(repo.resolve("templates/generated/Physical.scala"), physical)
          )
          views = GeneratedRoots.Provided(
            List(GeneratedLocation(GeneratedRootId(task, OutputName("source")), OutputKind.File, physical))
          )
          jar <- LocalPublish.sourceJar(
            repo,
            graph,
            Cell(app, version, Platform.Jvm),
            views,
            repo.resolve("sources.jar"),
          )
          names   <- entries(jar)
          body    <- entry(jar, "demo/Generated.scala")
          missing <- LocalPublish
            .sourceJar(
              repo,
              graph,
              Cell(app, version, Platform.Jvm),
              GeneratedRoots.Provided(Nil),
              repo.resolve("missing.jar"),
            )
            .either
          js      <- LocalPublish.sourceJar(repo, graph, Cell(app, version, Platform.Js), views, repo.resolve("js.jar"))
          jsNames <- entries(js)
        yield assertTrue(
          names == Set("Main.scala", "demo/Generated.scala"),
          body == "package demo\nobject Generated",
          missing == Left(StageRootError.MissingGenerated(GeneratedRootId(task, OutputName("source")))),
          jsNames == Set("Main.scala"),
        )
      }
    },
    test("actual local POM includes Main edges and libraries, never Test fixture edges or libraries") {
      ZIO.scoped {
        for
          repo <- Workspace.work("histos-projection-pom")
          main    = module("main", Jvm)(version)
          fixture = module("fixture", Jvm)(version)
          graph <- ZIO.fromEither(
            Projects(
              main,
              fixture,
              module("app", Jvm)(version)
                .dependsOn(main)
                .testDependsOn(fixture)
                .libraryDependencies(dep"com.example:main-library:1.0.0")
                .testDependencies(dep"com.example:test-library:1.0.0"),
            )
          )
          revision = Revision.Release(histos.version.ReleaseVersion(histos.version.ShipNumber.trusted(1, 0, 0)))
          deps <- ZIO.fromEither(
            LocalPublish.dependencies(
              graph,
              Organization("com.example"),
              app,
              version,
              Platform.Jvm,
              revision,
              List(ProjectName("main") -> revision),
            )
          )
          _   <- Workspace.writeString(repo.resolve("app/src/Main.scala"), "object Main")
          jar <- LocalPublish.sourceJar(
            repo,
            graph,
            Cell(app, version, Platform.Jvm),
            GeneratedRoots.Existing,
            repo.resolve("source.jar"),
          )
          bytes     <- ZIO.attemptBlocking(Chunk.fromArray(Files.readAllBytes(jar)))
          artifact  <- ZIO.fromEither(ArtifactId.from("app_3"))
          published <- ZIO
            .serviceWithZIO[Publisher](
              _.publish(Artifact(Organization("com.example"), artifact, revision, bytes, deps, sources = Some(bytes)))
            )
            .provideLayer(Publisher.local(repo.resolve("maven")))
          pom <- published match
            case Published.Local(_, path, _, _, _) => Workspace.readString(path)
            case Published.Snapshot(_, _)          => ZIO.fail(histos.central.PublishError.EmptyJar)
        yield assertTrue(
          pom.contains("<artifactId>main_3</artifactId>"),
          pom.contains("<artifactId>main-library</artifactId>"),
          !pom.contains("fixture"),
          !pom.contains("test-library"),
        )
      }
    },
    test("producer inputs wake consumers, File extents exclude siblings and generated subtrees do not self-trigger") {
      ZIO.scoped {
        for
          repo <- Workspace.work("histos-producer-watch")
          base <- ZIO.fromEither(Projects(module("app", Jvm)(version)))
          graph = base.withPlugins(List(RegisteredPlugin(generator.id, generator.contribute)(generator)))
          _          <- Workspace.writeString(repo.resolve("app/src/Main.scala"), "object Main")
          _          <- Workspace.writeString(repo.resolve("templates/source.txt"), "old")
          _          <- Workspace.writeString(repo.resolve("config/input.txt"), "old")
          _          <- Workspace.writeString(repo.resolve("config/sibling.txt"), "old")
          projection <- ZIO.fromEither(SourceWatch.project(repo, graph, List(node())))
          before     <- SourceWatch.take(projection)
          _          <- Workspace.writeString(repo.resolve("templates/generated/Physical.scala"), "generated")
          _          <- Workspace.writeString(repo.resolve("templates/generated/assets/new/deep.js"), "generated")
          _          <- Workspace.writeString(repo.resolve("config/sibling.txt"), "new")
          unchanged  <- SourceWatch.take(projection)
          _          <- Workspace.writeString(repo.resolve("templates/source.txt"), "new")
          changed    <- SourceWatch.take(projection)
          waiting    <- SourceWatch.since(projection, before).forkScoped
          _          <- TestClock.adjust(SourceWatch.Poll + SourceWatch.Settle)
          woke       <- waiting.join
          affected = Affected.project(graph, List("templates/source.txt"))
          outputs  = Affected.project(graph, List("templates/generated/assets/new/deep.js"))
        yield assertTrue(
          before == unchanged,
          changed != before,
          woke == List(repo.resolve("templates/source.txt")),
          affected == Right(Affected.Selection.Modules(List(app))),
          outputs == Right(Affected.Selection.Modules(Nil)),
          projection.inputs.contains(SourceWatch.Root(repo.resolve("config/input.txt"), OutputKind.File)),
        )
      }
    },
    test("producer Module material inputs include their shared Main dependency closure") {
      ZIO.scoped {
        for
          repo <- Workspace.work("histos-module-input-watch")
          library = module("library", Jvm)(version).sources(ProjectPath("shared-input"))
          base <- ZIO.fromEither(
            Projects(module("app", Jvm)(version), module("host", Jvm)(version).dependsOn(library), library)
          )
          contribution = generator.contribute.copy(tasks =
            generator.contribute.tasks.map(task =>
              if task.name == TaskName("input") then
                task.copy(inputs =
                  task.inputs :+
                    DeclaredInput.Module(ProjectName("host"), None, Some(Platform.Jvm))
                )
              else task
            )
          )
          graph = base.withPlugins(List(RegisteredPlugin(generator.id, contribution)(generator)))
          _          <- Workspace.writeString(repo.resolve("shared-input/Helper.scala"), "object Helper")
          projection <- ZIO.fromEither(SourceWatch.project(repo, graph, List(node())))
          before     <- SourceWatch.take(projection)
          _          <- Workspace.writeString(repo.resolve("shared-input/Helper.scala"), "object Changed")
          after      <- SourceWatch.take(projection)
        yield assertTrue(
          before != after,
          projection.inputs.exists(_.contains(repo.resolve("shared-input/Helper.scala"))),
          Affected.project(graph, List("shared-input/Helper.scala")) ==
            Right(Affected.Selection.Modules(List(app, ProjectName("host"), ProjectName("library")))),
        )
      }
    },
    test("preview carries exact resources and producer closure using the supplied physical generation") {
      ZIO.scoped {
        for
          repo <- Workspace.work("histos-projection-preview")
          base <- ZIO.fromEither(
            Projects(
              module("app", Jvm)(version).roots(
                ScopedRoot(RepoPath("resources"), SourceSet.Main, PlatformSelection.All, RootKind.Resources)
              )
            )
          )
          graph    = base.withPlugins(List(RegisteredPlugin(generator.id, generator.contribute)(generator)))
          physical = repo.resolve(".histos/views/source.scala")
          _ <- Workspace.writeString(physical, "object Generated")
          _ <- Workspace.writeString(repo.resolve("resources/apps/browser.js"), "client")
          views = GeneratedRoots.Provided(
            List(GeneratedLocation(GeneratedRootId(task, OutputName("source")), OutputKind.File, physical))
          )
          projection <- Plan.project(repo, graph, List(node()), views)
          compiled   <- Plan.compile(repo, graph, List(app), views)
          invalid    <- Plan.project(repo, graph, List(node()), GeneratedRoots.Provided(Nil)).either
        yield assertTrue(
          projection.plan.nodes.contains(BuildNode.Generate(task)),
          projection.plan.nodes.contains(BuildNode.Generate(PluginTaskId(generator.id, TaskName("input")))),
          projection.stages.exists(
            _.files.resources == List(ResourceName("apps/browser.js") -> repo.resolve("resources/apps/browser.js"))
          ),
          projection.stages.exists(_.files.sources.exists(_.path == physical)),
          compiled.units.exists {
            case unit: histos.preview.PlanUnit.Chosen   => unit.sources.contains(physical)
            case histos.preview.PlanUnit.NotSelected(_) => false
          },
          invalid == Left(StageRootError.MissingGenerated(GeneratedRootId(task, OutputName("source")))),
        )
      }
    },
    test("clean projection retains File versus Directory ownership, never their parent") {
      for
        base <- ZIO.fromEither(Projects(module("app", Jvm)(version)))
        graph = base.withPlugins(List(RegisteredPlugin(generator.id, generator.contribute)(generator)))
        repo  = Path.of("/tmp/projection-clean")
        projection <- ZIO.fromEither(Products.project(repo, graph, List(app)))
      yield assertTrue(
        projection.roots.exists(root =>
          root.path == repo.resolve("templates/generated/Physical.scala") && root.extent == OutputKind.File
        ),
        projection.roots.exists(root =>
          root.path == repo.resolve("templates/generated/assets") && root.extent == OutputKind.Directory
        ),
        !projection.roots.exists(_.path == repo.resolve("templates/generated")),
      )
    },
    test("invalid stage selection retains the typed DAG error") {
      for graph <- ZIO.fromEither(Projects(module("app", Jvm)(version)))
      yield assertTrue(
        SourceWatch.project(Path.of("."), graph, List(node(Platform.Js))) == Left(
          BuildDagError.UnknownNode(node(Platform.Js))
        )
      )
    },
  )

  private def entries(path: Path) = ZIO.acquireReleaseWith(ZIO.attemptBlocking(new ZipFile(path.toFile)))(zip =>
    ZIO.attemptBlocking(zip.close()).orDie
  )(zip => ZIO.attemptBlocking(zip.entries().asScala.map(_.getName).toSet))

  private def entry(path: Path, name: String) = ZIO.acquireReleaseWith(ZIO.attemptBlocking(new ZipFile(path.toFile)))(
    zip => ZIO.attemptBlocking(zip.close()).orDie
  )(zip =>
    ZIO
      .fromOption(Option(zip.getEntry(name)))
      .flatMap(found =>
        ZIO.acquireReleaseWith(ZIO.attemptBlocking(zip.getInputStream(found)))(stream =>
          ZIO.attemptBlocking(stream.close()).orDie
        )(stream => ZIO.attemptBlocking(new String(stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)))
      )
  )
end ProjectionSpec
