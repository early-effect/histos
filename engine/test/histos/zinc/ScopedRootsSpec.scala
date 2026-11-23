package histos.zinc

import histos.workspace.Workspace
import histos.*
import histos.pack.AssetSync
import histos.plugin.*

import java.nio.file.Files

import zio.ZIO
import zio.test.*

object ScopedRootsSpec extends ZIOSpecDefault:
  private val version    = ScalaVersion("3.10.1")
  private val moduleName = ProjectName("app")

  private val emitted = new HistosPlugin:
    val id                       = PluginId("scoped-roots")
    def contribute: Contribution = Contribution(
      List(
        DeclaredTask(
          TaskName("files"),
          Seal.Hermetic,
          Set.empty,
          Nil,
          List(
            DeclaredOutput.File(OutputName("source"), RepoPath("target/mixed/Arbitrary.scala")),
            DeclaredOutput.File(OutputName("resource"), RepoPath("target/mixed/arbitrary.js")),
          ),
          List(
            TaskDep.EmitScoped(
              moduleName,
              SourceSet.Main,
              ScopedRoot.jvmNative,
              EmittedContent.SourceFile(ArtifactPath("demo/Generated.scala")),
              OutputName("source"),
            ),
            TaskDep.EmitScoped(
              moduleName,
              SourceSet.Test,
              PlatformSelection.All,
              EmittedContent.ResourceFile(ResourceName("apps-browser/counter-view.js")),
              OutputName("resource"),
            ),
          ),
        )
      ),
      Nil,
      Nil,
    )
    def run(request: RunRequest): ZIO[zio.Scope & PluginScopes, PluginRunError, PluginOutput] =
      ZIO.succeed(PluginOutput(request.id.render, Nil, Nil))

  private def stage(platform: Platform, scope: SourceSet): CompilationStage =
    CompilationStage(Cell(moduleName, version, platform), scope)

  def spec = suite("conventional shared platform roots")(
    test("scalajvmnative selects one physical source on JVM and Native, never JS") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-shared-platform-roots")
          shared = root.resolve("scalajvmnative/dev/earlyeffect/heddle/Shared.scala")
          _      <- Workspace.writeString(shared, "package dev.earlyeffect.heddle\nobject Shared\n")
          jvm    <- Sources.select(root, ScalaVersion("3.10.1"), Platform.Jvm)
          native <- Sources.select(root, ScalaVersion("3.10.1"), Platform.Native)
          js     <- Sources.select(root, ScalaVersion("3.10.1"), Platform.Js)
          jar    <- Sources.packaged(root, ScalaVersion("3.10.1"), Platform.Jvm)
        yield assertTrue(
          jvm == List(shared),
          native == List(shared),
          js.isEmpty,
          jar == List(PackagedSource("dev/earlyeffect/heddle/Shared.scala", shared)),
        )
      }
    },
    test("platform names after a package or language root stay in package paths") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-platform-package-paths")
          entries = List(
            "src/main/scala/scalajvmnative/Shared.scala" -> "scalajvmnative/Shared.scala",
            "src/main/scala/jvm/Jvm.scala"               -> "jvm/Jvm.scala",
            "dev/scalajvmnative/Shared.scala"            -> "dev/scalajvmnative/Shared.scala",
            "scala/scalajvmnative/Shared.scala"          -> "scala/scalajvmnative/Shared.scala",
          )
          _ <- ZIO.foreachDiscard(entries) { (path, _) =>
            Workspace.writeString(root.resolve(path), "object Shared\n")
          }
          results <- ZIO.foreach(Platform.values.toList) { platform =>
            for
              selected <- Sources.select(root, ScalaVersion("3.10.1"), platform)
              packaged <- Sources.packaged(root, ScalaVersion("3.10.1"), platform)
            yield assertTrue(
              selected.toSet == entries.map((path, _) => root.resolve(path)).toSet,
              packaged.toSet == entries.map((path, entry) => PackagedSource(entry, root.resolve(path))).toSet,
            )
          }
        yield results.foldLeft(assertTrue(true))(_ && _)
      }
    },
    test("shared platform unions intersect Scala filters and preserve main/test separation") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-platform-filter-intersection")
          paths = List(
            "jvm/Jvm.scala",
            "scalajvm/JvmAlias.scala",
            "js/Js.scala",
            "scalajs/JsAlias.scala",
            "native/Native.scala",
            "scalanative/NativeAlias.scala",
            "scalajvmnative/scala-3/Series.scala",
            "scalajvmnative/scala-3.10.1/Exact.scala",
            "scalajvmnative/scala-2.13/Other.scala",
            "scalajvmnative/scala-3.9.0/OtherExact.scala",
            "scalajvmnative/Top.scala",
            "scalajvmnative/Java.java",
            "scalajvmnative/test/SharedSpec.scala",
          )
          _           <- ZIO.foreachDiscard(paths)(path => Workspace.writeString(root.resolve(path), ""))
          jvm         <- Sources.select(root, ScalaVersion("3.10.1"), Platform.Jvm)
          native      <- Sources.select(root, ScalaVersion("3.10.1"), Platform.Native)
          js          <- Sources.select(root, ScalaVersion("3.10.1"), Platform.Js)
          jvmTests    <- Sources.tests(root, ScalaVersion("3.10.1"), Platform.Jvm)
          nativeTests <- Sources.tests(root, ScalaVersion("3.10.1"), Platform.Native)
          jsTests     <- Sources.tests(root, ScalaVersion("3.10.1"), Platform.Js)
          packaged    <- Sources.packaged(root, ScalaVersion("3.10.1"), Platform.Native)
          shared = Set(
            root.resolve("scalajvmnative/scala-3/Series.scala"),
            root.resolve("scalajvmnative/scala-3.10.1/Exact.scala"),
            root.resolve("scalajvmnative/Top.scala"),
          )
          tests = List(root.resolve("scalajvmnative/test/SharedSpec.scala"))
        yield assertTrue(
          jvm.toSet == shared ++ Set(
            root.resolve("jvm/Jvm.scala"),
            root.resolve("scalajvm/JvmAlias.scala"),
            root.resolve("scalajvmnative/Java.java"),
          ),
          native.toSet == shared ++ Set(
            root.resolve("native/Native.scala"),
            root.resolve("scalanative/NativeAlias.scala"),
          ),
          js.toSet == Set(root.resolve("js/Js.scala"), root.resolve("scalajs/JsAlias.scala")),
          jvmTests == tests,
          nativeTests == tests,
          jsTests.isEmpty,
          packaged.map(_.entry).toSet == Set(
            "Native.scala",
            "NativeAlias.scala",
            "Series.scala",
            "Exact.scala",
            "Top.scala",
          ),
        )
      }
    },
    test("scoped exact roots and emitted Files preserve logical entries without reading siblings") {
      ZIO.scoped {
        for
          repo  <- Workspace.work("histos-exact-stage-roots")
          graph <- ZIO.fromEither(
            Projects(
              module("app", Jvm, Js, Native)(version)
                .clang(ClangMajor(21))
                .roots(
                  ScopedRoot(RepoPath("shared/main"), SourceSet.Main, ScopedRoot.jvmNative, RootKind.Sources),
                  ScopedRoot(RepoPath("shared/test"), SourceSet.Test, PlatformSelection.All, RootKind.Sources),
                  ScopedRoot(RepoPath("assets/main"), SourceSet.Main, PlatformSelection.All, RootKind.Resources),
                )
            )
          )
          linked = graph.withPlugins(List(RegisteredPlugin(emitted.id, emitted.contribute)(emitted)))
          roots  = StageRoots.of(linked)
          _ <- Workspace.writeString(repo.resolve("shared/main/jvm/Package.scala"), "package jvm\nobject Package\n")
          _ <- Workspace.writeString(repo.resolve("shared/test/demo/Fixture.scala"), "package demo\nobject Fixture\n")
          _ <- Workspace.writeString(repo.resolve("assets/main/main.txt"), "main")
          _ <- Workspace.writeString(repo.resolve("target/mixed/Arbitrary.scala"), "package demo\nobject Generated\n")
          _ <- Workspace.writeString(repo.resolve("target/mixed/Sibling.scala"), "object MustNotCompile\n")
          _ <- Workspace.writeString(repo.resolve("target/mixed/arbitrary.js"), "counter")
          _ <- Workspace.writeString(repo.resolve("target/mixed/sibling.js"), "must not copy")
          main   <- roots.read(repo, stage(Platform.Jvm, SourceSet.Main))
          native <- roots.read(repo, stage(Platform.Native, SourceSet.Main))
          js     <- roots.read(repo, stage(Platform.Js, SourceSet.Main))
          tests  <- roots.read(repo, stage(Platform.Js, SourceSet.Test))
          classes = repo.resolve("classes")
          _       <- AssetSync.sync(classes, None, tests.resources, tests.assets, repo)
          text    <- Workspace.readString(classes.resolve("apps-browser/counter-view.js"))
          sibling <- ZIO.attemptBlocking(Files.exists(classes.resolve("sibling.js")))
        yield assertTrue(
          main.sources.map(_.entry).toSet == Set("jvm/Package.scala", "demo/Generated.scala"),
          native.sources == main.sources,
          js.sources.isEmpty,
          tests.sources.map(_.entry) == List("demo/Fixture.scala"),
          tests.resources == List(
            ResourceName("apps-browser/counter-view.js") -> repo.resolve("target/mixed/arbitrary.js")
          ),
          main.resources.map(_._1.value) == List("main.txt"),
          text == "counter",
          !sibling,
          roots
            .inventory(stage(Platform.Jvm, SourceSet.Main))
            .exists(_.exists {
              case StageRoot.SourceFile(path, entry, _) =>
                path == RepoPath("target/mixed/Arbitrary.scala") && entry == ArtifactPath("demo/Generated.scala")
              case StageRoot.Sources(_, _, _) | StageRoot.Resources(_, _, _) | StageRoot.ResourceFile(_, _, _) => false
            }),
        )
      }
    },
    test("generated trees read leased physical views without accepting publication or embedded links") {
      ZIO.scoped {
        for
          repo  <- Workspace.work("histos-generated-tree-view")
          graph <- ZIO.fromEither(Projects(module("app", Jvm, Js)(version)))
          contribution = Contribution(
            List(
              DeclaredTask(
                TaskName("trees"),
                Seal.Hermetic,
                Set.empty,
                Nil,
                List(
                  DeclaredOutput.Directory(OutputName("source"), RepoPath("target/source-tree")),
                  DeclaredOutput.Directory(OutputName("resource"), RepoPath("target/resource-tree")),
                ),
                List(
                  TaskDep.EmitScoped(
                    moduleName,
                    SourceSet.Main,
                    PlatformSelection.All,
                    EmittedContent.SourceTree,
                    OutputName("source"),
                  ),
                  TaskDep.EmitScoped(
                    moduleName,
                    SourceSet.Test,
                    PlatformSelection.All,
                    EmittedContent.ResourceTree,
                    OutputName("resource"),
                  ),
                ),
              )
            ),
            Nil,
            Nil,
          )
          linked    = graph.withPlugins(List(RegisteredPlugin(emitted.id, contribution)(emitted)))
          task      = PluginTaskId(emitted.id, TaskName("trees"))
          sources   = repo.resolve(".histos/views/sources")
          resources = repo.resolve(".histos/views/resources")
          _ <- Workspace.writeString(sources.resolve("jvm/Generated.scala"), "package jvm\nobject Generated\n")
          _ <- Workspace.writeString(resources.resolve("apps-browser/view.js"), "view")
          _ <- Workspace.createDirectories(repo.resolve("target"))
          _ <- ZIO
            .attemptBlocking(Files.createSymbolicLink(repo.resolve("target/source-tree"), sources))
            .mapError(_ => histos.workspace.IoFailure(sources, histos.workspace.IoFault.Write))
          _ <- ZIO
            .attemptBlocking(Files.createSymbolicLink(repo.resolve("target/resource-tree"), resources))
            .mapError(_ => histos.workspace.IoFailure(resources, histos.workspace.IoFault.Write))
          views = GeneratedRoots.Provided(
            List(
              GeneratedLocation(GeneratedRootId(task, OutputName("source")), OutputKind.Directory, sources),
              GeneratedLocation(GeneratedRootId(task, OutputName("resource")), OutputKind.Directory, resources),
            )
          )
          roots = StageRoots.of(linked)
          main    <- roots.read(repo, stage(Platform.Js, SourceSet.Main), views)
          tests   <- roots.read(repo, stage(Platform.Jvm, SourceSet.Test), views)
          alias   <- roots.read(repo, stage(Platform.Js, SourceSet.Main)).either
          missing <- roots.read(repo, stage(Platform.Js, SourceSet.Main), GeneratedRoots.Provided(Nil)).either
          _       <- ZIO
            .attemptBlocking(
              Files.createSymbolicLink(sources.resolve("Escape.scala"), repo.resolve("target/source-tree"))
            )
            .mapError(_ => histos.workspace.IoFailure(sources, histos.workspace.IoFault.Write))
          embedded <- roots.read(repo, stage(Platform.Js, SourceSet.Main), views).either
        yield assertTrue(
          main.sources == List(PackagedSource("jvm/Generated.scala", sources.resolve("jvm/Generated.scala"))),
          main.resources.isEmpty,
          tests.sources.isEmpty,
          tests.resources == List(ResourceName("apps-browser/view.js") -> resources.resolve("apps-browser/view.js")),
          main.generatedPaths == Set(sources.resolve("jvm/Generated.scala")),
          alias.left.exists { case StageRootError.Link(_) => true; case _ => false },
          missing == Left(StageRootError.MissingGenerated(GeneratedRootId(task, OutputName("source")))),
          embedded.left.exists { case StageRootError.Link(_) => true; case _ => false },
        )
      }
    },
    test("resource names collide across distinct exact roots instead of silently overwriting") {
      ZIO.scoped {
        for
          repo  <- Workspace.work("histos-stage-resource-collision")
          graph <- ZIO.fromEither(
            Projects(
              module("app", Jvm)(version).roots(
                ScopedRoot(RepoPath("one"), SourceSet.Main, PlatformSelection.All, RootKind.Resources),
                ScopedRoot(RepoPath("two"), SourceSet.Main, PlatformSelection.All, RootKind.Resources),
              )
            )
          )
          _      <- Workspace.writeString(repo.resolve("one/shared.txt"), "one")
          _      <- Workspace.writeString(repo.resolve("two/shared.txt"), "two")
          result <- StageRoots.of(graph).read(repo, stage(Platform.Jvm, SourceSet.Main)).either
        yield assertTrue(result.left.exists {
          case StageRootError.ResourceCollision(name, paths) =>
            name == ResourceName("shared.txt") && paths.toSet == Set(
              repo.resolve("one/shared.txt"),
              repo.resolve("two/shared.txt"),
            )
          case _ => false
        })
      }
    },
  )
end ScopedRootsSpec
