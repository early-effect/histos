package histos.clean

import histos.*
import histos.cache.ContentStore
import histos.load.{PluginProducts, ProductError, ProductRequest}
import histos.plugin.*
import histos.suite.HistosSpec
import histos.workspace.Workspace
import java.nio.file.Files
import zio.*
import zio.test.*

object GeneratedCleanSpec extends HistosSpec:
  private val declared = DeclaredTask(
    TaskName("generate"),
    Seal.Hermetic,
    Set.empty,
    Nil,
    List(
      DeclaredOutput.File(OutputName("source"), RepoPath("mixed/Generated.scala")),
      DeclaredOutput.Directory(OutputName("assets"), RepoPath("mixed/assets")),
    ),
    List(
      TaskDep.EmitScoped(
        ProjectName("app"),
        SourceSet.Main,
        PlatformSelection.All,
        EmittedContent.SourceFile(ArtifactPath("Generated.scala")),
        OutputName("source"),
      ),
      TaskDep.EmitScoped(
        ProjectName("app"),
        SourceSet.Main,
        PlatformSelection.All,
        EmittedContent.ResourceTree,
        OutputName("assets"),
      ),
    ),
  )
  private val plugin = new HistosPlugin:
    val id         = PluginId("clean-projection")
    def contribute = Contribution(List(declared), Nil, Nil)
    def run(request: RunRequest): ZIO[Scope & PluginScopes, PluginRunError, PluginOutput] =
      ZIO.fail(PluginRunError.MissingDestination(OutputName("source")))
  private val task = PluginTaskId(plugin.id, declared.name)

  def spec = suite("generated clean projection")(
    test("active read leases reject clean before ordinary products are erased; File siblings survive retirement") {
      ZIO.scoped {
        for
          repo <- Workspace.work("histos-clean-generations")
          base <- ZIO.fromEither(Projects(module("app", Jvm)(ScalaVersion("3.10.1"))))
          graph = base.withPlugins(List(RegisteredPlugin(plugin.id, plugin.contribute)(plugin)))
          products <- PluginProducts.make()
          request = ProductRequest(repo, declared, InvocationId.derive("clean-projection"))
          cleaner = GeneratedCleaner[Any, ProductError](id =>
            ZIO.fail(ProductError.InvalidManifest).unless(id == task) *> products.clean(request)
          )
          _         <- Workspace.writeString(repo.resolve("mixed/User.scala"), "object User")
          _         <- Workspace.writeString(repo.resolve(".histos/classes/app/keep.class"), "prior compile")
          committed <- ZIO
            .scoped {
              for
                staged   <- products.stage(request)
                source   <- ZIO.fromEither(staged.destinations.file(OutputName("source")))
                assets   <- ZIO.fromEither(staged.destinations.directory(OutputName("assets")))
                _        <- Workspace.writeString(source, "object Generated")
                _        <- Workspace.writeString(assets.resolve("nested/view.js"), "view")
                manifest <- products.seal(
                  staged,
                  PluginOutput(
                    "generated",
                    List(WrittenFile(OutputName("source"), RepoPath("mixed/Generated.scala"))),
                    List(WrittenDirectory(OutputName("assets"), RepoPath("mixed/assets"))),
                  ),
                )
                committed <- products.commit(staged, manifest)
              yield committed
            }
            .provideSomeLayer[Scope](ZLayer.succeed(ContentStore.in(repo.resolve(".histos/cas"))))
          rejected <- ZIO.scoped {
            for
              _      <- committed.read
              result <- Products.namedUsing(cleaner)(repo, graph, List(ProjectName("app"))).either
              kept   <- ZIO.attemptBlocking(Files.exists(repo.resolve(".histos/classes/app/keep.class")))
            yield (result, kept)
          }
          cleaned <- Products.namedUsing(cleaner)(repo, graph, List(ProjectName("app")))
          user    <- Workspace.readString(repo.resolve("mixed/User.scala"))
          gone    <- ZIO.attemptBlocking(
            Files.notExists(repo.resolve("mixed/Generated.scala")) &&
              Files.notExists(repo.resolve("mixed/assets")) && Files.notExists(repo.resolve(".histos/classes/app"))
          )
        yield assertTrue(
          rejected == (Left(ProductError.ActiveReaders), true),
          cleaned == List(ProjectName("app")),
          user == "object User",
          gone,
        )
      }
    }
  )
end GeneratedCleanSpec
