package histos.load

import histos.cache.ContentStore
import histos.resolve.Resolve
import histos.workspace.Workspace
import histos.{Module, Platform, Resolution}

import slayer.stubbed

import zio.{Scope, ZIO, ZLayer}
import zio.test.*

object LoaderProjectLawSpec extends ZIOSpecDefault:
  def spec = suite("one module loads")(
    test("a build with no plugins and no dependencies names that module"):
      val program =
        Workspace
          .work("histos-load-one")
          .mapError(LoadError.Io(_))
          .flatMap: root =>
            val source = root.resolve("histos.scala")
            val layer  =
              (stubbed[Resolve] ++ ZLayer.succeed(ContentStore.in(root.resolve("store")))) >>> Loader.live
            val run =
              for
                api   <- LoaderApiJar.of(classOf[histos.HistosBuild], root.resolve("histos-api.jar"))
                _     <- Workspace.writeString(source, buildFile).mapError(LoadError.Io(_))
                found <- ZIO.serviceWithZIO[Loader](_.load(root, api))
              yield found.graph.projects match
                case (module: Module) :: Nil =>
                  assertTrue(
                    module.name.value == "loaderlaw",
                    module.platforms.all == List(Platform.Jvm),
                    module.versions.all.map(_.value) == List("3.10.0"),
                    module.dependencies.isEmpty,
                    module.testDependencies.isEmpty,
                    module.dependsOn.isEmpty,
                    found.graph.plugins.isEmpty,
                    found.plugins.links.isEmpty,
                    found.graph.resolution == Resolution.Solve,
                  )
                case other =>
                  val modules = other.collect { case module: Module => module.name.value }
                  assertTrue(modules == List("loaderlaw"), other.lengthIs == 1)
            run.provideSomeLayer[Scope](layer)
      program
  )

  private val buildFile =
    """import histos.*
      |
      |object build extends HistosBuild:
      |  def graph =
      |    val loaderlaw = module("loaderlaw", Jvm)(scala"3.10.0")
      |    Projects(loaderlaw)
      |""".stripMargin
end LoaderProjectLawSpec
