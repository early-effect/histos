package histos.load

import histos.plugin.PluginRunError
import histos.resolve.{Repository, Resolve}
import histos.workspace.Workspace
import histos.Dep

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*

import zio.test.*
import zio.{Promise, Ref, Scope, ZIO, ZLayer}

/** Red proof that wiring resources die before the loaded build is returned. */
object PluginLifetimeSpec extends ZIOSpecDefault:
  def spec = suite("plugin lifetime")(
    test("a service acquired while loading is open for the caller and closed with that scope") {
      ZIO.scoped {
        for
          repo   <- Workspace.work("histos-p6-life").foldZIO(failure => ZIO.dieMessage(failure.message), ZIO.succeed)
          marker <- ZIO.succeed(repo.resolve("marker.txt").toAbsolutePath.normalize)
          api    <- apiJar(repo.resolve("api.jar"))
          _      <- write(repo.resolve("histos.scala"), build)
          _      <- write(repo.resolve("histos.wiring.scala"), wiring(marker))
          during <- ZIO.scoped {
            ZIO
              .serviceWithZIO[Loader](_.load(repo, api))
              .provideSome[Scope](ZLayer.succeed(silent) >>> Loader.live) *>
              ZIO.attemptBlocking(Files.readString(marker)).orDie
          }
          after <- ZIO.attemptBlocking(Files.readString(marker)).orDie
        yield assertTrue(during == "open", after == "closed")
      }
    },
    test("a retired generation stays open until its last lease releases") {
      ZIO.scoped {
        for
          root <- Workspace.work("histos-p6-lease").foldZIO(failure => ZIO.dieMessage(failure.message), ZIO.succeed)
          marker = root.resolve("marker.txt")
          leases   <- GenerationLeases.make
          _        <- leases.use(held(marker))
          gate     <- Promise.make[Nothing, Unit]
          entered  <- Promise.make[Nothing, Unit]
          fiber    <- ZIO.scoped(leases.lease *> entered.succeed(()) *> gate.await).fork
          _        <- entered.await
          _        <- leases.retire
          heldOpen <- ZIO.attemptBlocking(Files.readString(marker)).orDie
          _        <- gate.succeed(())
          _        <- fiber.join
          released <- ZIO.attemptBlocking(Files.readString(marker)).orDie
        yield assertTrue(heldOpen == "open", released == "closed")
      }
    },
    test("a closed generation refuses a lease and does not run the task") {
      for
        leases <- GenerationLeases.make
        _      <- leases.discard
        ran    <- Ref.make(false)
        exit   <- ZIO.scoped(leases.lease *> ran.set(true)).either
        saw    <- ran.get
      yield assertTrue(exit == Left(PluginRunError.Closed), saw == false)
    },
  ) @@ TestAspect.withLiveClock

  private val build: String =
    """|import histos.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"3.10.1")
       |    Projects(app)
       |""".stripMargin

  private def wiring(marker: Path): String =
    s"""|import zio.{ZIO, ZLayer}
        |
        |object HistosWiring:
        |  val layer: ZLayer[Any, Nothing, histos.plugin.Plugins] =
        |    ZLayer.scoped:
        |      ZIO.acquireRelease(
        |        ZIO.attemptBlocking {
        |          java.nio.file.Files.writeString(java.nio.file.Path.of("${marker.toString}"), "open")
        |          histos.plugin.Plugins(zio.Chunk.empty)
        |        }.orDie
        |      )(_ =>
        |        ZIO.attemptBlocking {
        |          java.nio.file.Files.writeString(java.nio.file.Path.of("${marker.toString}"), "closed")
        |        }.orDie
        |      )
        |""".stripMargin

  private def held(marker: Path): ZIO[Scope, Nothing, Unit] =
    ZIO.acquireRelease(
      ZIO.attemptBlocking {
        Files.writeString(marker, "open", StandardCharsets.UTF_8)
        ()
      }.orDie
    )(_ =>
      ZIO.attemptBlocking {
        Files.writeString(marker, "closed", StandardCharsets.UTF_8)
        ()
      }.orDie
    )

  private def write(path: Path, text: String): ZIO[Any, Nothing, Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, text, StandardCharsets.UTF_8)
      ()
    }.orDie

  private def apiJar(destination: Path): ZIO[Any, Nothing, Path] =
    ZIO.attemptBlocking {
      val located = Path.of(classOf[histos.HistosBuild].getProtectionDomain.getCodeSource.getLocation.toURI)
      if Files.isRegularFile(located) then located
      else
        Files.createDirectories(destination.getParent)
        val out  = new ZipOutputStream(Files.newOutputStream(destination))
        val walk = Files.walk(located)
        try
          walk.iterator.asScala.foreach { path =>
            if Files.isRegularFile(path) then
              val name = located.relativize(path).toString.replace('\\', '/')
              out.putNextEntry(new ZipEntry(name))
              out.write(Files.readAllBytes(path))
              out.closeEntry()
          }
        finally
          walk.close()
          out.close()
        destination
    }.orDie

  private def silent: Resolve = new Resolve:
    def resolveTool(request: histos.resolve.ToolRequest)                          = ZIO.dieMessage("resolveTool")
    def resolve(cell: histos.resolve.ResolveCell)                                 = ZIO.dieMessage("resolve")
    def fetch(artifact: histos.resolve.LockedArtifact)                            = ZIO.dieMessage("fetch")
    def resolvePlugin(dependency: Dep, repositories: histos.resolve.Repositories) =
      ZIO.dieMessage("resolvePlugin")
    def sources(organization: String, name: String, revision: String, repository: Repository) =
      ZIO.dieMessage("sources")
end PluginLifetimeSpec
