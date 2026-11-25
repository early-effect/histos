package histos.load

import histos.plugin.{PluginEvents, PluginRunError, PluginScopes, Program, ProgramEvent, ResourceLife}
import histos.resolve.{Repository, Resolve}
import histos.suite.HistosSpec
import histos.workspace.Workspace
import histos.Dep

import java.net.ServerSocket
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import zio.test.*
import zio.{Promise, Ref, Scope, ZIO, ZLayer, durationInt}

/** A wiring file is not a load input. Generation leases still close with their last holder. */
object PluginLifetimeSpec extends HistosSpec:
  def spec = lifetime @@ TestAspect.withLiveClock @@ TestAspect.timeout(30.seconds)

  private val lifetime = suite("plugin lifetime")(
    test("a lone histos.wiring.scala is not compiled") {
      ZIO.scoped {
        for
          repo   <- Workspace.work("histos-p6-life").foldZIO(failure => ZIO.dieMessage(failure.message), ZIO.succeed)
          api    <- apiJar(repo.resolve("api.jar"))
          _      <- write(repo.resolve("histos.scala"), build)
          _      <- write(repo.resolve("histos.wiring.scala"), "this is not scala {{{")
          loaded <- ZIO
            .serviceWithZIO[Loader](_.load(repo, api))
            .provideSome[Scope](ZLayer.succeed(silent) >>> Loader.live)
        yield assertTrue(loaded.catalog.isEmpty, loaded.graph.projects.nonEmpty)
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
    test("a generation without plugin resources does not hold a foreground host") {
      ZIO.scoped:
        for
          leases <- GenerationLeases.make
          _      <- ZIO.addFinalizer(leases.retire)
          _      <- leases.use(ZIO.addFinalizer(ZIO.unit))
          done   <- leases.awaitResources.as(true)
        yield assertTrue(done)
    },
    test("retirement closes plugin resources before build resources and only once") {
      for
        leases <- GenerationLeases.make
        closed <- Ref.make(Vector.empty[String])
        _      <- leases.use(ZIO.addFinalizer(closed.update(_ :+ "build")))
        _      <- leases.generation.extend(ZIO.addFinalizer(closed.update(_ :+ "plugin")))
        _      <- leases.retire *> leases.retire
        order  <- closed.get
      yield assertTrue(order == Vector("plugin", "build"))
    },
    test("a foreground generation owns an actual preview child after its invocation ends") {
      ZIO.scoped:
        for
          root <- Workspace
            .work("histos-local-preview")
            .foldZIO(failure => ZIO.dieMessage(failure.message), ZIO.succeed)
          source = root.resolve("PreviewChild.java")
          _      <- write(source, previewChild)
          opened <- Promise.make[PluginRunError, (Long, Int)]
          ended  <- Promise.make[Nothing, Unit]
          owner  <- ZIO.scoped {
            for
              leases <- GenerationLeases.make
              _      <- ZIO.addFinalizer(leases.retire)
              _      <- ZIO.scoped {
                leases.lease *> ZIO
                  .serviceWithZIO[PluginScopes] { scopes =>
                    scopes(ResourceLife.Generation).extend(
                      Program.start(
                        List(java, source.toString),
                        root,
                        PluginEvents.closedInvocation,
                        event =>
                          event match
                            case ProgramEvent.Out(line) => opened.complete(ZIO.fromEither(address(line))).unit
                            case ProgramEvent.Err(_)    => ZIO.unit,
                      ) *> opened.await
                    )
                  }
                  .provideSomeLayer[Scope](PluginScopes.live(leases.generation))
              }
              _ <- ended.succeed(())
              _ <- leases.awaitResources
            yield ()
          }.forkScoped
          endpoint <- opened.await
          _        <- ended.await
          (pid, port) = endpoint
          alive   <- ZIO.attemptBlocking(ProcessHandle.of(pid).toScala.exists(_.isAlive)).orDie
          waiting <- owner.poll
          exit    <- owner.interrupt
          gone    <- ZIO.attemptBlocking(ProcessHandle.of(pid).toScala.forall(!_.isAlive)).orDie
          rebound <- ZIO.acquireRelease(ZIO.attemptBlocking(new ServerSocket(port)).orDie)(socket =>
            ZIO.attemptBlocking(socket.close()).orDie
          )
        yield assertTrue(alive, waiting.isEmpty, exit.isInterrupted, gone, rebound.getLocalPort == port)
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
  )

  private val java = Path.of(sys.props.getOrElse("java.home", "")).resolve("bin/java").toString

  private def address(line: String): Either[PluginRunError, (Long, Int)] =
    line.split(":").toList match
      case rawPid :: rawPort :: Nil =>
        for
          pid  <- rawPid.toLongOption.toRight(PluginRunError.Defect("preview pid"))
          port <- rawPort.toIntOption.toRight(PluginRunError.Defect("preview port"))
        yield (pid, port)
      case _ => Left(PluginRunError.Defect("preview address"))

  private val previewChild =
    """import java.net.ServerSocket;
      |class PreviewChild {
      |  public static void main(String[] args) throws Exception {
      |    try (var socket = new ServerSocket(0)) {
      |      System.out.println(ProcessHandle.current().pid() + ":" + socket.getLocalPort());
      |      socket.accept().close();
      |    }
      |  }
      |}
      |""".stripMargin

  private val build: String =
    """|import histos.*
       |
       |object build extends HistosBuild:
       |  def graph =
       |    val app = module("app", Jvm)(scala"3.10.1")
       |    Projects(app)
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
