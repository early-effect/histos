package histos.load

import histos.cache.ContentStore
import histos.plugin.{
  ApiDigest,
  Contribution,
  DeclaredTask,
  EntryName,
  HistosPlugin,
  PluginArgs,
  PluginId,
  PluginOutput,
  PluginRunError,
  PluginScopes,
  PluginTaskId,
  RegisteredPlugin,
  ResourceLife,
  RunRequest,
  Seal,
  TaskName,
}
import histos.resolve.{DependencyLock, ResolutionService, Resolve}
import histos.schedule.{ActionCache, RemoteCache}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{ProjectGraph, Splits}

import java.net.{InetAddress, ServerSocket, Socket}
import java.nio.charset.StandardCharsets

import slayer.{any, stub, stubbed}

import zio.{Scope, ZIO, ZLayer, durationInt}
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** A generation acquire answers after `run` returns and dies with the runtime. An invocation acquire dies with `run`.
  */
object PluginScopeLawSpec extends ZIOSpecDefault:
  private val pluginId = PluginId.trusted("law")
  private val held     = TaskName.trusted("held")
  private val brief    = TaskName.trusted("brief")

  def spec = suite("plugin scope laws")(
    test("a generation socket answers after the task returns, and an invocation socket does not"):
      Workspace.work("histos-plugin-scope").catchAll(error => ZIO.dieMessage(error.message)).flatMap { root =>
        val run =
          for
            _     <- stub[ResolutionService](_.current)(ZIO.succeed(DependencyLock(Nil)))
            _     <- stub[Resolve](_.fetch(any[histos.resolve.LockedArtifact]))(ZIO.dieMessage("fetched"))
            found <- ZIO.scoped {
              for
                threads <- PluginThreads.make(ClassLoader.getSystemClassLoader)
                runtime <- PluginRuntime.loaded(
                  root,
                  graph,
                  ClassLoader.getSystemClassLoader,
                  threads,
                  Nil,
                  ApiDigest.trusted("ab" * 32),
                )
                stayed         <- runtime.run(PluginTaskId(pluginId, held), PluginArgs.empty)
                generationPort <- portOf(stayed.text)
                whileLoaded    <- ask(generationPort)
                dropped        <- runtime.run(PluginTaskId(pluginId, brief), PluginArgs.empty)
                invocationPort <- portOf(dropped.text)
                afterReturn    <- ask(invocationPort)
              yield (whileLoaded, afterReturn, generationPort)
            }
            afterClose <- ask(found._3)
          yield assertTrue(
            found._1 == Right("open"),
            found._2 == Left(Ask.Closed),
            afterClose == Left(Ask.Closed),
          )
        run.provideSomeLayer[Scope](
          ZLayer.succeed(ActionCache.miss) ++
            RemoteCache.off ++
            stubbed[Resolve] ++
            ZLayer.succeed(ContentStore.in(root.resolve("store"))) ++
            stubbed[ResolutionService]
        )
      }
  ) @@ TestAspect.withLiveClock @@ TestAspect.withLiveSystem

  private enum Ask:
    case Closed

  private def portOf(text: String): ZIO[Any, Nothing, Int] =
    ZIO.fromOption(text.toIntOption).orDieWith(_ => new IllegalStateException(text))

  private def ask(port: Int): ZIO[Any, Nothing, Either[Ask, String]] =
    ZIO
      .attemptBlockingInterrupt {
        val socket = new Socket(InetAddress.getLoopbackAddress, port)
        try
          val read = socket.getInputStream.readNBytes(4)
          new String(read, StandardCharsets.US_ASCII)
        finally socket.close()
      }
      .timeout(1.second)
      .map {
        case Some(text) => Right(text)
        case None       => Left(Ask.Closed)
      }
      .orElseSucceed(Left(Ask.Closed))

  private val graph =
    ProjectGraph.trusted(
      Nil,
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      List(
        RegisteredPlugin(
          pluginId,
          EntryName.trusted("demo.Plugin$"),
          Contribution(List(task(held), task(brief)), Nil, Nil),
        )(BindPlugin)
      ),
    )

  private def task(name: TaskName): DeclaredTask =
    DeclaredTask(name, Seal.Open, Set.empty, Nil, Nil, Nil)

  private object BindPlugin extends HistosPlugin:
    def id: PluginId             = pluginId
    def contribute               = Contribution(Nil, Nil, Nil)
    def run(request: RunRequest) =
      val life = if request.id.name == held then ResourceLife.Generation else ResourceLife.Invocation
      ZIO.serviceWithZIO[PluginScopes] { scopes =>
        scopes(life)
          .extend(ZIO.acquireRelease(ZIO.attemptBlocking(bind))(socket => ZIO.attemptBlocking(socket.close()).ignore))
          .mapBoth(_ => PluginRunError.Defect("bind"), socket => PluginOutput(socket.getLocalPort.toString, Nil))
      }

  private def bind: ServerSocket =
    val socket = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
    // A named Runnable: scalafmt 3.11.5 rewrites a try/catch lambda argument into code that does not parse.
    val serve: Runnable = () =>
      try
        val client = socket.accept()
        try client.getOutputStream.write("open".getBytes(StandardCharsets.US_ASCII))
        finally client.close()
      catch case _: java.io.IOException => ()
    val accept = new Thread(serve, "histos-plugin-scope")
    accept.setDaemon(true)
    accept.start()
    socket
end PluginScopeLawSpec
