package histos.server

import histos.cli.{Cli, CliExit}
import histos.load.{BuildNotes, PluginLink, PluginRuntime}
import histos.plugin.{PluginArgs, PluginOutput, PluginTaskId}
import histos.task.{TaskError, TaskId, TaskResult}
import histos.zinc.GraphCompile
import histos.{Jvm, Projects, ScalaVersion, module}

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.stream.Take
import zio.{
  Chunk,
  Dequeue,
  IO,
  NonEmptyChunk,
  Promise,
  Queue,
  Ref,
  Scope,
  ZIO,
  ZLayer,
  durationInt
}
import zio.test.*

object RunSpec extends ZIOSpecDefault:
  private val task = "dev.example/hold"
  private val call = Call.Task(task, PluginArgs.empty)

  private val runs = suite("run id")(
    test(
      "two runs of one task keep both fibers, and cancel interrupts only the first"
    ):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              gates <- Queue.unbounded[Promise[Nothing, String]]
              running <- serve(root, holding(gates))
              socket = running.record.socket
              first <- live(socket, call)
              feedA <- first.next.timeout(15.seconds)
              gateA <- gates.take.timeout(15.seconds)
              second <- live(socket, call)
              feedB <- second.next.timeout(15.seconds)
              gateB <- gates.take.timeout(15.seconds)
              idA = feedA.flatMap(runText)
              idB = feedB.flatMap(runText)
              stopped <- cancel(socket, idA)
              afterA <- untilExit(first).timeout(5.seconds)
              _ <- ZIO.foreachDiscard(gateB)(_.succeed("beta"))
              afterB <- untilExit(second).timeout(5.seconds)
              finished <- cancel(socket, idB)
            yield assertTrue(
              gateA.isDefined,
              gateB.isDefined,
              idA.isDefined,
              idB.isDefined,
              idA != idB,
              idA != Some(task),
              told(stopped, "cancelled"),
              exited(stopped, CliExit.Success.code),
              afterA.exists(exited(_, CliExit.Interrupted.code)),
              !afterA.exists(said(_, "beta")),
              afterB.exists(said(_, "beta")),
              afterB.exists(exited(_, CliExit.Success.code)),
              idB.exists(id => failed(finished, s"no run '$id'"))
            )
          program.provideSome[Scope & histos.resolve.LockedTools](
            liveLayers(root)
          )
    ,
    test(
      "a plugin task prints its run id before it finishes, and cancel of that id interrupts it"
    ):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              gates <- Queue.unbounded[Promise[Nothing, String]]
              running <- serve(root, holding(gates))
              socket = running.record.socket
              open <- live(socket, call)
              feed <- open.next.timeout(15.seconds)
              gate <- gates.take.timeout(15.seconds)
              id = feed.flatMap(runText)
              stopped <- cancel(socket, id)
              after <- untilExit(open).timeout(5.seconds)
            yield assertTrue(
              id.isDefined,
              id != Some(task),
              gate.isDefined,
              told(stopped, "cancelled"),
              exited(stopped, CliExit.Success.code),
              after.exists(exited(_, CliExit.Interrupted.code)),
              !after.exists(said(_, "still"))
            )
          program.provideSome[Scope & histos.resolve.LockedTools](
            liveLayers(root)
          )
    ,
    test("an unknown id fails and does not interrupt a live run"):
      ZIO.scoped:
        temp.flatMap: root =>
          val program =
            for
              gates <- Queue.unbounded[Promise[Nothing, String]]
              running <- serve(root, holding(gates))
              socket = running.record.socket
              open <- live(socket, call)
              feed <- open.next.timeout(15.seconds)
              gate <- gates.take.timeout(15.seconds)
              id = feed.flatMap(runText)
              missing <- Client.talk(socket, Call.Cancel(task)).runCollect
              _ <- ZIO.foreachDiscard(gate)(_.succeed("lived"))
              after <- untilExit(open).timeout(5.seconds)
            yield assertTrue(
              id.isDefined,
              id != Some(task),
              failed(missing, s"no run '$task'"),
              after.exists(said(_, "lived")),
              after.exists(exited(_, CliExit.Success.code))
            )
          program.provideSome[Scope & histos.resolve.LockedTools](
            liveLayers(root)
          )
  )

  private def holding(
      gates: Queue[Promise[Nothing, String]]
  ): PluginRuntime =
    new PluginRuntime:
      def links: List[PluginLink] = Nil
      def bind: ZIO[Any, TaskError, Map[TaskId, IO[TaskError, TaskResult]]] =
        ZIO.succeed(Map.empty)
      def run(id: PluginTaskId, args: PluginArgs) =
        if id.render != task || args != PluginArgs.empty then
          ZIO.succeed(PluginOutput(id.render, Nil))
        else
          Promise.make[Nothing, String].flatMap { gate =>
            gates.offer(gate) *> gate.await.map(text => PluginOutput(text, Nil))
          }

  private final class Live(
      queue: Dequeue[Take[ServerError, Feed]],
      pending: Ref[Chunk[Feed]]
  ):
    def next: IO[ServerError, Feed] =
      pending
        .modify { chunk =>
          NonEmptyChunk.fromChunk(chunk) match
            case Some(items) => (Some(items.head), items.tail)
            case None        => (None, Chunk.empty)
        }
        .flatMap {
          case Some(feed) => ZIO.succeed(feed)
          case None       =>
            queue.take.flatMap { take =>
              take.done
                .mapError {
                  case Some(error) => error
                  case None        => ServerError.Protocol
                }
                .flatMap { chunk =>
                  NonEmptyChunk.fromChunk(chunk) match
                    case Some(items) => pending.set(items.tail).as(items.head)
                    case None        => next
                }
            }
        }

  private def live(socket: Path, asked: Call): ZIO[Scope, ServerError, Live] =
    for
      queue <- Client.talk(socket, asked).toQueueUnbounded
      pending <- Ref.make(Chunk.empty[Feed])
    yield Live(queue, pending)

  private def untilExit(open: Live): IO[ServerError, Chunk[Feed]] =
    def loop(seen: Chunk[Feed]): IO[ServerError, Chunk[Feed]] =
      open.next.flatMap {
        case exit @ Feed.Exit(_, _) => ZIO.succeed(seen :+ exit)
        case other                  => loop(seen :+ other)
      }
    loop(Chunk.empty)

  private def cancel(
      socket: Path,
      id: Option[String]
  ): ZIO[Any, ServerError, Chunk[Feed]] =
    id match
      case Some(found) => Client.talk(socket, Call.Cancel(found)).runCollect
      case None        => ZIO.succeed(Chunk.empty)

  private def runText(feed: Feed): Option[String] =
    feed match
      case Feed.Out(text) =>
        val prefix = "run "
        if text.startsWith(prefix) then
          val raw = text.drop(prefix.length)
          if raw.matches(
              "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"
            )
          then Some(raw)
          else None
        else None
      case _ => None

  private def said(feeds: Chunk[Feed], text: String): Boolean =
    feeds.exists {
      case Feed.Out(line) => line == text
      case _              => false
    }

  private def told(feeds: Chunk[Feed], text: String): Boolean =
    said(feeds, text)

  private def exited(feeds: Chunk[Feed], code: Int): Boolean =
    feeds.exists {
      case Feed.Exit(found, err) => found == code && err.isEmpty
      case _                     => false
    }

  private def failed(feeds: Chunk[Feed], err: String): Boolean =
    feeds.exists {
      case Feed.Exit(code, found) =>
        code == CliExit.TaskFailed.code && found == err
      case _ => false
    }

  private def serve(root: Path, plugins: PluginRuntime) =
    ZIO
      .fromEither(Projects(module("demo", Jvm)(ScalaVersion("3.10.0"))))
      .flatMap { built =>
        ZIO.attempt(write(root, "histos.scala", "object build\n")) *>
          GraphCompile.jdkHere.flatMap { jdk =>
            histos.AppToolFixture.at(root, built)(
              Daemon.open(
                root,
                built,
                jdk,
                Cli.VersionText,
                root.resolve("missing-api.jar"),
                Nil,
                ZIO.succeed(BuildNotes(false, Nil)),
                plugins
              )
            )
          }
      }

  private def liveLayers(root: Path) =
    histos.AppToolFixture.resolve ++
      ZLayer.succeed(histos.cache.ContentStore.in(root.resolve("cache"))) >+>
      histos.load.Loader.live

  private def write(root: Path, name: String, text: String): Unit =
    val path = root.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, text)

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(
      ZIO.attempt(Files.createTempDirectory("histos-run"))
    ): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths
            .sortBy(_.getNameCount)
            .reverse
            .foreach(path => Files.deleteIfExists(path))
      }.ignore

  def spec =
    runs.provideSomeLayerShared[zio.test.TestEnvironment & zio.Scope](
      histos.AppToolFixture.layer(
        histos.Projects(
          histos.module("jvm", histos.Jvm)(histos.ScalaVersion("3.10.0"))
        )
      )
    ) @@ TestAspect.withLiveSystem @@ TestAspect.withLiveClock @@
      TestAspect.timeout(45.seconds)
