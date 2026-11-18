package histos.build

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.load.{Loader, LoadedBuild}
import histos.publish.IvyRoot
import histos.report.{BuildFault, CallResult}
import histos.resolve.Resolve
import histos.process.{StdinFeed, Terminal}
import histos.client.CallerFacts
import histos.lock.InvocationEnvelope
import histos.server.{Call, Client, ConsentDesk, Feed, Message, Perform, Say, ServerError}
import histos.task.RunId

import java.nio.file.Path

import zio.stream.ZStream
import zio.{Chunk, Fiber, Ref, UIO, ZIO}

/** One item from a running call. A fault is [[Beat.Finished]]. The stream fails when the call never produced one. */
enum Beat:
  case Heard(event: BuildEvent)
  case Bytes(data: Chunk[Byte])
  case Finished(result: CallResult)

/** Local and remote execution of one [[histos.server.Call]]. The listener on a local build runs as the call produces,
  * and the stream is that same transcript.
  */
trait Build[-R]:
  def perform(call: Call): ZStream[R, ServerError, Beat]

object Build:
  def result(beats: Chunk[Beat]): Option[CallResult] =
    beats.reverse.collectFirst { case Beat.Finished(found) => found }

  def feed(beat: Beat): Feed =
    beat match
      case Beat.Heard(event)     => Feed.Event(event)
      case Beat.Bytes(data)      => Feed.Output(data)
      case Beat.Finished(result) => Feed.Done(result)

  def relay[R, E](terminal: Terminal, call: Call)(
      use: histos.process.RunRelay => ZIO[R, E, Unit]
  ): ZIO[R, E, Unit] =
    StdinFeed.of(terminal, call.attendance) match
      case StdinFeed.Live   => ZIO.scoped(histos.process.LiveInput.open.flatMap(use))
      case StdinFeed.Closed => use(histos.process.RunRelay.Closed)

  def write(bytes: Chunk[Byte]): UIO[Unit] =
    ZIO.attemptBlocking {
      val out = java.lang.System.out
      out.write(bytes.toArray)
      out.flush()
    }.ignore

  def attached(
      socket: Path,
      terminal: Terminal = Terminal.Detached,
      envelope: Call => UIO[InvocationEnvelope] = CallerFacts.detached,
  ): Build[Any] =
    new Build[Any]:
      def perform(call: Call): ZStream[Any, ServerError, Beat] =
        ZStream.unwrap(envelope(call).map(ready => Client.talk(socket, ready, terminal))).map {
          case Feed.Event(event)  => Beat.Heard(event)
          case Feed.Output(bytes) => Beat.Bytes(bytes)
          case Feed.Done(result)  => Beat.Finished(result)
        }

  def here[R <: Resolve & Loader & ContentStore & IvyRoot](
      repo: Path,
      apiJar: Path,
      machine: Machine,
      runs: Ref[Map[RunId, Fiber[ServerError, Unit]]],
      track: Boolean,
      version: String,
      adopt: Call => ZIO[R, ServerError, Either[BuildFault, Option[LoadedBuild]]],
      edges: Say => Perform.Edges[R],
      hear: Beat => UIO[Unit],
      terminal: Terminal = Terminal.Detached,
      envelope: Call => UIO[InvocationEnvelope] = CallerFacts.detached,
  ): Build[R] =
    new Build[R]:
      def perform(call: Call): ZStream[R, ServerError, Beat] =
        def replay(found: Chunk[Beat]): ZStream[R, ServerError, Beat] =
          ZStream.fromChunk(found)
        ZStream.unwrap[R, ServerError, Beat] {
          for
            heard <- Ref.make(Chunk.empty[Beat])
            say = new Say({
              case Message.Event(event) =>
                val beat = Beat.Heard(event)
                heard.update(_ :+ beat) *> hear(beat)
              case Message.Output(encoded) =>
                histos.server.RunBytes.decode(encoded) match
                  case Some(bytes) =>
                    val beat = Beat.Bytes(bytes)
                    heard.update(_ :+ beat) *> hear(beat)
                  case None => ZIO.unit
              case Message.Done(found) =>
                val beat = Beat.Finished(found)
                heard.update(_ :+ beat) *> hear(beat)
              case Message.Ready | Message.Marker(_) => ZIO.unit
            })
            _ <- adopt(call).flatMap { loaded =>
              val mode = loaded match
                case Right(Some(have)) => have.graph.resolution
                case _                 => histos.Resolution.Solve
              histos.resolve.ResolutionService
                .open(repo, mode)
                .mapError(ServerError.Resolve(_))
                .flatMap { report =>
                  histos.zinc.CompileMemo.provide(
                    histos.zinc.Compilers.provide(histos.schedule.Parallelism.default.cpu)(
                      relay(terminal, call)(stdin =>
                        envelope(call).flatMap: ready =>
                          // This process is the session. The daemon desk is not in scope.
                          ConsentDesk
                            .open(
                              repo,
                              ConsentDesk.strategy(ready.context.execution, ready.lock),
                              ConsentDesk.command,
                              ConsentDesk.preparer(repo),
                              ConsentDesk.feed(say),
                            )
                            .flatMap: desk =>
                              Perform
                                .run(
                                  repo,
                                  apiJar,
                                  loaded,
                                  machine,
                                  runs,
                                  track,
                                  version,
                                  call,
                                  say,
                                  edges(say),
                                  desk,
                                  stdin,
                                  caller = Some(ready.context),
                                  lock = ready.lock,
                                )
                                .provideSomeLayer(zio.ZLayer.succeed(report) ++ histos.format.Formatters.live)
                      )
                    )
                  )
                }
            }
            beats <- heard.get
          yield replay(beats)
        }
end Build
