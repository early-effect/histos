package histos.build

import histos.cache.ContentStore
import histos.jdk.Machine
import histos.load.{Loader, LoadedBuild}
import histos.report.{BuildFault, CallResult}
import histos.resolve.Resolve
import histos.server.{Call, Client, Feed, Message, Perform, Say, ServerError}
import histos.task.RunId

import java.nio.file.Path

import zio.stream.ZStream
import zio.{Chunk, Fiber, Ref, UIO, ZIO}

/** One item from a running call. A fault is [[Beat.Finished]]. The stream fails when the call never produced one. */
enum Beat:
  case Heard(event: BuildEvent)
  case Finished(result: CallResult)

/** Local and remote execution of one [[Call]]. The listener on a local build runs as the call produces, and the stream
  * is that same transcript.
  */
trait Build[-R]:
  def perform(call: Call): ZStream[R, ServerError, Beat]

object Build:
  def result(beats: Chunk[Beat]): Option[CallResult] =
    beats.reverse.collectFirst { case Beat.Finished(found) => found }

  def feed(beat: Beat): Feed =
    beat match
      case Beat.Heard(event)     => Feed.Event(event)
      case Beat.Finished(result) => Feed.Done(result)

  def attached(socket: Path): Build[Any] =
    new Build[Any]:
      def perform(call: Call): ZStream[Any, ServerError, Beat] =
        Client.talk(socket, call).map {
          case Feed.Event(event) => Beat.Heard(event)
          case Feed.Done(result) => Beat.Finished(result)
        }

  def here[R <: Resolve & Loader & ContentStore](
      repo: Path,
      apiJar: Path,
      machine: Machine,
      runs: Ref[Map[RunId, Fiber[ServerError, Unit]]],
      track: Boolean,
      version: String,
      adopt: Call => ZIO[R, ServerError, Either[BuildFault, Option[LoadedBuild]]],
      edges: Say => Perform.Edges[R],
      hear: Beat => UIO[Unit],
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
              case Message.Done(found) =>
                val beat = Beat.Finished(found)
                heard.update(_ :+ beat) *> hear(beat)
              case _ => ZIO.unit
            })
            _ <- adopt(call).flatMap { loaded =>
              Perform.run(repo, apiJar, loaded, machine, runs, track, version, call, say, edges(say))
            }
            beats <- heard.get
          yield replay(beats)
        }
end Build
