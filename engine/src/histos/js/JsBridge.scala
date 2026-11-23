package histos.js

import histos.report.WireJson.given

import java.nio.file.Path

import zio.{Console, IO, ZIO}
import zio.json.*

private[js] enum JsPhase derives JsonCodec:
  case Captured, BeforeIr, IrLoaded, Linked, MinifierStarted

private[js] final case class JsReady(
    phase: JsPhase,
    pid: Long,
    scratch: String,
    output: String,
    result: String,
    temporary: String,
    script: Option[String] = None,
    descendant: Option[Long] = None,
) derives JsonCodec

private[js] enum JsResult derives JsonCodec:
  case Script(path: String)
  case Failed(error: JsLinkError)

private[js] enum JsResume derives JsonCodec:
  case Continue

private[js] object JsBridge:
  def checkpoint(ready: JsReady): IO[JsLinkError, Unit] =
    for
      _    <- Console.printLine(ready.toJson).mapError(_ => JsLinkError.Bridge)
      line <- ZIO
        .attemptBlockingInterrupt(Option(scala.io.StdIn.readLine()))
        .mapError(_ => JsLinkError.Bridge)
        .someOrFail(JsLinkError.Bridge)
      _ <- ZIO.fromEither(line.fromJson[JsResume]).mapError(_ => JsLinkError.Bridge)
    yield ()

  def ready(phase: JsPhase, result: Path, output: Path, script: Option[Path] = None): IO[JsLinkError, JsReady] =
    ZIO
      .attempt {
        Option(result.getParent).map: scratch =>
          JsReady(
            phase,
            ProcessHandle.current().pid(),
            scratch.toString,
            output.toString,
            result.toString,
            java.lang.System.getProperty("java.io.tmpdir"),
            script.map(_.toString),
          )
      }
      .mapError(_ => JsLinkError.Bridge)
      .someOrFail(JsLinkError.Bridge)
end JsBridge
