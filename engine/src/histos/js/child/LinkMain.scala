package histos.js.child

import histos.js.{JsBridge, JsLinkError, JsPhase, JsResult, Linker}
import histos.pack.MainClass
import histos.{JsModuleKind, JsOptimize}
import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO, ZIOAppArgs, ZIOAppDefault}
import zio.json.*

/** The ordinary Scala 3 linker and Splice run inside one owned process group. */
object LinkMain extends ZIOAppDefault:
  def run: ZIO[ZIOAppArgs, JsLinkError, Unit] =
    for
      args   <- getArgs
      result <- ZIO
        .fromOption(args.headOption)
        .orElseFail(JsLinkError.Bridge)
        .flatMap(raw => ZIO.attempt(Path.of(raw)).mapError(_ => JsLinkError.Bridge))
      reported <- execute(args.toList).either
      report = reported.fold(JsResult.Failed(_), path => JsResult.Script(path.toAbsolutePath.toString))
      _ <- ZIO
        .attemptBlocking(Files.writeString(result, report.toJson))
        .mapError(_ => JsLinkError.Io(IoFailure(result, IoFault.Write)))
    yield ()

  private def execute(args: List[String]): IO[JsLinkError, Path] =
    for
      request <- decode(args)
      _       <- checkpoint(request, JsPhase.BeforeIr)
      linked  <- Linker.link(
        request.classpath,
        request.output,
        request.kind,
        request.main,
        request.arguments,
        request.optimize,
        checkpoint(request, JsPhase.IrLoaded),
      )
      _      <- checkpoint(request, JsPhase.Linked, Some(linked))
      script <- request.optimize match
        case JsOptimize.Fast => ZIO.succeed(linked)
        case JsOptimize.Full =>
          val output = request.output.resolveSibling("full.js")
          val full   =
            if request.observed then
              histos.splice.Splice.fullObserved(linked, output): started =>
                JsBridge
                  .ready(JsPhase.MinifierStarted, request.result, request.output, Some(linked))
                  .map(_.copy(temporary = started.temporary.toString, descendant = Some(started.pid)))
                  .flatMap(JsBridge.checkpoint)
            else histos.splice.Splice.full(linked, output)
          full.mapError:
            case error: JsLinkError               => error
            case error: histos.splice.SpliceError => JsLinkError.Minifier(error.message)
    yield script

  private final case class Request(
      result: Path,
      output: Path,
      kind: JsModuleKind,
      optimize: JsOptimize,
      main: Option[String],
      arguments: List[String],
      classpath: List[Path],
      observed: Boolean,
  )

  private def checkpoint(request: Request, phase: JsPhase, script: Option[Path] = None): IO[JsLinkError, Unit] =
    if request.observed then JsBridge.ready(phase, request.result, request.output, script).flatMap(JsBridge.checkpoint)
    else ZIO.unit

  private def decode(args: List[String]): IO[JsLinkError, Request] = args match
    case result :: output :: kind :: mode :: entry :: main :: observed :: count :: rest =>
      for
        selectedKind <- ZIO.fromOption(JsModuleKind.values.find(_.token == kind)).orElseFail(JsLinkError.Bridge)
        selectedMode <- ZIO.fromOption(JsOptimize.values.find(_.token == mode)).orElseFail(JsLinkError.Bridge)
        size <- ZIO.fromOption(count.toIntOption.filter(n => n >= 0 && n <= rest.size)).orElseFail(JsLinkError.Bridge)
        mainClass <- entry match
          case "main" =>
            ZIO.fromEither(MainClass.from(main)).mapError(_ => JsLinkError.Bridge).map(value => Some(value.binary))
          case "exports" => ZIO.none
          case _         => ZIO.fail(JsLinkError.Bridge)
        paths <- ZIO
          .attempt((Path.of(result), Path.of(output), rest.drop(size).map(Path.of(_))))
          .mapError(_ => JsLinkError.Bridge)
        observation <- observed match
          case "observed" => ZIO.succeed(true)
          case "ordinary" => ZIO.succeed(false)
          case _          => ZIO.fail(JsLinkError.Bridge)
      yield Request(paths._1, paths._2, selectedKind, selectedMode, mainClass, rest.take(size), paths._3, observation)
    case _ => ZIO.fail(JsLinkError.Bridge)
end LinkMain
