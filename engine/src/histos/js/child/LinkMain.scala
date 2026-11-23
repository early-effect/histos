package histos.js.child

import histos.js.{JsLinkError, Linker}
import histos.pack.MainClass
import histos.{JsModuleKind, JsOptimize}
import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, Path}

import zio.{IO, ZIO, ZIOAppArgs, ZIOAppDefault}

/** The ordinary Scala 3 linker and Splice run inside one owned process group. */
object LinkMain extends ZIOAppDefault:
  def run: ZIO[ZIOAppArgs, JsLinkError, Unit] =
    for
      args    <- getArgs
      request <- decode(args.toList)
      linked  <- Linker.link(
        request.classpath,
        request.output,
        request.kind,
        request.main,
        request.arguments,
        request.optimize,
      )
      script <- request.optimize match
        case JsOptimize.Fast => ZIO.succeed(linked)
        case JsOptimize.Full =>
          histos.splice.Splice
            .full(linked, request.output.resolveSibling("full.js"))
            .mapError(error => JsLinkError.Minifier(error.message))
      _ <- ZIO
        .attemptBlocking(Files.writeString(request.result, script.toAbsolutePath.toString))
        .mapError(_ => JsLinkError.Io(IoFailure(request.result, IoFault.Write)))
    yield ()

  private final case class Request(
      result: Path,
      output: Path,
      kind: JsModuleKind,
      optimize: JsOptimize,
      main: Option[String],
      arguments: List[String],
      classpath: List[Path],
  )

  private def decode(args: List[String]): IO[JsLinkError, Request] = args match
    case result :: output :: kind :: mode :: entry :: main :: count :: rest =>
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
      yield Request(paths._1, paths._2, selectedKind, selectedMode, mainClass, rest.take(size), paths._3)
    case _ => ZIO.fail(JsLinkError.Bridge)
end LinkMain
