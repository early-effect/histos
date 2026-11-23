package histos.js

import histos.{JsModuleKind, JsOptimize}
import histos.workspace.{IoFailure, IoFault}

import java.nio.file.{Files, Path}

import org.scalajs.linker.interface.{ModuleInitializer, ModuleKind, StandardConfig}
import org.scalajs.linker.{PathIRContainer, PathOutputDirectory, StandardImpl}
import org.scalajs.logging.{Level, Logger}

import zio.{IO, ZIO}

/** Foreign Future adapter, used only inside the owned link child. Killing that process cancels physical work. */
private[js] object Linker:
  def link(
      classpath: List[Path],
      output: Path,
      kind: JsModuleKind,
      main: Option[String],
      arguments: List[String],
      optimize: JsOptimize,
  ): IO[JsLinkError, Path] =
    val base   = StandardConfig().withOptimizer(optimize == JsOptimize.Full).withSourceMap(false).withBatchMode(true)
    val config = kind match
      case JsModuleKind.ESModule => base.withModuleKind(ModuleKind.ESModule)
      case JsModuleKind.CommonJS => base.withModuleKind(ModuleKind.CommonJSModule)
      case JsModuleKind.NoModule => base
    val init = main.toList.map: name =>
      if arguments.isEmpty then ModuleInitializer.mainMethodWithArgs(name, "main")
      else ModuleInitializer.mainMethodWithArgs(name, "main", arguments)
    ZIO.scoped:
      for
        _ <- ZIO
          .attemptBlocking(Files.createDirectories(output))
          .mapError(_ => JsLinkError.Io(IoFailure(output, IoFault.Write)))
        linker <- ZIO.attempt(StandardImpl.linker(config)).mapError(failed)
        cache  <- ZIO.acquireRelease(ZIO.attempt(StandardImpl.irFileCache().newCache).mapError(failed))(cache =>
          ZIO.attempt(cache.free()).ignore
        )
        containers <- ZIO.fromFuture(ec => PathIRContainer.fromClasspath(classpath)(using ec)).mapError(failed)
        ir         <- ZIO.fromFuture(ec => cache.cached(containers._1)(using ec)).mapError(failed)
        report     <- ZIO
          .fromFuture(ec => linker.link(ir, init, PathOutputDirectory(output), logger)(using ec))
          .mapError(failed)
        script <- ZIO
          .fromOption(report.publicModules.headOption)
          .orElseFail(JsLinkError.Linker("the linker wrote no module"))
      yield output.resolve(script.moduleFileName)

  private def failed(error: Throwable): JsLinkError =
    JsLinkError.Linker(Option(error.getMessage).getOrElse(error.getClass.getName))

  private val logger: Logger = new Logger:
    def log(level: Level, message: => String): Unit =
      if level == Level.Error then System.err.println(message)
    def trace(thrown: => Throwable): Unit = thrown.printStackTrace(System.err)
end Linker
