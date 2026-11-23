package histos.js

import histos.{JsModuleKind, JsOptimize}

import java.nio.file.{Files, Path}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

import org.scalajs.linker.interface.{ModuleInitializer, ModuleKind, StandardConfig}
import org.scalajs.linker.{PathIRContainer, PathOutputDirectory, StandardImpl}
import org.scalajs.logging.{Level, Logger}

/** Links `.sjsir` in this process. The linker is `scalajs-linker_3`, the same Scala histos runs.
  */
object Linker:
  def link(
      classpath: List[Path],
      output: Path,
      kind: JsModuleKind,
      main: Option[String],
      arguments: List[String],
      optimize: JsOptimize,
  ): Either[String, Path] =
    try
      Files.createDirectories(output)
      val optimized = optimize == JsOptimize.Full
      val base      = StandardConfig().withOptimizer(optimized).withSourceMap(false).withBatchMode(true)
      val config    = kind match
        case JsModuleKind.ESModule => base.withModuleKind(ModuleKind.ESModule)
        case JsModuleKind.CommonJS => base.withModuleKind(ModuleKind.CommonJSModule)
        case JsModuleKind.NoModule => base
      val linker     = StandardImpl.linker(config)
      val containers = Await.result(PathIRContainer.fromClasspath(classpath), Duration.Inf)._1
      val ir         = Await.result(StandardImpl.irFileCache().newCache.cached(containers), Duration.Inf)
      val init       = main.toList.map: name =>
        if arguments.isEmpty then ModuleInitializer.mainMethodWithArgs(name, "main")
        else ModuleInitializer.mainMethodWithArgs(name, "main", arguments)
      val report = Await.result(linker.link(ir, init, PathOutputDirectory(output), logger), Duration.Inf)
      report.publicModules.headOption match
        case Some(module) => Right(output.resolve(module.jsFileName))
        case None         => Left("the linker wrote no module")
    catch
      case NonFatal(thrown) =>
        Left(Option(thrown.getMessage).getOrElse(thrown.getClass.getName))

  private val logger: Logger = new Logger:
    def log(level: Level, message: => String): Unit =
      if level == Level.Error then System.err.println(message)
    def trace(thrown: => Throwable): Unit = thrown.printStackTrace(System.err)
end Linker
