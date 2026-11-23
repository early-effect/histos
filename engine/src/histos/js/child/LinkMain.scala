package histos.js.child

import java.nio.file.{Files, Path}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import org.scalajs.linker.interface.{ModuleInitializer, ModuleKind, StandardConfig}
import org.scalajs.linker.{PathIRContainer, PathOutputDirectory, StandardImpl}
import org.scalajs.logging.{Level, Logger}

/** Links `.sjsir` into one script. Settings arrive as environment variables. There is no generated source.
  */
object LinkMain:
  def main(args: Array[String]): Unit =
    val code =
      try body()
      catch
        case NonFatal(thrown) =>
          thrown.printStackTrace(System.err)
          1
    if code != 0 then System.exit(code)

  private def body(): Int =
    val output    = Path.of(sys.env("JS_OUT"))
    val mainClass = sys.env.get("JS_MAIN").map(_.trim).filter(_.nonEmpty)
    val classpath = Files.readAllLines(Path.of(sys.env("JS_CLASSPATH"))).asScala.map(Path.of(_)).toSeq
    Files.createDirectories(output)
    val optimize   = sys.env.get("JS_OPTIMIZE").contains("true")
    val base       = StandardConfig().withOptimizer(optimize).withSourceMap(false).withBatchMode(true)
    val config     = moduleKind(sys.env.getOrElse("JS_MODULE", "nomodule"), base)
    val linker     = StandardImpl.linker(config)
    val containers = Await.result(PathIRContainer.fromClasspath(classpath), Duration.Inf)._1
    val ir         = Await.result(StandardImpl.irFileCache().newCache.cached(containers), Duration.Inf)
    val arguments  = userArgs
    val init       = mainClass.toSeq.map: name =>
      if arguments.isEmpty then ModuleInitializer.mainMethodWithArgs(name, "main")
      else ModuleInitializer.mainMethodWithArgs(name, "main", arguments)
    val report = Await.result(linker.link(ir, init, PathOutputDirectory(output), logger), Duration.Inf)
    report.publicModules.headOption match
      case Some(module) =>
        Files.writeString(Path.of("js.path"), output.resolve(module.jsFileName).toAbsolutePath.toString)
        0
      case None =>
        System.err.println("the linker wrote no module")
        1

  private def moduleKind(kind: String, base: StandardConfig): StandardConfig =
    kind match
      case "esmodule" => base.withModuleKind(ModuleKind.ESModule)
      case "commonjs" => base.withModuleKind(ModuleKind.CommonJSModule)
      case _          => base

  private def userArgs: List[String] =
    sys.env.get("JS_ARGS") match
      case None          => Nil
      case Some(encoded) =>
        encoded.split("\n", -1).toList match
          case countText :: rest =>
            val count = scala.util.Try(countText.toInt).toOption.getOrElse(0)
            rest
              .take(count)
              .map: line =>
                new String(java.util.Base64.getDecoder.decode(line), java.nio.charset.StandardCharsets.UTF_8)
          case Nil => Nil

  private val logger: Logger = new Logger:
    def log(level: Level, message: => String): Unit =
      if level == Level.Error then System.err.println(message)
    def trace(thrown: => Throwable): Unit = thrown.printStackTrace(System.err)
end LinkMain
