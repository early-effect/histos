package histos.js.child

import histos.testrun.{SuiteMatch, SuitePattern, TestFilter}

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import org.scalajs.ir.ClassKind
import org.scalajs.jsenv.{Input, JSEnv}
import org.scalajs.linker.interface.{ModuleInitializer, ModuleKind, StandardConfig}
import org.scalajs.linker.interface.unstable.{IRContainerImpl, IRFileImpl}
import org.scalajs.linker.{PathIRContainer, PathOutputDirectory, StandardImpl}
import org.scalajs.logging.{Level, Logger as JsLogger}
import org.scalajs.testing.adapter.TestAdapter

import sbt.testing.*

/** Links the Scala.js test bridge and runs one test framework. Patterns and runner arguments arrive as environment
  * variables.
  */
object SuiteMain:
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
    val framework = sys.env("JS_FRAMEWORK")
    val kind      = sys.env.getOrElse("JS_MODULE", "nomodule")
    val classpath = Files.readAllLines(Path.of(sys.env("JS_CLASSPATH"))).asScala.map(Path.of(_)).toSeq
    Files.createDirectories(output)
    val containers = Await.result(PathIRContainer.fromClasspath(classpath), Duration.Inf)._1
    val shapes     = readShapes(containers)
    val cached     = Await.result(StandardImpl.irFileCache().newCache.cached(containers), Duration.Inf)
    val init       = Seq(ModuleInitializer.mainMethod("org.scalajs.testing.bridge.Bridge", "start"))
    val report     = Await.result(
      StandardImpl.linker(config(kind)).link(cached, init, PathOutputDirectory(output), logger),
      Duration.Inf,
    )
    report.publicModules.headOption match
      case None =>
        System.err.println("the linker wrote no module")
        1
      case Some(module) =>
        val script        = output.resolve(module.jsFileName)
        val adapterConfig =
          if useRunEnv then TestAdapter.Config().withEnv(sys.env.toMap) else TestAdapter.Config()
        val adapter = new TestAdapter(jsEnv, Seq(input(kind, script)), adapterConfig)
        try run(adapter, framework, shapes)
        finally adapter.close()

  private def useRunEnv: Boolean =
    sys.env.getOrElse("JS_ENV", "node") != "firefox"

  private def run(adapter: TestAdapter, framework: String, shapes: Array[Shape]): Int =
    adapter.loadFrameworks(List(List(framework))) match
      case Some(found) :: Nil =>
        val tasks = discover(shapes, found.fingerprints).filter(task => allowed(task.fullyQualifiedName))
        if tasks.isEmpty then if patterns.isEmpty then 2 else 3
        else
          val runner = found.runner(runnerArgs, Array.empty[String], getClass.getClassLoader)
          val failed = new AtomicBoolean(false)
          walk(runner.tasks(tasks.toArray), failed)
          val summary = runner.done()
          if summary != null && summary.nonEmpty then println(summary)
          if failed.get then 1 else 0
      case _ =>
        System.err.println("the framework was not linked")
        2

  private def config(kind: String): StandardConfig =
    val base = StandardConfig().withOptimizer(false).withSourceMap(false).withBatchMode(true)
    kind match
      case "esmodule" => base.withModuleKind(ModuleKind.ESModule)
      case "commonjs" => base.withModuleKind(ModuleKind.CommonJSModule)
      case _          => base

  private def input(kind: String, script: Path): Input =
    kind match
      case "esmodule" => Input.ESModule(script)
      case "commonjs" => Input.CommonJSModule(script)
      case _          => Input.Script(script)

  private def runnerArgs: Array[String] =
    val countText = sys.env.getOrElse(TestFilter.ArgCount, "")
    val count     = if countText.isEmpty then 0 else Integer.parseInt(countText)
    Array.tabulate(count)(index => sys.env.getOrElse(s"${TestFilter.ArgPrefix}$index", ""))

  private def patterns: List[SuitePattern] =
    sys.env
      .get("JS_PATTERNS")
      .toList
      .flatMap: raw =>
        raw.split("\n", -1).toList.filter(_.nonEmpty).flatMap(text => SuitePattern.from(text).toOption)

  private def allowed(name: String): Boolean =
    SuiteMatch.allows(patterns, name)

  private def discover(shapes: Array[Shape], fingerprints: Array[Fingerprint]): List[TaskDef] =
    val byName = shapes.map(shape => shape.name -> shape).toMap
    shapes.toList.flatMap: shape =>
      fingerprints.toList.collect:
        case finger: SubclassFingerprint if matches(shape, finger, byName) =>
          new TaskDef(taskName(shape), finger, false, Array(new SuiteSelector))

  private def matches(shape: Shape, finger: SubclassFingerprint, byName: Map[String, Shape]): Boolean =
    if finger.isModule != shape.module then false
    else shape.parents.exists(parent => extendsParent(parent, finger.superclassName, byName, Set(shape.name)))

  private def extendsParent(name: String, parent: String, byName: Map[String, Shape], seen: Set[String]): Boolean =
    if name == parent then true
    else if seen.contains(name) then false
    else
      byName.get(name) match
        case None        => false
        case Some(shape) => shape.parents.exists(next => extendsParent(next, parent, byName, seen + name))

  private def taskName(shape: Shape): String =
    if shape.module && shape.name.endsWith("$") then shape.name.dropRight(1) else shape.name

  private def readShapes(containers: Seq[org.scalajs.linker.interface.IRContainer]): Array[Shape] =
    val files = containers.flatMap: container =>
      Await.result(IRContainerImpl.fromIRContainer(container).sjsirFiles, Duration.Inf)
    files
      .map: file =>
        val tree    = Await.result(IRFileImpl.fromIRFile(file).tree, Duration.Inf)
        val parents = (tree.superClass.toList ++ tree.interfaces).map(_.name.nameString).toArray
        Shape(tree.className.nameString, tree.kind == ClassKind.ModuleClass, parents)
      .toArray

  private def walk(tasks: Array[Task], failed: AtomicBoolean): Unit =
    tasks.foreach: task =>
      val more = task.execute(handler(failed), Array(console))
      if more != null && more.nonEmpty then walk(more, failed)

  private def handler(failed: AtomicBoolean): EventHandler = new EventHandler:
    def handle(event: Event): Unit =
      event.status match
        case Status.Failure | Status.Error | Status.Canceled => failed.set(true)
        case _                                               => ()

  private val console: Logger = new Logger:
    def ansiCodesSupported(): Boolean  = false
    def error(message: String): Unit   = System.err.println(message)
    def warn(message: String): Unit    = System.err.println(message)
    def info(message: String): Unit    = System.out.println(message)
    def debug(message: String): Unit   = System.out.println(message)
    def trace(thrown: Throwable): Unit = thrown.printStackTrace(System.err)

  private val logger: JsLogger = new JsLogger:
    def log(level: Level, message: => String): Unit =
      if level == Level.Error then System.err.println(message)
    def trace(thrown: => Throwable): Unit = thrown.printStackTrace(System.err)

  private def jsEnv: JSEnv =
    sys.env.getOrElse("JS_ENV", "node") match
      case "firefox" =>
        new chekhov.jsenv.ChekhovJSEnv(chekhov.ChekhovBrowser.Firefox, true, false, Map.empty)
      case "jsdom" =>
        throw new IllegalArgumentException("jsdom is not a supported JavaScript environment")
      case _ =>
        val config = org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withExecutable(sys.env("JS_NODE")).withSourceMap(false)
        new org.scalajs.jsenv.nodejs.NodeJSEnv(config)
end SuiteMain

final case class Shape(name: String, module: Boolean, parents: Array[String])
