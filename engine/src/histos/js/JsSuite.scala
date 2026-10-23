package histos.js

import histos.node.NodeBinary
import histos.pack.MainClass
import histos.process.{ChildClasspath, ChildRequest}
import histos.testrun.SuitePattern
import histos.zinc.{CompileError, CompileRequest, CompileToolchain, JvmCompile, ResolvedJdk, ScalacFlags}
import histos.{JsEnv, JsModuleKind, ScalaVersion}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/**
 * Link the Scala.js test bridge and run `org.scalajs.testing.adapter.TestAdapter` in a child JVM.
 * The engine does not load `org.scalajs.linker` or a `JSEnv`.
 */
object JsSuite:
  private val BridgeScala: ScalaVersion = ScalaVersion.trusted("2.13.18")
  private val FirefoxScala: ScalaVersion = ScalaVersion.trusted("3.9.0")
  private val Linker: String = "1.22.0"
  private val SuiteMain: String = "subprocess.JsSuite"
  private val BridgeJar: String = "scalajs-test-bridge_2.13"

  def run(
      version: ScalaVersion,
      env: JsEnv,
      kind: JsModuleKind,
      framework: String,
      classpath: List[Path],
      work: Path,
      jdk: ResolvedJdk,
      processEnv: Map[String, String],
      node: NodeBinary,
      patterns: List[SuitePattern] = Nil,
  ): IO[JsSuiteError, ChildRequest] =
    val output = work.resolve("out")
    val bridgeRoot = work.resolve("bridge")
    val classes = bridgeRoot.resolve("classes")
    val source = bridgeRoot.resolve("src").resolve("subprocess").resolve("JsSuite.scala")
    val listed = work.resolve("classpath.txt")
    val child = work.resolve("child")
    for
      libraries <- ScalaJs.libraries(version).mapError(JsSuiteError.Library(_))
      tools <- ZIO.attemptBlockingInterrupt(fetch(artifacts(env))).mapError(_ => JsSuiteError.Unavailable).flatMap(ZIO.fromEither)
      bridge <- ZIO.attemptBlockingInterrupt(fetch(List(Artifact("org.scala-js", BridgeJar, Linker)), transitive = false))
        .mapError(_ => JsSuiteError.Unavailable)
        .flatMap(ZIO.fromEither)
      _ <- write(listed, (classpath ++ libraries ++ bridge).map(_.toAbsolutePath.toString).mkString("\n"))
      _ <- write(source, suite(env, patterns))
      _ <- JvmCompile.compile(CompileRequest(
             CompileToolchain(compiler(env), histos.Platform.Jvm, jdk, ScalacFlags.empty),
             List(source),
             tools,
             classes,
             bridgeRoot.resolve("analysis"),
             Nil,
           )).mapError(JsSuiteError.Library(_))
      _ <- empty(output)
      _ <- empty(child)
    yield ChildRequest(
      histos.Platform.Jvm,
      jdk.home.resolve("bin").resolve("java"),
      ChildClasspath(classes, tools),
      MainClass.trusted(SuiteMain),
      child,
      processEnv ++ JsRun.nodeEnv(env, node) ++ Map(
        "JS_OUT" -> output.toAbsolutePath.toString,
        "JS_CLASSPATH" -> listed.toAbsolutePath.toString,
        "JS_FRAMEWORK" -> framework,
        "JS_MODULE" -> kind.token,
      ),
    )

  private def compiler(env: JsEnv): ScalaVersion = env match
    case JsEnv.Firefox => FirefoxScala
    case JsEnv.Node | JsEnv.Jsdom => BridgeScala

  private def artifacts(env: JsEnv): List[Artifact] = env match
    case JsEnv.Node =>
      List(
        Artifact("org.scala-js", "scalajs-linker_2.13", Linker),
        Artifact("org.scala-js", "scalajs-sbt-test-adapter_2.13", Linker),
        Artifact("org.scala-js", "scalajs-env-nodejs_2.13", "1.6.0"),
      )
    case JsEnv.Jsdom =>
      List(
        Artifact("org.scala-js", "scalajs-linker_2.13", Linker),
        Artifact("org.scala-js", "scalajs-sbt-test-adapter_2.13", Linker),
        Artifact("org.scala-js", "scalajs-env-jsdom-nodejs_2.13", "1.1.1"),
      )
    case JsEnv.Firefox =>
      List(
        Artifact("org.scala-js", "scalajs-linker_3", Linker),
        Artifact("org.scala-js", "scalajs-sbt-test-adapter_3", Linker),
        Artifact("rocks.earlyeffect", "chekhov-jsenv_3", "0.1.3"),
      )

  /** Chekhov rejects `RunConfig` env. Node and jsdom take the child environment, which is only `PATH`. */
  private def useRunEnv(env: JsEnv): Boolean = env match
    case JsEnv.Firefox        => false
    case JsEnv.Node | JsEnv.Jsdom => true

  private def suite(env: JsEnv, patterns: List[SuitePattern]): String =
    """package subprocess
      |
      |import java.nio.file.Files
      |import java.nio.file.Path
      |import java.util.concurrent.atomic.AtomicBoolean
      |
      |import scala.jdk.CollectionConverters._
      |
      |import scala.concurrent.Await
      |import scala.concurrent.duration.Duration
      |import scala.concurrent.ExecutionContext.Implicits.global
      |import scala.util.control.NonFatal
      |
      |import org.scalajs.ir.ClassKind
      |import org.scalajs.jsenv.JSEnv
      |import org.scalajs.linker.PathIRContainer
      |import org.scalajs.linker.PathOutputDirectory
      |import org.scalajs.linker.StandardImpl
      |import org.scalajs.linker.interface.ModuleInitializer
      |import org.scalajs.linker.interface.ModuleKind
      |import org.scalajs.linker.interface.StandardConfig
      |import org.scalajs.linker.interface.unstable.IRContainerImpl
      |import org.scalajs.linker.interface.unstable.IRFileImpl
      |import org.scalajs.logging.Level
      |import org.scalajs.logging.{Logger => JsLogger}
      |import org.scalajs.testing.adapter.TestAdapter
      |
      |import sbt.testing._
      |
      |object JsSuite {
      |  def main(args: Array[String]): Unit = {
      |    val code = try body() catch { case NonFatal(thrown) =>
      |      thrown.printStackTrace(System.err)
      |      1
      |    }
      |    if (code != 0) System.exit(code)
      |  }
      |
      |  private def body(): Int = {
      |    val output = Path.of(sys.env("JS_OUT"))
      |    val framework = sys.env("JS_FRAMEWORK")
      |    val kind = sys.env.getOrElse("JS_MODULE", "nomodule")
      |    val classpath = Files.readAllLines(Path.of(sys.env("JS_CLASSPATH"))).asScala.map(Path.of(_)).toSeq
      |    Files.createDirectories(output)
      |    val containers = Await.result(PathIRContainer.fromClasspath(classpath), Duration.Inf)._1
      |    val shapes = readShapes(containers)
      |    val cached = Await.result(StandardImpl.irFileCache().newCache.cached(containers), Duration.Inf)
      |    val init = Seq(ModuleInitializer.mainMethod("org.scalajs.testing.bridge.Bridge", "start"))
      |    val report = Await.result(StandardImpl.linker(config(kind)).link(cached, init, PathOutputDirectory(output), logger), Duration.Inf)
      |    report.publicModules.headOption match {
      |      case None =>
      |        System.err.println("the linker wrote no module")
      |        1
      |      case Some(module) =>
      |        val script = output.resolve(module.jsFileName)
      |        val jsEnv: JSEnv = __ENV__
      |        val adapterConfig = if (__USE_ENV__) TestAdapter.Config().withEnv(sys.env.toMap) else TestAdapter.Config()
        val adapter = new TestAdapter(jsEnv, Seq(input(kind, script)), adapterConfig)
      |        try run(adapter, framework, shapes) finally adapter.close()
      |    }
      |  }
      |
      |  private def run(adapter: TestAdapter, framework: String, shapes: Array[Shape]): Int = {
      |    adapter.loadFrameworks(List(List(framework))) match {
      |      case Some(found) :: Nil =>
      |        val tasks = discover(shapes, found.fingerprints).filter(task => suiteAllowed(task.fullyQualifiedName()))
      |        if (tasks.isEmpty) { if (suitePatterns.length == 0) 2 else 3 }
      |        else {
      |          val runner = found.runner(Array.empty[String], Array.empty[String], getClass.getClassLoader)
      |          val failed = new AtomicBoolean(false)
      |          walk(runner.tasks(tasks.toArray), failed)
      |          val summary = runner.done()
      |          if (summary != null && summary.nonEmpty) println(summary)
      |          if (failed.get) 1 else 0
      |        }
      |      case _ =>
      |        System.err.println("the framework was not linked")
      |        2
      |    }
      |  }
      |
      |  private def config(kind: String): StandardConfig = {
      |    val base = StandardConfig().withOptimizer(false).withSourceMap(false).withBatchMode(true)
      |    kind match {
      |      case "esmodule" => base.withModuleKind(ModuleKind.ESModule)
      |      case "commonjs" => base.withModuleKind(ModuleKind.CommonJSModule)
      |      case _          => base
      |    }
      |  }
      |
      |  private def input(kind: String, script: Path): org.scalajs.jsenv.Input = kind match {
      |    case "esmodule" => org.scalajs.jsenv.Input.ESModule(script)
      |    case "commonjs" => org.scalajs.jsenv.Input.CommonJSModule(script)
      |    case _          => org.scalajs.jsenv.Input.Script(script)
      |  }
      |
      |__SUITES__
      |  private def discover(shapes: Array[Shape], fingerprints: Array[Fingerprint]): List[TaskDef] = {
      |    val byName = shapes.map(shape => (shape.name, shape)).toMap
      |    shapes.toList.flatMap { shape =>
      |      fingerprints.toList.collect {
      |        case finger: SubclassFingerprint if matches(shape, finger, byName) =>
      |          new TaskDef(taskName(shape), finger, false, Array(new SuiteSelector))
      |      }
      |    }
      |  }
      |
      |  private def matches(shape: Shape, finger: SubclassFingerprint, byName: Map[String, Shape]): Boolean = {
      |    if (finger.isModule() != shape.module) false
      |    else shape.parents.exists(parent => extendsParent(parent, finger.superclassName(), byName, Set(shape.name)))
      |  }
      |
      |  private def extendsParent(name: String, parent: String, byName: Map[String, Shape], seen: Set[String]): Boolean = {
      |    if (name == parent) true
      |    else if (seen.contains(name)) false
      |    else byName.get(name) match {
      |      case None => false
      |      case Some(shape) => shape.parents.exists(next => extendsParent(next, parent, byName, seen + name))
      |    }
      |  }
      |
      |  private def taskName(shape: Shape): String =
      |    if (shape.module && shape.name.endsWith("$")) shape.name.substring(0, shape.name.length - 1)
      |    else shape.name
      |
      |  private def readShapes(containers: Seq[org.scalajs.linker.interface.IRContainer]): Array[Shape] = {
      |    val files = containers.flatMap(container => Await.result(IRContainerImpl.fromIRContainer(container).sjsirFiles, Duration.Inf))
      |    files.map { file =>
      |      val tree = Await.result(IRFileImpl.fromIRFile(file).tree, Duration.Inf)
      |      val parents = (tree.superClass.toList ++ tree.interfaces).map(_.name.nameString).toArray
      |      new Shape(tree.className.nameString, tree.kind == ClassKind.ModuleClass, parents)
      |    }.toArray
      |  }
      |
      |  private def walk(tasks: Array[Task], failed: AtomicBoolean): Unit = {
      |    var index = 0
      |    while (index < tasks.length) {
      |      val more = tasks(index).execute(handler(failed), Array(console))
      |      if (more != null && more.nonEmpty) walk(more, failed)
      |      index += 1
      |    }
      |  }
      |
      |  private def handler(failed: AtomicBoolean): EventHandler = new EventHandler {
      |    def handle(event: Event): Unit = event.status match {
      |      case Status.Failure | Status.Error | Status.Canceled => failed.set(true)
      |      case _                                              => ()
      |    }
      |  }
      |
      |  private val console: Logger = new Logger {
      |    def ansiCodesSupported(): Boolean = false
      |    def error(message: String): Unit = System.err.println(message)
      |    def warn(message: String): Unit = System.err.println(message)
      |    def info(message: String): Unit = System.out.println(message)
      |    def debug(message: String): Unit = System.out.println(message)
      |    def trace(thrown: Throwable): Unit = thrown.printStackTrace(System.err)
      |  }
      |
      |  private val logger = new JsLogger {
      |    def log(level: Level, message: => String): Unit =
      |      if (level == Level.Error) System.err.println(message)
      |    def trace(thrown: => Throwable): Unit = thrown.printStackTrace(System.err)
      |  }
      |}
      |
      |final class Shape(val name: String, val module: Boolean, val parents: Array[String])
      |""".stripMargin.replace("__ENV__", JsRun.envExpr(env)).replace("__USE_ENV__", if useRunEnv(env) then "true" else "false").replace("__SUITES__", SuitePattern.embed(patterns))

  private final case class Artifact(organization: String, name: String, revision: String)

  private def fetch(wanted: List[Artifact], transitive: Boolean = true): Either[JsSuiteError, List[Path]] =
    val dependencies = wanted.map: item =>
      val base = coursier.Dependency(
        coursier.Module(coursier.Organization(item.organization), coursier.ModuleName(item.name)),
        coursier.version.VersionConstraint(item.revision),
      )
      if transitive then base else base.copy(transitive = false)
    val got = coursier.Fetch(coursier.cache.FileCache())(using coursier.util.Task.sync)
      .noMirrors
      .withGradleModuleSupport(false)
      .withRepositories(Seq(coursier.Repositories.central))
      .addDependencies(dependencies*)
      .eitherResult()
    got match
      case Left(_) => Left(JsSuiteError.Unavailable)
      case Right(result) =>
        val jars = result.detailedArtifacts0.toList.flatMap: (_, publication, _, file) =>
          val classified = publication match
            case Right(published) => published.classifier.nonEmpty
            case Left(variant)    => variant.classifier.nonEmpty
          if file.getName.endsWith(".jar") && !classified then List(file.toPath) else Nil
        if jars.isEmpty then Left(JsSuiteError.Unavailable) else Right(jars)

  private def write(path: Path, text: String): IO[JsSuiteError, Unit] =
    ZIO.attemptBlocking {
      Option(path.getParent).foreach(parent => Files.createDirectories(parent))
      Files.writeString(path, text, StandardCharsets.UTF_8)
      ()
    }.mapError(_ => JsSuiteError.Run(s"could not write $path"))

  private def empty(path: Path): IO[JsSuiteError, Unit] =
    ZIO.attemptBlocking {
      if Files.exists(path) then
        val walk = Files.walk(path)
        val paths =
          try walk.iterator.asScala.toList
          finally walk.close()
        paths.sortBy(_.getNameCount).reverse.foreach(file => Files.deleteIfExists(file))
      Files.createDirectories(path)
      ()
    }.mapError(_ => JsSuiteError.Run(s"could not prepare $path"))

enum JsSuiteError:
  case Unavailable
  case Library(error: CompileError)
  case Run(text: String)

  def message: String = this match
    case Unavailable    => "the Scala.js test adapter could not be fetched"
    case Library(error) => error.message
    case Run(text)      => text
