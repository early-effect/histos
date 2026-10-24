package histos.native

import histos.jdk.JdkRuntime
import histos.pack.MainClass
import histos.process.{ChildClasspath, ChildRequest}
import histos.testrun.SuitePattern
import histos.workspace.Workspace
import histos.zinc.{
  CompileError,
  CompileRequest,
  CompileToolchain,
  JvmCompile,

  ScalacFlags
}
import histos.Platform
import histos.resolve.{LockedTools, ResolveError}

import java.nio.file.Path

import zio.{IO, ZIO}

/** One native test-adapter run. The engine does not load `scala.scalanative`.
  */
final case class NativeSuiteRequest(
    toolchain: NativeToolchain,
    framework: String,
    binary: Path,
    classpath: List[Path],
    work: Path,
    jdk: JdkRuntime,
    processEnv: Map[String, String],
    patterns: List[SuitePattern] = Nil
)

/** Run a linked native test binary through
  * `scala.scalanative.testinterface.adapter.TestAdapter`. The engine does not
  * load `scala.scalanative`.
  */
object NativeSuite:
  private val SuiteMain: String = "subprocess.NativeSuite"

  def run(
      request: NativeSuiteRequest
  ): ZIO[LockedTools, NativeSuiteError, ChildRequest] =
    val NativeSuiteRequest(
      toolchain,
      framework,
      binary,
      classpath,
      work,
      jdk,
      processEnv,
      patterns
    ) = request
    val bridgeRoot = work.resolve("runner")
    val classes = bridgeRoot.resolve("classes")
    val source = bridgeRoot
      .resolve("src")
      .resolve("subprocess")
      .resolve("NativeSuite.scala")
    val shapes = work.resolve("shapes.txt")
    val child = work.resolve("runner-child")
    for
      found <- ClassShapes.read(classpath).mapError(NativeSuiteError.Shapes(_))
      view <- ZIO.service[LockedTools]
      tools <- view
        .materialize(
          histos.native.NativeRequests
            .suite(toolchain, view.catalog, view.repositories)
        )
        .mapError(NativeSuiteError.Resolved(_))
        .map(_.artifacts.map(_.path))
      _ <- write(shapes, encode(found))
      _ <- write(source, suite(patterns))
      _ <- JvmCompile
        .compile(
          CompileRequest(
            CompileToolchain(
              view.catalog.native.bridgeScala,
              Platform.Jvm,
              jdk,
              ScalacFlags.empty
            ),
            List(source),
            tools,
            classes,
            bridgeRoot.resolve("analysis"),
            Nil
          )
        )
        .mapError(NativeSuiteError.Compile(_))
      _ <- empty(child)
    yield ChildRequest(
      Platform.Jvm,
      jdk.home.resolve("bin").resolve("java"),
      ChildClasspath(classes, tools),
      MainClass.trusted(SuiteMain),
      child,
      processEnv ++ Map(
        "NATIVE_BINARY" -> binary.toAbsolutePath.toString,
        "NATIVE_FRAMEWORK" -> framework,
        "NATIVE_SHAPES" -> shapes.toAbsolutePath.toString
      )
    )

  private def encode(shapes: List[ClassShapes.Shape]): String =
    shapes
      .map(shape =>
        s"${shape.name}\t${if shape.module then "1" else "0"}\t${shape.parents.mkString(",")}"
      )
      .mkString("\n")

  private def suite(patterns: List[SuitePattern]): String =
    """package subprocess
      |
      |import java.nio.file.Files
      |import java.nio.file.Path
      |import java.util.concurrent.atomic.AtomicBoolean
      |
      |import scala.jdk.CollectionConverters.*
      |import scala.util.control.NonFatal
      |
      |import scala.scalanative.testinterface.adapter.TestAdapter
      |
      |import sbt.testing.*
      |
      |object NativeSuite:
      |  def main(args: Array[String]): Unit =
      |    val code =
      |      try body()
      |      catch
      |        case NonFatal(thrown) =>
      |          thrown.printStackTrace(System.err)
      |          1
      |    if code != 0 then System.exit(code)
      |
      |  private def body(): Int =
      |    val binary = sys.env.getOrElse("NATIVE_BINARY", "")
      |    val framework = sys.env.getOrElse("NATIVE_FRAMEWORK", "")
      |    val shapesPath = sys.env.getOrElse("NATIVE_SHAPES", "")
      |    if binary.isEmpty || framework.isEmpty || shapesPath.isEmpty then
      |      System.err.println("the native test runner was not given a binary, a framework, and shapes")
      |      1
      |    else
      |      val shapes = readShapes(Path.of(shapesPath))
      |      val config = TestAdapter.Config().withBinaryFile(new java.io.File(binary)).withEnvVars(sys.env.toMap)
      |      val adapter = new TestAdapter(config)
      |      try run(adapter, framework, shapes)
      |      finally adapter.close()
      |
      |  private def run(adapter: TestAdapter, framework: String, shapes: Array[Shape]): Int =
      |    adapter.loadFrameworks(List(List(framework))) match
      |      case Some(found) :: Nil =>
      |        val tasks = discover(shapes, found.fingerprints).filter(task => suiteAllowed(task.fullyQualifiedName()))
      |        if tasks.isEmpty then
      |          if suitePatterns.length == 0 then 2 else 3
      |        else
      |          val runner = found.runner(Array.empty[String], Array.empty[String], getClass.getClassLoader)
      |          val failed = new AtomicBoolean(false)
      |          walk(runner.tasks(tasks.toArray), failed)
      |          val summary = runner.done()
      |          if summary != null && summary.nonEmpty then println(summary)
      |          if failed.get then 1 else 0
      |      case _ =>
      |        System.err.println("the framework was not linked")
      |        2
      |
      |__SUITES__
      |  private def discover(shapes: Array[Shape], fingerprints: Array[Fingerprint]): List[TaskDef] =
      |    val byName = shapes.map(shape => shape.name -> shape).toMap
      |    shapes.toList.flatMap: shape =>
      |      fingerprints.toList.collect:
      |        case finger: SubclassFingerprint if matches(shape, finger, byName) =>
      |          new TaskDef(taskName(shape), finger, false, Array(new SuiteSelector))
      |
      |  private def matches(shape: Shape, finger: SubclassFingerprint, byName: Map[String, Shape]): Boolean =
      |    if finger.isModule() != shape.module then false
      |    else shape.parents.exists(parent => extendsParent(parent, finger.superclassName(), byName, Set(shape.name)))
      |
      |  private def extendsParent(name: String, parent: String, byName: Map[String, Shape], seen: Set[String]): Boolean =
      |    if name == parent then true
      |    else if seen.contains(name) then false
      |    else
      |      byName.get(name) match
      |        case None => false
      |        case Some(shape) => shape.parents.exists(next => extendsParent(next, parent, byName, seen + name))
      |
      |  private def taskName(shape: Shape): String =
      |    if shape.module && shape.name.endsWith("$") then shape.name.substring(0, shape.name.length - 1)
      |    else shape.name
      |
      |  private def readShapes(path: Path): Array[Shape] =
      |    Files.readAllLines(path).asScala.flatMap(shapeOf).toArray
      |
      |  private def shapeOf(line: String): Option[Shape] =
      |    if line.isEmpty then None
      |    else
      |      line.split("\t", -1) match
      |        case Array(name, flag, parents) if name.nonEmpty =>
      |          val listed = if parents.isEmpty then Array.empty[String] else parents.split(",", -1)
      |          Some(new Shape(name, flag == "1", listed))
      |        case _ => None
      |
      |  private def walk(tasks: Array[Task], failed: AtomicBoolean): Unit =
      |    var index = 0
      |    while index < tasks.length do
      |      val more = tasks(index).execute(handler(failed), Array(console))
      |      if more != null && more.nonEmpty then walk(more, failed)
      |      index += 1
      |
      |  private def handler(failed: AtomicBoolean): EventHandler = new EventHandler:
      |    def handle(event: Event): Unit =
      |      event.status match
      |        case Status.Failure | Status.Error | Status.Canceled => failed.set(true)
      |        case _                                              => ()
      |
      |  private val console: Logger = new Logger:
      |    def ansiCodesSupported(): Boolean = false
      |    def error(message: String): Unit = System.err.println(message)
      |    def warn(message: String): Unit = System.err.println(message)
      |    def info(message: String): Unit = System.out.println(message)
      |    def debug(message: String): Unit = System.out.println(message)
      |    def trace(thrown: Throwable): Unit = thrown.printStackTrace(System.err)
      |
      |final class Shape(val name: String, val module: Boolean, val parents: Array[String])
      |""".stripMargin.replace("__SUITES__", SuitePattern.embed(patterns))

  private def write(path: Path, text: String): IO[NativeSuiteError, Unit] =
    Workspace
      .writeString(path, text)
      .mapError(_ => NativeSuiteError.Write(path))

  private def empty(path: Path): IO[NativeSuiteError, Unit] =
    Workspace.recreate(path).mapError(_ => NativeSuiteError.Write(path)).unit

enum NativeSuiteError:
  case Resolved(error: ResolveError)
  case Unavailable
  case Shapes(error: ClassShapeError)
  case Compile(error: CompileError)
  case Write(path: Path)

  def message: String = this match
    case Resolved(error) => error.message
    case Unavailable     => "the Scala Native test runner could not be fetched"
    case Shapes(error)   => error.message
    case Compile(error)  => error.message
    case Write(path)     => s"could not write $path"
