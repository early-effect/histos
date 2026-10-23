package histos.testrun

import histos.pack.MainClass

import java.nio.file.Path

/** The child that loads an sbt `Framework`. The engine does not. */
object ProviderRunner:
  val Main: MainClass = MainClass.trusted("subprocess.Provider")

  /** The child uses this status when the framework discovers nothing. It is not `no tests`. */
  val NoneFound: Int = 2

  /** Suites were discovered, and none of them matched the pattern. */
  val NoneMatched: Int = 3

  /** Stable source. The framework, the class directory, and the patterns are `provider.txt` in the work directory. */
  def source: String =
    s"""package subprocess
       |
       |import sbt.testing._
       |
       |import java.util.concurrent.atomic.AtomicBoolean
       |
       |object Provider {
       |  def main(args: Array[String]): Unit = {
       |    val lines = java.nio.file.Files.readString(java.nio.file.Path.of(System.getenv("HISTOS_PROVIDER"))).split("\\n", -1)
       |    val frameworkName = if (lines.length > 0) lines(0) else ""
       |    val classes = java.nio.file.Path.of(if (lines.length > 1) lines(1) else "")
       |    val loader = getClass.getClassLoader
       |    val raw = Class.forName(frameworkName, true, loader).getDeclaredConstructor().newInstance()
       |    val framework = classOf[Framework].cast(raw)
       |    val discovered = discover(classes, framework.fingerprints, loader).filter(task => suiteAllowed(task.fullyQualifiedName()))
       |    if (discovered.isEmpty) System.exit(if (suitePatterns.length == 0) $NoneFound else $NoneMatched)
       |    val runner = framework.runner(Array.empty[String], Array.empty[String], loader)
       |    val failed = new AtomicBoolean(false)
       |    run(runner.tasks(discovered.toArray), failed)
       |    val summary = runner.done()
       |    if (summary != null && summary.nonEmpty) println(summary)
       |    if (failed.get) System.exit(1)
       |  }
       |
       |  private def run(tasks: Array[Task], failed: AtomicBoolean): Unit = {
       |    var index = 0
       |    while (index < tasks.length) {
       |      val more = tasks(index).execute(handler(failed), Array(console))
       |      if (more != null && more.nonEmpty) run(more, failed)
       |      index += 1
       |    }
       |  }
       |
       |  private def handler(failed: AtomicBoolean): EventHandler = new EventHandler {
       |    def handle(event: Event): Unit =
       |      event.status match {
       |        case Status.Failure | Status.Error | Status.Canceled => failed.set(true)
       |        case _                                              => ()
       |      }
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
       |  private val suitePatterns: Array[String] = {
       |    val lines = java.nio.file.Files.readString(java.nio.file.Path.of(System.getenv("HISTOS_PROVIDER"))).split("\\n", -1)
       |    if (lines.length <= 2) Array.empty[String]
       |    else lines.drop(2).filter(_.nonEmpty).map(compiled)
       |  }
       |
       |  private def compiled(pattern: String): String = {
       |    val body = pattern.flatMap {
       |      case '*' => ".*"
       |      case '?' => "."
       |      case c if "\\\\.+()[]{}^$$|".contains(c) => "\\\\" + c
       |      case c => c.toString
       |    }
       |    "^" + body + "$$"
       |  }
       |
       |  private def suiteAllowed(name: String): Boolean = {
       |    val shown = if (name.endsWith("$$") && !name.endsWith("$$$$")) name.substring(0, name.length - 1) else name
       |    if (suitePatterns.length == 0) true
       |    else suitePatterns.exists(pattern => shown.matches(pattern) || name.matches(pattern))
       |  }
       |
       |  private def discover(root: java.nio.file.Path, fingerprints: Array[Fingerprint], loader: ClassLoader): List[TaskDef] = {
       |    val names = classNames(root)
       |    fingerprints.toList.flatMap {
       |      case finger: SubclassFingerprint =>
       |        names.flatMap(name => matched(name, finger.isModule, finger.superclassName, loader).map { _ =>
       |          new TaskDef(name, finger, false, Array(new SuiteSelector))
       |        })
       |      case finger: AnnotatedFingerprint =>
       |        names.flatMap { name =>
       |          val runtime = if (finger.isModule) name + "$$" else name
       |          loaded(runtime, loader).filter(hasAnnotation(_, finger.annotationName, loader)).map { _ =>
       |            new TaskDef(name, finger, false, Array(new SuiteSelector))
       |          }
       |        }
       |      case _ => Nil
       |    }
       |  }
       |
       |  private def matched(name: String, module: Boolean, superclass: String, loader: ClassLoader): Option[Class[?]] =
       |    loaded(if (module) name + "$$" else name, loader).filter { cls =>
       |      loaded(superclass, loader).exists(parent => parent.isAssignableFrom(cls) && (cls != parent))
       |    }
       |
       |  private def hasAnnotation(cls: Class[?], annotation: String, loader: ClassLoader): Boolean =
       |    loaded(annotation, loader).exists(mark => cls.getAnnotations.exists(found => mark.isInstance(found)))
       |
       |  private def loaded(name: String, loader: ClassLoader): Option[Class[?]] =
       |    try Some(Class.forName(name, false, loader))
       |    catch { case _: ClassNotFoundException | _: NoClassDefFoundError => None }
       |
       |  private def classNames(root: java.nio.file.Path): List[String] = {
       |    if (!java.nio.file.Files.isDirectory(root)) Nil
       |    else {
       |      val walk = java.nio.file.Files.walk(root)
       |      val found = scala.collection.mutable.ListBuffer.empty[String]
       |      try {
       |        val iterator = walk.iterator()
       |        while (iterator.hasNext()) {
       |          val path = iterator.next()
       |          binary(root, path).foreach(found += _)
       |        }
       |      } finally walk.close()
       |      found.toList.sorted
       |    }
       |  }
       |
       |  private def binary(root: java.nio.file.Path, path: java.nio.file.Path): Option[String] = {
       |    if (!java.nio.file.Files.isRegularFile(path)) None
       |    else {
       |      val name = root.relativize(path).toString.replace('\\\\', '/').replace('/', '.')
       |      if (!name.endsWith(".class")) None
       |      else {
       |        val dropped = name.substring(0, name.length - ".class".length)
       |        if (dropped.isEmpty || dropped.contains("$$")) None else Some(dropped)
       |      }
       |    }
       |  }
       |}
       |""".stripMargin

  def note(framework: String, classes: Path, patterns: List[SuitePattern]): String =
    (framework :: classes.toAbsolutePath.toString :: patterns.map(_.value)).mkString("\n") + "\n"
