package histos.cli

import histos.jdk.Toolchain
import histos.pack.{Assembly, AssemblyError, MainClass}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object AssemblySpec extends ZIOSpecDefault:
  private val mainFile = Path.of("histos", "cli", "Main.class")
  private val utf8 = StandardCharsets.UTF_8

  private val checks = suite("runtime assembly")(
    test("the runtime jar prints the version"):
      ZIO.scoped:
        for
          classpath <- System.property("java.class.path")
          home <- System.property("java.home")
          verdict <- (classpath, home) match
            case (Some(text), Some(value)) => run(text, Path.of(value, "bin", "java"))
            case _                          => ZIO.succeed(assertTrue(classpath.isDefined && home.isDefined))
        yield verdict
    ,
  )

  private def run(classpath: String, javaBin: Path): ZIO[Scope, Throwable | AssemblyError, TestResult] =
    val entries = split(classpath)
    val withMain = entries.filter(path => Files.isDirectory(path) && Files.isRegularFile(path.resolve(mainFile)))
    val mainOnly = withMain.filterNot(path => Files.isRegularFile(path.resolve("histos").resolve("cli").resolve("BootstrapSpec.class")))
    val chosen = if mainOnly.nonEmpty then mainOnly else withMain
    val jars = entries.filter(runtimeJar)
    chosen match
      case directory :: Nil =>
        for
          root <- temp
          assembled <- Assembly.write(List(directory), jars, MainClass.trusted("histos.cli.Main"), root.resolve("histos.jar"))
          ran <- ZIO.attempt(exec(assembled.path, root.resolve("work"), javaBin))
          names <- ZIO.attempt(namesOf(assembled.path))
          manifest <- ZIO.attempt(manifestOf(assembled.path))
        yield
          val lines = ran.output.linesIterator.map(_.trim).filter(_.nonEmpty).toList
          assertTrue(
            ran.finished,
            ran.code == 0,
            lines == List(Cli.VersionText),
            names.contains("histos/cli/Main.class"),
            !names.exists(_.startsWith("scala/scalanative/")),
            manifest.contains("Main-Class: histos.cli.Main"),
          )
      case other =>
        ZIO.succeed(assertTrue(other.map(_.toString) == List("the classes directory that contains histos.cli.Main")))

  private def runtimeJar(path: Path): Boolean =
    Files.isRegularFile(path) && path.getFileName.toString.endsWith(".jar") && !excluded(path)

  private def excluded(path: Path): Boolean =
    val text = path.toString
    text.contains("zio-test") ||
      text.contains("test-interface") ||
      text.contains("scala-native") ||
      text.contains("org.scala-native")

  private def split(classpath: String): List[Path] =
    classpath.split(java.io.File.pathSeparator).toList.filter(_.nonEmpty).map(Path.of(_))

  private def exec(jar: Path, work: Path, javaBin: Path): Ran =
    Files.createDirectories(work)
    val builder = new ProcessBuilder(javaBin.toString, Toolchain.NativeAccess, "-jar", jar.toString, "version")
    val out = work.resolve("out")
    builder.directory(work.toFile)
    builder.redirectOutput(out.toFile)
    builder.redirectError(work.resolve("err").toFile)
    val process = builder.start()
    val finished = process.waitFor(120, TimeUnit.SECONDS)
    if !finished then process.destroyForcibly()
    val output = if Files.isRegularFile(out) then Files.readString(out) else ""
    Ran(finished, if finished then process.exitValue() else -1, output)

  private def namesOf(path: Path): Set[String] =
    val zip = new ZipFile(path.toFile)
    try zip.entries.asScala.map(_.getName).toSet
    finally zip.close()

  private def manifestOf(path: Path): String =
    val zip = new ZipFile(path.toFile)
    try
      Option(zip.getEntry("META-INF/MANIFEST.MF")) match
        case None => ""
        case Some(entry) =>
          val in = zip.getInputStream(entry)
          try new String(in.readAllBytes(), utf8)
          finally in.close()
    finally zip.close()

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-runtime"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  private final case class Ran(finished: Boolean, code: Int, output: String)

  def spec = checks @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes)
