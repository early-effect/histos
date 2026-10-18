package histos.cli

import histos.jdk.Toolchain

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.*
import zio.test.*

object AssemblySpec extends ZIOSpecDefault:
  private val utf8 = StandardCharsets.UTF_8

  private val checks = suite("runtime assembly")(
    test("the runtime jar prints the version"):
      ZIO.scoped:
        val found = rooted("dist/histos.jar").find(path => Files.isRegularFile(path))
        found match
          case None => ZIO.succeed(assertTrue(found.nonEmpty))
          case Some(jar) =>
            for
              root <- temp
              ran <- ZIO.attempt(exec(jar, root.resolve("work"), javaBin))
              names <- ZIO.attempt(namesOf(jar))
              manifest <- ZIO.attempt(manifestOf(jar))
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
    ,
  )

  private def rooted(name: String): List[Path] =
    val root = sys.env.get("HISTOS_REPO").filter(_.nonEmpty).map(Path.of(_))
    root.toList.map(_.resolve(name)) ++ List(Path.of(name), Path.of("..").resolve(name))

  private def javaBin: Path =
    Path.of(java.lang.System.getProperty("java.home")).resolve("bin").resolve("java")

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
    val err = work.resolve("err")
    val errors = if Files.isRegularFile(err) then Files.readString(err) else ""
    Ran(finished, if finished then process.exitValue() else -1, output + errors)

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
