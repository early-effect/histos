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
        for
          repo    <- System.env("HISTOS_REPO")
          home    <- System.property("user.home")
          found   <- ZIO.attemptBlocking(runtimeJars(repo, home).find(path => Files.isRegularFile(path)))
          checked <- found match
            case None      => ZIO.succeed(assertTrue(found.nonEmpty))
            case Some(jar) =>
              for
                root <- temp
                _    <- ZIO.attempt {
                  val work = root.resolve("work")
                  Files.createDirectories(work.resolve(".histos"))
                  Files.writeString(
                    work.resolve(".histos").resolve("build.properties"),
                    "histos.version=0.0.2-SNAPSHOT\n",
                  )
                }
                ran      <- ZIO.attempt(exec(jar, root.resolve("work"), javaBin))
                names    <- ZIO.attempt(namesOf(jar))
                manifest <- ZIO.attempt(manifestOf(jar))
              yield
                val lines = ran.stdout.linesIterator.map(_.trim).filter(_.nonEmpty).toList
                assertTrue(
                  ran.finished,
                  ran.code == 0,
                  lines == List("histos 0.0.2-SNAPSHOT"),
                  names.contains("histos/cli/Main.class"),
                  !names.exists(_.startsWith("scala/scalanative/")),
                  manifest.contains("Main-Class: histos.cli.Main"),
                )
        yield checked
  )

  private def rooted(repo: Option[String], name: String): List[Path] =
    repo.filter(_.nonEmpty).map(Path.of(_)).toList.map(_.resolve(name)) ++ List(
      Path.of(name),
      Path.of("..").resolve(name),
    )

  /** `dist/histos.jar` when assembly wrote it, then the jar Coursier cached for this coordinate. */
  private def runtimeJars(repo: Option[String], home: Option[String]): List[Path] =
    rooted(repo, "dist/histos.jar") ++ coursierJars(home)

  private def coursierJars(home: Option[String]): List[Path] =
    val roots = home
      .filter(_.nonEmpty)
      .map(Path.of(_))
      .toList
      .flatMap: root =>
        List(
          root.resolve(".cache").resolve("coursier").resolve("v1"),
          root.resolve("Library").resolve("Caches").resolve("Coursier").resolve("v1"),
        )
    roots.flatMap: cache =>
      if !Files.isDirectory(cache) then Nil
      else
        val walk = Files.walk(cache)
        try
          walk.iterator.asScala
            .filter: path =>
              val name     = Option(path.getFileName).map(_.toString).getOrElse("")
              val launcher = Path.of("rocks", "earlyeffect", "histos").toString + java.io.File.separator
              Files.isRegularFile(path) && name.startsWith("histos-") && name.endsWith(".jar") &&
              !name.startsWith("histos-api") && !name.contains("-sources") && !name.contains("-javadoc") &&
              path.toString.contains(launcher)
            .toList
            .sortBy(path => -Files.getLastModifiedTime(path).toMillis)
        finally walk.close()

  private def javaBin: Path =
    Path.of(java.lang.System.getProperty("java.home")).resolve("bin").resolve("java")

  private def exec(jar: Path, work: Path, javaBin: Path): Ran =
    Files.createDirectories(work)
    val builder = new ProcessBuilder(javaBin.toString, Toolchain.NativeAccess, "-jar", jar.toString, "version")
    val out     = work.resolve("out")
    builder.directory(work.toFile)
    builder.redirectOutput(out.toFile)
    builder.redirectError(work.resolve("err").toFile)
    val process  = builder.start()
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
        case None        => ""
        case Some(entry) =>
          val in = zip.getInputStream(entry)
          try new String(in.readAllBytes(), utf8)
          finally in.close()
    finally zip.close()

  private def temp: ZIO[Scope, Throwable, Path] =
    ZIO.acquireRelease(ZIO.attempt(Files.createTempDirectory("histos-runtime"))): dir =>
      ZIO.attempt {
        if Files.exists(dir) then
          val walk  = Files.walk(dir)
          val paths =
            try walk.iterator.asScala.toList
            finally walk.close()
          paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
      }.ignore

  private final case class Ran(finished: Boolean, code: Int, stdout: String)

  def spec = checks @@ TestAspect.withLiveSystem @@ TestAspect.timeout(3.minutes)
end AssemblySpec
