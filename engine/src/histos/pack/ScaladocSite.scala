package histos.pack

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile

import scala.jdk.CollectionConverters.*

import zio.ZIO

/** The scaladoc site sbt packs into `-javadoc.jar`. A non-zero exit fails. Warnings fail only when the caller asks, so
  * a publish matches sbt and the CI check can be stricter.
  */
object ScaladocSite:
  def render(
      classes: Path,
      classpath: List[Path],
      project: String,
      scalaVersion: String,
      output: Path,
      failOnWarning: Boolean,
  ): ZIO[Any, ScaladocFault, Unit] =
    val home = Option(System.getProperty("java.home")).filter(_.nonEmpty)
    home match
      case Some(javaHome) =>
        toolClasspath(scalaVersion).flatMap: running =>
          val separator = java.io.File.pathSeparator
          val command   = List(
            Path.of(javaHome, "bin", "java").toString,
            "-cp",
            running + separator + classpath.mkString(separator),
            "dotty.tools.scaladoc.Main",
            "-d",
            output.toString,
            "-project",
            project,
            classes.toString,
          )
          ZIO
            .scoped {
              ZIO
                .acquireRelease(
                  ZIO
                    .attemptBlocking {
                      recreate(output)
                      new ProcessBuilder(command.asJava)
                        .redirectErrorStream(true)
                        .start()
                    }
                    .mapError(error => ScaladocFault.Reported(fault(error)))
                )(process => ZIO.attemptBlocking(process.destroyForcibly()).ignore)
                .flatMap: process =>
                  ZIO
                    .attemptBlocking {
                      val text = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
                      val code = process.waitFor()
                      (code, text)
                    }
                    .mapError(error => ScaladocFault.Reported(fault(error)))
            }
            .flatMap: (code, text) =>
              if code != 0 || (failOnWarning && noticed(text)) then ZIO.fail(ScaladocFault.Reported(shown(text, code)))
              else ZIO.unit
      case None => ZIO.fail(ScaladocFault.Reported("the running JVM did not name java.home"))

  /** The assembly jar merges two Scala libraries, so `scala.caps` is both an object and a package. Scaladoc then runs
    * from the published tool jars instead of that jar.
    */
  private def toolClasspath(scalaVersion: String): ZIO[Any, ScaladocFault, String] =
    val running = Option(System.getProperty("java.class.path")).filter(_.nonEmpty)
    running match
      case Some(classpath) if !mergedScala(classpath) => ZIO.succeed(classpath)
      case _                                          =>
        if !scalaVersion.startsWith("3.") then
          ZIO.fail(ScaladocFault.Reported(s"scaladoc for Scala $scalaVersion is not available"))
        else fetchTool(scalaVersion)

  private def mergedScala(classpath: String): Boolean =
    classpath.split(java.io.File.pathSeparator).exists(mergedJar)

  private def mergedJar(raw: String): Boolean =
    val path = Path.of(raw)
    if !Files.isRegularFile(path) then false
    else
      val zip = new ZipFile(path.toFile)
      try
        val names = zip.entries.asScala.map(_.getName).toList
        names.contains("scala/caps.class") && names.exists(_.startsWith("scala/caps/"))
      finally zip.close()

  private def fetchTool(scalaVersion: String): ZIO[Any, ScaladocFault, String] =
    ZIO
      .attemptBlocking {
        val cache = coursier.cache
          .FileCache()
          .copy(cachePolicies =
            Seq(
              coursier.cache.CachePolicy.LocalOnly,
              coursier.cache.CachePolicy.FetchMissing,
            )
          )
        val fetch = coursier
          .Fetch(cache)(using coursier.util.Task.sync)
          .addDependencies(
            coursier.Dependency(
              coursier.Module(coursier.Organization("org.scala-lang"), coursier.ModuleName("scaladoc_3")),
              coursier.version.VersionConstraint(scalaVersion),
            )
          )
        fetch.run().map(_.toPath).mkString(java.io.File.pathSeparator)
      }
      .mapError(error => ScaladocFault.Reported(fault(error)))

  private val Ansi = "\u001b\\[[0-9;]*m".r

  private def noticed(text: String): Boolean =
    Ansi
      .replaceAllIn(text, "")
      .linesIterator
      .exists: line =>
        val words = line.trim
        words.matches("(?i)[1-9][0-9]* warnings? found.*") ||
        words.matches("(?i)[1-9][0-9]* errors? found.*")

  private def shown(text: String, code: Int): String =
    val body = text.trim
    if body.nonEmpty then body else s"scaladoc exited $code"

  private def recreate(output: Path): Unit =
    if Files.exists(output) then
      val walk  = Files.walk(output)
      val paths =
        try walk.iterator.asScala.toList
        finally walk.close()
      paths.sortBy(_.getNameCount).reverse.foreach(path => Files.deleteIfExists(path))
    Files.createDirectories(output)

  private def fault(error: Throwable): String =
    Option(error.getMessage).getOrElse(error.getClass.getName)
end ScaladocSite

enum ScaladocFault:
  case Reported(detail: String)

  def text: String = this match
    case Reported(detail) => detail
end ScaladocFault
