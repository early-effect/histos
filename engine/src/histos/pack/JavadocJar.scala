package histos.pack

import java.nio.file.Path

import zio.IO

/** The javadoc jar Central requires on a release. The bytes are the scaladoc site of that module. */
object JavadocJar:
  def write(
      classes: Path,
      classpath: List[Path],
      project: String,
      scalaVersion: String,
      site: Path,
      destination: Path,
  ): IO[JarError, Jarred] =
    ScaladocSite
      .render(classes, classpath, project, scalaVersion, site, failOnWarning = false)
      .mapError(fault => JarError.Scaladoc(fault.text))
      .flatMap(_ => ClassJar.write(site, destination))
