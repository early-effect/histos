package histos.coverage

import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

/** Compiler arguments and the compile-key text for one coverage cell.
  *
  * Scala 3 instruments in the compiler. Scala 2.13 uses the scoverage plugin. The exclusion separator is the one that
  * series accepts: a comma for Scala 3, a semicolon for the plugin.
  */
object Flags:
  def scala3(dataDir: Path, excludes: Excludes): List[String] =
    List(s"-coverage-out:${dataDir.toAbsolutePath.normalize}") ++
      joined("-coverage-exclude-classlikes:", ",", excludes.classlikes) ++
      joined("-coverage-exclude-files:", ",", excludes.files)

  def scala2(dataDir: Path, sourceRoot: Path, plugin: Path, excludes: Excludes): List[String] =
    List(
      s"-Xplugin:${plugin.toAbsolutePath.normalize}",
      s"-P:scoverage:dataDir:${dataDir.toAbsolutePath.normalize}",
      s"-P:scoverage:sourceRoot:${sourceRoot.toAbsolutePath.normalize}",
    ) ++
      joined("-P:scoverage:excludedPackages:", ";", excludes.classlikes) ++
      joined("-P:scoverage:excludedFiles:", ";", excludes.files) ++
      List("-Yrangepos")

  /** The row a coverage compile adds. A normal compile does not write it. The last line is the plugin revision. Scala 3
    * passes an empty revision because the compiler instruments itself.
    */
  def identity(
      series: String,
      dataDir: Path,
      sourceRoot: Path,
      excludes: Excludes,
      revision: String,
  ): String =
    List(
      series,
      dataDir.toAbsolutePath.normalize.toString,
      sourceRoot.toAbsolutePath.normalize.toString,
      excludes.classlikes.mkString(","),
      excludes.files.mkString(","),
      revision,
    ).mkString("\n")

  def classesDir(repo: Path, module: ProjectName, version: ScalaVersion, platform: Platform): Path =
    root(repo, "classes", module, version, platform)

  def analysisDir(repo: Path, module: ProjectName, version: ScalaVersion, platform: Platform): Path =
    root(repo, "analysis", module, version, platform)

  private def root(
      repo: Path,
      kind: String,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  ): Path =
    repo
      .resolve(".histos")
      .resolve("coverage")
      .resolve(kind)
      .resolve(module.value)
      .resolve(version.value)
      .resolve(
        platform.token
      )

  private def joined(prefix: String, separator: String, values: List[String]): List[String] =
    if values.isEmpty then Nil
    else List(prefix + values.mkString(separator))
end Flags
