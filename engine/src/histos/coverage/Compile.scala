package histos.coverage

import histos.resolve.{ResolvedArtifact, ToolId, ToolInput, ToolRole, ToolVariant}
import histos.{ScalaVersion, ToolRevision}

import java.nio.file.Path

/** The plugin jar, the compiler-classpath jars, and the test-runtime jar for one Scala 2.13 version. */
final case class CoveragePlugin(version: ScalaVersion, jar: Path, tools: List[ToolInput], runtime: Path)

/** What a coverage compile knows. Scala 3 leaves `plugins` empty. */
final case class CoverageCompile(
    excludes: Excludes,
    revision: ToolRevision,
    plugins: List[CoveragePlugin],
):
  def plugin(version: ScalaVersion): Option[CoveragePlugin] =
    plugins.find(_.version == version)

object CoveragePlugin:
  def from(version: ScalaVersion, artifacts: List[ResolvedArtifact]): Either[CellFault, CoveragePlugin] =
    val owned = artifacts.filter: artifact =>
      artifact.name.startsWith("scalac-scoverage-") &&
        !artifact.name.contains("_sjs") &&
        !artifact.name.contains("_native")
    val runtime = owned.filter(_.name.startsWith("scalac-scoverage-runtime_2.13"))
    val plugin  = owned.filter(_.name.startsWith("scalac-scoverage-plugin"))
    (plugin, runtime) match
      case (jar :: Nil, run :: Nil) =>
        val compiler = owned.filter(_.path != run.path)
        val id       = ToolId(ToolRole.Scoverage, version, ToolVariant.Jvm)
        Right(CoveragePlugin(version, jar.path, List(ToolInput(id, compiler)), run.path))
      case _ => Left(CellFault.Plugin(version))
