package histos.coverage

import histos.zinc.ScalaSeries
import histos.resolve.ToolInput
import histos.{Module, Platform, ProjectGraph, ProjectName, ScalaVersion, ToolRevision}

import java.nio.file.Path

/** A cell the report names and does not instrument. */
final case class Skipped(module: ProjectName, version: ScalaVersion, platform: Platform)

/** One instrumented compile: the arguments, the key text, the directories, and the compiler-classpath jars. */
final case class Instrumented(
    argv: List[String],
    identity: String,
    classes: Path,
    analysis: Path,
    tools: List[ToolInput],
)

/** Why a cell could not be instrumented. */
enum CellFault:
  case Plugin(version: ScalaVersion)
  case Unmeasured(version: ScalaVersion, platform: Platform)

  def message: String = this match
    case Plugin(version)               => s"the scoverage plugin for ${version.value} could not be selected"
    case Unmeasured(version, platform) => s"coverage does not instrument ${version.value} on ${platform.token}"
end CellFault

object Cell:
  def measures(version: ScalaVersion, platform: Platform): Boolean =
    platform == Platform.Jvm && (ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala3) | Some(ScalaSeries.Scala2(13)) => true
      case _                                                       => false)

  def series(version: ScalaVersion): String =
    ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala3)        => "3"
      case Some(ScalaSeries.Scala2(minor)) => s"2.$minor"
      case None                            => version.value

  /** Scala 3 instruments in the compiler, so the plugin revision is empty. Scala 2.13 records it. */
  def revision(version: ScalaVersion, plugin: ToolRevision): String =
    ScalaSeries.of(version) match
      case Some(ScalaSeries.Scala2(13)) => plugin.value
      case _                            => ""

  def versions(graph: ProjectGraph): Set[String] =
    graph.projects
      .collect { case module: Module => module }
      .flatMap(_.versions.all)
      .filter(version => measures(version, Platform.Jvm))
      .map(_.value)
      .toSet

  def skipped(modules: List[Module]): List[Skipped] =
    modules.flatMap: module =>
      module.versions.all.flatMap: version =>
        module.platforms.all.flatMap: platform =>
          if measures(version, platform) then Nil
          else List(Skipped(module.name, version, platform))

  def instrumented(
      repo: Path,
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      spec: CoverageCompile,
  ): Either[CellFault, Instrumented] =
    if !measures(version, platform) then Left(CellFault.Unmeasured(version, platform))
    else
      val classes  = Flags.classesDir(repo, module, version, platform)
      val analysis = Flags.analysisDir(repo, module, version, platform)
      val text     = Flags.identity(series(version), classes, repo, spec.excludes, revision(version, spec.revision))
      ScalaSeries.of(version) match
        case Some(ScalaSeries.Scala3) =>
          Right(Instrumented(Flags.scala3(classes, spec.excludes), text, classes, analysis, Nil))
        case Some(ScalaSeries.Scala2(13)) =>
          spec.plugin(version) match
            case Some(plugin) =>
              Right(
                Instrumented(
                  Flags.scala2(classes, repo, plugin.jar, spec.excludes),
                  text,
                  classes,
                  analysis,
                  plugin.tools,
                )
              )
            case None => Left(CellFault.Plugin(version))
        case _ => Left(CellFault.Unmeasured(version, platform))
end Cell
