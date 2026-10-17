package histos.resolve

import histos.{Dep, DepKind, Platform, ScalaVersion}

/** Platform suffixes from design section 5.2. The fetch uses these values. */
object PlatformSuffix:
  val Js: String     = "_sjs1"
  val Native: String = "_native0.5"

/** The published artifact name. The Scala suffix is applied here, not when a `Dep` is parsed. */
object ArtifactName:
  /** The name Histos publishes for one of its modules. Binary suffix, same rule as a `::` dependency. */
  def published(module: histos.ProjectName, version: ScalaVersion, platform: Platform): String =
    s"${module.value}${platformSuffix(platform)}_${binary(version)}"

  def of(dependency: Dep, version: ScalaVersion, platform: Platform): String =
    dependency.kind match
      case DepKind.Java   => dependency.name
      case DepKind.Binary => s"${dependency.name}${platformSuffix(platform)}_${binary(version)}"
      case DepKind.Full   => s"${dependency.name}${platformSuffix(platform)}_${version.value}"

  private def platformSuffix(platform: Platform): String =
    platform match
      case Platform.Jvm    => ""
      case Platform.Js     => PlatformSuffix.Js
      case Platform.Native => PlatformSuffix.Native

  /** Scala 3 publishes `_3`. Scala 2 publishes `_2.13` (major.minor). */
  private def binary(version: ScalaVersion): String =
    version.value.split("\\.", 3) match
      case Array("3", _, _)       => "3"
      case Array(major, minor, _) => s"$major.$minor"
      case _                      => version.value
