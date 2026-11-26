package histos.resolve

import histos.{Dep, DepKind, Module, Platform, ProjectGraph, ProjectName, Publication, SbtSeries, ScalaVersion}
import histos.version.ShipName

/** Platform suffixes from design section 5.2. The fetch uses these values. */
object PlatformSuffix:
  val Js: String     = "_sjs1"
  val Native: String = "_native0.5"

/** The published artifact name. The Scala suffix is applied here, not when a `Dep` is parsed.
  */
object ArtifactName:
  /** The name Histos publishes for one of its modules. Binary suffix, same rule as a `::` dependency.
    */
  def published(
      base: String,
      version: ScalaVersion,
      platform: Platform,
  ): String =
    s"$base${platformSuffix(platform)}_${binary(version)}"

  /** sbt 2 on Scala 3 is `name_sbt2_3`. The platform suffix stays in front of the series, as sbt writes it. */
  def plugin(
      base: String,
      version: ScalaVersion,
      platform: Platform,
      series: SbtSeries,
  ): String =
    s"$base${platformSuffix(platform)}_sbt${series.value}_${binary(version)}"

  /** The unsuffixed name a module publishes under.
    *
    * An explicit `.artifact` wins. Otherwise the ship that owns the module supplies the name: the member whose name is
    * the ship name publishes as that name, a name that already starts with `ship-` stays, and every other member
    * publishes as `ship-module`. A module in no ship publishes as its name.
    */
  def base(module: Module, ship: Option[ShipName]): String =
    module.published.fold(implied(module.name.value, ship))(_.value)

  /** The ship that lists this module, if any. A module is in at most one ship. */
  def owning(graph: ProjectGraph, name: ProjectName): Option[ShipName] =
    graph.catalog.ships.collectFirst:
      case ship if ship.members.contains(name) => ship.name

  /** The artifact id a module publishes under. An assembly keeps its bare base. */
  def module(module: Module, version: ScalaVersion, platform: Platform, ship: Option[ShipName]): String =
    val root = base(module, ship)
    module.publication match
      case Publication.Assembly(_)       => root
      case Publication.SbtPlugin(series) => plugin(root, version, platform, series)
      case Publication.Library           => published(root, version, platform)
      case Publication.Application       => published(root, version, platform)

  private def implied(moduleName: String, ship: Option[ShipName]): String =
    ship match
      case None       => moduleName
      case Some(name) =>
        val family = name.value
        if moduleName == family || moduleName.startsWith(s"$family-") then moduleName
        else s"$family-$moduleName"

  def of(dependency: Dep, version: ScalaVersion, platform: Platform): String =
    dependency.kind match
      case DepKind.Java   => dependency.name
      case DepKind.Binary =>
        dependency.sbtSeries match
          case Some(series) => plugin(dependency.name, version, platform, series)
          case None         => published(dependency.name, version, platform)
      case DepKind.Full =>
        s"${dependency.name}${platformSuffix(platform)}_${version.value}"

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
end ArtifactName
