package histos

import _root_.scala.annotation.publicInBinary

/** One or more platforms. The builder requires the first, so empty is not representable. */
final case class Platforms(first: Platform, rest: List[Platform]):
  def all: List[Platform] = first :: rest

/** One or more full Scala versions. The builder requires the first. */
final case class ScalaVersions(first: ScalaVersion, rest: List[ScalaVersion]):
  def all: List[ScalaVersion] = first :: rest

/** A project as written, before [[Projects]] checks names, duplicates, and cycles. */
sealed trait ProjectDef:
  def name: String
  def position: SourcePos

final case class ModuleDef private (
    name: String,
    platforms: Platforms,
    versions: ScalaVersions,
    dependencies: List[Dep],
    dependsOn: List[ProjectDef],
    position: SourcePos,
) extends ProjectDef:
  def libraryDependencies(deps: Dep*): ModuleDef =
    new ModuleDef(name, platforms, versions, dependencies ++ deps.toList, dependsOn, position)

  def dependsOn(projects: ProjectDef*): ModuleDef =
    new ModuleDef(name, platforms, versions, dependencies, dependsOn ++ projects.toList, position)

object ModuleDef:
  /** Spliced by `module`. Source code outside this package cannot call it. */
  @publicInBinary private[histos] def make(
      name: String,
      platforms: Platforms,
      versions: ScalaVersions,
      position: SourcePos,
  ): ModuleDef =
    new ModuleDef(name, platforms, versions, Nil, Nil, position)

final case class AggregateDef private (
    name: String,
    children: List[ProjectDef],
    position: SourcePos,
) extends ProjectDef

object AggregateDef:
  /** Spliced by `aggregate`. Source code cannot call it. */
  @publicInBinary private[histos] def make(name: String, children: List[ProjectDef], position: SourcePos): AggregateDef =
    new AggregateDef(name, children, position)
