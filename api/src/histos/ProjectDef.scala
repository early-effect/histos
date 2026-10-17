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
    testDependencies: List[Dep],
    dependsOn: List[ProjectDef],
    clang: ClangPin,
    test: Option[TestMain],
    position: SourcePos,
    provider: Option[TestProvider],
) extends ProjectDef:
  /** Compile classpath. These are the dependencies the published pom lists. */
  def libraryDependencies(deps: Dep*): ModuleDef =
    new ModuleDef(name, platforms, versions, dependencies ++ deps.toList, testDependencies, dependsOn, clang, test, position, provider)

  /** Test classpath only. Compile does not see them, and publish does not write them. */
  def testDependencies(deps: Dep*): ModuleDef =
    new ModuleDef(name, platforms, versions, dependencies, testDependencies ++ deps.toList, dependsOn, clang, test, position, provider)

  def dependsOn(projects: ProjectDef*): ModuleDef =
    new ModuleDef(name, platforms, versions, dependencies, testDependencies, dependsOn ++ projects.toList, clang, test, position, provider)

  /** The clang major this Native module must find on `PATH`. */
  def clang(major: ClangMajor): ModuleDef =
    new ModuleDef(name, platforms, versions, dependencies, testDependencies, dependsOn, ClangPin.Major(major), test, position, provider)

  /** Opt out of the test provider and run this main. */
  def test(main: TestMain): ModuleDef =
    new ModuleDef(name, platforms, versions, dependencies, testDependencies, dependsOn, clang, Some(main), position, provider)

  /** Run this framework in the child. `TestProvider.Zio` is the one `histos test` uses when this is unset. */
  def testProvider(provider: TestProvider): ModuleDef =
    new ModuleDef(name, platforms, versions, dependencies, testDependencies, dependsOn, clang, test, position, Some(provider))

object ModuleDef:
  /** Spliced by `module`. Source code outside this package cannot call it. */
  @publicInBinary private[histos] def make(
      name: String,
      platforms: Platforms,
      versions: ScalaVersions,
      position: SourcePos,
  ): ModuleDef =
    new ModuleDef(name, platforms, versions, Nil, Nil, Nil, ClangPin.Unused, None, position, None)

final case class AggregateDef private (
    name: String,
    children: List[ProjectDef],
    position: SourcePos,
) extends ProjectDef

object AggregateDef:
  /** Spliced by `aggregate`. Source code cannot call it. */
  @publicInBinary private[histos] def make(name: String, children: List[ProjectDef], position: SourcePos): AggregateDef =
    new AggregateDef(name, children, position)
