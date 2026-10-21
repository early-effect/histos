package histos

import _root_.scala.annotation.publicInBinary

import histos.plugin.{ModuleNote, NoteKey, NoteText, PluginId}

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

/** One upstream, before [[Projects]] checks it. No platforms means every platform of the depender. */
final case class ProjectEdge(project: ProjectDef, platforms: Option[List[Platform]])

final case class ModuleDef private (
    name: String,
    platforms: Platforms,
    versions: ScalaVersions,
    dependencies: List[Dep],
    testDependencies: List[Dep],
    dependsOn: List[ProjectEdge],
    clang: ClangPin,
    test: Option[TestMain],
    position: SourcePos,
    provider: Option[TestProvider],
    nativeVersion: Option[NativeVersion],
    directory: Option[ProjectPath],
    jsEnv: JsEnv = JsEnv.Node,
    jsModule: JsModuleKind = JsModuleKind.NoModule,
    application: Option[MainClass] = None,
    notes: List[ModuleNote] = Nil,
    flags: ScalacFlags = ScalacFlags.empty,
    published: Option[ArtifactBase] = None,
) extends ProjectDef:
  /** Compile classpath. These are the dependencies the published pom lists. */
  def libraryDependencies(deps: Dep*): ModuleDef =
    copy(dependencies = dependencies ++ deps.toList)

  /** Test classpath only. Compile does not see them, and publish does not write them. */
  def testDependencies(deps: Dep*): ModuleDef =
    copy(testDependencies = testDependencies ++ deps.toList)

  def dependsOn(projects: ProjectDef*): ModuleDef =
    add(projects.toList.map(project => ProjectEdge(project, None)))

  /** These upstreams are on the classpath only for the named platforms. */
  def dependsOn(platform: Platform, rest: Platform*)(projects: ProjectDef*): ModuleDef =
    val chosen = (platform :: rest.toList).distinct
    add(projects.toList.map(project => ProjectEdge(project, Some(chosen))))

  private def add(edges: List[ProjectEdge]): ModuleDef =
    copy(dependsOn = dependsOn ++ edges)

  /** The clang major this Native module must find on `PATH`. */
  def clang(major: ClangMajor): ModuleDef =
    copy(clang = ClangPin.Major(major))

  /** Opt out of the test provider and run this main. */
  def test(main: TestMain): ModuleDef =
    copy(test = Some(main))

  /** Run this framework in the child. `TestProvider.Zio` is the one `histos test` uses when this is unset. */
  def testProvider(provider: TestProvider): ModuleDef =
    copy(provider = Some(provider))

  /** The Scala Native toolchain this module links with. Unset means the engine default. */
  def nativeVersion(version: NativeVersion): ModuleDef =
    copy(nativeVersion = Some(version))

  /** Sources live in `<directory>/src` instead of `<name>/src`. */
  def in(directory: ProjectPath): ModuleDef =
    copy(directory = Some(directory))

  /** Node, jsdom, or Firefox. A module that does not target Js stays on Node. */
  def jsEnv(env: JsEnv): ModuleDef =
    copy(jsEnv = env)

  /** `NoModule` is the linker default. Ascent's ES modules name [[JsModuleKind.ESModule]]. */
  def jsModule(kind: JsModuleKind): ModuleDef =
    copy(jsModule = kind)

  /** The class `histos run` starts. A module that does not name one is not linked. */
  def main(main: MainClass): ModuleDef =
    copy(application = Some(main))

  /** Apply a function this module already has. A plugin publishes `ModuleDef => ModuleDef`. */
  def pipe(f: ModuleDef => ModuleDef): ModuleDef =
    f(this)

  /** The published artifact id, without a Scala or platform suffix. A second call replaces it. */
  def artifact(name: ArtifactBase): ModuleDef =
    copy(published = Some(name))

  /** Replace this module's scalac flags. A second call replaces the list. It does not append. */
  def scalac(flag: ScalacFlag, rest: ScalacFlag*): ModuleDef =
    copy(flags = ScalacFlags.of(flag, rest*))

  /** Leave plugin-owned text on this module. A repeated plugin and key is refused later. */
  def note(plugin: PluginId, key: NoteKey, value: NoteText): ModuleDef =
    copy(notes = notes :+ ModuleNote(plugin, key, value))

object ModuleDef:
  /** Spliced by `module`. Source code outside this package cannot call it. */
  @publicInBinary private[histos] def make(
      name: String,
      platforms: Platforms,
      versions: ScalaVersions,
      position: SourcePos,
  ): ModuleDef =
    new ModuleDef(name, platforms, versions, Nil, Nil, Nil, ClangPin.Unused, None, position, None, None, None)

final case class AggregateDef private (
    name: String,
    children: List[ModuleDef],
    position: SourcePos,
) extends ProjectDef

object AggregateDef:
  /** Spliced by `aggregate`. Source code cannot call it. */
  @publicInBinary private[histos] def make(name: String, children: List[ModuleDef], position: SourcePos): AggregateDef =
    new AggregateDef(name, children, position)
