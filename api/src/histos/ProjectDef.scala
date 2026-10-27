package histos

import _root_.scala.annotation.publicInBinary

import histos.plugin.{ModuleNote, NoteKey, NoteText, PluginId, RepoPath}

/** One or more platforms. The builder requires the first, so empty is not representable.
  */
final case class Platforms(first: Platform, rest: List[Platform]):
  def all: List[Platform] = first :: rest

/** One or more full Scala versions. The builder requires the first. */
final case class ScalaVersions(first: ScalaVersion, rest: List[ScalaVersion]):
  def all: List[ScalaVersion] = first :: rest

/** A project as written, before [[Projects]] checks names, duplicates, and cycles.
  */
sealed trait ProjectDef:
  def name: String
  def position: SourcePos

/** One upstream, before [[Projects]] checks it. No platforms means every platform of the depender.
  */
final case class ProjectEdge(
    project: ProjectDef,
    platforms: Option[List[Platform]],
)

/** A module as written. `P` is the platforms `module(...)` named, as a union of the case singleton types.
  *
  * Js settings exist only when [[Platform.Js]] is in `P`. Native settings exist only when [[Platform.Native]] is in
  * `P`. There is no `copy`: a caller cannot put a Js field on a JVM module by copying.
  */
sealed trait ModuleDef[P] extends ProjectDef:
  /** Compile classpath. These are the dependencies the published pom lists. */
  def libraryDependencies(deps: Dep*): ModuleDef[P]

  /** Test classpath only. Compile does not see them, and publish does not write them. */
  def testDependencies(deps: Dep*): ModuleDef[P]

  def dependsOn(projects: ProjectDef*): ModuleDef[P]

  /** These upstreams are on the classpath only for the named platforms. */
  def dependsOn(platform: Platform, rest: Platform*)(projects: ProjectDef*): ModuleDef[P]

  /** Opt out of the test provider and run this main. */
  def test(main: TestMain): ModuleDef[P]

  /** Run this framework in the child. `TestProvider.Zio` is the one `histos test` uses when this is unset. */
  def testProvider(provider: TestProvider): ModuleDef[P]

  /** Sources live in `<directory>/src` instead of `<name>/src`. */
  def in(directory: ProjectPath): ModuleDef[P]

  /** The class `histos run` starts. A module that does not name one is not linked. */
  def main(main: MainClass): ModuleDef[P]

  /** Apply a function of this same module. A plugin publishes `ModuleDef[P] => ModuleDef[P]`, so it cannot add a
    * platform.
    */
  def pipe(f: ModuleDef[P] => ModuleDef[P]): ModuleDef[P] =
    f(this)

  /** The published artifact id, without a Scala or platform suffix. A second call replaces it. */
  def artifact(name: ArtifactBase): ModuleDef[P]

  /** Publish the library jar. A second publication on this module is refused at load. */
  def library: ModuleDef[P]

  /** Publish an sbt plugin. `series` is the binary series: `sbt"2"` stages `name_sbt2_3` on Scala 3. */
  def sbtPlugin(series: SbtSeries): ModuleDef[P]

  /** Publish one JVM fat jar. The pom lists no dependencies. A second publication is refused at load. */
  def assembly(main: MainClass): ModuleDef[P]

  /** Publish the thin jar. The pom lists the classpath. A second publication is refused at load. */
  def application: ModuleDef[P]

  /** A file this assembly already produced. It is not a Maven artifact. A second file, or a file on a module that is
    * not an assembly, is refused at load.
    */
  def asset(file: RepoPath): ModuleDef[P]

  /** Replace this module's scalac flags. A second call replaces the list. It does not append. */
  def scalac(flag: ScalacFlag, rest: ScalacFlag*): ModuleDef[P]

  /** Leave plugin-owned text on this module. A repeated plugin and key is refused later. */
  def note(plugin: PluginId, key: NoteKey, value: NoteText): ModuleDef[P]

  private[histos] def rewriteJsVersion(version: JsVersion): ModuleDef[P]
  private[histos] def rewriteJsEnv(env: JsEnv): ModuleDef[P]
  private[histos] def rewriteJsModule(kind: JsModuleKind): ModuleDef[P]
  private[histos] def rewriteNativeVersion(version: NativeVersion): ModuleDef[P]
  private[histos] def rewriteClang(major: ClangMajor): ModuleDef[P]
end ModuleDef

object ModuleDef:
  /** Spliced by `module`. Source code outside this package cannot call it. */
  @publicInBinary private[histos] def make[P](
      name: String,
      platforms: Platforms,
      versions: ScalaVersions,
      position: SourcePos,
  ): ModuleDef[P] =
    new ModuleBody[P](
      name,
      platforms,
      versions,
      Nil,
      Nil,
      Nil,
      ClangPin.Unused,
      None,
      position,
      None,
      None,
      None,
      None,
      JsEnv.Node,
      JsModuleKind.NoModule,
      None,
      Nil,
      ScalacFlags.empty,
      None,
      Nil,
      Nil,
    )

  /** Node, jsdom, or Firefox. */
  extension [P](module: ModuleDef[P])(using Platform.Js.type <:< P)
    def jsVersion(version: JsVersion): ModuleDef[P] =
      module.rewriteJsVersion(version)

    def jsEnv(env: JsEnv): ModuleDef[P] =
      module.rewriteJsEnv(env)

    /** `NoModule` is the linker default. Ascent's ES modules name [[JsModuleKind.ESModule]]. */
    def jsModule(kind: JsModuleKind): ModuleDef[P] =
      module.rewriteJsModule(kind)
  end extension

  extension [P](module: ModuleDef[P])(using Platform.Native.type <:< P)
    /** The clang major this Native module must find on `PATH`. */
    def clang(major: ClangMajor): ModuleDef[P] =
      module.rewriteClang(major)

    /** The Scala Native toolchain this module links with. Unset means the engine default. */
    def nativeVersion(version: NativeVersion): ModuleDef[P] =
      module.rewriteNativeVersion(version)
  end extension
end ModuleDef

/** The stored module. Graph reads these fields. Callers see [[ModuleDef]], whose platform methods are the extensions.
  */
private[histos] final class ModuleBody[P](
    override val name: String,
    val platforms: Platforms,
    val versions: ScalaVersions,
    val dependencies: List[Dep],
    val testDependencies: List[Dep],
    val dependsOn: List[ProjectEdge],
    val clang: ClangPin,
    val test: Option[TestMain],
    override val position: SourcePos,
    val provider: Option[TestProvider],
    val nativeVersion: Option[NativeVersion],
    val jsVersion: Option[JsVersion],
    val directory: Option[ProjectPath],
    val jsEnv: JsEnv,
    val jsModule: JsModuleKind,
    val mainClass: Option[MainClass],
    val notes: List[ModuleNote],
    val flags: ScalacFlags,
    val published: Option[ArtifactBase],
    val publications: List[Publication],
    val assets: List[RepoPath],
) extends ModuleDef[P]:
  override def libraryDependencies(deps: Dep*): ModuleDef[P] =
    revise(dependencies = dependencies ++ deps.toList)

  override def testDependencies(deps: Dep*): ModuleDef[P] =
    revise(testDependencies = testDependencies ++ deps.toList)

  override def dependsOn(projects: ProjectDef*): ModuleDef[P] =
    add(projects.toList.map(project => ProjectEdge(project, None)))

  override def dependsOn(platform: Platform, rest: Platform*)(projects: ProjectDef*): ModuleDef[P] =
    val chosen = (platform :: rest.toList).distinct
    add(projects.toList.map(project => ProjectEdge(project, Some(chosen))))

  override def test(main: TestMain): ModuleDef[P] =
    revise(tested = Some(main))

  override def testProvider(provider: TestProvider): ModuleDef[P] =
    revise(provider = Some(provider))

  override def in(directory: ProjectPath): ModuleDef[P] =
    revise(directory = Some(directory))

  override def main(main: MainClass): ModuleDef[P] =
    revise(mainClass = Some(main))

  override def artifact(name: ArtifactBase): ModuleDef[P] =
    revise(published = Some(name))

  override def library: ModuleDef[P] =
    revise(publications = publications :+ Publication.Library)

  override def sbtPlugin(series: SbtSeries): ModuleDef[P] =
    revise(publications = publications :+ Publication.SbtPlugin(series))

  override def assembly(main: MainClass): ModuleDef[P] =
    revise(publications = publications :+ Publication.Assembly(main))

  override def application: ModuleDef[P] =
    revise(publications = publications :+ Publication.Application)

  override def asset(file: RepoPath): ModuleDef[P] =
    revise(assets = assets :+ file)

  override def scalac(flag: ScalacFlag, rest: ScalacFlag*): ModuleDef[P] =
    revise(flags = ScalacFlags.of(flag, rest*))

  override def note(plugin: PluginId, key: NoteKey, value: NoteText): ModuleDef[P] =
    revise(notes = notes :+ ModuleNote(plugin, key, value))

  override private[histos] def rewriteJsVersion(version: JsVersion): ModuleDef[P] =
    revise(jsVersion = Some(version))

  override private[histos] def rewriteJsEnv(env: JsEnv): ModuleDef[P] =
    revise(jsEnv = env)

  override private[histos] def rewriteJsModule(kind: JsModuleKind): ModuleDef[P] =
    revise(jsModule = kind)

  override private[histos] def rewriteNativeVersion(version: NativeVersion): ModuleDef[P] =
    revise(nativeVersion = Some(version))

  override private[histos] def rewriteClang(major: ClangMajor): ModuleDef[P] =
    revise(clang = ClangPin.Major(major))

  private def add(edges: List[ProjectEdge]): ModuleDef[P] =
    revise(edges = dependsOn ++ edges)

  private def revise(
      dependencies: List[Dep] = this.dependencies,
      testDependencies: List[Dep] = this.testDependencies,
      edges: List[ProjectEdge] = this.dependsOn,
      clang: ClangPin = this.clang,
      tested: Option[TestMain] = this.test,
      provider: Option[TestProvider] = this.provider,
      nativeVersion: Option[NativeVersion] = this.nativeVersion,
      jsVersion: Option[JsVersion] = this.jsVersion,
      directory: Option[ProjectPath] = this.directory,
      jsEnv: JsEnv = this.jsEnv,
      jsModule: JsModuleKind = this.jsModule,
      mainClass: Option[MainClass] = this.mainClass,
      notes: List[ModuleNote] = this.notes,
      flags: ScalacFlags = this.flags,
      published: Option[ArtifactBase] = this.published,
      publications: List[Publication] = this.publications,
      assets: List[RepoPath] = this.assets,
  ): ModuleDef[P] =
    new ModuleBody[P](
      name,
      platforms,
      versions,
      dependencies,
      testDependencies,
      edges,
      clang,
      tested,
      position,
      provider,
      nativeVersion,
      jsVersion,
      directory,
      jsEnv,
      jsModule,
      mainClass,
      notes,
      flags,
      published,
      publications,
      assets,
    )
end ModuleBody

final case class AggregateDef private (
    name: String,
    children: List[ModuleDef[?]],
    position: SourcePos,
) extends ProjectDef

object AggregateDef:
  /** Spliced by `aggregate`. Source code cannot call it. */
  @publicInBinary private[histos] def make(
      name: String,
      children: List[ModuleDef[?]],
      position: SourcePos,
  ): AggregateDef =
    new AggregateDef(name, children, position)
