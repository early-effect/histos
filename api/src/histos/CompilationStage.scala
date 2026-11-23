package histos

import histos.plugin.{RepoPath, SourceSet}

/** A module, Scala version, and platform identify one cell. Scope identifies a compiler stage within it. */
final case class Cell(module: ProjectName, version: ScalaVersion, platform: Platform)

final case class CompilationStage(cell: Cell, scope: SourceSet)

enum RootKind:
  case Sources
  case Resources

/** An exact repository-relative directory, without implicit layout expansion. Generated File consumption uses
  * [[histos.plugin.EmittedContent]], never a widened parent [[ScopedRoot]].
  */
final case class ScopedRoot(
    path: RepoPath,
    scope: SourceSet,
    platforms: PlatformSelection,
    kind: RootKind,
):
  def applies(scope: SourceSet, platform: Platform): Boolean =
    this.scope == scope && platforms.contains(platform)

object ScopedRoot:
  /** The conventional combined directory shared by JVM and Native. */
  val jvmNative: PlatformSelection = PlatformSelection.only(Platform.Jvm, Platform.Native)
