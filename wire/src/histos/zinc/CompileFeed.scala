package histos.zinc

import histos.jdk.{Sha256, ToolchainError}
import histos.resolve.ResolveError
import histos.select.SelectError
import histos.{Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

import zio.NonEmptyChunk

final case class CompiledCell(
    module: ProjectName,
    version: ScalaVersion,
    platform: Platform,
    classes: Path,
    /** `.class`, `.sjsir`, and `.nir` currently in `classes`, including leftovers.
      */
    classFiles: Int,
    key: Option[Sha256],
    changed: Boolean,
    sources: Option[SourceCount],
    /** Class files this round wrote. Absent when that set is not known. */
    classesWritten: Option[Int],
)

/** One cell's console. Diagnostics stay with that cell. Parallel cells are not interleaved.
  */
enum CompileFeed:
  case Began(module: ProjectName, version: ScalaVersion, platform: Platform)
  case Diagnostic(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      problem: CompileProblem,
  )
  case Miss(module: ProjectName, version: ScalaVersion, platform: Platform, change: KeyMiss)
  case Done(cell: CompiledCell)
  case Failed(error: GraphCompileError)

enum GraphCompileError:
  case Runtime(error: histos.jdk.ToolchainError)
  case Target(error: SelectError)
  case UnknownCell(module: ProjectName, version: ScalaVersion)
  case NoRoot(module: ProjectName)
  case NoSources(module: ProjectName, root: Path)
  case MissingUpstream(
      module: ProjectName,
      dependency: ProjectName,
      version: ScalaVersion,
      platform: Platform,
  )
  case IncompatibleUpstream(
      module: ProjectName,
      dependency: ProjectName,
      version: ScalaVersion,
      upstream: ScalaVersion,
      platform: Platform,
  )
  case Cycle(module: ProjectName)
  case Lock(error: ResolveError)
  case LockRead(path: Path)
  case RootRead(path: Path)
  case Output(path: Path)
  case Failed(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      error: CompileError,
  )
  case Cells(errors: NonEmptyChunk[GraphCompileError])

  def message: String = this match
    case Runtime(error)               => error.message
    case Target(error)                => error.message
    case UnknownCell(module, version) =>
      s"'${module.value}' ${version.value} is not a cell on the requested platform"
    case NoRoot(module) =>
      s"'${module.value}' has no source root. Expected ${module.value}/src, or src when it is the only module"
    case NoSources(module, root) =>
      s"'${module.value}' has no main or test sources under $root"
    case MissingUpstream(module, dependency, version, platform) =>
      s"'${module.value}' depends on '${dependency.value}', which has no ${version.value} ${platform.token} classes"
    case IncompatibleUpstream(module, dependency, version, upstream, platform) =>
      s"'${module.value}' ${version.value} cannot read '${dependency.value}' ${upstream.value} ${platform.token}"
    case Cycle(module)                            => s"'${module.value}' is in a compile cycle"
    case Lock(error)                              => error.message
    case LockRead(path)                           => s"could not read $path"
    case RootRead(path)                           => s"could not read source roots under $path"
    case Output(path)                             => s"could not write $path"
    case Failed(module, version, platform, error) =>
      s"'${module.value}' ${version.value} ${platform.token} failed to compile\n${error.message}"
    case Cells(errors) => errors.map(_.message).mkString("\n")
end GraphCompileError
