package histos.resolve

import histos.jdk.Sha256
import histos.workspace.{IoFailure, Workspace}
import histos.plugin.{LinkStrategy, PluginCoordinate, PluginLoadError}
import histos.{
  Aggregate,
  Dep,
  DepKind,
  Module,
  Platform,
  ProjectGraph,
  ProjectName,
  ScalaVersion
}

import java.nio.file.{
  AtomicMoveNotSupportedException,
  Files,
  Path,
  StandardCopyOption
}

import zio.{IO, ULayer, ZIO, ZLayer}

enum ResolveError:
  case ToolRoot(id: ToolId, root: histos.resolve.ToolRoot, fault: ToolRootFault)
  case NoTool(id: ToolId)
  case ToolDrift(id: ToolId)
  case CompilerVersion(error: histos.ScalaVersionError)
  case NativeOrigin(error: histos.native.NativeToolchainError)
  case Range(dependency: Dep)
  case NotFound(organization: String, name: String, revision: String)
  case Checksum(path: Path, expected: Sha256, actual: Sha256)
  case LockDrift(
      module: ProjectName,
      scalaVersion: ScalaVersion,
      platform: Platform,
      scope: ClasspathScope
  )
  case PluginDrift(dependency: Dep)
  case NoLock(path: Path)
  case UnsupportedPlatform(platform: Platform)
  case Lock(error: LockError)
  case Io(failure: IoFailure)
  case Plugin(error: PluginLoadError)
  case Repository(op: RepositoryIo)

  def message: String = this match
    case ResolveError.ToolRoot(id, root, fault) =>
      s"tool ${id.token} has ${fault.toString.toLowerCase(java.util.Locale.ROOT)} root ${root.organization}:${root.name}:${root.revision}"
    case ResolveError.NoTool(id) =>
      s"tool ${id.token} is missing from the dependency lock"
    case ResolveError.ToolDrift(id) =>
      s"tool ${id.token} does not match the dependency lock"
    case ResolveError.CompilerVersion(error) => error.message
    case ResolveError.NativeOrigin(error)    => error.message
    case ResolveError.Range(dependency)      =>
      s"${shown(dependency)} is not an exact revision"
    case ResolveError.NotFound(organization, name, revision) =>
      s"$organization:$name:$revision was not found"
    case ResolveError.Checksum(path, _, _) =>
      s"checksum of $path does not match the lock"
    case ResolveError.LockDrift(module, version, platform, scope) =>
      s"${scope.header} ${module.value} ${version.value} ${PlatformToken.render(platform)} does not match the dependency lock"
    case ResolveError.PluginDrift(dependency) =>
      s"${shown(dependency)} does not match the dependency lock"
    case ResolveError.NoLock(path) => s"no dependency lock at $path"
    case ResolveError.UnsupportedPlatform(platform) =>
      s"resolve does not fetch ${PlatformToken.render(platform)}"
    case ResolveError.Lock(error)    => error.message
    case ResolveError.Io(failure)    => failure.message
    case ResolveError.Plugin(error)  => error.message
    case ResolveError.Repository(op) =>
      s"resolve failed while ${op.toString.toLowerCase}"

  private def shown(dependency: Dep): String =
    val separator = dependency.kind match
      case DepKind.Java   => ":"
      case DepKind.Binary => "::"
      case DepKind.Full   => ":::"
    s"${dependency.organization}$separator${dependency.name}:${dependency.revision}"

/** The resolution seam. Tests substitute a fixture. The live value is the
  * Coursier adapter.
  */
