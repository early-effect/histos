package histos.resolve

import histos.doctor.{Diagnostic, Noted, Suggest}
import histos.format.ScalafmtConfError
import histos.jdk.Sha256
import histos.plugin.PluginLoadError
import histos.workspace.IoFailure
import histos.{Dep, DepKind, Platform, ProjectName, ScalaVersion}

import java.nio.file.Path

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
      scope: ClasspathScope,
      changed: List[String] = Nil,
  )
  case PluginDrift(dependency: Dep, changed: List[String] = Nil)
  case NoLock(path: Path)
  case UnsupportedPlatform(platform: Platform)
  case Lock(error: LockError)
  case Io(failure: IoFailure)
  case Plugin(error: PluginLoadError)
  case Repository(op: RepositoryIo)
  case Download(misses: ::[ArtifactMiss])
  case Scalafmt(error: ScalafmtConfError)

  /** Every artifact that failed did so in transit, after Coursier's own retries, so a later fetch may succeed. */
  def transient: Boolean = this match
    case ResolveError.Download(misses) => misses.forall(_.fault == DownloadFault.Transient)
    case _                             => false

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
    case ResolveError.LockDrift(module, version, platform, scope, _) =>
      s"${scope.header} ${module.value} ${version.value} ${PlatformToken.render(platform)} does not match the dependency lock"
    case ResolveError.PluginDrift(dependency, _) =>
      s"${shown(dependency)} does not match the dependency lock"
    case ResolveError.NoLock(path)                  => s"no dependency lock at $path"
    case ResolveError.UnsupportedPlatform(platform) =>
      s"resolve does not fetch ${PlatformToken.render(platform)}"
    case ResolveError.Lock(error)    => error.message
    case ResolveError.Io(failure)    => failure.message
    case ResolveError.Plugin(error)  => error.message
    case ResolveError.Repository(op) =>
      s"resolve failed while ${op.toString.toLowerCase}"
    case ResolveError.Download(misses) =>
      ("could not download:" :: misses.map(miss => s"${miss.url}: ${miss.detail}")).mkString("\n")
    case ResolveError.Scalafmt(error) => error.message

  def diagnose: Diagnostic = this match
    case ResolveError.LockDrift(_, _, _, _, changed) =>
      val explain =
        if changed.isEmpty then "The cell is absent from the lock, or its coordinates moved."
        else changed.mkString("\n")
      Noted.carry("resolve.lock-drift", message, explain, "histos resolve --update")
    case ResolveError.PluginDrift(_, changed) =>
      val explain =
        if changed.isEmpty then "The plugin coordinate does not match the lock."
        else changed.mkString("\n")
      Noted.carry("resolve.plugin-drift", message, explain, "histos resolve --update")
    case ResolveError.ToolDrift(_) =>
      Noted.carry("resolve.tool-drift", message, "The locked tool coordinate moved.", "histos resolve --update")
    case ResolveError.Scalafmt(error) =>
      Noted.carry("format.config", message, error.fix, "histos resolve --update")
    case ResolveError.NoLock(_) =>
      Noted.carry("resolve.no-lock", message, "Resolve needs a dependency lock.", "histos resolve --update")
    case ResolveError.NotFound(organization, name, revision) =>
      val explain = Suggest
        .binary(organization, name, revision)
        .getOrElse("The coordinate is not in the configured repositories.")
      Noted.carry("resolve.not-found", message, explain, "histos resolve --update")
    case ResolveError.Download(_) if transient =>
      Noted.carry(
        "resolve.download",
        message,
        "Each download failed in transit, after Coursier's own retries. A file already in the cache is not fetched again.",
        "histos resolve",
      )
    case other =>
      Noted.carry("resolve.failed", other.message, other.message, "histos resolve --update")

  private def shown(dependency: Dep): String =
    val separator = dependency.kind match
      case DepKind.Java   => ":"
      case DepKind.Binary => "::"
      case DepKind.Full   => ":::"
    s"${dependency.organization}$separator${dependency.name}:${dependency.revision}"
end ResolveError
