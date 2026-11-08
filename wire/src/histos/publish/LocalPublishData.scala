package histos.publish

import histos.doctor.{Diagnostic, Noted}
import histos.central.PublishError
import histos.pack.{AssemblyError, JarError}
import histos.resolve.ResolveError
import histos.version.{CiVersion, ShipName, VersionError}
import histos.select.SelectError
import histos.zinc.{CompileError, GraphCompileError}
import histos.{Platform, PomFact, ProjectName, ScalaVersion}
import java.nio.file.Path

enum LocalPublishError:
  case Version(error: VersionError)
  case Target(error: SelectError)
  case Compile(error: GraphCompileError)
  case Jar(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      error: JarError,
  )
  case SourceRead(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      error: CompileError,
  )
  case NoSources(module: ProjectName, version: ScalaVersion, platform: Platform)
  case Publish(error: PublishError)
  case Read(path: Path)
  case NoOrganization
  case NothingToPublish
  case Development(ship: ShipName, compile: CiVersion)
  case NoCommit
  case UnknownShip(ship: ShipName)
  case NotPublished(module: ProjectName)
  case SnapshotDependency(ship: ShipName, dependency: String)
  case Unknown(raw: String)
  case UnpublishedDependency(module: ProjectName, dependency: ProjectName)
  case Facts(missing: ::[PomFact])
  case BadDependency(organization: String, name: String, revision: String)
  case Credentials
  case Io(path: String)
  case Stock(override val message: String)
  case Classpath(error: ResolveError)
  case Runtime(error: CompileError)
  case Pack(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      error: AssemblyError,
  )
  case MissingClasses(module: ProjectName, version: ScalaVersion, platform: Platform)
  case MissingClient(ship: ShipName, path: Path)

  def message: String = this match
    case Version(error)                        => error.message
    case Target(error)                         => error.message
    case Compile(error)                        => error.message
    case Jar(module, version, platform, error) =>
      s"'${module.value}' ${version.value} ${platform.token} failed to jar\n${error.message}"
    case SourceRead(module, version, platform, error) =>
      s"'${module.value}' ${version.value} ${platform.token} failed to read sources\n${error.message}"
    case NoSources(module, version, platform) =>
      s"'${module.value}' ${version.value} ${platform.token} has no sources to publish"
    case Publish(error) => error.message
    case Read(path)     => s"could not read $path"
    case NoOrganization =>
      "a publish needs an organization. Name it with Projects.organization(organization\"com.example\")"
    case NothingToPublish           => "nothing publishes"
    case Development(ship, compile) =>
      histos.version.ShipAdvice.development(ship, compile)
    case NoCommit =>
      "could not read the commit at HEAD. A snapshot coordinate is that commit."
    case UnknownShip(ship)                    => s"ship '${ship.value}' is not in the catalog"
    case NotPublished(module)                 => s"'${module.value}' is not in a ship"
    case SnapshotDependency(ship, dependency) =>
      s"${ship.value} depends on snapshot $dependency, so it is not published"
    case Unknown(raw)                              => s"'$raw' is not a ship, an aggregate, or a module"
    case UnpublishedDependency(module, dependency) =>
      s"'${module.value}' depends on '${dependency.value}', which is not published"
    case Facts(missing) =>
      s"a release needs ${missing.map(_.word).mkString(", ")}"
    case BadDependency(organization, name, revision) =>
      s"$organization:$name:$revision is not a publishable dependency"
    case Credentials                            => "SONATYPE_USERNAME and SONATYPE_PASSWORD are required"
    case Io(path)                               => s"could not write $path"
    case Stock(message)                         => message
    case Classpath(error)                       => error.message
    case Runtime(error)                         => error.message
    case Pack(module, version, platform, error) =>
      s"'${module.value}' ${version.value} ${platform.token} failed to assemble\n${error.message}"
    case MissingClasses(module, version, platform) =>
      s"'${module.value}' has no ${version.value} ${platform.token} classes"
    case MissingClient(ship, path) =>
      s"${ship.value} builds clients on snapshots, and $path is not a file. The snapshots workflow downloads each client there."

  def diagnose: Diagnostic = this match
    case Publish(error)   => error.diagnose
    case Classpath(error) => error.diagnose
    case Target(error)    => error.diagnose
    case Credentials      =>
      Noted.error(
        "publish.credentials",
        message,
        "Neither variable is set. This is not a rejected login.",
        Noted.steps(
          "Export SONATYPE_USERNAME and SONATYPE_PASSWORD.",
          "A 401 after they are set says the repository rejected the credentials.",
        ),
      )
    case other =>
      Noted.carry("publish.failed", other.message, other.message, "histos help publish")
end LocalPublishError
