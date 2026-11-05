package histos.program

import histos.doctor.{Diagnostic, Noted}
import histos.node.NodeMiss
import histos.pack.{MainClassError}
import histos.resolve.{ResolveError}
import histos.zinc.{GraphCompileError}
import histos.{Aggregate, Platform, ProjectName, ScalaVersion}
import java.nio.file.{Path}

enum ProgramError:
  case Tool(
      module: ProjectName,
      version: ScalaVersion,
      error: histos.toolchain.PlatformToolError,
  )
  case Absent(name: String, near: List[String] = Nil)
  case Aggregate(name: ProjectName)
  case NoMain(name: ProjectName)
  case Choose(name: ProjectName, platforms: List[Platform])
  case Unbuilt(name: ProjectName, platform: Platform, built: List[Platform])
  case NoClang(name: ProjectName, version: ScalaVersion)
  case Main(error: MainClassError)
  case Compile(error: GraphCompileError)
  case Library(error: histos.zinc.CompileError)
  case Lock(error: ResolveError)
  case LockRead(path: Path)
  case Link(
      module: ProjectName,
      version: ScalaVersion,
      platform: Platform,
      detail: String,
  )
  case Process(module: ProjectName, version: ScalaVersion, detail: String)
  case NoNode(module: ProjectName, version: ScalaVersion, miss: NodeMiss)
  case MissingUpstream(
      module: ProjectName,
      dependency: ProjectName,
      version: ScalaVersion,
  )
  case Work(path: Path)
  case Root(path: Path)

  def message: String = this match
    case Tool(_, _, error)       => error.message
    case Absent(name, _)        => s"'$name' is not a module"
    case Aggregate(name)         => s"'${name.value}' is an aggregate"
    case NoMain(name)            => s"'${name.value}' does not name a main"
    case Choose(name, platforms) =>
      s"'${name.value}' builds ${joined(platforms)}. Name one: histos run ${name.value} ${sample(platforms)}"
    case Unbuilt(name, platform, built) =>
      s"'${name.value}' does not build ${platform.token}. It builds ${joined(built)}"
    case NoClang(name, version) =>
      s"'${name.value}' ${version.value} native does not name a clang major"
    case Main(error)                             => error.message
    case Compile(error)                          => error.message
    case Library(error)                          => error.message
    case Lock(error)                             => error.message
    case LockRead(path)                          => s"could not read $path"
    case Link(module, version, platform, detail) =>
      s"'${module.value}' ${version.value} ${platform.token} failed to link\n$detail"
    case Process(module, version, detail) =>
      s"'${module.value}' ${version.value} did not start\n$detail"
    case NoNode(module, version, miss) =>
      s"'${module.value}' ${version.value} js has no Node to run on: ${miss.message}"
    case MissingUpstream(module, dependency, version) =>
      s"'${module.value}' depends on '${dependency.value}', which has no ${version.value} classes"
    case Work(path) => s"could not prepare the run directory $path"
    case Root(path) => s"could not read $path"

  def diagnose: Diagnostic = this match
    case Absent(_, near) =>
      val explain = near match
        case head :: _ => s"did you mean $head?"
        case Nil       => "The name is not a module in this graph."
      val fix = near.headOption match
        case Some(guess) => Noted.command(s"histos run $guess")
        case None        => Noted.command("histos graph")
      Noted.error("run.absent", message, explain, fix)
    case Lock(error) => error.diagnose
    case other       => Noted.carry("run.failed", other.message, other.message, "histos help run")
end ProgramError

private def joined(platforms: List[Platform]): String =
  val names = platforms.map(_.token)
  names match
    case Nil               => ""
    case one :: Nil        => one
    case one :: two :: Nil => s"$one or $two"
    case many              =>
      val leading = many.dropRight(1).mkString(", ")
      val ending  = many.takeRight(1).mkString
      s"$leading, or $ending"

private def sample(platforms: List[Platform]): String =
  platforms match
    case head :: _ => head.token
    case Nil       => "jvm"
