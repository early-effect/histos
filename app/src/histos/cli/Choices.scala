package histos.cli

import histos.doctor.{Area, FixKind, FixMode}
import histos.plugin.{JobName, JobNameError, PluginId, PluginIdError}

/** `new` and `init`. `--bare` wins over `--yes`. `--yes` confirms the full scaffold and the default branch. */
enum ScaffoldChoice:
  case Bare
  case Confirmed
  case Prompt

object ScaffoldChoice:
  def from(bare: Boolean, yes: Boolean): ScaffoldChoice =
    if bare then Bare
    else if yes then Confirmed
    else Prompt

/** How doctor prints. */
enum ReportForm:
  case Text
  case Json

/** Whether doctor lists every check. */
enum DoctorDetail:
  case Brief
  case Verbose

/** A built-in area, or a plugin id that matches no built-in check. */
enum DoctorScope:
  case BuiltIn(area: Area)
  case Plugin(id: PluginId)

  def token: String = this match
    case BuiltIn(area) => DoctorScope.token(area)
    case Plugin(id)    => id.value
end DoctorScope

object DoctorScope:
  def token(area: Area): String =
    area match
      case Area.Toolchain       => "toolchain"
      case Area.BuildDefinition => "build"
      case Area.Workflows       => "workflows"
      case Area.RepoSettings    => "repo"
      case Area.SupplyChain     => "supply-chain"
      case Area.SitePublish     => "site"
      case Area.Release         => "release"
      case Area.Plugin(id)      => id.value

  def parse(raw: String): Either[DoctorScopeError, DoctorScope] =
    builtIn(raw) match
      case Some(area) => Right(BuiltIn(area))
      case None       =>
        PluginId.from(raw) match
          case Right(id)   => Right(Plugin(id))
          case Left(error) => Left(DoctorScopeError.Plugin(error))

  def preflight(raw: String): Either[JobNameError, JobName] =
    JobName.from(raw)

  private def builtIn(raw: String): Option[Area] =
    raw match
      case "toolchain"    => Some(Area.Toolchain)
      case "build"        => Some(Area.BuildDefinition)
      case "workflows"    => Some(Area.Workflows)
      case "repo"         => Some(Area.RepoSettings)
      case "supply-chain" => Some(Area.SupplyChain)
      case "site"         => Some(Area.SitePublish)
      case "release"      => Some(Area.Release)
      case _              => None
end DoctorScope

enum DoctorScopeError:
  case Plugin(error: PluginIdError)

  def message: String = this match
    case Plugin(error) => error.message
end DoctorScopeError

object FixFlags:
  /** The two flags doctor still takes, recovered from the mode the command stores. */
  def apply(mode: FixMode): (Boolean, Boolean) =
    mode match
      case FixMode.Print        => (false, false)
      case FixMode.Apply(kinds) =>
        val github = kinds.contains(FixKind.GitHubSetting)
        (true, !github)
