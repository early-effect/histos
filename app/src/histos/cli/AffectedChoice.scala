package histos.cli

import histos.ProjectName

/** What `test --affected` asked to run. An empty list runs nothing. `all` is the whole suite. */
enum AffectedChoice:
  case Unset
  case NoneSelected
  case Everything
  case Only(modules: List[ProjectName])

object AffectedChoice:
  def select(projects: List[ProjectName], choice: AffectedChoice): Option[List[ProjectName]] =
    choice match
      case AffectedChoice.Unset         => Some(projects)
      case AffectedChoice.NoneSelected  => None
      case AffectedChoice.Everything    => Some(projects)
      case AffectedChoice.Only(modules) =>
        val wanted = if projects.isEmpty then modules else projects.filter(modules.contains)
        if wanted.isEmpty then None else Some(wanted)
