package histos.cli

import histos.{PlatformSelection, ProjectName, ScalaSelection, TestSelection, TestTargets}

/** An affected filter never changes an explicit request into a whole-suite or focused request. */
enum AffectedChoice:
  case Unset
  case NoneSelected
  case Everything
  case Only(modules: List[ProjectName])
  case Within(eligible: List[ProjectName], affected: AffectedChoice)

object AffectedChoice:
  def selection(
      projects: List[ProjectName],
      choice: AffectedChoice,
      platforms: PlatformSelection,
      versions: ScalaSelection = ScalaSelection.All,
  ): TestSelection =
    val targets = projects match
      case Nil           => TestTargets.All
      case first :: rest => TestTargets.Named(first, rest)
    def applyChoice(base: TestTargets, choice: AffectedChoice): TestSelection = choice match
      case AffectedChoice.Unset | AffectedChoice.Everything => TestSelection(base, platforms, scala = versions)
      case AffectedChoice.NoneSelected                      => TestSelection(base, platforms, Some(Nil), versions)
      case AffectedChoice.Only(modules)                     => TestSelection(base, platforms, Some(modules), versions)
      case AffectedChoice.Within(eligible, affected)        =>
        base match
          case TestTargets.All               => applyChoice(TestTargets.Filtered(eligible), affected)
          case TestTargets.Filtered(modules) =>
            applyChoice(TestTargets.Filtered(modules.filter(eligible.contains)), affected)
          case TestTargets.Named(_, _) =>
            val selected = applyChoice(base, affected)
            selected.copy(affected = Some(eligible.filter(name => selected.affected.forall(_.contains(name)))))
    applyChoice(targets, choice)
