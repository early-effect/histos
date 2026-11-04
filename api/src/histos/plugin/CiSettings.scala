package histos.plugin

/** Runners, action pins, and secrets the workflow may name. A missing pin or runner is a load error, not a string that
  * happens to be unpinned.
  */
final case class CiSettings(
    runners: Map[RunnerName, String],
    actions: Map[ActionName, ActionPin],
    secrets: Set[SecretName],
    env: List[(EnvName, EnvValue)],
    workflows: Set[String],
)

object CiSettings:
  val empty: CiSettings = CiSettings(Map.empty, Map.empty, Set.empty, Nil, Set.empty)
