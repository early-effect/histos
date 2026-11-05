package histos.plugin

import histos.JobTimeout

/** Runners, action pins, and secrets the workflow may name. A missing pin or runner is a load error, not a string that
  * happens to be unpinned. `timeout` is the cap on every generated job.
  */
final case class CiSettings(
    runners: Map[RunnerName, String],
    actions: Map[ActionName, ActionPin],
    secrets: Set[SecretName],
    env: List[(EnvName, EnvValue)],
    workflows: Set[String],
    timeout: JobTimeout = JobTimeout.default,
)

object CiSettings:
  val empty: CiSettings = CiSettings(Map.empty, Map.empty, Set.empty, Nil, Set.empty)
