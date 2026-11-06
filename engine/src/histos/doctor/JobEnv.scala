package histos.doctor

import histos.supply.StepOutcome

/** Facts the generated job already knows. Secret presence sees organization secrets. The app step used the App JWT. */
final case class JobEnv(
    present: Map[String, Boolean],
    step: Option[StepOutcome],
    actions: Boolean,
    githubToken: Option[String],
)

object JobEnv:
  val empty: JobEnv = JobEnv(Map.empty, None, false, None)

  def from(env: Map[String, String]): JobEnv =
    val present = env.flatMap: (key, value) =>
      val mark = "HISTOS_SECRET_PRESENT_"
      if key.startsWith(mark) then Some(key.drop(mark.length) -> (value == "true"))
      else None
    JobEnv(
      present,
      env.get("HISTOS_STEP_APP").flatMap(StepOutcome.parse),
      env.get("GITHUB_ACTIONS").contains("true"),
      env.get("GITHUB_TOKEN").filter(_.nonEmpty),
    )
end JobEnv
