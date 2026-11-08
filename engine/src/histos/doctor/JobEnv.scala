package histos.doctor

import histos.supply.{StepOutcome, Venue}

/** Facts the generated job already knows. Secret presence sees organization secrets. The app step outcome says whether
  * the mint step produced the App token. `venue` is `Actions` only inside a GitHub Actions run.
  */
final case class JobEnv(
    present: Map[String, Boolean],
    step: Option[StepOutcome],
    venue: Venue,
)

object JobEnv:
  val empty: JobEnv = JobEnv(Map.empty, None, Venue.Laptop)

  def from(env: Map[String, String]): JobEnv =
    val present = env.flatMap: (key, value) =>
      val mark = "HISTOS_SECRET_PRESENT_"
      if key.startsWith(mark) then Some(key.drop(mark.length) -> (value == "true"))
      else None
    JobEnv(
      present,
      env.get("HISTOS_STEP_APP").flatMap(StepOutcome.parse),
      if env.get("GITHUB_ACTIONS").contains("true") then Venue.Actions else Venue.Laptop,
    )
end JobEnv
