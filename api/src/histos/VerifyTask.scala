package histos

import histos.plugin.{JobName, PluginTaskId, TaskName}

/** One task the verify section runs. The job command is `./histos run <id>`. */
final case class VerifyTask(
    name: JobName,
    run: VerifyRun,
    link: ClientLink = ClientLink.Skip,
)

enum VerifyRun:
  /** `./histos run <name>` for a built-in task. `doc` stays [[DocCheck]]. */
  case BuiltIn(name: TaskName)

  /** `./histos run plugin/task`. */
  case Plugin(id: PluginTaskId)

  def command: String = this match
    case BuiltIn(name) => name.value
    case Plugin(id)    => id.render
end VerifyRun

/** Whether that verify job links the native client before the task. */
enum ClientLink:
  case Skip
  case Native

/** Where test, scaladoc, format, and verify tasks get Histos. Pin is the setup action. Local is a publish job that runs
  * `./histos publish local`, then those jobs install that jar. Affected, versions, workflow, supply-chain,
  * dependency-snapshot, and cache-seed stay on the pin either way.
  */
enum SuiteHost:
  case Pin
  case Local
