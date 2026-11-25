package histos

import histos.plugin.{JobName, PluginTaskId, TaskName}

/** One task or typed test selection the verify section runs. */
final case class VerifyTask(
    name: JobName,
    run: VerifyRun,
    link: ClientLink = ClientLink.Skip,
)

enum VerifyRun:
  /** `./histos run <name>` for a built-in task. `doc` stays [[DocCheck]]. */
  case BuiltIn(name: TaskName)

  /** A typed test selection; generation renders `./histos test`, not a built-in run task. */
  case Tests(selection: TestSelection)

  /** `./histos run plugin/task`. */
  case Plugin(id: PluginTaskId)

  def command: String = this match
    case BuiltIn(name)    => name.value
    case Plugin(id)       => id.render
    case Tests(selection) =>
      val modules = selection.targets match
        case TestTargets.All                => Nil
        case TestTargets.Named(first, rest) => (first :: rest).map(_.value).distinct
        case TestTargets.Filtered(modules)  => List("--suite", jsonModules(modules))
      val platforms = selection.platforms match
        case PlatformSelection.All          => Nil
        case PlatformSelection.Only(chosen) =>
          List("--platform", chosen.all.distinct.map(_.token).mkString(","))
      val versions = selection.scala match
        case ScalaSelection.All               => Nil
        case ScalaSelection.Only(first, rest) =>
          List("--scala", (first :: rest).map(_.label).mkString(","))
      val affected = selection.affected.toList.flatMap(names => List("--affected", jsonModules(names)))
      (List("test") ++ modules ++ platforms ++ versions ++ affected).mkString(" ")

  private def jsonModules(modules: List[ProjectName]): String =
    "'[" + modules.distinct.map(name => "\"" + name.value + "\"").mkString(",") + "]'"
end VerifyRun

/** Whether that verify job links the native client before the task. */
enum ClientLink:
  case Skip
  case Native
