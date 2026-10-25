package histos.cli

import histos.plugin.PluginArgs
import histos.server.Call
import histos.task.TaskId
import histos.workflow.WorkflowAction

/** The call a command sends. `mcp`, `bsp`, `server`, and `observe` are not
  * operations.
  */
object Operation:
  def call(command: Command): Either[CliParseError, Call] =
    command match
      case Command.Watch(inner, _) => operation(inner).map(Call.Watch(_))
      case other                   => operation(other)

  /** A watch payload is already a call. A nested watch or `bsp` is usage. */
  def underWatch(inner: Call): Either[CliParseError, Call] =
    inner match
      case Call.Watch(_) =>
        Left(CliParseError.WatchCommand("watch does not nest"))
      case Call.Bsp => Left(CliParseError.Rejected)
      case other    => Right(other)

  /** The command under `~`. A nested watch is not an operation. */
  def operation(command: Command): Either[CliParseError, Call] =
    command match
      case Command.Watch(_, _) =>
        Left(CliParseError.WatchCommand("watch does not nest"))
      case Command.Mcp | Command.Bsp | Command.Server | Command.Observe =>
        Left(CliParseError.Rejected)
      case Command.Version           => Right(Call.Version)
      case Command.Graph             => Right(Call.Graph)
      case Command.Run(id, platform) =>
        if id == TaskId.Inspect || id == TaskId.Versions then
          Right(Call.Task(id.value, PluginArgs.empty))
        else Right(Call.Run(id.value, platform.map(_.token)))
      case Command.Plugin(id, args) => Right(Call.Task(id.render, args))
      case Command.Test(projects)   =>
        Right(Call.Test(projects, Nil, quick = false))
      case Command.TestOnly(modules, patterns) =>
        Right(Call.Test(modules, patterns.map(_.value), quick = false))
      case Command.TestQuick(modules, patterns) =>
        Right(Call.Test(modules, patterns.map(_.value), quick = true))
      case Command.Compile(projects)     => Right(Call.Compile(projects))
      case Command.Clean(projects)       => Right(Call.Clean(projects))
      case Command.CleanFull             => Right(Call.CleanFull)
      case Command.Assembly(destination) =>
        Right(Call.Assembly(destination))
      case Command.Publish(repository, projects) =>
        Right(Call.Publish(repository, projects))
      case Command.Toolchain          => Right(Call.Toolchain)
      case Command.CachePath          => Right(Call.CachePath)
      case Command.CacheList          => Right(Call.CacheList)
      case Command.CacheClean(dryRun) => Right(Call.CacheClean(dryRun))
      case Command.Resolve(action)    => Right(Call.Resolve(resolve(action)))
      case Command.Workflow(action)   => Right(Call.Workflow(workflow(action)))
      case Command.LauncherUpdate     => Right(Call.LauncherUpdate)
      case Command.Reload             => Right(Call.Reload)
      case Command.Reboot             => Right(Call.Reboot)
      case Command.Cancel(id)         => Right(Call.Cancel(id))
      case Command.PreviewCompile(projects) =>
        Right(Call.PreviewCompile(projects))
      case Command.PreviewTest(projects) =>
        Right(Call.PreviewTest(projects))
      case Command.PreviewPublish(projects) =>
        Right(Call.PreviewPublish(projects))
      case Command.PreviewClean(projects) =>
        Right(Call.PreviewClean(projects))
      case Command.PreviewCleanFull =>
        Right(Call.PreviewCleanFull)
      case Command.PreviewRun(id, platform) =>
        Right(Call.PreviewRun(id.value, platform.map(_.token)))
      case Command.PreviewPlugin(id) =>
        Right(Call.PreviewRun(id.render, None))

  def resolve(action: ResolveAction): String =
    action match
      case ResolveAction.Verify => "verify"
      case ResolveAction.Update => "update"
      case ResolveAction.Check  => "check"

  def workflow(action: WorkflowAction): String =
    action match
      case WorkflowAction.Check  => "check"
      case WorkflowAction.Update => "update"
