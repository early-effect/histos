package histos.cli

import histos.plugin.PluginArgs
import histos.server.Call
import histos.task.TaskId
import histos.workflow.WorkflowAction

/** The call a command sends. `mcp`, `bsp`, and `server` are entry points, not
  * operations.
  */
object Operation:
  def call(command: Command): Either[CliParseError, Call] =
    command match
      case Command.Mcp | Command.Bsp | Command.Server =>
        Left(CliParseError.Rejected)
      case Command.Watch(_, args) => Right(Call.Watch(args))
      case other                  => operation(other)

  /** The command under `~`. A nested watch is not an operation. */
  def operation(command: Command): Either[CliParseError, Call] =
    command match
      case Command.Watch(_, _) =>
        Left(CliParseError.WatchCommand("watch does not nest"))
      case Command.Mcp | Command.Bsp | Command.Server =>
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
      case Command.Cancel(id) => Right(Call.Cancel(id))
      case Command.PreviewCompile(projects) =>
        Right(Call.PreviewCompile(projects))

  def resolve(action: ResolveAction): String =
    action match
      case ResolveAction.Verify => "verify"
      case ResolveAction.Update => "update"
      case ResolveAction.Check  => "check"

  def workflow(action: WorkflowAction): String =
    action match
      case WorkflowAction.Check  => "check"
      case WorkflowAction.Update => "update"
