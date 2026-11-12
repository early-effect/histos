package histos.cli

import histos.plugin.PluginArgs
import histos.server.{Call, WatchPolicy}
import histos.task.TaskId
import histos.testrun.TestMode

/** The call a command sends. `mcp`, `bsp`, `server`, `portal`, and `observe` are not operations. Neither is `reboot`:
  * it replaces the server a watch runs on, and its call names the identity of the process that sends it.
  */
object Operation:
  def call(command: Command): Either[CliParseError, Call] =
    command match
      case Command.Watch(inner, args) =>
        operation(inner).map(call => Call.Watch(call, policyOf(args)))
      case other => operation(other)

  /** A watch payload is already a call. A nested watch or `bsp` is usage. `--restart` repeats a program. */
  def underWatch(inner: Call, policy: WatchPolicy): Either[CliParseError, Call] =
    (inner, policy) match
      case (Call.Watch(_, _), _) =>
        Left(CliParseError.WatchCommand("watch does not nest"))
      case (Call.Bsp, _)                         => Left(CliParseError.Rejected)
      case (Call.Run(_, _), WatchPolicy.Restart) => Right(inner)
      case (_, WatchPolicy.Restart)              =>
        Left(CliParseError.WatchCommand("--restart watches a program"))
      case (other, WatchPolicy.After) => Right(other)

  private def policyOf(args: List[String]): WatchPolicy =
    if args.headOption.contains("--restart") then WatchPolicy.Restart else WatchPolicy.After

  /** The command under `~`. A nested watch is not an operation. */
  def operation(command: Command): Either[CliParseError, Call] =
    command match
      case Command.Watch(_, _) =>
        Left(CliParseError.WatchCommand("watch does not nest"))
      case Command.Mcp | Command.Bsp | Command.Server | Command.Portal | Command.Observe | Command.Reboot |
          Command.New(_, _, _, _) | Command.Init(_, _, _, _) | Command.Doctor(_, _, _, _, _, _, _) |
          Command.Explain(_) | Command.Deps(_) | Command.Setup(_, _, _) | Command.Bump(_, _) | Command.Help(_) |
          Command.Coverage(_, _, _) =>
        Left(CliParseError.Rejected)
      case Command.Version           => Right(Call.Version)
      case Command.Graph             => Right(Call.Graph)
      case Command.Run(id, platform) =>
        if id.builtIn then Right(Call.Task(id.value, PluginArgs.empty))
        else Right(Call.Run(id.value, platform))
      case Command.Plugin(id, args)         => Right(Call.Task(id.render, args))
      case Command.Test(projects, affected) =>
        AffectedChoice.select(projects, affected) match
          case None =>
            Left(CliParseError.Pattern("nothing is affected"))
          case Some(names) =>
            Right(Call.Test(names, Nil, TestMode.Run))
      case Command.Affected(_) =>
        Left(CliParseError.Rejected)
      case Command.TestOnly(modules, patterns) =>
        Right(Call.Test(modules, patterns, TestMode.Run))
      case Command.TestQuick(modules, patterns) =>
        Right(Call.Test(modules, patterns, TestMode.Quick))
      case Command.Compile(projects)     => Right(Call.Compile(projects))
      case Command.Client                => Right(Call.Client)
      case Command.Clean(projects)       => Right(Call.Clean(projects))
      case Command.CleanFull             => Right(Call.CleanFull)
      case Command.Assembly(destination) =>
        Right(Call.Assembly(destination))
      case Command.PublishLocal(tokens) =>
        Right(Call.PublishLocal(tokens))
      case Command.PublishSnapshots(tokens) =>
        Right(Call.PublishSnapshots(tokens))
      case Command.PublishRelease(automatic, tokens) =>
        Right(Call.PublishRelease(tokens, automatic))
      case Command.PublishPromote(id) =>
        Right(Call.PublishPromote(id))
      case Command.PublishDrop(id) =>
        Right(Call.PublishDrop(id))
      case Command.PublishAssets(tokens) =>
        Right(Call.PublishAssets(tokens))
      case Command.Toolchain                => Right(Call.Toolchain)
      case Command.CachePath                => Right(Call.CachePath)
      case Command.CacheList                => Right(Call.CacheList)
      case Command.CacheClean(mode)         => Right(Call.CacheClean(mode))
      case Command.CachePromote(from, to)   => Right(Call.CachePromote(from, to))
      case Command.Resolve(action)          => Right(Call.Resolve(action))
      case Command.Workflow(action)         => Right(Call.Workflow(action))
      case Command.Fmt(targets, mode)       => Right(Call.Format(targets, mode))
      case Command.Launcher(action)         => Right(Call.Launcher(action))
      case Command.Reload                   => Right(Call.Reload)
      case Command.Cancel(id)               => Right(Call.Cancel(id))
      case Command.PreviewCompile(projects) =>
        Right(Call.PreviewCompile(projects))
      case Command.PreviewTest(projects) =>
        Right(Call.PreviewTest(projects))
      case Command.PreviewPublishLocal(tokens) =>
        Right(Call.PreviewPublishLocal(tokens))
      case Command.PreviewPublishSnapshots(tokens) =>
        Right(Call.PreviewPublishSnapshots(tokens))
      case Command.PreviewClean(projects) =>
        Right(Call.PreviewClean(projects))
      case Command.PreviewCleanFull =>
        Right(Call.PreviewCleanFull)
      case Command.PreviewRun(id, platform) =>
        Right(Call.PreviewRun(id.value, platform))
      case Command.PreviewPlugin(id) =>
        Right(Call.PreviewRun(id.render, None))
end Operation
