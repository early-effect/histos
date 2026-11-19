package histos.cli

import histos.lock.{DispositionSubject, LockMode}

/** One parsed command and the explicit lock disposition, when the caller set one. */
final case class InvocationLine(command: Command, lock: Option[LockMode])

/** The disposition the current command carries. Helpers that send a call read it. */
final case class LockChoice(mode: Option[LockMode])

/** Which commands prepare dependencies. Watch uses the command it repeats. */
object LockGrammar:
  def subject(command: Command): DispositionSubject =
    command match
      case Command.Compile(_) | Command.Test(_, _, _, _) | Command.Run(_, _, _) | Command.Plugin(_, _) |
          Command.PublishLocal(_) | Command.PublishSnapshots(_) | Command.PublishRelease(_, _) | Command.Assembly(_) |
          Command.PreviewCompile(_) | Command.PreviewTest(_, _, _, _) | Command.PreviewPublishLocal(_) |
          Command.PreviewPublishSnapshots(_) | Command.PreviewRun(_, _) | Command.PreviewPlugin(_) |
          Command.Coverage(_, _, _) =>
        DispositionSubject.Prepare
      case Command.Resolve(action) => DispositionSubject.Resolve(action)
      case Command.Watch(inner, _) => subject(inner)
      case Command.Version | Command.ToolchainClient | Command.Graph | Command.Clean(_, _) | Command.PublishPromote(_) |
          Command.PublishDrop(_) | Command.PublishAssets(_) | Command.PublishWithdraw | Command.Mcp | Command.Bsp |
          Command.Server | Command.Portal | Command.CachePath | Command.CacheList | Command.CacheClean(_) |
          Command.CachePromote(_, _) | Command.CacheForget(_) | Command.CacheVerify(_) | Command.Workflow(_) |
          Command.Fmt(_, _) | Command.Reload | Command.Reboot | Command.Stop | Command.Cancel(_) |
          Command.PreviewClean(_, _) | Command.Observe | Command.New(_, _, _) | Command.Init(_, _, _) |
          Command.Doctor(_, _, _, _, _, _) | Command.Explain(_) | Command.Deps(_) | Command.Setup(_, _, _) |
          Command.Bump(_, _) | Command.Help(_) | Command.Affected(_) =>
        DispositionSubject.Plain
