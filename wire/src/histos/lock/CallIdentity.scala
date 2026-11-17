package histos.lock

import histos.plugin.{CommandIdentity, CommandToken, PluginCoordinate, PluginTaskId, ResolveVerb}
import histos.server.{Call, ResolveAction}

/** The command identity and the rerun words for one [[Call]]. The daemon does not invent either. */
object CallIdentity:
  def identity(call: Call): CommandIdentity =
    call match
      case Call.Compile(projects)                         => CommandIdentity.Compile(projects)
      case Call.Test(projects, _, _)                      => CommandIdentity.Test(projects)
      case Call.Resolve(ResolveAction.Verify)             => CommandIdentity.Resolve(ResolveVerb.Verify)
      case Call.Resolve(ResolveAction.Update)             => CommandIdentity.Resolve(ResolveVerb.Update)
      case Call.Resolve(ResolveAction.Check)              => CommandIdentity.Resolve(ResolveVerb.Check)
      case Call.Resolve(ResolveAction.Refresh(selectors)) => CommandIdentity.Refresh(selectors)
      case Call.Format(_, _)                              => CommandIdentity.Format
      case Call.PublishLocal(_) | Call.PublishSnapshots(_) | Call.PublishRelease(_, _) | Call.PublishPromote(_) |
          Call.PublishDrop(_) | Call.PublishAssets(_) | Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) =>
        CommandIdentity.Publish
      case Call.Task(id, _) =>
        PluginTaskId.parse(id) match
          case Right(task) => CommandIdentity.Task(task)
          case Left(_)     => other(id, "task")
      case Call.Watch(inner, _)      => identity(inner)
      case Call.Run(id, _, _, _)     => other(id, "run")
      case Call.PreviewRun(id, _)    => other(id, "preview-run")
      case Call.Assembly(_)          => CommandIdentity.Other(CommandToken("assembly"))
      case Call.Clean(_, _)          => CommandIdentity.Other(CommandToken("clean"))
      case Call.PreviewCompile(_)    => CommandIdentity.Other(CommandToken("preview-compile"))
      case Call.PreviewTest(_, _, _) => CommandIdentity.Other(CommandToken("preview-test"))
      case Call.PreviewClean(_, _)   => CommandIdentity.Other(CommandToken("preview-clean"))
      case Call.Graph                => CommandIdentity.Other(CommandToken("graph"))
      case Call.Cancel(_)            => CommandIdentity.Other(CommandToken("cancel"))
      case Call.Workflow(_)          => CommandIdentity.Other(CommandToken("workflow"))
      case Call.Reload               => CommandIdentity.Other(CommandToken("reload"))
      case Call.Reboot(_)            => CommandIdentity.Other(CommandToken("reboot"))
      case Call.Version              => CommandIdentity.Other(CommandToken("version"))
      case Call.CachePath            => CommandIdentity.Other(CommandToken("cache-path"))
      case Call.CacheList            => CommandIdentity.Other(CommandToken("cache-list"))
      case Call.CacheClean(_)        => CommandIdentity.Other(CommandToken("cache-clean"))
      case Call.CachePromote(_, _)   => CommandIdentity.Other(CommandToken("cache-promote"))
      case Call.CacheForget(_)       => CommandIdentity.Other(CommandToken("cache-forget"))
      case Call.CacheVerify(_)       => CommandIdentity.Other(CommandToken("cache-verify"))
      case Call.Bsp                  => CommandIdentity.Other(CommandToken("bsp"))

  def words(call: Call): List[String] =
    call match
      case Call.Compile(projects)                         => "compile" :: projects.map(_.value)
      case Call.Test(projects, _, _)                      => "test" :: projects.map(_.value)
      case Call.Resolve(ResolveAction.Verify)             => List("resolve")
      case Call.Resolve(ResolveAction.Update)             => List("resolve", "--update")
      case Call.Resolve(ResolveAction.Check)              => List("resolve", "--check")
      case Call.Resolve(ResolveAction.Refresh(selectors)) =>
        "resolve" :: "--refresh" :: selectors.map(_.value)
      case Call.Run(id, platform, _, _) =>
        "run" :: id :: platform.toList.map(_.token)
      case Call.Task(id, _)                 => List(id)
      case Call.Watch(inner, _)             => "~" :: words(inner)
      case Call.Format(_, _)                => List("fmt")
      case Call.PublishLocal(_)             => List("publish", "local")
      case Call.PublishSnapshots(_)         => List("publish", "snapshots")
      case Call.PublishRelease(_, _)        => List("publish", "release")
      case Call.PublishPromote(_)           => List("publish", "promote")
      case Call.PublishDrop(_)              => List("publish", "drop")
      case Call.PublishAssets(_)            => List("publish", "assets")
      case Call.Assembly(_)                 => List("assembly")
      case Call.Clean(_, _)                 => List("clean")
      case Call.Graph                       => List("graph")
      case Call.Version                     => List("version")
      case Call.Workflow(_)                 => List("workflow")
      case Call.Reload                      => List("reload")
      case Call.Reboot(_)                   => List("reboot")
      case Call.Cancel(_)                   => List("cancel")
      case Call.CachePath                   => List("cache", "path")
      case Call.CacheList                   => List("cache", "list")
      case Call.CacheClean(_)               => List("cache", "clean")
      case Call.CachePromote(_, _)          => List("cache", "promote")
      case Call.CacheForget(_)              => List("cache", "forget")
      case Call.CacheVerify(_)              => List("cache", "verify")
      case Call.Bsp                         => List("bsp")
      case Call.PreviewCompile(projects)    => "preview" :: "compile" :: projects.map(_.value)
      case Call.PreviewTest(projects, _, _) => "preview" :: "test" :: projects.map(_.value)
      case Call.PreviewPublishLocal(_)      => List("preview", "publish", "local")
      case Call.PreviewPublishSnapshots(_)  => List("preview", "publish", "snapshots")
      case Call.PreviewClean(_, _)          => List("preview", "clean")
      case Call.PreviewRun(id, platform)    =>
        "preview" :: "run" :: id :: platform.toList.map(_.token)

  def rerun(call: Call, mode: LockMode): String =
    ("histos" :: words(call) ::: List(s"--lock=${LockPrecedence.token(mode)}")).mkString(" ")

  def summary(offer: LockOffer): List[String] =
    val base = offer.base match
      case LockRevisionState.Absent            => "absent"
      case LockRevisionState.Present(revision) => revision.value
    val rows = offer.scope.diff.entries.map(entry => s"${change(entry.change)} ${entry.coordinate.render}")
    List(
      s"Lock repair required: ${offer.proposal.value}",
      s"Proposal: ${offer.proposal.value}, base lock: $base",
      s"Scope: ${offer.scope.summary.value}",
    ) ++ rows

  private def change(kind: LockChange): String =
    kind match
      case LockChange.Added   => "added"
      case LockChange.Removed => "removed"
      case LockChange.Changed => "changed"

  private def other(raw: String, fallback: String): CommandIdentity =
    CommandToken.from(raw) match
      case Right(token) => CommandIdentity.Other(token)
      case Left(_)      => CommandIdentity.Other(CommandToken.trusted(fallback))
end CallIdentity
