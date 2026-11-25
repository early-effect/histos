package histos.lock

import histos.plugin.{CommandIdentity, CommandToken, PluginTaskId, ResolveVerb}
import histos.server.{Call, CatalogQuery, ResolveAction}
import histos.{CompileExtent, PlatformSelection, ScalaSelection, TestSelection, TestTargets}
import histos.report.WireJson.given
import zio.json.*

/** The command identity and the rerun words for one [[histos.server.Call]]. The daemon does not invent either. */
object CallIdentity:
  def identity(call: Call): CommandIdentity =
    call match
      case Call.Compile(projects, platforms, versions, _) => CommandIdentity.Compile(projects, platforms, versions)
      case Call.Test(selection, _, _, _)                  => CommandIdentity.Test(selection)
      case Call.Resolve(ResolveAction.Verify)             => CommandIdentity.Resolve(ResolveVerb.Verify)
      case Call.Resolve(ResolveAction.Update)             => CommandIdentity.Resolve(ResolveVerb.Update)
      case Call.Resolve(ResolveAction.Check)              => CommandIdentity.Resolve(ResolveVerb.Check)
      case Call.Resolve(ResolveAction.Refresh(selectors)) => CommandIdentity.Refresh(selectors)
      case Call.Format(_, _)                              => CommandIdentity.Format
      case Call.PublishCheck(_, _) | Call.PublishLocal(_, _) | Call.PublishWithdraw | Call.PublishSnapshots(_) |
          Call.PublishRelease(_, _) | Call.PublishPromote(_) | Call.PublishDrop(_) | Call.PublishAssets(_) |
          Call.PreviewPublishLocal(_) | Call.PreviewPublishSnapshots(_) =>
        CommandIdentity.Publish
      case Call.Task(id, _) =>
        PluginTaskId.parse(id) match
          case Right(task) => CommandIdentity.Task(task)
          case Left(_)     => other(id, "task")
      case Call.Watch(inner, _)                           => identity(inner)
      case Call.Run(id, platforms, _, _, main, versions)  => runIdentity(id, platforms, main, versions)
      case Call.PreviewRun(id, platforms, main, versions) => runIdentity(id, platforms, main, versions)
      case Call.Assembly(_)                               => CommandIdentity.Other(CommandToken("assembly"))
      case Call.Clean(_, _)                               => CommandIdentity.Other(CommandToken("clean"))
      case Call.PreviewCompile(_, _, _)                   => CommandIdentity.Other(CommandToken("preview-compile"))
      case Call.PreviewTest(_, _, _, _)                   => CommandIdentity.Other(CommandToken("preview-test"))
      case Call.PreviewClean(_, _)                        => CommandIdentity.Other(CommandToken("preview-clean"))
      case Call.Graph                                     => CommandIdentity.Other(CommandToken("graph"))
      case Call.Cancel(_)                                 => CommandIdentity.Other(CommandToken("cancel"))
      case Call.Workflow(_)                               => CommandIdentity.Other(CommandToken("workflow"))
      case Call.Reload                                    => CommandIdentity.Other(CommandToken("reload"))
      case Call.Reboot(_)                                 => CommandIdentity.Other(CommandToken("reboot"))
      case Call.Stop                                      => CommandIdentity.Other(CommandToken("stop"))
      case Call.Version                                   => CommandIdentity.Other(CommandToken("version"))
      case Call.CachePath                                 => CommandIdentity.Other(CommandToken("cache-path"))
      case Call.CacheList                                 => CommandIdentity.Other(CommandToken("cache-list"))
      case Call.CacheClean(_)                             => CommandIdentity.Other(CommandToken("cache-clean"))
      case Call.CachePromote(_, _)                        => CommandIdentity.Other(CommandToken("cache-promote"))
      case Call.CacheForget(_)                            => CommandIdentity.Other(CommandToken("cache-forget"))
      case Call.CacheVerify(_)                            => CommandIdentity.Other(CommandToken("cache-verify"))
      case Call.Bsp                                       => CommandIdentity.Other(CommandToken("bsp"))
      case Call.Catalog(_)                                => CommandIdentity.Other(CommandToken("complete"))

  def words(call: Call): List[String] =
    call match
      case Call.Compile(projects, selection, versions, sources) =>
        val tests = sources match
          case CompileExtent.Main        => Nil
          case CompileExtent.MainAndTest => List("--tests")
        "compile" :: (projects.map(_.value) ++ platforms(selection) ++ scalas(versions) ++ tests)
      case Call.Test(selection, _, _, _)                  => "test" :: testWords(selection)
      case Call.Resolve(ResolveAction.Verify)             => List("resolve")
      case Call.Resolve(ResolveAction.Update)             => List("resolve", "--update")
      case Call.Resolve(ResolveAction.Check)              => List("resolve", "--check")
      case Call.Resolve(ResolveAction.Refresh(selectors)) =>
        "resolve" :: "--refresh" :: selectors.map(_.value)
      case Call.Run(id, selection, _, args, main, versions) =>
        "run" :: id :: (selection.toList.flatMap(platforms) ++ versions.toList.flatMap(scalas) ++ main.toList
          .flatMap(value => List("--main", value.value)) ++
          (if args.isEmpty then Nil else "--" :: args))
      case Call.Task(id, _)                                   => List(id)
      case Call.Watch(inner, _)                               => "~" :: words(inner)
      case Call.Format(_, _)                                  => List("fmt")
      case Call.PublishCheck(_, _)                            => List("publish", "check")
      case Call.PublishLocal(_, _)                            => List("publish", "local")
      case Call.PublishWithdraw                               => List("publish", "withdraw")
      case Call.PublishSnapshots(_)                           => List("publish", "snapshots")
      case Call.PublishRelease(_, _)                          => List("publish", "release")
      case Call.PublishPromote(_)                             => List("publish", "promote")
      case Call.PublishDrop(_)                                => List("publish", "drop")
      case Call.PublishAssets(_)                              => List("publish", "assets")
      case Call.Assembly(_)                                   => List("assembly")
      case Call.Clean(_, _)                                   => List("clean")
      case Call.Graph                                         => List("graph")
      case Call.Version                                       => List("version")
      case Call.Workflow(_)                                   => List("workflow")
      case Call.Reload                                        => List("reload")
      case Call.Reboot(_)                                     => List("reboot")
      case Call.Stop                                          => List("stop")
      case Call.Cancel(_)                                     => List("cancel")
      case Call.CachePath                                     => List("cache", "path")
      case Call.CacheList                                     => List("cache", "list")
      case Call.CacheClean(_)                                 => List("cache", "clean")
      case Call.CachePromote(_, _)                            => List("cache", "promote")
      case Call.CacheForget(_)                                => List("cache", "forget")
      case Call.CacheVerify(_)                                => List("cache", "verify")
      case Call.Bsp                                           => List("bsp")
      case Call.PreviewCompile(projects, selection, versions) =>
        "preview" :: "compile" :: (projects.map(_.value) ++ platforms(selection) ++ scalas(versions))
      case Call.PreviewTest(selection, _, _, _)           => "preview" :: "test" :: testWords(selection)
      case Call.PreviewPublishLocal(_)                    => List("preview", "publish", "local")
      case Call.PreviewPublishSnapshots(_)                => List("preview", "publish", "snapshots")
      case Call.PreviewClean(_, _)                        => List("preview", "clean")
      case Call.PreviewRun(id, selection, main, versions) =>
        "preview" :: "run" :: id :: (selection.toList
          .flatMap(platforms) ++ versions.toList.flatMap(scalas) ++ main.toList
          .flatMap(value => List("--main", value.value)))
      case Call.Catalog(query) =>
        query match
          case CatalogQuery.Projects       => List("complete", "projects")
          case CatalogQuery.Modules        => List("complete", "modules")
          case CatalogQuery.Plugins        => List("complete", "plugins")
          case CatalogQuery.Flags(command) => List("complete", "flags", command)

  def rerun(call: Call, mode: LockMode): String =
    val (command, arguments) = words(call).span(_ != "--")
    ("histos" :: command ::: List(s"--lock=${LockPrecedence.token(mode)}") ::: arguments).map(quoted).mkString(" ")

  private def quoted(word: String): String =
    if word.nonEmpty && word.forall(char => char.isLetterOrDigit || "_./,:=+-".contains(char)) then word
    else "'" + word.replace("'", "'\\''") + "'"

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

  private def runIdentity(
      id: String,
      platforms: Option[PlatformSelection],
      main: Option[histos.MainClass],
      versions: Option[ScalaSelection],
  ): CommandIdentity =
    CommandToken.from(id) match
      case Right(token) => CommandIdentity.Run(token, platforms, main, versions)
      case Left(_)      => other(id, "run")

  private def platforms(selection: PlatformSelection): List[String] = selection match
    case PlatformSelection.All          => Nil
    case PlatformSelection.Only(values) => List("--platform", values.all.map(_.token).mkString(","))

  private def scalas(selection: ScalaSelection): List[String] = selection match
    case ScalaSelection.All               => Nil
    case ScalaSelection.Only(first, rest) =>
      List("--scala", (first :: rest).map(_.label).mkString(","))

  private def testWords(selection: TestSelection): List[String] =
    val targets = selection.targets match
      case TestTargets.All                => Nil
      case TestTargets.Named(first, rest) => (first :: rest).map(_.value)
      case TestTargets.Filtered(modules)  => List("--suite", modules.toJson)
    targets ++ platforms(selection.platforms) ++ scalas(selection.scala) ++ selection.affected.toList.flatMap(names =>
      List("--affected", names.toJson)
    )

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
