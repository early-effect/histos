package histos.client

import histos.format.{FormatMode, FormatTargets}
import histos.plugin.{PluginArgs, PluginTaskId}
import histos.server.{Call, CatalogQuery, CleanExtent, ResolveAction}
import histos.task.TaskId
import histos.testrun.{TestFilter, TestMode}
import histos.{Platform, ProjectName, TestSelection, TestTargets}

import zio.NonEmptyChunk

/** Batch argv the native client sends. Anything else is the JVM client's command. A program is not a call until the
  * client fills the shell.
  */
object Batch:
  enum Parsed:
    case Ready(call: Call)
    case Program(id: String, platform: Option[Platform], args: List[String])

    /** `None` is a kind this client does not ask. It still stays off the JVM path. */
    case Names(query: Option[CatalogQuery])
  end Parsed

  def parse(args: List[String]): Option[Parsed] =
    args match
      case "compile" :: rest => projects(rest).map(names => Parsed.Ready(Call.Compile(names)))
      case "test" :: rest    =>
        projects(rest).map: names =>
          val targets = names match
            case first :: rest => TestTargets.Named(first, rest)
            case Nil           => TestTargets.All
          Parsed.Ready(Call.Test(TestSelection(targets), Nil, TestMode.Run, TestFilter.empty))
      case "run" :: name :: rest =>
        val (tokens, argv) = splitDash(rest)
        tokens match
          case Nil          => runCall(name, None, argv)
          case token :: Nil => runCall(name, Some(token), argv)
          case _            => None
      case "clean" :: "--full" :: Nil => Some(Parsed.Ready(Call.Clean(Nil, CleanExtent.Full)))
      case "clean" :: rest            =>
        projects(rest).map(names => Parsed.Ready(Call.Clean(names, CleanExtent.Products)))
      case "fmt" :: "--check" :: rest =>
        projects(rest).map(names => Parsed.Ready(Call.Format(targets(names), FormatMode.Check)))
      case "fmt" :: rest => projects(rest).map(names => Parsed.Ready(Call.Format(targets(names), FormatMode.Write)))
      case "resolve" :: "--refresh" :: rest =>
        projects(rest).map(names => Parsed.Ready(Call.Resolve(ResolveAction.Refresh(names))))
      case "complete" :: rest => Some(Parsed.Names(CatalogQuery.parse(rest)))
      case _                  => None

  private def splitDash(args: List[String]): (List[String], List[String]) =
    def loop(seen: List[String], rest: List[String]): (List[String], List[String]) =
      rest match
        case "--" :: tail => (seen.reverse, tail)
        case head :: tail => loop(head :: seen, tail)
        case Nil          => (seen.reverse, Nil)
    loop(Nil, args)

  /** No project is the build. */
  private def targets(names: List[ProjectName]): FormatTargets =
    NonEmptyChunk.fromIterableOption(names).fold(FormatTargets.Build)(FormatTargets.Projects(_))

  private def projects(raw: List[String]): Option[List[ProjectName]] =
    raw.foldLeft[Option[List[ProjectName]]](Some(Nil)): (acc, token) =>
      acc.flatMap: have =>
        if token.startsWith("-") then None
        else ProjectName.from(token).toOption.map(name => have :+ name)

  /** The same split `run` uses: a module, `inspect`, `versions`, `doc`, or a plugin task. */
  private def runCall(name: String, platform: Option[String], argv: List[String]): Option[Parsed] =
    if name.contains('/') then
      platform match
        case Some(_)               => None
        case None if argv.nonEmpty => None
        case None                  =>
          PluginTaskId.parse(name).toOption.map(id => Parsed.Ready(Call.Task(id.render, PluginArgs.empty)))
    else
      TaskId
        .from(name)
        .toOption
        .flatMap: id =>
          val task = id.builtIn
          platform match
            case Some(_) if task            => None
            case _ if task && argv.nonEmpty => None
            case None if task               => Some(Parsed.Ready(Call.Task(id.value, PluginArgs.empty)))
            case None                       => Some(Parsed.Program(id.value, None, argv))
            case Some(text)                 =>
              Platform.from(text).toOption.map(chosen => Parsed.Program(id.value, Some(chosen), argv))
end Batch
