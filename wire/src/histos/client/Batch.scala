package histos.client

import histos.plugin.{PluginArgs, PluginTaskId}
import histos.server.Call
import histos.task.TaskId
import histos.testrun.TestMode
import histos.{Platform, ProjectName}

/** Batch argv the native client sends. Anything else is the JVM client's command. */
object Batch:
  def parse(args: List[String]): Option[Call] =
    args match
      case "compile" :: rest     => projects(rest).map(Call.Compile(_))
      case "test" :: rest        => projects(rest).map(names => Call.Test(names, Nil, TestMode.Run))
      case "run" :: name :: rest =>
        rest match
          case Nil          => runCall(name, None)
          case token :: Nil => runCall(name, Some(token))
          case _            => None
      case "clean" :: rest    => projects(rest).map(Call.Clean(_))
      case "cleanFull" :: Nil => Some(Call.CleanFull)
      case _                  => None

  private def projects(raw: List[String]): Option[List[ProjectName]] =
    raw.foldLeft[Option[List[ProjectName]]](Some(Nil)): (acc, token) =>
      acc.flatMap: have =>
        if token.startsWith("-") then None
        else ProjectName.from(token).toOption.map(name => have :+ name)

  /** The same split `run` uses: a module, `inspect`, `versions`, `doc`, or a plugin task. */
  private def runCall(name: String, platform: Option[String]): Option[Call] =
    if name.contains('/') then
      platform match
        case Some(_) => None
        case None    =>
          PluginTaskId.parse(name).toOption.map(id => Call.Task(id.render, PluginArgs.empty))
    else
      TaskId
        .from(name)
        .toOption
        .flatMap: id =>
          val task = id.builtIn
          platform match
            case Some(_) if task => None
            case None            =>
              Some(
                if task then Call.Task(id.value, PluginArgs.empty)
                else Call.Run(id.value, None)
              )
            case Some(text) =>
              Platform.from(text).toOption.map(chosen => Call.Run(id.value, Some(chosen)))
end Batch
