package histos

import histos.plugin.*
import histos.workflow.JobYaml

import zio.test.*

/** A command job is build data. It does not need a plugin, and the pin check is a step after setup. */
object CommandJobLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  def spec = suite("command job laws")(
    test("unless the pin client is present, the step links, and a build that omits the job has no such step"):
      val asked   = CommandJob(JobName("link-client"), HistosCommand.Client).unless(PinClient.Present)
      val plain   = CommandJob(JobName("link-client"), HistosCommand.Client)
      val withJob = Projects.job(asked)(module("core", Jvm)(scala3))
      val without = Projects(module("core", Jvm)(scala3))
      val linked  = withJob.flatMap(graph => JobYaml.render(graph, "main"))
      val quiet   = without.flatMap(graph => JobYaml.render(graph, "main"))
      (linked, quiet) match
        case (Right(body), Right(other)) =>
          val text = body.ci.mkString("\n")
          val rest = other.ci.mkString("\n")
          assertTrue(
            asked.when == ClientWhen.PinMissing,
            plain.when == ClientWhen.Always,
            text.contains("link-client"),
            unquotedRun(text, "histos-client") == Some(PinClient.line(ClientWhen.PinMissing)),
            text.indexOf("uses: ./.github/actions/setup") < text.indexOf(PinClient.skipText),
            !text.contains("histos.link"),
            !text.contains("./histos run"),
            !rest.contains("link-client"),
            !rest.contains(PinClient.skipText),
          )
        case (Left(error), _) => assertTrue(error.toString == "")
        case (_, Left(error)) => assertTrue(error.toString == "")
  )

  /** YAML quotes the run script, so the law compares the unquoted step to the script the printer was given. */
  private def unquotedRun(text: String, needle: String): Option[String] =
    text.linesIterator
      .map(_.trim)
      .find(line => line.startsWith("- run:") && line.contains(needle))
      .map: line =>
        val body    = line.stripPrefix("- run:").trim
        val wrapped =
          if body.startsWith("\"") && body.endsWith("\"") then body.drop(1).dropRight(1)
          else body
        wrapped.replace("\\\"", "\"").replace("\\\\", "\\")
end CommandJobLawSpec
