package histos.server

import histos.build.BuildEvent
import histos.preview.Plan
import histos.report.CallResult
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
  MainClass,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  Resolution,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import zio.{Chunk, Ref, ZIO}
import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** Preview calls report the plan, and a run that is not a module is a fault. */
object CallsPreviewLawSpec extends ZIOSpecDefault:
  private val scala3  = ScalaVersion("3.10.0")
  private val lawName = ProjectName.trusted("previewlaw")

  def spec = suite("calls preview laws")(
    test("compile and test preview the cell, a task run names the task, and an absent run is a fault"):
      Workspace.work("histos-calls-preview").flatMap: dir =>
        for
          expectedCompile <- Plan.show(dir, graph, List(lawName))
          expectedTest    <- Plan.test(dir, graph, List(lawName)).map(Plan.renderTest(dir, _))
          expectedClean <- Plan.clean(dir, graph, List(lawName)).map(Plan.renderClean(dir, _))
          (compiled, compiledHeard) <- speak(say => Calls.previewCompile(dir, graph, List(lawName), say))
          (tested, testedHeard)     <- speak(say => Calls.previewTest(dir, graph, List(lawName), say))
          (cleaned, cleanedHeard)   <- speak(say => Calls.previewClean(dir, graph, List(lawName), say))
          (ran, ranHeard)           <- speak(say => Calls.previewRun(graph, lawName.value, None, say))
          (task, taskHeard)         <- speak(say => Calls.previewRun(graph, "doc", None, say))
          (platform, platformHeard) <- speak(say => Calls.previewRun(graph, "doc", Some(Platform.Jvm), say))
          (absent, absentHeard)     <- speak(say => Calls.previewRun(graph, "missing", None, say))
        yield assertTrue(
          expectedCompile.contains("previewlaw 3.10.0 jvm selected"),
          compiled == Right(()),
          outs(compiledHeard) == expectedCompile,
          expectedTest.contains("previewlaw 3.10.0 jvm selected"),
          tested == Right(()),
          outs(testedHeard) == expectedTest,
          expectedClean.nonEmpty,
          cleaned == Right(()),
          outs(cleanedHeard) == expectedClean,
          ran == Right(()),
          outs(ranHeard) == "previewlaw jvm\nupstreams",
          task == Right(()),
          outs(taskHeard) == "doc",
          platform == Right(()),
          done(platformHeard).exists(faultContains("run doc does not take a platform")),
          absent == Right(()),
          done(absentHeard).exists(faultContains("'missing' is not a module")),
        )
  )

  private def faultContains(text: String)(result: CallResult): Boolean =
    result match
      case CallResult.Fault(fault) => fault.message.contains(text)
      case _                       => false

  private def done(heard: Chunk[Message]): Option[CallResult] =
    heard.collect { case Message.Done(result) => result }.lastOption

  private def outs(heard: Chunk[Message]): String =
    heard.collect { case Message.Event(BuildEvent.Out(text)) => text }.mkString("\n")

  private def speak[R, E](use: Say => ZIO[R, E, Unit]): ZIO[R, Nothing, (Either[E, Unit], Chunk[Message])] =
    for
      messages <- Ref.make(Chunk.empty[Message])
      say = new Say(message => messages.update(_ :+ message))
      result <- use(say).either
      heard  <- messages.get
    yield (result, heard)

  private def graph: ProjectGraph =
    val law = Module(
      lawName,
      Platforms(Platform.Jvm, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      application = Some(MainClass("Hello")),
    )
    ProjectGraph.trusted(
      List(law),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      resolution = Resolution.Lock,
    )
end CallsPreviewLawSpec
