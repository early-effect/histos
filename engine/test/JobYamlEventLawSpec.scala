package histos

import histos.plugin.*
import histos.workflow.*

import _root_.scala.collection.immutable.::

import zio.{Scope, ZIO}
import zio.test.*

/** In-tree plugin. `Projects` copies [[contribute]] and does not call [[run]]. */
private final class EventEcho(jobs: List[CiJob]) extends HistosPlugin:
  def id: PluginId = PluginId("test.events")

  def contribute: Contribution =
    Contribution(List(DeclaredTask(TaskName("echo"), Seal.Open, Set.empty, Nil, Nil, Nil)), Nil, jobs)

  def run(request: RunRequest): ZIO[Scope, PluginRunError, PluginOutput] =
    ZIO.succeed(PluginOutput(request.id.render, Nil))
end EventEcho

/** Events that would wake `ci.yml` stay off that file. The owned file is the only printer for those lines. */
object JobYamlEventLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"
  private val echo   = TaskName("echo")

  private val wideOn =
    """on:
      |  pull_request:
      |    types:
      |      - opened
      |      - synchronize
      |      - reopened
      |      - labeled
      |      - unlabeled
      |    paths:
      |      - "docs/**"
      |  push:
      |    branches:
      |      - "trunk"
      |      - "release"
      |    tags:
      |      - "v*"
      |    paths:
      |      - "engine/**"
      |  workflow_dispatch:
      |    inputs:
      |      mode:
      |        description: mode
      |        required: false
      |        type: string
      |        default: "fast"
      |      live:
      |        description: live
      |        required: false
      |        type: boolean
      |      tier:
      |        description: tier
      |        required: false
      |        type: choice
      |  merge_group:
      |  release:
      |  workflow_run:
      |    workflows:
      |      - "ci.yml"
      |    types:
      |      - completed
      |""".stripMargin

  def spec = suite("job yaml event laws")(
    test("a split file prints every event arm, and a cron file keeps only the events that belong beside a schedule"):
      val rendered = graph.flatMap(found => JobYaml.render(found, "trunk"))
      val wide     = text(rendered, "histos-wide.yml")
      val paths    = text(rendered, "histos-paths.yml")
      val bare     = text(rendered, "histos-bare.yml")
      val matrix   = text(rendered, "histos-matrix.yml")
      val nightly  = text(rendered, "histos-nightly.yml")
      val names    = rendered.map(_.files.map(_.name))
      val heard    = List(
        "github.event_name == 'pull_request'",
        "github.event_name == 'push'",
        "github.event_name == 'workflow_dispatch'",
        "github.event_name == 'schedule'",
        "github.event_name == 'merge_group'",
        "github.event_name == 'release'",
        "github.event_name == 'workflow_run'",
        "startsWith(github.ref, 'refs/tags/')",
        "startsWith(github.ref, 'refs/tags/v')",
        "github.ref == 'refs/tags/v1.2.3'",
        "SHA: ${{ github.sha }}",
        "REF: ${{ github.ref_name }}",
        "TOK: ${{ github.token }}",
        "KEY: ${{ secrets.HISTOS_TOKEN }}",
        "NOTE: \"hi\"",
        "OUT: ${{ env.HOME }}",
      ).filterNot(wide.contains)
      assertTrue(
        fault(rendered) == "",
        names == Right(
          List(
            "histos-wide.yml",
            "histos-paths.yml",
            "histos-bare.yml",
            "histos-matrix.yml",
            "histos-nightly.yml",
          )
        ),
        rendered.map(_.ci) == Right(Nil),
        wide.contains(wideOn),
        heard == Nil,
        paths.contains("  pull_request:\n    paths:\n      - \"docs/**\"\n"),
        !paths.contains("types:"),
        bare.contains("  pull_request:\n    types:\n      - labeled\n  push:\n"),
        !bare.contains("branches:"),
        !bare.contains("tags:"),
        !bare.contains("paths:"),
        matrix.contains("          - name: \"one\"\n            environment: \"live\"\n            MODE: \"fast\""),
        matrix.contains("          - name: \"two\"\n            MODE: \"\""),
        matrix.contains("    environment: ${{ matrix.environment }}"),
        nightly.contains("  workflow_dispatch:\n  schedule:\n    - cron: \"17 4 * * 1\"\n"),
        !nightly.contains("inputs:"),
        nightly.contains("github.event_name == 'schedule'"),
      )
  )

  private def graph: Either[BuildError, ProjectGraph] =
    Projects
      .organization(organization"example.com")
      .ci(CiSettings.empty.copy(secrets = Set(SecretName("HISTOS_TOKEN"))))(
        use(EventEcho(List(wideJob, pathJob, bareJob, matrixJob, nightlyJob))),
        module("core", Jvm)(scala3),
      )

  private def wideJob: TaskJob =
    TaskJob(
      JobName("wide"),
      echo,
      trigger = Wide,
      env = List(
        EnvName("SHA")  -> EnvValue.Github(GithubField.Sha),
        EnvName("REF")  -> EnvValue.Github(GithubField.RefName),
        EnvName("TOK")  -> EnvValue.Github(GithubField.Token),
        EnvName("KEY")  -> EnvValue.Secret(SecretName("HISTOS_TOKEN")),
        EnvName("NOTE") -> EnvValue.Plain("hi"),
        EnvName("OUT")  -> EnvValue.Env(EnvName("HOME")),
      ),
    )

  private def pathJob: TaskJob =
    TaskJob(JobName("paths"), echo, trigger = PathPull)

  private def bareJob: TaskJob =
    TaskJob(JobName("bare"), echo, trigger = BarePush)

  private def nightlyJob: TaskJob =
    TaskJob.nightly(JobName("nightly"), echo, Cron("17 4 * * 1"))

  private def matrixJob: CiJob =
    val arranged = TaskArrange(
      JobName("matrix"),
      RunLine.Task(echo),
      JobPermissions.read,
      Nil,
      Cond.True,
      List(On.PullRequest(List(PullType.Labeled), Nil)),
    )
    val rows = ::(
      Target(TargetName("one"), Some(EnvironmentName("live")), List(EnvName("MODE") -> EnvValue.Plain("fast"))),
      List(Target(TargetName("two"))),
    )
    new CiJob:
      def name: JobName = arranged.name
      def step: JobStep =
        arranged.line match
          case RunLine.Task(name)   => JobStep.Task(name)
          case RunLine.Client(when) => JobStep.Command(HistosCommand.Client, when)
      def permissions: JobPermissions = arranged.permissions
      def needs: List[JobName]        = arranged.needs
      def render: List[JobRender]     = List(JobRender.Fan(arranged, rows))

  private def text(rendered: Either[BuildError | CiError, JobYaml.Rendered], name: String): String =
    rendered.toOption.flatMap(_.files.find(_.name == name).map(_.body)).getOrElse("")

  private def fault(rendered: Either[BuildError | CiError, JobYaml.Rendered]): String =
    rendered match
      case Left(error) => error.toString
      case Right(_)    => ""
end JobYamlEventLawSpec

private object Wide extends Trigger:
  def on: List[On] =
    List(
      On.PullRequest(On.defaultPull ++ List(PullType.Labeled, PullType.Unlabeled), List("docs/**")),
      On.Push(List(PushBranch.Default, PushBranch.Named("release")), List("v*"), List("engine/**")),
      On.Dispatch(
        List(
          DispatchInput(InputKey("mode"), InputKind.Text, Some("fast")),
          DispatchInput(InputKey("live"), InputKind.Boolean, None),
          DispatchInput(InputKey("tier"), InputKind.Choice, None),
        )
      ),
      On.MergeGroup,
      On.Release,
      On.WorkflowRun(List("ci.yml")),
    )

  def cond: Cond =
    Cond.AnyOf(
      List(
        Cond.Event(EventKind.PullRequest),
        Cond.Event(EventKind.Push),
        Cond.Event(EventKind.Dispatch),
        Cond.Event(EventKind.Schedule),
        Cond.Event(EventKind.MergeGroup),
        Cond.Event(EventKind.Release),
        Cond.Event(EventKind.WorkflowRun),
        Cond.Event(EventKind.Tag),
        Cond.TagMatches(""),
        Cond.TagMatches("*"),
        Cond.TagMatches("v*"),
        Cond.TagMatches("v1.2.3"),
      )
    )
end Wide

private object PathPull extends Trigger:
  def on: List[On] = List(On.PullRequest(On.defaultPull, List("docs/**")))
  def cond: Cond   = Cond.True

private object BarePush extends Trigger:
  def on: List[On] = List(On.PullRequest(List(PullType.Labeled), Nil), On.Push(Nil, Nil, Nil))
  def cond: Cond   = Cond.True
