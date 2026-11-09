package histos

import histos.plugin.*
import histos.workflow.*

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** `ci.yml` keeps graph env, dispatch inputs, tags, merge, and release on the one header. */
object WorkflowHeaderLawSpec extends ZIOSpecDefault:
  private val scala3 = scala"3.10.0"

  given CiBranch = CiBranch("main")

  private val hidden = SecretName("HIDDEN")
  private val plain  = EnvName("PLAIN")
  private val wide   = new Trigger:
    def on: List[On] =
      Trigger.Ci.on ++ List(
        On.Dispatch(
          List(
            DispatchInput(InputKey("mode"), InputKind.Text, Some("fast")),
            DispatchInput(InputKey("dry"), InputKind.Boolean, None),
            DispatchInput(InputKey("pick"), InputKind.Choice, Some("a")),
          )
        ),
        On.MergeGroup,
        On.Release,
        On.Push(Nil, List("v*"), Nil),
      )
    def cond: Cond = Trigger.Ci.cond

  def spec = suite("workflow header laws")(
    test("the header prints env, inputs, tags, merge, and release, and drops a secret"):
      val settings = CiSettings.empty.copy(
        secrets = Set(hidden),
        env = List(
          plain             -> EnvValue.Plain("hi"),
          EnvName("SHA")    -> EnvValue.Github(GithubField.Sha),
          EnvName("REF")    -> EnvValue.Github(GithubField.RefName),
          EnvName("TOK")    -> EnvValue.Github(GithubField.Token),
          EnvName("FROM")   -> EnvValue.Env(plain),
          EnvName("SECRET") -> EnvValue.Secret(hidden),
        ),
      )
      val job   = TaskJob(JobName("header"), TaskName("echo"), trigger = wide)
      val built = Projects
        .organization(organization"example.com")
        .ci(settings)(use(new HeaderEcho(List(job))), module("core", Jvm)(scala3))
      built match
        case Left(error)  => assertTrue(error.message == "")
        case Right(graph) =>
          Workflow.render(graph) match
            case Left(error) => assertTrue(error.message == "")
            case Right(body) =>
              body.split("jobs:", 2).toList match
                case header :: jobs :: Nil =>
                  assertTrue(
                    header.contains("  PLAIN: \"hi\""),
                    header.contains("${{ github.sha }}"),
                    header.contains("${{ github.ref_name }}"),
                    header.contains("${{ github.token }}"),
                    header.contains("${{ env.PLAIN }}"),
                    !header.contains("HIDDEN"),
                    !header.contains("secrets."),
                    jobs.contains("SECRET: ${{ secrets.HIDDEN }}"),
                    body.contains("  merge_group:"),
                    body.contains("  release:"),
                    body.contains("v*"),
                    body.contains("mode:"),
                    body.contains("type: string"),
                    body.contains("type: boolean"),
                    body.contains("type: choice"),
                    body.contains("default: \"fast\""),
                    body.contains("default: \"a\""),
                  )
                case other => assertTrue(other == List("the header has no jobs key"))
  )
end WorkflowHeaderLawSpec

private final class HeaderEcho(jobs: List[CiJob]) extends HistosPlugin:
  def id: PluginId = PluginId("test.header")

  def contribute: Contribution =
    Contribution(
      List(DeclaredTask(TaskName("echo"), Seal.Open, Set.empty, Nil, Nil, Nil)),
      Nil,
      jobs,
    )

  def run(request: RunRequest): zio.ZIO[zio.Scope, PluginRunError, PluginOutput] =
    zio.ZIO.succeed(PluginOutput(request.id.render, Nil))
end HeaderEcho
