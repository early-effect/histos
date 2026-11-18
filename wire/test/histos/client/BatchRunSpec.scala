package histos.client

import histos.Platform
import histos.lock.ExecutionKind
import histos.plugin.Interaction
import histos.server.{Call, ResolveAction}
import histos.ProjectName
import histos.report.WireJson.given

import zio.json.*
import zio.test.*

object BatchRunSpec extends ZIOSpecDefault:
  def spec = suite("run arguments on the native client")(
    test("tokens after -- are the program arguments and jvm stays the platform") {
      assertTrue(
        Batch.parse(List("run", "app", "jvm", "--", "hello", "world")) ==
          Some(Batch.Parsed.Program("app", Some(Platform.Jvm), List("hello", "world")))
      )
    },
    test("a run without -- has an empty argument list") {
      assertTrue(
        Batch.parse(List("run", "app", "jvm")) == Some(Batch.Parsed.Program("app", Some(Platform.Jvm), Nil))
      )
    },
    test("a token before -- is not an argument") {
      assertTrue(Batch.parse(List("run", "app", "jvm", "extra")).isEmpty)
    },
    test("a flag after -- is an argument") {
      assertTrue(
        Batch.parse(List("run", "app", "--", "--verbose")) ==
          Some(Batch.Parsed.Program("app", None, List("--verbose")))
      )
    },
    test("a lock flag is not encoded on the fast path") {
      assertTrue(
        Batch.parse(List("compile", "--lock=frozen")).isEmpty,
        Batch.parse(List("compile", "app")) == Some(Batch.Parsed.Ready(Call.Compile(List(ProjectName("app"))))),
      )
    },
    test("resolve --refresh is a native call and a lock flag is not") {
      assertTrue(
        Batch.parse(List("resolve", "--refresh")) ==
          Some(Batch.Parsed.Ready(Call.Resolve(ResolveAction.Refresh(Nil)))),
        Batch.parse(List("resolve", "--refresh", "shots")) ==
          Some(Batch.Parsed.Ready(Call.Resolve(ResolveAction.Refresh(List(ProjectName("shots")))))),
        Batch.parse(List("resolve", "--refresh", "--lock=frozen")).isEmpty,
        Batch.parse(List("resolve")).isEmpty,
      )
    },
    test("the native caller reads ci and interaction from the process it starts in") {
      for posted <- CallerFacts.native(Call.Compile(Nil), Map("CI" -> "true"), prompts = false)
      yield assertTrue(
        CallerFacts.execution(Map("CI" -> "true")) == ExecutionKind.Ci,
        CallerFacts.execution(Map("GITHUB_ACTIONS" -> "true")) == ExecutionKind.Ci,
        CallerFacts.execution(Map.empty) == ExecutionKind.Local,
        CallerFacts.interaction(true) == Interaction.Offers,
        CallerFacts.interaction(false) == Interaction.Unattended,
        posted.context.client == CallerFacts.native,
        posted.context.execution == ExecutionKind.Ci,
        posted.context.interaction == Interaction.Unattended,
        posted.toJson.fromJson[histos.lock.InvocationEnvelope] == Right(posted),
      )
    },
  )
