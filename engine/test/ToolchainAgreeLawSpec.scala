package histos.doctor

import histos.{
  ClangMajor,
  ClangPin,
  Module,
  NodeFloor,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  RunnerTools,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
  ToolRevision,
}
import histos.version.Catalog

import java.nio.file.Path

import zio.test.{ZIOSpecDefault, assertTrue, suite, test}

/** A generated job installs the tools the modules name. A lower floor or a different clang major is a finding. */
object ToolchainAgreeLawSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")

  def spec = suite("toolchain agree laws")(
    test("runnerTools names Node 25 for Js and the clang major for Native, and a mismatch says which one"):
      val js      = module("jslaw", Platform.Js)
      val native  = module("natlaw", Platform.Native, ClangPin.Major(ClangMajor(21)))
      val other   = module("natlaw2", Platform.Native, ClangPin.Major(ClangMajor(22)))
      val llvm21  = Some(ToolRevision.trusted("21.0.0"))
      val node25  = Some(NodeFloor.trusted(25))
      val covered = RunnerTools(node25, llvm21, None)
      val low     = covered.copy(node = Some(NodeFloor.trusted(20)))
      val oldLlvm = RunnerTools(None, llvm21, None)
      val quoted  = ToolchainChecks.exportLine(Path.of("/opt/my node"))
      val plain   = ToolchainChecks.exportLine(Path.of("/opt/node"))
      assertTrue(
        note(ToolchainChecks.cover(graph(List(js)))) ==
          "Js modules have no runnerTools.node, so the test job does not install Node. Add " +
          call(node25, None) + ".",
        note(ToolchainChecks.cover(graph(List(native)))) ==
          "Native modules have no runnerTools.llvm, so the test job does not install clang. The LLVM revision major is 21. Add " +
          call(None, llvm21) + ".",
        note(ToolchainChecks.cover(graph(List(js, native)))) ==
          "Js modules have no runnerTools.node, so the test job does not install Node. Native modules have no runnerTools.llvm, so the test job does not install clang. The LLVM revision major is 21. Add " +
          call(node25, llvm21) + ".",
        ToolchainChecks.cover(graph(List(js, native), covered)) ==
          Outcome.Ok(ToolchainChecks.coverId, "runnerTools covers the modules"),
        only(ToolchainChecks.agree(graph(List(js), low))) ==
          "NodeFloor(20) is below 25. Raise it to NodeFloor(25). Add " + call(node25, llvm21) + ".",
        only(ToolchainChecks.agree(graph(List(other), oldLlvm))) ==
          "LLVM 21.0.0 is major 21, and the modules name ClangMajor(22). Raise the revision. Add " +
          call(None, Some(ToolRevision.trusted("22.0.0"))) + ".",
        only(ToolchainChecks.agree(graph(List(native, other), oldLlvm))) ==
          "LLVM 21.0.0 cannot match ClangMajor 21 and 22. Make the Native modules name one clang major, then set the ToolRevision major to that number. Add " +
          call(None, llvm21) + ".",
        ToolchainChecks.agree(graph(List(js, native), covered)) ==
          List(Outcome.Ok(ToolchainChecks.agreeId, "runnerTools agree with the modules")),
        plain == "export HISTOS_NODE=/opt/node",
        quoted == "export HISTOS_NODE='/opt/my node'",
      )
  )

  private def call(node: Option[NodeFloor], llvm: Option[ToolRevision]): String =
    RunnerCall.show(RunnerTools(node, llvm, None))

  private def only(outcomes: List[Outcome]): String =
    outcomes match
      case found :: Nil => note(found)
      case many         => many.map(note).mkString(";")

  private def note(outcome: Outcome): String = outcome match
    case Outcome.Ok(_, text)       => text
    case Outcome.Found(diagnostic) => diagnostic.summary
    case Outcome.Skipped(_, _, _)  => "skipped"

  private def note(outcome: Option[Outcome]): String =
    outcome.fold("missing")(note)

  private def graph(modules: List[Module], tools: RunnerTools = RunnerTools.empty): ProjectGraph =
    ProjectGraph.trusted(
      modules,
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
      runnerTools = tools,
    )

  private def module(name: String, platform: Platform, pin: ClangPin = ClangPin.Unused): Module =
    Module(
      ProjectName.trusted(name),
      Platforms(platform, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      clang = pin,
    )
end ToolchainAgreeLawSpec
