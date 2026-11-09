package histos.toolchain

import histos.jdk.{JdkMajor, JdkRead, JdkSurvey, JdkVersion, Machine, Sha256, Toolchain, ToolchainError}
import histos.native.{ClangMiss, NativeDeclaration}
import histos.node.{NodeBinary, NodeError, NodeMajor, NodeMiss, NodeSource, NodeSurvey, NodeToolchain, NodeVersion}
import histos.version.Catalog
import histos.workspace.Workspace
import histos.{
  ClangMajor,
  ClangPin,
  Module,
  Platform,
  Platforms,
  ProjectGraph,
  ProjectName,
  ScalaVersion,
  ScalaVersions,
  SourcePos,
  Splits,
}

import java.nio.file.Path

import zio.ZIO
import zio.test.{TestAspect, ZIOSpecDefault, assertTrue, suite, test}

/** A native module with no clang major, or two majors, fails before clang is probed. Node stays idle. */
object SurveyLawSpec extends ZIOSpecDefault:
  private val scala3 = ScalaVersion("3.10.0")
  private val jdk    = JdkSurvey(
    Path.of("/jdk"),
    JdkVersion.trusted("21.0.2", JdkMajor.trusted(21)),
    RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab")),
  )
  private val node = NodeSurvey(
    NodeBinary(Path.of("/node")),
    NodeVersion.trusted("25.0.0", NodeMajor.trusted(25)),
    RuntimeIdentity.Local(Path.of("/node"), Sha256.trusted("cd")),
  )
  private val bare    = Reached(jdk, None)
  private val reached = Reached(jdk, Some(node))
  private val missing = ProjectName.trusted("nativelaw")
  private val left    = ProjectName.trusted("leftlaw")
  private val right   = ProjectName.trusted("rightlaw")

  def spec = suite("survey laws")(
    test("shown keeps every line already collected, and a jdk failure has none"):
      val jdkLine    = Toolchain.text(jdk)
      val both       = s"$jdkLine\n${NodeToolchain.text(node)}"
      val undeclared = Survey.shown(SurveyError.Undeclared(bare, missing))
      val split      = Survey.shown(SurveyError.Split(reached, left, right))
      val clang      = Survey.shown(SurveyError.Clang(bare, ClangMiss.Absent))
      val nodeLine   = Survey.shown(SurveyError.Node(jdk, NodeMiss.Unresolved(NodeError.BadMajor(0))))
      assertTrue(
        Survey.shown(SurveyError.Jdk(ToolchainError.Denied)) == "",
        Survey.shown(SurveyError.Jdk(ToolchainError.NoUserHome)) == "",
        Survey.shown(SurveyError.Jdk(ToolchainError.NoArchitecture)) == "",
        Survey.shown(SurveyError.Incomplete("full report", "detail")) == "full report",
        undeclared == jdkLine,
        nodeLine == jdkLine,
        clang == jdkLine,
        split == both,
        Survey.clangProblem(ClangMiss.Absent).contains("clang is not on PATH"),
        Survey.clangProblem(ClangMiss.NotOnPath(ClangMajor(21))).contains("21"),
        Survey.clangProblem(ClangMiss.BadPath("nope")).contains("nope"),
        undeclared.contains("jdk 21.0.2"),
      )
    ,
    test("a native module with no clang major fails clang and does not ask for node"):
      Workspace
        .work("histos-survey-missing")
        .flatMap: dir =>
          Machine
            .load(dir)
            .flatMap: machine =>
              Survey
                .collect(machine, one(missing, ClangPin.Unused))
                .map: found =>
                  val jdkDetail = found.jdk match
                    case JdkRead.Chosen(_, _)     => ""
                    case JdkRead.Failed(error, _) => error.message
                  val clang = found.clang match
                    case ClangSlot.Failed(message) => message
                    case ClangSlot.Idle            => "idle"
                    case ClangSlot.Ready(_)        => "ready"
                  assertTrue(
                    jdkDetail == "",
                    found.node == NodeSlot.Idle,
                    clang.contains(missing.value),
                    clang.contains(NativeDeclaration.Missing(missing).message),
                  )
    ,
    test("two clang majors disagree before clang is probed"):
      Workspace
        .work("histos-survey-split")
        .flatMap: dir =>
          Machine
            .load(dir)
            .flatMap: machine =>
              val graph = ProjectGraph.trusted(
                List(
                  native(left, ClangPin.Major(ClangMajor(20))),
                  native(right, ClangPin.Major(ClangMajor(21))),
                ),
                Splits.aggregate,
                Catalog.empty,
                None,
                Nil,
              )
              Survey
                .collect(machine, graph)
                .map: found =>
                  val clang = found.clang match
                    case ClangSlot.Failed(message) => message
                    case ClangSlot.Idle            => "idle"
                    case ClangSlot.Ready(_)        => "ready"
                  assertTrue(
                    found.node == NodeSlot.Idle,
                    clang.contains(left.value),
                    clang.contains(right.value),
                    clang.contains("clang is resolved from PATH"),
                  ),
  ) @@ TestAspect.withLiveSystem

  private def one(name: ProjectName, clang: ClangPin): ProjectGraph =
    ProjectGraph.trusted(
      List(native(name, clang)),
      Splits.aggregate,
      Catalog.empty,
      None,
      Nil,
    )

  private def native(name: ProjectName, clang: ClangPin): Module =
    Module(
      name,
      Platforms(Platform.Native, Nil),
      ScalaVersions(scala3, Nil),
      Nil,
      Nil,
      SourcePos("law.scala", 1, 1),
      clang = clang,
    )
end SurveyLawSpec
