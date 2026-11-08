package histos.doctor

import histos.{NodeFloor, NodePath, RunnerTools, ToolRevision}

import zio.test.*

object HygieneLawSpec extends ZIOSpecDefault:
  def spec = suite("hygiene laws")(
    test("a release finding is a warning, and a missing key is the command"):
      val credentials = ReleaseDiagnostic.CredentialsMissing()
      val rejected    = ReleaseDiagnostic.CredentialsRejected(401)
      val hex         = ReleaseDiagnostic.KeyHex()
      val missing     = ReleaseDiagnostic.KeyMissing("abcd")
      assertTrue(
        List(credentials, rejected, hex, missing).forall(_.level == Level.Warning),
        credentials.check.value == "central.credentials",
        rejected.check.value == "central.credentials",
        hex.check.value == "pgp.key",
        missing.check.value == "pgp.key",
        command(missing) == Some("gpg --keyserver keyserver.ubuntu.com --send-keys abcd"),
        steps(credentials).nonEmpty,
        steps(rejected).nonEmpty,
        steps(hex).nonEmpty,
      )
    ,
    test("a pages finding is a warning on pages.enabled"):
      val source = PagesDiagnostic.Source()
      val branch = PagesDiagnostic.Branch("main")
      assertTrue(
        source.level == Level.Warning,
        branch.level == Level.Warning,
        source.check.value == "pages.enabled",
        branch.check.value == "pages.enabled",
        steps(source) == List(PagesDiagnostic.fixText),
        steps(branch) == List(PagesDiagnostic.fixText),
        branch.summary.contains("main"),
      )
    ,
    test("toolchain findings and the explain catalog are the same checks"):
      val findings = List(
        ToolchainDiagnostic.Jdk("detail"),
        ToolchainDiagnostic.NodeFloor("detail"),
        ToolchainDiagnostic.NodeShadowed("export HISTOS_NODE=/usr/bin/node", "detail"),
        ToolchainDiagnostic.Clang("detail"),
        ToolchainDiagnostic.Cover("detail", "shape"),
        ToolchainDiagnostic.Agree("detail", "shape"),
      )
      val ids    = findings.map(_.check.value).toSet
      val levels = findings.map(item => item.check.value -> item.level).toMap
      assertTrue(
        ids == ToolchainExplain.known.split(", ").toSet,
        ids.forall(id => ToolchainExplain.text(id).isDefined),
        ToolchainExplain.text("not.a.check").isEmpty,
        levels == Map(
          "jdk.source"         -> Level.Error,
          "node.floor"         -> Level.Error,
          "node.shadowed"      -> Level.Warning,
          "clang.source"       -> Level.Error,
          "runner-tools.cover" -> Level.Error,
          "runner-tools.agree" -> Level.Error,
        ),
        command(ToolchainDiagnostic.NodeShadowed("export HISTOS_NODE=/usr/bin/node", "detail")) ==
          Some("export HISTOS_NODE=/usr/bin/node"),
        steps(ToolchainDiagnostic.Jdk("detail")).nonEmpty,
      )
    ,
    test("a runner call renders the options the build would write"):
      val path  = NodePath.trusted("say \"hi\"")
      val slash = NodePath.trusted("a\\b")
      val tools = RunnerTools(
        Some(NodeFloor.trusted(25)),
        Some(ToolRevision.trusted("21.0.0")),
        None,
        Some(path),
      )
      assertTrue(
        RunnerCall.show(RunnerTools.empty) == ".runnerTools(RunnerTools(None, None, None))",
        RunnerCall.show(tools).contains("Some(NodeFloor(25))"),
        RunnerCall.show(tools).contains("Some(ToolRevision(\"21.0.0\"))"),
        RunnerCall.show(RunnerTools(None, None, None, Some(path))).contains("""NodePath("say \"hi\"")"""),
        RunnerCall.show(RunnerTools(None, None, None, Some(slash))).contains("""NodePath("a\\b")"""),
      ),
  )

  private def command(diagnostic: Diagnostic): Option[String] =
    diagnostic.fixes.toList.collectFirst { case Fix.Command(line) => line.value }

  private def steps(diagnostic: Diagnostic): List[String] =
    diagnostic.fixes.toList.collect { case Fix.Steps(head :: _) => head }
end HygieneLawSpec
