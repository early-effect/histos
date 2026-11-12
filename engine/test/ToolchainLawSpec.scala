package histos.toolchain

import histos.jdk.{JdkRead, JdkRuntime, JdkSource, Os, Sha256, Toolchain, ToolchainError}
import histos.native.NativeOrigin
import histos.node.{NodeMajor, NodeShadow, NodeSight, NodeToolchain, NodeVersion}

import java.nio.file.Path

import zio.test.*

object ToolchainLawSpec extends ZIOSpecDefault:
  def spec = suite("toolchain laws")(
    test("a missing component is zero, a pre-release sorts below its release, and a build sorts above it"):
      assertTrue(
        VersionOrder.compare("1.2", "1.2.0") == 0,
        VersionOrder.compare("1.2.3", "1.2") > 0,
        VersionOrder.compare("1.9.0", "1.10.0") < 0,
        VersionOrder.atLeast("1.8.0_292", "8", stripJavaOne = true),
        !VersionOrder.atLeast("1.7.0", "8", stripJavaOne = true),
        !VersionOrder.atLeast("1.8", "8", stripJavaOne = false),
        VersionOrder.compare("1.2.3-RC1", "1.2.3") < 0,
        VersionOrder.compare("1.2.3-RC1", "1.2.2") > 0,
        VersionOrder.compare("1.2.3-RC10", "1.2.3-RC2") > 0,
        VersionOrder.compare("1.2.3-RC1+7", "1.2.3-RC1") > 0,
        VersionOrder.compare("1.2.3+7", "1.2.3") > 0,
        VersionOrder.compare("1.2.3+7", "1.2.3+13") < 0,
        !VersionOrder.atLeast("25.0.0-rc.1", "25", stripJavaOne = false),
      )
    ,
    test("the newest node on PATH is the release, not its release candidate"):
      val old       = NodeSight(Path.of("/old/bin/node"), NodeVersion.trusted("24.0.0", NodeMajor.trusted(24)))
      val candidate = NodeSight(Path.of("/rc/bin/node"), NodeVersion.trusted("25.0.0-rc.1", NodeMajor.trusted(25)))
      val release   = NodeSight(Path.of("/new/bin/node"), NodeVersion.trusted("25.0.0", NodeMajor.trusted(25)))
      assertTrue(
        NodeToolchain.shadow(List(old, release, candidate)) == Some(NodeShadow(old, release)),
        NodeToolchain.shadow(List(old, candidate, release)) == Some(NodeShadow(old, release)),
      )
    ,
    test("a host token round-trips, and an other os or an unsupported architecture has no token"):
      assertTrue(
        List(
          HostPlatform.MacosX86_64,
          HostPlatform.MacosAarch64,
          HostPlatform.LinuxX86_64,
          HostPlatform.LinuxAarch64,
          HostPlatform.WindowsX86_64,
          HostPlatform.WindowsAarch64,
        ).forall { platform =>
          platform.token match
            case Some(token) => HostPlatform.from(token) == Right(platform)
            case None        => false
        },
        HostPlatform.from("nope") == Left(HostPlatformError.Unknown("nope")),
        HostPlatform.of(Os.Mac, Architecture.Aarch64) == HostPlatform.MacosAarch64,
        HostPlatform.of(Os.Other, Architecture.X86_64).token == None,
        HostPlatform.of(Os.Mac, Architecture.Unsupported("sparc")).token == None,
        Architecture.fromMachine("x86_64") == Architecture.X86_64,
        Architecture.fromMachine("amd64") == Architecture.X86_64,
        Architecture.fromMachine("aarch64") == Architecture.Aarch64,
        Architecture.fromMachine("arm64") == Architecture.Aarch64,
        Architecture.fromMachine("sparc") == Architecture.Unsupported("sparc"),
      )
    ,
    test("a local override is one absolute path"):
      assertTrue(
        LocalOverride.parse(" \n") == Left(LocalOverrideError.Empty),
        LocalOverride.parse("/tmp/a\n/tmp/b") == Left(LocalOverrideError.Multiple),
        LocalOverride.parse("jdk") == Left(LocalOverrideError.Relative("jdk")),
        LocalOverride.parse("\u0000") == Left(LocalOverrideError.Invalid("\u0000")),
        LocalOverride.parse("/tmp/jdk") == Right(Path.of("/tmp/jdk")),
      )
    ,
    test("an idle node and clang say they are not required, and a failed jdk is not a survey"):
      val failed  = ToolchainError.Absent(Toolchain.Floor)
      val source  = Some(JdkSource.Running)
      val idle    = Collected(JdkRead.Failed(failed, source), NodeSlot.Idle, ClangSlot.Idle)
      val runtime = JdkRuntime(
        Path.of("/jdk"),
        Toolchain.Floor,
        RuntimeIdentity.Local(Path.of("/jdk"), Sha256.trusted("ab")),
      )
      val ready = Collected(JdkRead.Chosen(runtime, JdkSource.Running), NodeSlot.Idle, ClangSlot.Idle)
      assertTrue(
        idle.text.contains("node not required"),
        idle.text.contains("clang not required"),
        idle.text.contains(Toolchain.failure(failed, source)),
        idle.toSurvey == None,
        ready.toSurvey match
          case Some(survey) =>
            survey.node == None && survey.clang == None && survey.jdkFrom == JdkSource.Running.label
          case None => false
        ,
        NodeToolchain.Floor.value == "25",
      )
    ,
    test("an empty path list does not search the process path"):
      for
        missing <- RuntimeFiles.firstOnPath(None, List("sh"))
        empty   <- RuntimeFiles.firstOnPath(Some(""), List("sh"))
        invalid <- RuntimeFiles.firstOnPath(Some("\u0000"), List("sh")).exit
      yield assertTrue(
        missing == None,
        empty == None,
        invalid match
          case zio.Exit.Failure(cause) =>
            cause.failureOption.contains(RuntimeLocationError.InvalidPath("\u0000"))
          case zio.Exit.Success(_) => false,
      ),
  )
