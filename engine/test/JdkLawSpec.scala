package histos.jdk

import histos.native.NativeOrigin
import histos.toolchain.Architecture
import histos.workspace.Workspace

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.*

object JdkLawSpec extends ZIOSpecDefault:
  def spec = suite("jdk laws")(
    test("a release file version is its major, and a missing, unclosed, or bad value is typed"):
      val parsed = JdkVersion.fromRelease("JAVA_VERSION=\"21.0.1\"")
      val plain  = JdkVersion.from("21.0.1")
      assertTrue(
        parsed match
          case Right(version) => version.major.value == 21 && version.value == "21.0.1"
          case Left(_)        => false
        ,
        plain match
          case Right(version) => version.major.value == 21
          case Left(_)        => false
        ,
        JdkVersion.from("nope") == Left(VersionError.Bad("nope")),
        JdkVersion.fromRelease("nope") == Left(ReleaseError.MissingKey),
        JdkVersion.fromRelease("JAVA_VERSION=\"21.0.1") == Left(ReleaseError.Unclosed),
        JdkVersion.fromRelease("JAVA_VERSION=\"nope\"") == Left(ReleaseError.BadVersion("nope")),
        parsed match
          case Right(version) =>
            JdkBuild.fromRelease("JAVA_RUNTIME_VERSION=\"21.0.1+7\"", version).value == "21.0.1+7" &&
            JdkBuild.fromRelease("nope", version).value == version.value
          case Left(_) => false,
      )
    ,
    test("an os name is mac, linux, windows, or other, and unix is linux because it contains nix"):
      assertTrue(
        Os.from("Darwin") == Os.Mac,
        Os.from("Linux") == Os.Linux,
        Os.from("Windows") == Os.Windows,
        Os.from("sunos") == Os.Other,
        Os.from("freebsd") == Os.Other,
        Os.from("unix") == Os.Linux,
      )
    ,
    test(
      "a missing toolchain file is not a running jdk, an empty file is the local override, and a home without javac is rejected"
    ):
      ZIO.scoped:
        Workspace
          .work("histos-jdk")
          .flatMap: repo =>
            val home = repo.resolve("jdk")
            for
              absent <- Toolchain.read(machine(repo, None))
              _      <- Workspace.writeString(repo.resolve(".histos").resolve("toolchain.local"), "")
              empty  <- Toolchain.read(machine(repo, None))
              _      <- Workspace.createDirectories(home)
              _      <- Workspace.writeString(
                repo.resolve(".histos").resolve("toolchain.local"),
                home.toAbsolutePath.toString,
              )
              rejected <- Toolchain.read(machine(repo, None))
              probed   <- Jdk.home(home)
            yield assertTrue(
              absent == JdkRead.Failed(ToolchainError.Absent(Toolchain.Floor), Some(JdkSource.Running)),
              empty == JdkRead.Failed(ToolchainError.Override(OverrideError.Empty), Some(JdkSource.Local)),
              rejected == JdkRead.Failed(
                ToolchainError.Rejected(home.toAbsolutePath, Reject.NoJavac),
                Some(JdkSource.Local),
              ),
              !probed.hasJavac,
              probed.release == Left(ReleaseError.NoFile),
            ),
  )

  private def machine(repo: Path, running: Option[Path]): Machine =
    Machine(
      Map.empty,
      "/home",
      Os.Mac,
      Architecture.Aarch64,
      repo,
      repo,
      running,
      NativeOrigin.Central,
    )
end JdkLawSpec
