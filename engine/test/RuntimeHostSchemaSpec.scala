package histos.toolchain

import histos.jdk.{Machine, Os, ToolchainError}
import java.nio.file.Path
import scala.compiletime.testing.typeCheckErrors
import zio.ZIO
import zio.test.*

object RuntimeHostSchemaSpec extends ZIOSpecDefault:
  def spec = suite("host platform")(
    test("machine architecture aliases normalize"):
      assertTrue(
        HostPlatform.of(
          Os.from("Darwin"),
          Architecture.fromMachine("arm64"),
        ) == HostPlatform.MacosAarch64,
        HostPlatform.of(
          Os.from("Linux"),
          Architecture.fromMachine("amd64"),
        ) == HostPlatform.LinuxX86_64,
        HostPlatform.of(
          Os.from("Windows 11"),
          Architecture.fromMachine("x86_64"),
        ) == HostPlatform.WindowsX86_64,
        Architecture.fromMachine(" ARM64 ") == Architecture.Aarch64,
        Architecture.fromMachine("riscv64") == Architecture.Unsupported(
          "riscv64"
        ),
        HostPlatform.from("macos-arm64") == Left(
          HostPlatformError.Unknown("macos-arm64")
        ),
      )
    ,
    test(
      "Machine captures os.arch and does not reinterpret a later property change"
    ):
      TestSystem.save.flatMap: restore =>
        (for
          _       <- TestSystem.putProperty("user.home", "/fixtures")
          _       <- TestSystem.putProperty("os.name", "Mac OS X")
          _       <- TestSystem.putProperty("os.arch", "arm64")
          first   <- Machine.load(Path.of("/fixtures/repo"))
          _       <- TestSystem.putProperty("os.arch", "riscv64")
          unknown <- Machine.load(Path.of("/fixtures/repo"))
          _       <- TestSystem.clearProperty("os.arch")
          missing <- Machine.load(Path.of("/fixtures/repo")).either
        yield assertTrue(
          first.architecture == Architecture.Aarch64,
          first.host == HostPlatform.MacosAarch64,
          unknown.host == HostPlatform
            .Unsupported(Os.Mac, Architecture.Unsupported("riscv64")),
          missing == Left(ToolchainError.NoArchitecture),
        )).ensuring(restore)
    ,
    test("host literals are checked at compile time"):
      for
        _ <- ZIO.unit
        valid = typeCheckErrors(
          "import histos.toolchain.HostPlatform; val host = HostPlatform(\"linux-x86_64\")"
        )
        invalid = typeCheckErrors(
          "import histos.toolchain.HostPlatform; val host = HostPlatform(\"linux-amd64\")"
        )
      yield assertTrue(
        valid == Nil,
        invalid.map(_.message) == List(
          "unknown canonical runtime host: linux-amd64"
        ),
      ),
  )
