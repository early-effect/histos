package histos.toolchain

import histos.jdk.{
  JdkLock,
  JdkVersion,
  Os,
  Machine,
  Settled,
  Toolchain,
  ToolchainError,
  JdkStage,
  Sha256
}
import histos.archive.*
import histos.native.NativeOrigin
import histos.workspace.{Workspace, IoFailure, IoFault}
import heddle.client.{Client, ClientError}
import heddle.http.{Body, Request, Response}
import java.nio.file.Path
import zio.{Chunk, IO, Ref, Scope}
import scala.compiletime.testing.typeCheckErrors
import histos.node.{NodeLock, NodeVersion}

import zio.ZIO
import zio.test.*

object RuntimeHostSchemaSpec extends ZIOSpecDefault:
  private def corpus(name: String) =
    for
      raw <- Live
        .live(zio.System.env("HISTOS_REPO"))
        .mapError(_ => IoFailure(Path.of("."), IoFault.Read))
      root <- ZIO
        .attemptBlocking(Path.of(raw.getOrElse(".")))
        .mapError(_ => IoFailure(Path.of("."), IoFault.Read))
      text <- Workspace.readString(
        root.resolve("engine/test/fixtures/runtime-lock").resolve(name)
      )
    yield text

  private val rows =
    s"archive macos-aarch64 file:///fixtures/mac.tar.gz ${"aa" * 32}\n" +
      s"archive linux-x86_64 https://example.test/linux.tar.gz ${"bb" * 32}\n" +
      s"archive windows-x86_64 https://example.test/windows.zip ${"cc" * 32}\n"

  def spec = suite("host-specific runtime schema")(
    test("machine architecture aliases normalize, lock aliases do not"):
      assertTrue(
        HostPlatform.of(
          Os.from("Darwin"),
          Architecture.fromMachine("arm64")
        ) == HostPlatform.MacosAarch64,
        HostPlatform.of(
          Os.from("Linux"),
          Architecture.fromMachine("amd64")
        ) == HostPlatform.LinuxX86_64,
        HostPlatform.of(
          Os.from("Windows 11"),
          Architecture.fromMachine("x86_64")
        ) == HostPlatform.WindowsX86_64,
        Architecture.fromMachine(" ARM64 ") == Architecture.Aarch64,
        Architecture.fromMachine("riscv64") == Architecture.Unsupported(
          "riscv64"
        ),
        HostPlatform.from("macos-arm64") == Left(
          HostPlatformError.Unknown("macos-arm64")
        )
      )
    ,
    test(
      "Machine captures os.arch and does not reinterpret a later property change"
    ):
      TestSystem.save.flatMap: restore =>
        (for
          _ <- TestSystem.putProperty("user.home", "/fixtures")
          _ <- TestSystem.putProperty("os.name", "Mac OS X")
          _ <- TestSystem.putProperty("os.arch", "arm64")
          first <- Machine.load(Path.of("/fixtures/repo"))
          _ <- TestSystem.putProperty("os.arch", "riscv64")
          unknown <- Machine.load(Path.of("/fixtures/repo"))
          _ <- TestSystem.clearProperty("os.arch")
          missing <- Machine.load(Path.of("/fixtures/repo")).either
        yield assertTrue(
          first.architecture == Architecture.Aarch64,
          first.host == HostPlatform.MacosAarch64,
          unknown.host == HostPlatform
            .Unsupported(Os.Mac, Architecture.Unsupported("riscv64")),
          missing == Left(ToolchainError.NoArchitecture)
        )).ensuring(restore)
    ,
    test(
      "Windows JDK provisioning plans java.exe from the selected Windows row"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-windows-pin")
          bytes <- ArchiveFixture.zip(
            "jdk/bin/javac.exe" -> Chunk.empty,
            "jdk/bin/java.exe" -> Chunk.empty,
            "jdk/release" -> Chunk.fromArray(
              "JAVA_VERSION=\"21.0.2\"\n".getBytes(
                java.nio.charset.StandardCharsets.UTF_8
              )
            )
          )
          source = root.resolve("windows.zip")
          _ <- ZIO.attemptBlocking(
            java.nio.file.Files.write(source, bytes.toArray)
          )
          version <- ZIO.fromEither(JdkVersion.from("21.0.2"))
          lock <- ZIO.fromEither(
            JdkLock.from(
              version,
              Chunk(
                HostArchive(
                  HostPlatform.WindowsX86_64,
                  ArchivePin(ArchiveSource.Local(source), Sha256.of(bytes))
                )
              )
            )
          )
          _ <- Workspace.writeString(
            root.resolve(".histos/jdk.lock"),
            JdkLock.render(lock)
          )
          machine = Machine(
            Map.empty,
            root.toString,
            Os.Windows,
            Architecture.X86_64,
            root.resolve("cache"),
            root,
            None,
            Settled.No,
            NativeOrigin.Central
          )
          startup <- Toolchain.prepare(machine)
          canonical <- ZIO.attemptBlocking(root.toRealPath())
        yield assertTrue(
          startup == histos.jdk.Startup.Replace(
            canonical
              .resolve("cache/jdks")
              .resolve(Sha256.of(bytes).value)
              .resolve("unpack/jdk/bin/java.exe")
          )
        )
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
        )
      )
    ,
    test("the shared multi-host golden file selects each exact URL and digest"):
      for
        text <- corpus("multi-host.lock")
        lock <- ZIO.fromEither(JdkLock.parse(text))
        mac <- ZIO.fromEither(lock.archive(HostPlatform.MacosAarch64))
        linux <- ZIO.fromEither(lock.archive(HostPlatform.LinuxX86_64))
        windows <- ZIO.fromEither(lock.archive(HostPlatform.WindowsX86_64))
        expectedMac <- ZIO.fromEither(
          ArchiveSource.from("file:///fixtures/mac%20jdk.tar.gz")
        )
        expectedLinux <- ZIO.fromEither(
          ArchiveSource.from("https://example.test/jdk.tar.gz?revision=locked")
        )
        expectedWindows <- ZIO.fromEither(
          ArchiveSource.from("https://example.test/jdk.zip")
        )
        shaMac <- ZIO.fromEither(Sha256.from("aa" * 32))
        shaLinux <- ZIO.fromEither(Sha256.from("bb" * 32))
        shaWindows <- ZIO.fromEither(Sha256.from("cc" * 32))
      yield assertTrue(
        mac == ArchivePin(expectedMac, shaMac),
        linux == ArchivePin(expectedLinux, shaLinux),
        windows == ArchivePin(expectedWindows, shaWindows),
        JdkLock.render(lock) == text,
        lock.archive(HostPlatform.LinuxAarch64) == Left(
          ToolArchiveError.NoArchive(HostPlatform.LinuxAarch64)
        ),
        lock.archive(
          HostPlatform.Unsupported(Os.Other, Architecture.X86_64)
        ) == Left(
          ToolArchiveError.NoArchive(
            HostPlatform.Unsupported(Os.Other, Architecture.X86_64)
          )
        )
      )
    ,
    test(
      "host selection fetches only the selected row and missing hosts fail before any client call"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-host-selection")
          bytes <- ArchiveFixture.zip(
            "jdk/bin/javac" -> Chunk.empty,
            "jdk/bin/java" -> Chunk.empty,
            "jdk/release" -> Chunk.fromArray(
              "JAVA_VERSION=\"21.0.2\"\n".getBytes(
                java.nio.charset.StandardCharsets.UTF_8
              )
            )
          )
          version <- ZIO.fromEither(JdkVersion.from("21.0.2"))
          url <- ZIO.fromEither(
            HttpsUrl.from("https://example.test/selected-linux.zip")
          )
          other <- ZIO.fromEither(
            HttpsUrl.from("https://unrelated.invalid/windows.zip")
          )
          lock <- ZIO.fromEither(
            JdkLock.from(
              version,
              Chunk(
                HostArchive(
                  HostPlatform.MacosAarch64,
                  ArchivePin(
                    ArchiveSource.Local(root.resolve("must-not-read")),
                    Sha256.of(Chunk.empty)
                  )
                ),
                HostArchive(
                  HostPlatform.LinuxX86_64,
                  ArchivePin(ArchiveSource.Remote(url), Sha256.of(bytes))
                ),
                HostArchive(
                  HostPlatform.WindowsX86_64,
                  ArchivePin(
                    ArchiveSource.Remote(other),
                    Sha256.of(Chunk.empty)
                  )
                )
              )
            )
          )
          _ <- Workspace.writeString(
            root.resolve(".histos/jdk.lock"),
            JdkLock.render(lock)
          )
          calls <- Ref.make(List.empty[String])
          client = new Client:
            def batched(request: Request): IO[ClientError, Response] = calls
              .update(_ :+ request.url.render)
              .as(Response(body = Body.fromBytes(bytes)))
            def streaming(request: Request): ZIO[Scope, ClientError, Response] =
              batched(request)
          machine = Machine(
            Map.empty,
            root.toString,
            Os.Linux,
            Architecture.X86_64,
            root.resolve("cache"),
            root,
            None,
            Settled.No,
            NativeOrigin.Central
          )
          tools = Toolchain(
            JdkStage(ArchiveStage(client, CommonsArchiveDecoder()))
          )
          actual <- tools.resolve(machine)
          unlisted <- tools
            .resolve(machine.copy(architecture = Architecture.Aarch64))
            .either
          unsupported <- tools
            .resolve(
              machine.copy(architecture = Architecture.Unsupported("riscv64"))
            )
            .either
          fetched <- calls.get
        yield assertTrue(
          actual.version == version,
          actual.identity == RuntimeIdentity.Locked(Sha256.of(bytes)),
          fetched == List(url.value),
          unlisted == Left(
            ToolchainError.Archive(
              ToolArchiveError.NoArchive(HostPlatform.LinuxAarch64)
            )
          ),
          unsupported == Left(
            ToolchainError.Archive(
              ToolArchiveError.NoArchive(
                HostPlatform.Unsupported(
                  Os.Linux,
                  Architecture.Unsupported("riscv64")
                )
              )
            )
          )
        )
    ,
    test(
      "one JDK lock accepts independent macOS, Linux, and Windows archive pins"
    ):
      for
        expected <- ZIO.fromEither(JdkVersion.from("21.0.2"))
        actual = JdkLock
          .parse("major 21\nversion 21.0.2\n" + rows)
          .map(_.version)
      yield assertTrue(actual == Right(expected))
    ,
    test("Node uses the same host archive schema"):
      for
        expected <- ZIO.fromEither(NodeVersion.from("22.14.0"))
        actual = NodeLock
          .parse("major 22\nversion 22.14.0\n" + rows)
          .map(_.version)
      yield assertTrue(actual == Right(expected))
  )
