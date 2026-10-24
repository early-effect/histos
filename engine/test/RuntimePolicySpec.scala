package histos.toolchain

import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.*

object RuntimePolicySpec extends ZIOSpecDefault:
  def spec = suite("runtime content identity")(
    test(
      "manifest exclusions use the canonical home even through a path alias"
    ):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-policy")
          home = root.resolve("content")
          alias = root.resolve("alias")
          _ <- Workspace.writeString(home.resolve("tool"), "content")
          _ <- Workspace.writeString(home.resolve("manifest"), "excluded")
          _ <- ZIO
            .attemptBlocking(Files.createSymbolicLink(alias, home))
            .mapError(_ => IoFailure(alias, IoFault.Create))
          direct <- RuntimeFiles.content(home, Set(home.resolve("manifest")))
          indirect <- RuntimeFiles.content(
            alias,
            Set(alias.resolve("manifest"))
          )
        yield assertTrue(direct == indirect)
    ,
    test("changing a linked local file changes the recorded content identity"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-policy")
          home <- jdk(root.resolve("local"))
          linked = root.resolve("linked")
          _ <- Workspace.writeString(linked, "first")
          _ <- ZIO
            .attemptBlocking(
              Files.createSymbolicLink(home.resolve("library"), linked)
            )
            .mapError(_ => IoFailure(home.resolve("library"), IoFault.Create))
          first <- RuntimeFiles.identity(home)
          _ <- Workspace.writeString(linked, "second")
          second <- RuntimeFiles.identity(home)
          canonical <- ZIO.attemptBlocking(home.toRealPath())
        yield assertTrue(
          (first match
            case RuntimeIdentity.Local(path, _) => path == canonical
          ),
          (second match
            case RuntimeIdentity.Local(path, _) => path == canonical
          ),
          first != second
        )
  )

  private def jdk(home: Path): zio.IO[IoFailure, Path] =
    Workspace.createDirectories(home.resolve("bin")) *>
      Workspace.writeString(home.resolve("bin/javac"), "javac") *>
      Workspace
        .writeString(home.resolve("release"), "JAVA_VERSION=\"21.0.2\"\n")
        .as(home)
