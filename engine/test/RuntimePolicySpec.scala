package histos.toolchain

import histos.workspace.{IoFailure, IoFault, Workspace}

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.*

object RuntimePolicySpec extends ZIOSpecDefault:
  private val build = "25.0.5+8-LTS"

  def spec = suite("runtime identity")(
    test("an alias shares the published build and another build does not"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-policy")
          home <- directory(root.resolve("local"))
          alias = root.resolve("alias")
          _ <- ZIO
            .attemptBlocking(Files.createSymbolicLink(alias, home))
            .mapError(_ => IoFailure(alias, IoFault.Create))
          direct <- RuntimeFiles.identity(home, build)
          indirect <- RuntimeFiles.identity(alias, build)
          other <- RuntimeFiles.identity(home, "25.0.5+9-LTS")
          canonical <- ZIO.attemptBlocking(home.toRealPath().normalize)
        yield assertTrue(
          direct == indirect,
          direct != other,
          (direct match
            case RuntimeIdentity.Local(path, _) => path == canonical
          )
        )
    ,
    test("replacing a file inside the home leaves the salt"):
      ZIO.scoped:
        for
          root <- Workspace.work("histos-runtime-policy")
          home <- directory(root.resolve("local"))
          linked = root.resolve("linked")
          _ <- Workspace.writeString(linked, "first")
          _ <- ZIO
            .attemptBlocking(
              Files.createSymbolicLink(home.resolve("library"), linked)
            )
            .mapError(_ => IoFailure(home.resolve("library"), IoFault.Create))
          first <- RuntimeFiles.identity(home, build)
          _ <- Workspace.writeString(linked, "second")
          second <- RuntimeFiles.identity(home, build)
        yield assertTrue(first == second)
  )

  private def directory(home: Path): zio.IO[IoFailure, Path] =
    Workspace.createDirectories(home.resolve("bin")) *>
      Workspace.writeString(home.resolve("bin/javac"), "javac").as(home)
