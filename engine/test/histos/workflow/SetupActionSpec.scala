package histos.workflow

import histos.suite.HistosSpec

import java.nio.file.{Files, Path}

import zio.ZIO
import zio.test.*

object SetupActionSpec extends HistosSpec:
  def spec = suite("the setup action")(
    test("caches the build store by commit and restores the newest os and arch store") {
      ZIO.attemptBlocking(actionText).map { found =>
        assertTrue(
          found.exists(_.contains("path: ~/.histos/build-cache")),
          found.exists(_.contains("key: histos-build-${{ runner.os }}-${{ runner.arch }}-${{ github.sha }}")),
          found.exists(_.contains("histos-build-${{ runner.os }}-${{ runner.arch }}-")),
        )
      }
    }
  )

  private def actionText: Option[String] =
    val start = Path.of(java.lang.System.getProperty("user.dir", ".")).toAbsolutePath.normalize
    walk(start).map(path => Files.readString(path))

  private def walk(dir: Path): Option[Path] =
    val file = dir.resolve(".github").resolve("actions").resolve("setup").resolve("action.yml")
    if Files.isRegularFile(file) then Some(file)
    else Option(dir.getParent).flatMap(walk)
end SetupActionSpec
